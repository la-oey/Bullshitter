source(here::here('model/fitting/helpers.R'))

# variables describing trials
# ksay [0 N], k [0, N], bs {T,F}, lie {T F}

# variables describing experiment parameters:
# BET {0.8}, liePenalty, faPenalty, numMarbles  (constant for all)
# p [0 1], utilSign {-1, 1},   (varies by condition)
# expt = list()
# expt$BET = 0.8
# expt$liePenalty = 10 # 10 # -5
# expt$faPenalty = 5 # 5 # 0
# expt$numMarbles = 10
# expt$utilSign = 1
# expt$p = 0.5


# model parameters
# params = list()
# params$lambda = 5
# params$detector = list('aversion' = 1,
#                        'softmax' = 0.5,
#                        'liar0' = 0.5)
# params$liar = list('aversion' = 1,
#                    'softmax' = 0.5,
#                    'detector0' = 0.1)



pK <- function(k, expt) {
  expt$BET*dbinom(k, expt$numMarbles, expt$p) + (1-expt$BET)*1/(expt$numMarbles+1)
}

liarValue <- function(ksay, lie, BS, expt) {
  utility = case_when(
    !BS ~ expt$utilSign*(2*ksay - expt$numMarbles),
    !lie ~ expt$utilSign*(2*ksay - expt$numMarbles) + expt$faPenalty,
    lie ~ -expt$liePenalty  # confirm that this is correct
  )
  return(utility)
}

detectorValue <- function(ksay, lie, BS, expt) {
  utility = case_when(
    !BS ~ expt$utilSign*(-2*ksay + expt$numMarbles),
    !lie ~ expt$utilSign*(-2*ksay + expt$numMarbles) - expt$faPenalty ,
    lie ~ expt$liePenalty
  )
  return(utility)
}

detectorEV_BS <- function(pLie, expt) {
  # ksay is a vector
  # plie is also a vector
  tmp <- expand.grid(ksay = 0:(expt$numMarbles), 
                     bs = c(T,F), 
                     lie = c(T,F))
  KSAY = tmp$ksay
  LIE = tmp$lie
  BS = tmp$bs
  P = pLie[KSAY+1]
  P = ifelse(LIE, P, 1-P)
  V = detectorValue(KSAY, LIE, BS, expt)
  EV = rowSums(matrix(V, ncol=2)*matrix(P, ncol=2))
  EV = matrix(EV, ncol=2)
  return(EV)
}


liarEV_KsayK <- function(pBS, expt) { 
  # pBS is P(BS | ksay) for all valid ksay [0:numMarbles]
  tmp <- expand.grid(k = 0:expt$numMarbles, 
              ksay = 0:expt$numMarbles,
              BS = c(TRUE, FALSE))
  K = tmp$k
  KSAY = tmp$ksay
  BS = tmp$BS
  P = pBS[KSAY+1]
  P = ifelse(BS, P, 1-P)
  V = liarValue(KSAY, lie = KSAY!=K, BS, expt)
  dims = c(expt$numMarbles+1, expt$numMarbles+1, 2)
  EV = apply(array(V, dim = dims)*array(P, dim = dims),
             MARGIN = c(1,2),
             FUN = sum)
  return(EV)
}

# collapses PK and PKSAY into pLie | KSAY, which is what the detector wants.
p_lie.ksay <- function(prob.k, prob.ksay.k) {
  M = length(prob.k)
  P.K <- matrix(rep(prob.k, each=M), nrow=M)
  LIE = 1-diag(M)
  rowSums(P.K*prob.ksay.k*LIE)/rowSums(P.K*prob.ksay.k)
}

predictTOM = function(params, expt){
  n.depths = pmin(40, pmax(1, qpois(0.999, params$lambda)))
  M = expt$numMarbles+1
  store.ev.ksay.k = array(NA, dim = c(M, M, n.depths))
  store.ev.bs.ksay = array(NA, dim = c(M, 2, n.depths))
  
  for(depth in 1:n.depths){
    if(depth == 1){
      store.ev.bs.ksay[,1,depth] = logit(params$liar$d0_prob)
      store.ev.bs.ksay[,2,depth] = 0
      
    } else { 
      # TODO: woah there, average EVs or average Ps?
      ksay_ev = poissonAverage(store.ev.ksay.k[,,1:(depth-1)],
                                 params$lambda, 3)
      p_lie = p_lie.ksay(pK(0:expt$numMarbles, expt),
                         softmaxMat(params$liar$softmax, ksay_ev))
      store.ev.bs.ksay[,,depth] = detectorEV_BS(pLie = p_lie,
                                               expt)
    } 
    bs_ev = poissonAverage(store.ev.bs.ksay[,,1:(depth-1)],
                           params$lambda, 3)
    
    pBS = softmaxMat(params$detector$softmax, bs_ev)

    store.ev.ksay.k[,,depth] = liarEV_KsayK(pBS, expt)
  }
  
  bs_ev = poissonAverage(store.ev.bs.ksay, params$lambda, 3)
  bs_ev[,2] = bs_ev[,2] + params$detector$aversion
  
  predictions = list()
  predictions$detector = softmaxMat(params$detector$softmax, bs_ev)

  ksay_ev = poissonAverage(store.ev.ksay.k, params$lambda, 3)
  diag(ksay_ev) = diag(ksay_ev) + params$liar$aversion

  predictions$liar = softmaxMat(params$liar$softmax, ksay_ev)
  
  return(predictions)
}


predictLiar1 <- function(params, expt){
  ksay_ev = liarEV_KsayK(pBS = rep(params$liar$d0_prob, expt$numMarbles+1), expt)
  diag(ksay_ev) = diag(ksay_ev) + params$liar$aversion
  return(list('liar' = softmaxMat(params$liar$softmax, ksay_ev)))
}

predictDetector1 <- function(params, expt){
  # liar0 = mixture model liar
  ksay_p0 = diag(expt$numMarbles+1)*params$detector$l0_prob + (1-params$detector$l0_prob)*1/(expt$numMarbles+1)
  pLie = p_lie.ksay(pK(0:expt$numMarbles, expt), ksay_p0)
  bs_ev = detectorEV_BS(pLie, expt)
  bs_ev[,2] = bs_ev[,2] + params$detector$aversion
  return(list('detector' = softmaxMat(params$detector$softmax, bs_ev)))
}

predictRational1 <- function(params, expt){
  return(c(predictLiar1(params, expt),
           predictDetector1(params, expt)))
  
}

