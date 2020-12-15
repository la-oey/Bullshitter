source('lying_modelFunctions_paramfitting_ed.R')

# random sender
p_t.ksay.r_p.L <- function(p, p.L) { #probability of not telling the truth
  P.K <- matrix(rep(p.k(0:numMarbles, p), each=numMarbles+1), nrow=numMarbles+1)
  P.L_KSAY.K <- matrix(p.L, nrow=numMarbles+1, ncol=numMarbles+1)
  LIE = 1-diag(numMarbles+1)
  rowSums(P.K*P.L_KSAY.K*LIE)/rowSums(P.K*P.L_KSAY.K)
}

# random receiver


noToM.s.predMat <- function(alph, eta.S){
  mapply(function(i, j){p.L_ksay.k.r(j, alph, eta.S, i, lastlvl=TRUE, rep(0.5,11))}, rep(c(0.2,0.5,0.8), 2), rep(c(1,-1), each=3))
}


# no TOM receiver sender input.
null0.2 <- p_t.ksay.r_p.L(0.2, rep(1/11,11))
null0.5 <- p_t.ksay.r_p.L(0.5, rep(1/11,11))
null0.8 <- p_t.ksay.r_p.L(0.8, rep(1/11,11))

nullVec <- function(br, kstar){
  case_when(
    br == 0.2 ~ null0.2[kstar+1],
    br == 0.5 ~ null0.5[kstar+1],
    br == 0.8 ~ null0.8[kstar+1]
  )
}


noToM.r.pred <- function(alph, eta.R){
  matrix(
    mapply(function(i,j,k) p.D_bs.ksay.r(i, j, k, alph, eta.R, lastlvl=TRUE, nullVec(j, i)), rep(0:10,6), rep(rep(c(0.2,0.5,0.8),each=11),2), rep(c(1,-1), each=33)),
    nrow=11
  )
}




poissonAverage = function(input, lambda){
  dims = dim(input)
  d = length(dims)
  ndepths = dims[d]
  p.depth = dpois(0:(ndepths-1), lambda) / ppois(ndepths-1, lambda)
  apply(input*array(rep(p.depth, each = prod(dims[-d])), dim = dims),
        MARGIN = (1:(d-1)),
        FUN = sum)
}



recurseToM.matrix <- function(alph, eta.S, eta.R, util, p){
  n.depths = 25
  prior = rep(0.5,11)
  store.ksay.k = array(NA, dim = c(11, 11, n.depths))
  store.ksay.k.simple = array(NA, dim = c(11, 11, n.depths))
  store.bs.ksay = array(NA, dim = c(11, n.depths))
  store.bs.ksay.simple = array(NA, dim = c(11, n.depths))
  for(depth in 1:n.depths){
    if(depth == 1){
      store.bs.ksay[,depth] = prior
      store.bs.ksay.simple[,depth] = prior
    } else {
      marginalized.caller = poissonAverage(store.ksay.k.simpe[,,1:(depth-1)], lambda)
      
      store.bs.ksay.simple[,depth] = mapply(p.D_bs.ksay.r,
                                            0:10,
                                            p,
                                            util,
                                            alph,
                                            eta.R,
                                            lastlvl=FALSE,
                                            p_true.ksay(p.k(0:numMarbles, p),
                                                        marginalized.caller)) 
      store.bs.ksay[,depth] = mapply(p.D_bs.ksay.r,
                                     0:10,
                                     p,
                                     util,
                                     alph,
                                     eta.R,
                                     lastlvl=TRUE,
                                     p_true.ksay(p.k(0:numMarbles, p),
                                                 marginalized.caller)) #mean of previous levels; weigh?
      
    }
    marginalized.liar = poissonAverage(matrix(store.bs.ksay.simple[,1:depth], nrow=numMarbles+1), lambda)
    
    store.ksay.k.simple[,,depth] = p.L_ksay.k.r(util,
                                                alph,
                                                eta.S,
                                                p,
                                                lastlvl=FALSE,
                                                marginalized.liar)
    store.ksay.k[,,depth] = p.L_ksay.k.r(util,
                                         alph,
                                         eta.S,
                                         p,
                                         lastlvl=TRUE,
                                         marginalized.liar)
  }
  return(list(store.bs.ksay, store.ksay.k))
  # [[1]] receiver P(BS | k*)
  # [[2]] sender P(k* | k)
}



recurseToM.weighted <- function(alph, eta.S, eta.R, util, p, lambda){
  matrices <- recurseToM.matrix(alph, eta.S, eta.R, util, p)
  n.depths = dim(matrices[[1]])[2] # should be equal to dim(matrices[[2]])[3]
  weightedR <- poissonAverage(martrices[[1]], lambda)
  # weightedR <- rowSums(matrices[[1]]*rep(dgeom(0:(n.depths-1), lambda), each=11))
  weightedS <- poissonAverage(martrices[[2]], lambda)
  # weightedS <- apply(matrices[[2]]*rep(dgeom(0:(n.depths-1), lambda), each=11*11), MARGIN=c(1,2), FUN=sum)
  return(list(weightedR, weightedS))
}