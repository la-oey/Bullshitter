# null models of all stripes


predictLiarRandom <- function(params, expt){
  M = expt$numMarbles+1
  P = matrix(1/M, ncol=M, nrow=M)
  return(list('liar'=P))
}

predictDetectorRandom = function(params, expt){
  M = expt$numMarbles+1
  P = matrix(1/2, ncol=2, nrow=M)
  return(list('detector'=P))
}


predictLiarMax = function(params, expt){
  M = expt$numMarbles+1
  lie_idx = ifelse(expt$utilSign == 1, M, 1)
  P = matrix(params$prob_unif/(M-1), nrow=M, ncol = M)
  P[,lie_idx] = (1-params$prob_unif)
  return(list('liar'=P))
}

predictLiarTruth = function(params, expt){
  M = expt$numMarbles+1
  P = matrix(params$prob_unif/(M-1), nrow=M, ncol = M)
  diag(P) = (1-params$prob_unif)
  return(list('liar'=P))
}

predictLiarPoisson <- function(params, expt){
  # params  lambda, weight
  
  M = expt$numMarbles+1
  KSAY = matrix(rep(0:expt$numMarbles, each=M), ncol=M)
  K = matrix(rep(0:expt$numMarbles, M), ncol=M)
  signedDiff = (KSAY-K)*expt$utilSign
  if(expt$utilSign==1){
    normalizer = ppois(10-K, params$lambda)
  } else {
    normalizer = ppois(K, params$lambda)
  }
  P = dpois((KSAY-K)*expt$utilSign, params$lambda)/normalizer
  P = params$prob_unif*P + (1-params$prob_unif)*(1/M)
  return(list('liar'=P))
}


predictLiarPoissonTrue <- function(params, expt){
  # params  lambda, weight

  M = expt$numMarbles+1
  KSAY = matrix(rep(0:expt$numMarbles, each=M), ncol=M)
  K = matrix(rep(0:expt$numMarbles, M), ncol=M)
  signedDiff = (KSAY-K)*expt$utilSign
  if(expt$utilSign==1){
    normalizer = ppois(10-K, params$lambda)
  } else {
    normalizer = ppois(K, params$lambda)
  }
  P = dpois((KSAY-K)*expt$utilSign, params$lambda)/normalizer
  P = params$prob_unif*P + (1-params$prob_unif)*(1/M)
  P = (1-params$prob_true)*P + (K==KSAY)*params$prob_true
  return(list('liar'=P))
}

predictDetectorNHST <- function(params, expt){
  kstar = 0:expt$numMarbles
  pval = ifelse(kstar <= expt$numMarbles * expt$p, 
                  pbinom(kstar, expt$numMarbles, expt$p, lower.tail=T), 
                  pbinom(kstar-1, expt$numMarbles, expt$p, lower.tail=F))
  pval = pmin(1-1e-5, pmax(1e-5, logistic(logit(pval)*params$b+params$a)))
  return(list('detector'=cbind(1-pval, pval)))
}
