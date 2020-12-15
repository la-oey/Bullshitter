setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7/")

require(tidyverse)

source('getDataCounts.R')
source("all_models.R")

senderEval <- function(modelPredictions, counts){
  sum(log(modelPredictions)*counts)
}



# N = counts of k and k*
# sum_ k sum_k*  log(p(k*|k))*N(k*|k)
noToM.s.eval <- function(alph, eta.S, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(noToM.s.predMat(alph, eta.S))*ns)
}


noToM.r.eval <- function(alph, eta.R, ns.T, ns.F){
  callBSmat = noToM.r.pred(alph, eta.R)
  sum(log(callBSmat)*ns.T + log(1-callBSmat)*ns.F)
}

noToM.LL <- function(alph, eta.S, eta.R){
  ns.l = humanLieCounts
  ns.T = humanDetectCounts.T
  ns.F = humanDetectCounts.F

  neg.log.lik = -noToM.r.eval(alph, eta.R, ns.T, ns.F) - noToM.s.eval(alph, eta.S, ns.l)
  neg.log.lik
}

noToM.fit <- summary(mle(noToM.LL,
                         start=list(alph=rnorm(1, 1, 0.5),
                                    eta.S=rnorm(1, 0, 1),
                                    eta.R=rnorm(1, 0, 1)),
                         method = "BFGS"))
noToM.fit

noToM.s.eval(noToM.fit@coef['alph','Estimate'], noToM.fit@coef['eta.S','Estimate'], humanLieCounts)
noToM.r.eval(noToM.fit@coef['alph','Estimate'], noToM.fit@coef['eta.R','Estimate'], humanDetectCounts.T, humanDetectCounts.F)


# iterate.D(1, 1, 1, 0.5, rep(0.5,11))
# recurseToM.matrix(1, 7, 6, 1, 0.5)[[1]]


# recurseToM.weighted <- function(alph, eta.S, eta.R, util, p, lambda){
#   matrices <- recurseToM.matrix(alph, eta.S, eta.R, util, p)
#   n.depths = dim(matrices[[1]])[2] # should be equal to dim(matrices[[2]])[3]
#   weightedR <- rowMeans(matrices[[1]])
#   weightedS <- apply(matrices[[2]], MARGIN=c(1,2), FUN=mean)
#   return(list(weightedR, weightedS))
# }
# recurseToM.weighted(1,7,6,1,0.5,0.1)


recurseToM.conds <- function(alph, eta.S, eta.R, lambda){
  #lambda = logitToProb(lambda)
  n.depths = 25
  store.ksay.k.full = array(NA, dim = c(11, 11, 6))
  store.bs.ksay.full = array(NA, dim = c(11, 6))
  utils = c(1,-1)
  ps = c(0.2, 0.5, 0.8)
  for(u in 1:length(utils)){
    for(p in 1:length(ps)){
      matr <- recurseToM.weighted(alph, eta.S, eta.R, utils[u], ps[p], lambda)
      store.bs.ksay.full[,(u-1)*length(ps)+p] <- matr[[1]]
      store.ksay.k.full[,,(u-1)*length(ps)+p] <- matr[[2]]
    }
  }
  return(list(store.bs.ksay.full, store.ksay.k.full))
}

recurseToM.s.eval <- function(matr, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(matr)*ns)
}
# recurseToM.s.eval(recurseToM.conds(1, 7, 6, 0.1)[[2]], ns.l)


recurseToM.r.eval <- function(matr, ns.T, ns.F){ #ns = 11 x 6 matrix of counts for all conditions
  sum(log(matr)*ns.T + log(1-matr)*ns.F)
}

# recurseToM.r.eval(recurseToM.conds(1, 7, 6, 0.1)[[1]],humanDetectCounts.T, humanDetectCounts.F)

recurseToM.LL <- function(alph, eta.S, eta.R){
  lambda = 0.1
  ns.l = array(humanLieCounts, dim=c(11,11,6))
  ns.T = humanDetectCounts.T
  ns.F = humanDetectCounts.F
  
  recurseToM.mat <- recurseToM.conds(alph, eta.S, eta.R, lambda)
  r.eval = -recurseToM.r.eval(recurseToM.mat[[1]], ns.T, ns.F)
  s.eval = - recurseToM.s.eval(recurseToM.mat[[2]], ns.l)
  print(paste("alph =", alph, "; lambda =", logitToProb(lambda), "; r =", r.eval, "; s =", s.eval))
  neg.log.lik =  r.eval + s.eval
  neg.log.lik
}

start_time <- Sys.time()
recurseToM.fit <- summary(mle(recurseToM.LL,
                                start=list(alph=rnorm(1, 1, 0.2),
                                           eta.S=rnorm(1, 0, 1),
                                           eta.R=rnorm(1, 0, 1)),
                                method = "BFGS"))
Sys.time() - start_time
recurseToM.fit25 <- recurseToM.fit
recurseToM.fit40 <- recurseToM.fit


recurseToM.r.fit6
recurseToM.r.fit8
recurseToM.r.fit10
