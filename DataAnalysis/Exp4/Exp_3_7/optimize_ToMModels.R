setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7/")

source("lying_modelFunctions_paramfitting_ed.R")
library(tidyverse)
bs.final <- read.csv("bsfinal_anon.csv")
humanLie <- bs.final %>%
  filter(roleCurrent == "bullshitter")
humanDetect <- bs.final %>%
  filter(roleCurrent == "bullshitDetector")

# 121 x 6 matrix
humanLieCounts <- humanLie %>%
  count(expt, probabilityRed, drawnRed, reportedDrawn) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), drawnRed=0:10, reportedDrawn=0:10, fill = list(n = 0)) %>%
  pull(n) %>%
  matrix(nrow=121)

unlist(list(matrix(c(5,4,1,2), nrow=2), c(1,2,3)))
# 22 x 6 matrix
humanDetectCounts <- humanDetect %>%
  count(expt, probabilityRed, reportedDrawn, callBS) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), reportedDrawn=0:10, callBS=c("True","False"), fill = list(n = 0))
  
humanDetectCounts.T <- humanDetectCounts %>%
  filter(callBS=="True") %>%
  pull(n) %>%
  matrix(nrow=11)
humanDetectCounts.F <- humanDetectCounts %>%
  filter(callBS=="False") %>%
  pull(n) %>%
  matrix(nrow=11)

noToM.s.predMat <- function(alph, eta.S){
  mapply(function(i, j){p.L_ksay.k.r(j, alph, eta.S, i, lastlvl=TRUE, rep(0.5,11))}, rep(c(0.2,0.5,0.8), 2), rep(c(1,-1), each=3))
}

colSums(noToM.s.predMat(1,6))

# N = counts of k and k*
# sum_ k sum_k*  log(p(k*|k))*N(k*|k)
noToM.s.eval <- function(alph, eta.S, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(noToM.s.predMat(alph, eta.S))*ns)
}

# noToM.s.LL <- function(alph, eta.S){
#   ns = humanLieCounts
#   neg.log.lik = -noToM.s.eval(alph, eta.S, ns)
#   neg.log.lik
# }
# 
# noToM.s.fit <- summary(mle(noToM.s.LL,
#                         start=list(alph=rnorm(1, 1, 0.2),
#                                    eta.S=rnorm(1, 0, 1)),
#                         method = "BFGS"))
# noToM.s.fit



numMarbles = 10
p_t.ksay.r_p.L <- function(p, p.L) { #probability of not telling the truth
  P.K <- matrix(rep(p.k(0:numMarbles, p), each=numMarbles+1), nrow=numMarbles+1)
  P.L_KSAY.K <- matrix(p.L, nrow=numMarbles+1, ncol=numMarbles+1)
  LIE = 1-diag(numMarbles+1)
  rowSums(P.K*P.L_KSAY.K*LIE)/rowSums(P.K*P.L_KSAY.K)
}

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

nullVec(0.2,0:10)


noToM.r.pred <- function(alph, eta.R){
  matrix(
    mapply(function(i,j,k) p.D_bs.ksay.r(i, j, k, alph, eta.R, lastlvl=TRUE, nullVec(j, i)), rep(0:10,6), rep(rep(c(0.2,0.5,0.8),each=11),2), rep(c(1,-1), each=33)),
    nrow=11
  )
}

noToM.r.pred(0.07,7)
humanDetectCounts.F
noToM.r.eval <- function(alph, eta.R, ns.T, ns.F){
  callBSmat = noToM.r.pred(alph, eta.R)
  sum(log(callBSmat)*ns.T + log(1-callBSmat)*ns.F)
}

# noToM.r.LL <- function(alph, eta.R){
#   ns.T = humanDetectCounts.T
#   ns.F = humanDetectCounts.F
#   
#   neg.log.lik = -noToM.r.eval(alph, eta.R, ns.T, ns.F)
#   neg.log.lik
# }
#
# noToM.r.fit <- summary(mle(noToM.r.LL,
#                            start=list(alph=rnorm(1, 1, 0.2),
#                                       eta.R=rnorm(1, 0, 1)),
#                            method = "BFGS"))
# noToM.r.fit



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
      store.bs.ksay.simple[,depth] = mapply(p.D_bs.ksay.r,
                                     0:10,
                                     p,
                                     util,
                                     alph,
                                     eta.R,
                                     lastlvl=FALSE,
                                     p_true.ksay(p.k(0:numMarbles, p),
                                                 apply(store.ksay.k.simple[,,1:(depth-1)], MARGIN = c(1, 2), FUN = mean))) 
      store.bs.ksay[,depth] = mapply(p.D_bs.ksay.r,
                                     0:10,
                                     p,
                                     util,
                                     alph,
                                     eta.R,
                                     lastlvl=TRUE,
                                     p_true.ksay(p.k(0:numMarbles, p),
                                                 apply(store.ksay.k.simple[,,1:(depth-1)], MARGIN = c(1, 2), FUN = mean))) #mean of previous levels; weigh?
      
    }
    store.ksay.k.simple[,,depth] = p.L_ksay.k.r(util,
                                         alph,
                                         eta.S,
                                         p,
                                         lastlvl=FALSE,
                                         apply(matrix(store.bs.ksay.simple[,1:depth], nrow=numMarbles+1), MARGIN = 1, FUN = mean))
    store.ksay.k[,,depth] = p.L_ksay.k.r(util,
                                         alph,
                                         eta.S,
                                         p,
                                         lastlvl=TRUE,
                                         apply(matrix(store.bs.ksay.simple[,1:depth], nrow=numMarbles+1), MARGIN = 1, FUN = mean))
  }
  return(list(store.bs.ksay, store.ksay.k))
  # [[1]] receiver P(BS | k*)
  # [[2]] sender P(k* | k)
}
iterate.D(1, 1, 1, 0.5, rep(0.5,11))
recurseToM.matrix(1, 7, 6, 1, 0.5)[[1]]


recurseToM.weighted <- function(alph, eta.S, eta.R, util, p, lambda){
  matrices <- recurseToM.matrix(alph, eta.S, eta.R, util, p)
  n.depths = dim(matrices[[1]])[2] # should be equal to dim(matrices[[2]])[3]
  weightedR <- rowSums(matrices[[1]]*rep(dgeom(0:(n.depths-1), lambda), each=11))
  weightedS <- apply(matrices[[2]]*rep(dgeom(0:(n.depths-1), lambda), each=11*11), MARGIN=c(1,2), FUN=sum)
  return(list(weightedR, weightedS))
}

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
recurseToM.s.eval(recurseToM.conds(1, 7, 6, 0.1)[[2]], ns.l)


recurseToM.r.eval <- function(matr, ns.T, ns.F){ #ns = 11 x 6 matrix of counts for all conditions
  sum(log(matr)*ns.T + log(1-matr)*ns.F)
}
recurseToM.r.eval(recurseToM.conds(1, 7, 6, 0.1)[[1]],humanDetectCounts.T, humanDetectCounts.F)



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
