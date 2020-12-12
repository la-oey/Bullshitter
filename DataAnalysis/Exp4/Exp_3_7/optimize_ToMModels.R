setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7/")

source("lying_modelFunctions_paramfitting.R")
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








mapply(p.D_bs.ksay.r,0:10, i, lastlvl=TRUE, iterate.L(d,i, rep(0.5,11)))

recurseToM.s.depth <- function(alph, eta.S, depth){
  mapply(
    function(i, j){
      p.L_ksay.k.r(j, alph, eta.S, i, lastlvl=TRUE, iterate.D(depth, j, alph, i, rep(0.5,11)))
    }, 
    rep(c(0.2,0.5,0.8), 2), 
    rep(rep(c(1,-1), each=3))
  )
}

recurseToM.s.weighted <- function(alph, eta.S, lambda){
  levels = 25 # -> 40
  apply(
    mapply(function(d){recurseToM.s.depth(alph, eta.S, d)*dgeom(d, lambda) / sum(dgeom(0:levels, lambda))}, 0:levels),
    1, sum
  )
}

recurseToM.s.eval <- function(alph, eta.S, lambda, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(recurseToM.s.weighted(alph, eta.S, lambda))*ns)
}
recurseToM.s.eval(1,7,0.1, humanLieCounts)

recurseToM.s.LL <- function(alph, eta.S){
  lambda = 0.1
  ns = humanLieCounts
  neg.log.lik = -recurseToM.s.eval(alph, eta.S, lambda, ns)
  neg.log.lik
}

recurseToM.s.fit <- summary(mle(recurseToM.s.LL,
                        start=list(alph=rnorm(1, 1, 0.2),
                                   eta.S=rnorm(1, 0, 1)),
                        method = "BFGS"))
recurseToM.s.fit






recurseToM.r.depth <- function(alph, eta.R, depth){
  mapply(
    function(i, j){
      mapply(p.D_bs.ksay.r,0:10, i, j, alph, eta.R, lastlvl=TRUE, iterate.L(depth,j, alph, i, rep(0.5,11)))
    }, 
    rep(c(0.2,0.5,0.8), 2), 
    rep(rep(c(1,-1), each=3))
  )
}
recurseToM.r.depth(1,7,1)

recurseToM.r.weighted <- function(alph, eta.R, lambda){
  levels = 5 # -> 40
  apply(
    mapply(function(d){recurseToM.r.depth(alph, eta.R, d)*dgeom(d, lambda) / sum(dgeom(0:levels, lambda))}, 0:levels),
    1, sum
  )
}
recurseToM.r.weighted(1,7,1)

recurseToM.r.eval <- function(alph, eta.R, lambda, ns.T, ns.F){ #ns = 121 x 6 matrix of counts for all conditions
  callBSmat = recurseToM.r.weighted(alph, eta.R, lambda)
  sum(log(callBSmat)*ns.T + log(1-callBSmat)*ns.F)
}
recurseToM.r.eval(1,7,0.1, humanDetectCounts.T, humanDetectCounts.F)

recurseToM.r.LL <- function(alph, eta.R){
  lambda = 0.1
  ns.T = humanDetectCounts.T
  ns.F = humanDetectCounts.F
  neg.log.lik = -recurseToM.r.eval(alph, eta.R, lambda, ns.T, ns.F)
  neg.log.lik
}

recurseToM.r.fit <- summary(mle(recurseToM.r.LL,
                                start=list(alph=rnorm(1, 1, 0.2),
                                           eta.R=rnorm(1, 0, 1)),
                                method = "BFGS"))
recurseToM.r.fit
