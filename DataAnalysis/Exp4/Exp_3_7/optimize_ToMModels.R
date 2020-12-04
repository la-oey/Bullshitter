setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7/")

source("lying_modelFunctions_paramfitting.R")
library(tidyverse)
bs.final <- read.csv("bsfinal_anon.csv")
humanLie <- bs.final %>%
  filter(roleCurrent == "bullshitter")
humanDetect <- bs.final %>%
  filter(roleCurrent == "bullshitDetector")

#p.L_ksay.k.r("expt4", 0.25, 10, 0.5, TRUE, iterate.D(0, "expt4", 0.25, 0.5))[0+2, 0+1]
#p.L_ksay.k.r <- function(expt, alph, eta.L, p, lastlvl=FALSE, p.D)
#iterate.D <- function(iter, expt, alph, p, prior=rep(0.1,11))
p.D_bs.ksay.r(0:10, 0.5, "expt4", 0.25, 7, lastlvl=TRUE, iterate.L(0,"expt4", 0.25, 0.5))
EV.D_bs.ksay.r(0, 0.5, TRUE, "expt4", 7, TRUE, rep(0.1,11))
p.D_bs.ksay.r(0, 0.5, "expt4", 0.25, 7, lastlvl=TRUE, rep(0.1,11))
mapply(p.D_bs.ksay.r, 0, 0.5, "expt4", 0.25, 7, lastlvl=TRUE, rep(0.1,11))
mapply(p.D_bs.ksay.r, 0:10, 0.5, "expt4", 0.25, 7, lastlvl=TRUE, p_t.ksay.r_p.L(0.5, rep(1/11,11)))[1]


noToM.s.eval <- function(k, kstar, util, br, alph, eta.S){
  log(p.L_ksay.k.r.i(k, kstar, util, alph, eta.S, br, TRUE, 0.5))
}
round(p.L_ksay.k.r(1, 1, 7, 0.5, TRUE, 0.5),4)
noToM.s.eval(k, kstar, util, br, 1, 0)

 
noToM.s.LL <- function(alph, eta.S){
  k = humanLie$drawnRed
  kstar = humanLie$reportedDrawn
  util = ifelse(humanLie$expt=="expt4", 1, -1)
  br = humanLie$probabilityRed
  pred = noToM.eval(k, kstar, expt, br, alph, eta.S)
  
  neg.log.lik = -1*sum(pred)
  neg.log.lik
}

length(br)
noToM.s.LL(1, 0)
EV.L_ksay.k.r(k, kstar, util, 7, br, lastlvl=F, 0.5)
p.L_ksay.k.r.i(k, kstar, util, 1, 7, br, F, 0.5)

noToM.s.fit <- summary(mle(noToM.s.LL,
                        start=list(alph=rnorm(1, 1, 0.5),
                                   eta.S=rnorm(1, 0, 1)),
                        method = "BFGS"))
noToM.s.fit





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
nullVec(humanDetect$probabilityRed, 5)



noToM.r.pred <- function(kstar, util, br, alph, eta.R){
  p.D_bs.ksay.r(kstar, br, util, alph, eta.R, lastlvl=TRUE, nullVec(br, kstar))
}
noToM.r.pred(kstar, util, br, 1, 0)

noToM.r.eval <- function(y, kstar, util, br, alph, eta.R){
  pmax(dbinom(y, 1, noToM.r.pred(kstar, util, br, alph, eta.R), log=T), -9999)
  # sum(
  #   pmax(dbinom(y, 1, noToM.r.pred(kstar, util, br, alph, eta.R), log=T), -9999)
  # )
}
noToM.r.eval(y, kstar, util, br, 1, 0)

noToM.r.LL <- function(alph, eta.R){
  kstar = humanDetect$reportedDrawn
  y = as.logical(humanDetect$callBS)
  util = ifelse(humanDetect$expt=="expt4", 1, -1)
  br = humanDetect$probabilityRed
  
  neg.log.lik = -sum(noToM.r.eval(y, kstar, util, br, alph, eta.R))
  neg.log.lik
}

noToM.r.LL(1,0)

noToM.r.fit <- summary(mle(noToM.r.LL,
                           start=list(alph=rnorm(1, 1, 0.5),
                                      eta.R=rnorm(1, 0, 1)),
                           method = "BFGS"))
noToM.r.fit



noToM.LL <- function(alph, eta.S, eta.R){
  k = bs.final$drawnRed
  kstar = bs.final$reportedDrawn
  y = bs.final$callBS
  util = ifelse(bs.final$expt=="expt4", 1, -1)
  br = bs.final$probabilityRed
  role = bs.final$roleCurrent
  eta = case_when(
    role == "bullshitter" ~ eta.S,
    role == "bullshitDetector" ~ eta.R
  )
  
  pred = ifelse(role == "bullshitter", noToM.s.eval(k, kstar, expt, br, alph, eta), noToM.r.eval(y, kstar, util, br, alph, eta))
  neg.log.lik = -sum(pred)
  neg.log.lik
}

noToM.fit <- summary(mle(noToM.LL,
                         start=list(alph=rnorm(1, 1, 0.5),
                                    eta.S=rnorm(1, 0, 1),
                                    eta.R=rnorm(1, 0, 1)),
                         method = "BFGS"))
noToM.fit
