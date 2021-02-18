setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/model/lauren/mleFits/")

source("ToMModelFunctions.R")
models.sources = paste0("models/",list.files("models/"))
sapply(models.sources, source)

library(tidyverse)
library(stats4)
bs.final <- read.csv("bsfinal_anon.csv")
humanLie <- bs.final %>%
  filter(roleCurrent == "bullshitter")
humanDetect <- bs.final %>%
  filter(roleCurrent == "bullshitDetector")

#### General Functions ####
logitToProb <- function(logit){
  exp(logit) / (1+exp(logit))
}

probToLogit <- function(prob){
  log(prob / (1 - prob))
}




# 121 x 6 matrix
humanLieCounts <- humanLie %>%
  count(expt, probabilityRed, drawnRed, reportedDrawn) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), drawnRed=0:10, reportedDrawn=0:10, fill = list(n = 0)) %>%
  pull(n) %>%
  matrix(nrow=121)

# 22 x 6 matrix
humanDetectCounts <- humanDetect %>%
  count(expt, probabilityRed, reportedDrawn, callBS) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), reportedDrawn=0:10, callBS=c(TRUE,FALSE), fill = list(n = 0))

humanDetectCounts.T <- humanDetectCounts %>%
  filter(callBS) %>%
  pull(n) %>%
  matrix(nrow=11)
humanDetectCounts.F <- humanDetectCounts %>%
  filter(!callBS) %>%
  pull(n) %>%
  matrix(nrow=11)


# get diagonal of a 3 dimensional array
getDiag <- function(arr){
  apply(arr, MARGIN=3, FUN=diag)
}

select_all_but_diag <- function(x) {
  matrix(x[lower.tri(x, diag = F) | upper.tri(x, diag = F)], 
         nrow = nrow(x) - 1, 
         ncol = ncol(x))
}
getLies <- function(arr){
  apply(arr, MARGIN=3, FUN=select_all_but_diag)
}



eval.s <- function(matr, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(matr)*ns)
}

eval.r <- function(matr, ns.T, ns.F){ #ns = 11 x 6 matrix of counts for all conditions
  sum(log(matr)*ns.T + log(1-matr)*ns.F)
}


####### LIES #######
evalLies = list(
  # # # # # # # #
  # # no ToM # #
  # # # # # # # #
  noToM = function(){
    print("no ToM")
    noToM.LL <- function(alph, eta.S){
      ns.l = array(
          humanLieCounts, 
          dim=c(11,11,6))
      pred = array(
        noToM.s.pred(alph, eta.S),
        dim=c(11,11,6))
      
      eval.lies = -eval.s(
        getLies(pred),
        getLies(ns.l)
      )
      eval.diag = -eval.s(
        getDiag(pred),
        getDiag(ns.l)
      )
      print(paste("alph =", alph, "; eta.S =", eta.S, "; lies =", eval.lies, "; diag =", eval.diag))
      neg.log.lik = eval.lies + eval.diag
      neg.log.lik
    }
    noToM.fit <- summary(mle(noToM.LL,
                             start=list(alph=rnorm(1, 1, 0.2),
                                        eta.S=rnorm(1, 0, 1)),
                             method = "BFGS"))
    noToM.fit
  },
  
  # # # # # # # # # # #
  # # recursive ToM # #
  # # # # # # # # # # #
  recurseToM = function(){
    print("recursive ToM")
    recurseToM.LL <- function(alph, eta.S, lambda){
      eta.R = 0
      ns.l = array(
          humanLieCounts, 
          dim=c(11,11,6))
      pred = recurseToM.pred(alph, eta.S, eta.R, lambda)[[2]]
      
      eval.lies = -eval.s(
        getLies(pred),
        getLies(ns.l)
      )
      eval.diag = -eval.s(
        getDiag(pred),
        getDiag(ns.l)
      )
      print(paste("alph =", alph, "; eta.S =", eta.S, "; lambda =", lambda, "; lies =", eval.lies, "; diag =", eval.diag))
      neg.log.lik = eval.lies + eval.diag
      neg.log.lik
    }
    recurseToM.fit <- summary(mle(recurseToM.LL,
                                  start=list(alph=rnorm(1, 1, 0.2),
                                             eta.S=rnorm(1, 0, 1),
                                             lambda=rnorm(1, 0, 1)),
                                  method = "BFGS"))
    recurseToM.fit
  },
  
  # # # # # # # # # # # #
  # # everybody lies # #
  # # # # # # # # # # # #
  everybodyLies = function(){
    print("everybody lies")
    everybodyLies.LL <- function(lambda, weight){
      ns.l = array(
          humanLieCounts, 
          dim=c(11,11,6))
      pred = array(
        everybodyLies.pred(lambda, weight),
        dim=c(11,11,6)
      )

      eval.lies = -eval.s(
        getLies(pred),
        getLies(ns.l)
      )
      eval.diag = -eval.s(
        getDiag(pred),
        getDiag(ns.l)
      )
      neg.log.lik = eval.lies + eval.diag
      neg.log.lik
    }
    everybodyLies.fit <- summary(mle(everybodyLies.LL,
                                     start=list(lambda=rnorm(1, 0, 1),
                                                weight=rnorm(1, 0, 1)),
                                     method = "BFGS"))
    everybodyLies.fit
  },
  
  # # # # # # # # # # # #
  # # some people lie # #
  # # # # # # # # # # # #
  someLies = function(){
    print("some people lie")
    someLies.LL <- function(pTrue, lambda, weight){
      ns.l = array(
          humanLieCounts, 
          dim=c(11,11,6))
      pred = array(
        someLies.pred(pTrue, lambda, weight),
        dim=c(11,11,6))
      
      eval.lies = -eval.s(
        getLies(pred),
        getLies(ns.l)
      )
      eval.diag = -eval.s(
        getDiag(pred),
        getDiag(ns.l)
      )
      neg.log.lik = eval.lies + eval.diag
      neg.log.lik
    }
    someLies.fit <- summary(mle(someLies.LL,
                                start=list(pTrue=rnorm(1,0,1),
                                           lambda=rnorm(1, 0, 1),
                                           weight=rnorm(1, 0, 1)),
                                method = "BFGS"))
    someLies.fit
  },
  
  # # # # # # # # # # #
  # # always truth # #
  # # # # # # # # # # #
  alwaysTruth = function(){
    print("always truth")
    alwaysTruth.LL <- function(pTrue){
      ns.l = array(
          humanLieCounts, 
          dim=c(11,11,6))
      pred = array(
            alwaysTruth.pred(pTrue),
            dim=c(11,11,6))
      
      eval.lies = -eval.s(
        getLies(pred),
        getLies(ns.l)
      )
      eval.diag = -eval.s(
        getDiag(pred),
        getDiag(ns.l)
      )
      neg.log.lik = eval.lies + eval.diag
      neg.log.lik
    }
    alwaysTruth.fit <- summary(mle(alwaysTruth.LL,
                                   start=list(pTrue=rnorm(1,0,1)),
                                   method = "BFGS"))
    alwaysTruth.fit
  }
)


noToMeval.lies = evalLies$noToM()
recurseToMeval.lies = evalLies$recurseToM()
everybodyLies.lies = evalLies$everybodyLies()
someLies.lies = evalLies$someLies()
alwaysTruth.lies = evalLies$alwaysTruth()














