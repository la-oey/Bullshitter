setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/model/lauren/mleFits/")

source("ToMModelFunctions.R")
models.sources = paste0("models/",list.files("models/"))
sapply(models.sources, source)

library(tidyverse)
library(stats4)
bs.final <- read.csv("bsfinal_anon.csv")
nrow(bs.final)
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
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), reportedDrawn=0:10, callBS=c("True","False"), fill = list(n = 0))
  
humanDetectCounts.T <- humanDetectCounts %>%
  filter(callBS=="True") %>%
  pull(n) %>%
  matrix(nrow=11)
humanDetectCounts.F <- humanDetectCounts %>%
  filter(callBS=="False") %>%
  pull(n) %>%
  matrix(nrow=11)





eval.s <- function(matr, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(matr)*ns)
}

eval.r <- function(matr, ns.T, ns.F){ #ns = 11 x 6 matrix of counts for all conditions
  sum(log(matr)*ns.T + log(1-matr)*ns.F)
}



modelsEval = list(
  # # # # # # # #
  # # no ToM # #
  # # # # # # # #
  noToM = function(){
    print("no ToM")
    noToM.LL <- function(alph, eta.S, eta.R){
      ns.l = humanLieCounts
      ns.T = humanDetectCounts.T
      ns.F = humanDetectCounts.F
      noToM.mat <- list(noToM.s.pred(alph, eta.S), noToM.r.pred(alph, eta.R))
      
      neg.log.lik = -eval.s(noToM.mat[[1]], ns.l) - eval.r(noToM.mat[[2]], ns.T, ns.F) 
      neg.log.lik
    }
    noToM.fit <- summary(mle(noToM.LL,
                             start=list(alph=rnorm(1, 1, 0.5),
                                        eta.S=rnorm(1, 0, 1),
                                        eta.R=rnorm(1, 0, 1)),
                             method = "BFGS"))
    noToM.fit
  },
  
  # # # # # # # # # # #
  # # recursive ToM # #
  # # # # # # # # # # #
  recurseToM = function(){
    print("recursive ToM")
    recurseToM.LL <- function(alph, eta.S, eta.R, lambda){
      ns.l = array(humanLieCounts, dim=c(11,11,6))
      ns.T = humanDetectCounts.T
      ns.F = humanDetectCounts.F

      recurseToM.mat <- recurseToM.pred(alph, eta.S, eta.R, lambda)
      r.eval = -eval.r(recurseToM.mat[[1]], ns.T, ns.F)
      s.eval = -eval.s(recurseToM.mat[[2]], ns.l)
      print(paste("alph =", alph, "; lambda =", lambda, "; r =", r.eval, "; s =", s.eval))
      neg.log.lik =  r.eval + s.eval + lambda^2 #bias lambda to be small
      neg.log.lik
    }
    recurseToM.fit <- summary(mle(recurseToM.LL,
                                  start=list(alph=rnorm(1, 1, 0.2),
                                             eta.S=rnorm(1, 0, 1),
                                             eta.R=rnorm(1, 0, 1),
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
      ns.l = humanLieCounts
      
      -eval.s(
        everybodyLies.pred(lambda, weight),
        ns.l
      )
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
      ns.l = humanLieCounts
      
      -eval.s(
        someLies.pred(pTrue, lambda, weight),
        ns.l
      )
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
  alwaysTruth = function(pTrue){
    print("always truth")
    alwaysTruth.LL <- function(pTrue){
      ns.l = humanLieCounts
      
      -eval.s(
        alwaysTruth.pred(pTrue),
        ns.l
      )
    }
    alwaysTruth.fit <- summary(mle(alwaysTruth.LL,
                                   start=list(pTrue=rnorm(1,0,1)),
                                   method = "BFGS"))
    alwaysTruth.fit
  },
  
  # # # # # # # # # # # # # # #
  # # signif testing detect # #
  # # # # # # # # # # # # # # #
  signifTesting = function(){
    print("signif testing")
    signifTesting.LL <- function(a, b){
      ns.T = humanDetectCounts.T
      ns.F = humanDetectCounts.F

      -eval.r(
        signifTesting.pred(a, b),
        ns.T,
        ns.F
      )
    }
    signifTesting.fit <- summary(mle(signifTesting.LL,
                                     start=list(a=rnorm(1,0,1),
                                                b=rnorm(1,0,1)),
                                     method = "BFGS"))
    signifTesting.fit
  },
  
  # # # # # # # # # # #
  # # random sender # #
  # # # # # # # # # # #
  randomSender = function(){
    print("random sender")
    randomSender.LL <- function(){
      ns.l = humanLieCounts
      
      -eval.s(
        randomSender.pred(),
        ns.l
      )
    }
    randomSender.fit <- randomSender.LL()
    randomSender.fit
  },
  
  # # # # # # # # # # #
  # # random receiver # #
  # # # # # # # # # # #
  randomReceiver = function(){
    print("random receiver")
    randomReceiver.LL <- function(){
      ns.T = humanDetectCounts.T
      ns.F = humanDetectCounts.F
      
      -eval.r(
        randomReceiver.pred(),
        ns.T,
        ns.F
      )
    }
    randomReceiver.fit <- randomReceiver.LL()
    randomReceiver.fit
  }
)










#   ///////////////////////////////
#  /////// EVALUATE MODELS ///////
# ///////////////////////////////

# no ToM
start_time <- Sys.time()
noToMeval = modelsEval$noToM()
print(Sys.time() - start_time)
noToMeval.s <- -2*eval.s(
  noToM.s.pred(
    noToMeval@coef['alph','Estimate'], 
    noToMeval@coef['eta.S','Estimate']), 
  humanLieCounts
)
noToMeval.r <- -2*eval.r(
  noToM.r.pred(
    noToMeval@coef['alph','Estimate'], 
    noToMeval@coef['eta.R','Estimate']), 
  humanDetectCounts.T, 
  humanDetectCounts.F
)







# recursive ToM
# recurseToMEval = modelsEval$recurseToM()
load("recurseToMfit.Rdata")
recurseToMeval.s <- -2*eval.s(
  recurseToM.pred(
    recurseToMeval@coef['alph','Estimate'],
    recurseToMeval@coef['eta.S','Estimate'],
    recurseToMeval@coef['eta.R','Estimate'],
    recurseToMeval@coef['lambda','Estimate'])[[2]],
  array(humanLieCounts, dim=c(11,11,6))
)
recurseToMeval.r <- -2*eval.r(
  recurseToM.pred(
    recurseToMeval@coef['alph','Estimate'],
    recurseToMeval@coef['eta.S','Estimate'],
    recurseToMeval@coef['eta.R','Estimate'],
    recurseToMeval@coef['lambda','Estimate'])[[1]],
  humanDetectCounts.T, 
  humanDetectCounts.F
)

recurseToMeval.s.diag <- -2*eval.s(
  apply(
    recurseToM.pred(
      recurseToMeval@coef['alph','Estimate'],
      recurseToMeval@coef['eta.S','Estimate'],
      recurseToMeval@coef['eta.R','Estimate'],
      recurseToMeval@coef['lambda','Estimate'])[[2]],
    MARGIN=3,
    FUN=diag),
  apply(
    array(
      humanLieCounts, 
      dim=c(11,11,6)),
    MARGIN=3,
    FUN=diag)
)


select_all_but_diag <- function(x) {
  matrix(x[lower.tri(x, diag = F) | upper.tri(x, diag = F)], 
         nrow = nrow(x) - 1, 
         ncol = ncol(x))
}
recurseToMeval.s.lies <- -2*eval.s(
  apply(
    recurseToM.pred(
      recurseToMeval@coef['alph','Estimate'],
      recurseToMeval@coef['eta.S','Estimate'],
      recurseToMeval@coef['eta.R','Estimate'],
      recurseToMeval@coef['lambda','Estimate'])[[2]],
    MARGIN=3,
    FUN=select_all_but_diag),
  apply(
    array(
      humanLieCounts, 
      dim=c(11,11,6)),
    MARGIN=3,
    FUN=select_all_but_diag)
)



# recursive ToM broken down by condition

preds <- recurseToM.pred(
  recurseToMeval@coef['alph','Estimate'],
  recurseToMeval@coef['eta.S','Estimate'],
  recurseToMeval@coef['eta.R','Estimate'],
  recurseToMeval@coef['lambda','Estimate'])
predsS <- preds[[2]]
trueS <- array(humanLieCounts, dim=c(11,11,6))
predsR <- preds[[1]]
for(i in 1:6){
  print(
    -2*eval.s(
      predsS[,,i],
      trueS[,,i]
    )
  )
}
for(i in 1:6){
  print(
    -2*eval.r(
      predsR[,i],
      humanDetectCounts.T[,i], 
      humanDetectCounts.F[,i]
    )
  )
}




# everybody lies
everybodyLiesEval = modelsEval$everybodyLies()
everybodyLiesEval@m2logL


# some people lie
somePeopleLieEval = modelsEval$someLies()
somePeopleLieEval@m2logL


# always truth
alwaysTruthEval = modelsEval$alwaysTruth()
alwaysTruthEval@m2logL


# signif testing detect
signifTestingEval = modelsEval$signifTesting()
signifTestingEval@m2logL


# random sender
randomSenderEval = modelsEval$randomSender()
2*randomSenderEval


# random receiver
randomReceiverEval = modelsEval$randomReceiver()
2*randomReceiverEval

