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


st = 1
end = 6
modelsEval = list(
  # # # # # # # #
  # # no ToM # #
  # # # # # # # #
  noToM = function(){
    print("no ToM")
    noToM.LL <- function(alph, eta.S, eta.R, weight){
      ns.l = array(humanLieCounts, dim=c(11,11,6))
      ns.T = humanDetectCounts.T
      ns.F = humanDetectCounts.F
      noToM.mat <- list(noToM.s.pred(alph, eta.S, weight), noToM.r.pred(alph, eta.R))
  
      s.eval = -eval.s(noToM.mat[[1]][,,st:end], ns.l[,,st:end])
      r.eval = -eval.r(noToM.mat[[2]][,st:end], ns.T[,st:end], ns.F[,st:end])
      print(paste("alph =", alph, "; eta.S =", eta.S, "; weight =", logitToProb(weight), "; r =", r.eval, "; s =", s.eval))
      
      neg.log.lik = r.eval + s.eval
      neg.log.lik + abs(weight)
    }
    noToM.fit <- summary(mle(noToM.LL,
                             start=list(alph=rnorm(1, 1, 0.2),
                                        eta.S=rnorm(1, 0, 1),
                                        eta.R=rnorm(1, 0, 1),
                                        weight=rnorm(1, 0, 1)),
                             method = "BFGS"))
    noToM.fit
  },
  
  # # # # # # # # # # #
  # # recursive ToM # #
  # # # # # # # # # # #
  recurseToM = function(){
    print("recursive ToM")
    recurseToM.LL <- function(alph, eta.S, eta.R, lambda, weight){
      ns.l = array(humanLieCounts, dim=c(11,11,6))
      ns.T = humanDetectCounts.T
      ns.F = humanDetectCounts.F

      recurseToM.mat <- recurseToM.pred(alph, eta.S, eta.R, lambda, weight)
      r.eval = -eval.r(recurseToM.mat[[1]][,st:end], ns.T[,st:end], ns.F[,st:end])
      s.eval = -eval.s(recurseToM.mat[[2]][,,st:end], ns.l[,,st:end])
      print(paste("alph =", alph, "; weight =", logitToProb(weight), "; lambda =", lambda, "; r =", r.eval, "; s =", s.eval))
      neg.log.lik = r.eval + s.eval
      neg.log.lik + abs(weight) #weight^2?
    }
    recurseToM.fit <- summary(mle(recurseToM.LL,
                                  start=list(alph=rnorm(1, 1, 0.2),
                                             eta.S=rnorm(1, 0, 1),
                                             eta.R=rnorm(1, 0, 1),
                                             lambda=rnorm(1, 0, 1),
                                             weight=rnorm(1, 0, 1)),
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
        everybodyLies.pred(lambda, weight)[,st:end],
        ns.l[,st:end]
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
        someLies.pred(pTrue, lambda, weight)[,st:end],
        ns.l[,st:end]
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
  alwaysTruth = function(){
    print("always truth")
    alwaysTruth.LL <- function(pTrue){
      ns.l = humanLieCounts
      -eval.s(
        alwaysTruth.pred(pTrue)[,st:end],
        ns.l[,st:end]
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
        signifTesting.pred(a, b)[,st:end],
        ns.T[,st:end],
        ns.F[,st:end]
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
        randomSender.pred()[,st:end],
        ns.l[,st:end]
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
        randomReceiver.pred()[,st:end],
        ns.T[,st:end],
        ns.F[,st:end]
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
load("Rdata/noToMfit.Rdata")
start_time <- Sys.time()
noToMeval = modelsEval$noToM()
print(Sys.time() - start_time)
# save(noToMeval, file="noToMfit.Rdata")

noToMeval.s <- -2*eval.s(
  array(
    noToM.s.pred(
      noToMeval@coef['alph','Estimate'], 
      noToMeval@coef['eta.S','Estimate'],
      noToMeval@coef['weight','Estimate']),
    dim=c(11,11,6))[,,st:end], 
  array(
    humanLieCounts, 
    dim=c(11,11,6))[,,st:end]
)
noToMeval.r <- -2*eval.r(
  noToM.r.pred(
    noToMeval@coef['alph','Estimate'], 
    noToMeval@coef['eta.R','Estimate'])[,st:end], 
  humanDetectCounts.T[,st:end], 
  humanDetectCounts.F[,st:end]
)







# recursive ToM
start_time <- Sys.time()
for(i in 1:50){
  tryCatch({
    recurseToMeval = modelsEval$recurseToM()
    break
  }, error = function(e){
    message(e)
  })
}
print(Sys.time() - start_time)
# save(recurseToMeval, file="Rdata/recurseToMfit.Rdata")

recurseToMeval.s <- -2*eval.s(
  recurseToM.pred(
    recurseToMeval@coef['alph','Estimate'],
    recurseToMeval@coef['eta.S','Estimate'],
    recurseToMeval@coef['eta.R','Estimate'],
    recurseToMeval@coef['lambda','Estimate'],
    recurseToMeval@coef['weight','Estimate'])[[2]][,,st:end],
  array(humanLieCounts, dim=c(11,11,6))[,,st:end]
)
recurseToMeval.r <- -2*eval.r(
  recurseToM.pred(
    recurseToMeval@coef['alph','Estimate'],
    recurseToMeval@coef['eta.S','Estimate'],
    recurseToMeval@coef['eta.R','Estimate'],
    recurseToMeval@coef['lambda','Estimate'],
    recurseToMeval@coef['weight','Estimate'])[[1]][,st:end],
  humanDetectCounts.T[,st:end], 
  humanDetectCounts.F[,st:end]
)

# # recursive ToM broken down by condition


# sts = c(1:6,1,4)
# ends = c(1:6,3,6)
# 
# for(i in 1:length(sts)){
#   st = sts[i]
#   end = ends[i]
#   start_time <- Sys.time()
#   for(i in 1:50){
#     tryCatch({
#       recurseToMeval.subset = modelsEval$recurseToM()
#       break
#     }, error = function(e){
#       message(e)
#     })
#   }
#   print(Sys.time() - start_time)
#   fileUtil = ifelse(st <= 3, "red", "blue")
#   fileP = case_when(
#     st == end & st %% 3 == 1 ~ "0.2",
#     st == end & st %% 3 == 2 ~ "0.5",
#     st == end & st %% 3 == 0 ~ "0.8",
#     TRUE ~ "NA"
#   )
#   filename = paste0("Rdata/recurseToMfit_", fileUtil, fileP, ".Rdata")
#   save(recurseToMeval.subset, file=filename)
# }









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








# Examine Truth vs Lies



liesTruthEval = list(
  noToM = function(){
    print("truth vs lies fit - no ToM")
    noToMeval.s.diag <- -2*eval.s(
      getDiag(
        array(
          noToM.s.pred(
            noToMeval@coef['alph','Estimate'], 
            noToMeval@coef['eta.S','Estimate'],
            noToMeval@coef['weight','Estimate']),
          dim=c(11,11,6))[,,st:end]
      ), 
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    noToMeval.s.lies <- -2*eval.s(
      getLies(
        array(
          noToM.s.pred(
            noToMeval@coef['alph','Estimate'], 
            noToMeval@coef['eta.S','Estimate'],
            noToMeval@coef['weight','Estimate']),
          dim=c(11,11,6))[,,st:end]
      ), 
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(noToMeval.s.diag, noToMeval.s.lies))
  },
  
  recurseToM = function(){
    print("truth vs lies fit - recursive ToM")
    recurseToMeval.s.diag = -2*eval.s(
      getDiag(
        recurseToM.pred(
          recurseToMeval@coef['alph','Estimate'],
          recurseToMeval@coef['eta.S','Estimate'],
          recurseToMeval@coef['eta.R','Estimate'],
          recurseToMeval@coef['lambda','Estimate'],
          recurseToMeval@coef['weight','Estimate'])[[2]][,,st:end]
      ),
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    recurseToMeval.s.lies = -2*eval.s(
      getLies(
        recurseToM.pred(
          recurseToMeval@coef['alph','Estimate'],
          recurseToMeval@coef['eta.S','Estimate'],
          recurseToMeval@coef['eta.R','Estimate'],
          recurseToMeval@coef['lambda','Estimate'],
          recurseToMeval@coef['weight','Estimate'])[[2]][,,st:end]
      ),
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(recurseToMeval.s.diag, recurseToMeval.s.lies))
  },
  
  everybodyLies = function(){
    print("truth vs lies fit - everybody lies")
    everybodyLiesEval.s.diag = -2*eval.s(
      getDiag(
        array(
          everybodyLies.pred(
            everybodyLiesEval@coef['lambda','Estimate'],
            everybodyLiesEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    everybodyLiesEval.s.lies = -2*eval.s(
      getLies(
        array(
          everybodyLies.pred(
            everybodyLiesEval@coef['lambda','Estimate'],
            everybodyLiesEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(everybodyLiesEval.s.diag, everybodyLiesEval.s.lies))
  },
  
  somePeopleLie = function(){
    print("truth vs lies fit - some people lie")
    somePeopleLieEval.s.diag = -2*eval.s(
      getDiag(
        array(
          someLies.pred(
            somePeopleLieEval@coef['pTrue','Estimate'],
            somePeopleLieEval@coef['lambda','Estimate'],
            somePeopleLieEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    somePeopleLieEval.s.lies = -2*eval.s(
      getLies(
        array(
          someLies.pred(
            somePeopleLieEval@coef['pTrue','Estimate'],
            somePeopleLieEval@coef['lambda','Estimate'],
            somePeopleLieEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(somePeopleLieEval.s.diag, somePeopleLieEval.s.lies))
  }
)

liesTruthEval$noToM()
liesTruthEval$recurseToM()
liesTruthEval$everybodyLies()
liesTruthEval$somePeopleLie()
