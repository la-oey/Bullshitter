
source(here::here('model', 'fitting', 'helpers.R'))
source(here::here('model/fitting/TOM.R'))
source(here::here('model/fitting/nullModels.R'))

model_expt_LL <- function(modelFx, params){
  LL = matrix(NA, 
              nrow=length(expts), 
              ncol=2, 
              dimnames = list(c(), c('liar', 'detector')))
  for(i in 1:length(expts)){
    predictions = modelFx(params, expts[[i]])
    for(a in names(predictions)){
      LL[i,a] = sum(log(predictions[[a]])*data[[i]][[a]])
    }
  }
  return(LL)
}



genWrap = function(modelFx, arglst){
  LP = paramsPriors(arglst)
  arglst = paramsTransform(arglst)
  params = paramsMake(arglst)  
  LL = sum(model_expt_LL(modelFx, params), na.rm = T)
  return(-1 * (LL+LP))
}

modelWrappers = list()
modelWrappers[['Rational1']] = function(liar.softmax,
                                        liar.aversion, 
                                        liar.d0_prob,
                                        detector.softmax,
                                        detector.aversion,
                                        detector.l0_prob){
  genWrap(predictRational1, as.list(match.call()[-1]))
}
# modelWrappers[['Rational1.b']] = function(softmax,
#                                          liar.aversion, 
#                                          detector.aversion){
#   genWrap(predictRational1, as.list(match.call()[-1]))
# }
# modelWrappers[['Rational1.c']] = function(softmax,
#                                          liar.d0_prob,
#                                          detector.l0_prob){
#   genWrap(predictRational1, as.list(match.call()[-1]))
# }
modelWrappers[['TOM.shared']] = function(lambda, 
                                         softmax,
                                         liar.aversion, 
                                         detector.aversion){
  genWrap(predictTOM, as.list(match.call()[-1]))
}
modelWrappers[['TOM']] = function(lambda,
                                  liar.softmax,
                                  liar.aversion, 
                                  detector.softmax,
                                  detector.aversion){
  genWrap(predictTOM, as.list(match.call()[-1]))
}
modelWrappers[['LiarRandom']] = function(prob){
  genWrap(predictLiarRandom, as.list(match.call()[-1]))
}
modelWrappers[['DetectorRandom']] = function(prob){
  genWrap(predictDetectorRandom, as.list(match.call()[-1]))
}
modelWrappers[['LiarMax']] = function(prob_unif){
  genWrap(predictLiarMax, as.list(match.call()[-1]))
}
modelWrappers[['LiarTruth']] = function(prob_unif){
  genWrap(predictLiarTruth, as.list(match.call()[-1]))
}
modelWrappers[['LiarPoisson']] = function(lambda,
                                          prob_unif){
  genWrap(predictLiarPoisson, as.list(match.call()[-1]))
}
modelWrappers[['LiarPoissonTrue']] = function(lambda, prob_unif, prob_true){
  genWrap(predictLiarPoissonTrue, as.list(match.call()[-1]))
}
modelWrappers[['DetectorNHST']] = function(a, b){
  genWrap(predictDetectorNHST, as.list(match.call()[-1]))
}



