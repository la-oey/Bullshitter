setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/model/lauren/mleFits")

source("ToMModelFunctions.R")



recurseToM.matrix2 <- function(alph.S, alph.R, eta.S, eta.R, util, p, lambda){
  n.depths = qpois(0.95, lambda) # 25
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
      marginalized.caller = poissonAverage(store.ksay.k.simple[,,1:(depth-1)], lambda)

      store.bs.ksay.simple[,depth] = mapply(p.D_bs.ksay.r,
                                            0:10,
                                            p,
                                            util,
                                            alph.R,
                                            eta.R,
                                            lastlvl=FALSE,
                                            p_true.ksay(p.k(0:numMarbles, p),
                                                        marginalized.caller))
      store.bs.ksay[,depth] = mapply(p.D_bs.ksay.r,
                                     0:10,
                                     p,
                                     util,
                                     alph.R,
                                     eta.R,
                                     lastlvl=TRUE,
                                     p_true.ksay(p.k(0:numMarbles, p),
                                                 marginalized.caller)) #mean of previous levels; weigh?

    }
    marginalized.liar = poissonAverage(matrix(store.bs.ksay.simple[,1:depth], nrow=numMarbles+1), lambda)

    store.ksay.k.simple[,,depth] = p.L_ksay.k.r(util,
                                                alph.S,
                                                eta.S,
                                                p,
                                                lastlvl=FALSE,
                                                marginalized.liar)
    store.ksay.k[,,depth] = p.L_ksay.k.r(util,
                                         alph.S,
                                         eta.S,
                                         p,
                                         lastlvl=TRUE,
                                         marginalized.liar)
  }
  return(list(store.bs.ksay, store.ksay.k))
  # [[1]] receiver P(BS | k*)
  # [[2]] sender P(k* | k)
}

recurseToM.weighted2 <- function(alph.S, alph.R, eta.S, eta.R, util, p, lambda){
  matrices <- recurseToM.matrix2(alph.S, alph.R, eta.S, eta.R, util, p, lambda)
  n.depths = dim(matrices[[1]])[2] # should be equal to dim(matrices[[2]])[3]
  weightedR <- poissonAverage(matrices[[1]], lambda)
  weightedS <- poissonAverage(matrices[[2]], lambda)
  return(list(weightedR, weightedS))
}

recurseToM.pred2 <- function(alph.S, alph.R, eta.S, eta.R, lambda){
  lambda = pmax(pmin(exp(lambda), 35),0.1)
  store.ksay.k.full = array(NA, dim = c(11, 11, 6))
  store.bs.ksay.full = array(NA, dim = c(11, 6))
  utils = c(1,-1)
  ps = c(0.2, 0.5, 0.8)
  for(u in 1:length(utils)){
    for(p in 1:length(ps)){
      matr <- recurseToM.weighted2(alph.S, alph.R, eta.S, eta.R, utils[u], ps[p], lambda)
      store.bs.ksay.full[,(u-1)*length(ps)+p] <- matr[[1]]
      store.ksay.k.full[,,(u-1)*length(ps)+p] <- matr[[2]]
    }
  }
  return(list(store.bs.ksay.full, store.ksay.k.full))
}
