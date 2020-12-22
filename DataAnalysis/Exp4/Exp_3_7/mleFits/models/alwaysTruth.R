

alwaysTruth.func <- function(k, kstar, pTrue){
  #ifelse(kstar == k, 1-0.05*10, 0.05)
  (kstar==k)*pTrue + (kstar !=k)*(1-pTrue)/10
}

alwaysTruth.pred <- function(pTrue){
  pTrue = logitToProb(pTrue)
  matrix(
    rep(
      mapply(
        function(k) alwaysTruth.func(k, 0:10, pTrue),
        0:10
      ),
      6
    ),
    nrow=121
  )
}

