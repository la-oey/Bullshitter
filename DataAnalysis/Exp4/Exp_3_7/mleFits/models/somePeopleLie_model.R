

someLies.poisson <- function(util, pTrue, lambda, weight){
  pTrue = logitToProb(pmin(10, pmax(-10, pTrue)))
  weight = logitToProb(pmin(10, pmax(-10, weight)))
  lambda = exp(lambda)
  mapply(
    function(k){
      kstar = 0:10
      (k==kstar)*pTrue + (1-pTrue)*(weight*dpois((kstar-k)*util, lambda)/ppois(ifelse(util==1, 10-k, k), lambda) + (1-weight)/11)
    },
    0:10
  )
}

someLies.pred <- function(pTrue, lambda, weight){
  matrix(
    mapply(
      function(u) rep(someLies.poisson(u, pTrue, lambda, weight), 3), #repeat for each base rate condition
      c(1, -1)), #repeat for each utility structure condition
    nrow=121)
}

