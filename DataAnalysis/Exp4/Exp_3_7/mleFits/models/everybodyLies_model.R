

everybodyLies.poisson <- function(util, lambda, weight){
  weight = logitToProb(pmin(10, pmax(-10, weight)))
  lambda = exp(lambda)
  mapply(
    function(k){
      kstar = 0:10
      weight*dpois((kstar-k)*util, lambda)/ppois(ifelse(util==1, 10-k, k), lambda)+(1-weight)*(1/11)
    },
    0:10
  )
}

everybodyLies.pred <- function(lambda, weight){
  matrix(
    mapply(
      function(u) rep(everybodyLies.poisson(u, lambda, weight), 3), #repeat for each base rate condition
      c(1, -1)), #repeat for each utility structure condition
    nrow=121)
}

