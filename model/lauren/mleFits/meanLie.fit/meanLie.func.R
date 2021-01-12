

# # # # # # #
# Functions #
# # # # # # #

lieLogisticModel <- function(k, beta, mu){
  logodds = beta * (k-mu)
  logitToProb(pmin(10, pmax(-10, logodds)))
}

p.lie.k <- function(k, beta, mu){
  lieLogisticModel(k, beta, mu)
}

p.kstar.k <- function(k, kstar, alph){
  alph = logitToProb(alph)
  dbinom(kstar, 10, alph) / (1-dbinom(k, 10, alph))  
}

lieLogisticBinom <- function(k, kstar, beta, mu, alph){
  p.lie = p.lie.k(k, beta, mu)
  ifelse(k == kstar, 1 - p.lie, p.lie * p.kstar.k(k, kstar, alph))
}

eval.s <- function(matr, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(matr)*ns)
}


lieLL <- function(counts, 
                  beta, 
                  mu0.2_4, alph0.2_4,
                  mu0.5_4, alph0.5_4,
                  mu0.8_4, alph0.8_4,
                  mu0.2_5, alph0.2_5,
                  mu0.5_5, alph0.5_5,
                  mu0.8_5, alph0.8_5){
  betas = c(rep(1*beta,3), rep(-1*beta,3))
  mus = c(mu0.2_4,mu0.5_4,mu0.8_4,mu0.2_5,mu0.5_5,mu0.8_5)
  alphs = c(alph0.2_4,alph0.5_4,alph0.8_4,alph0.2_5,alph0.5_5,alph0.8_5)
  
  lieLogisticBinom.pred = array(
    lieLogisticBinom(
      rep(rep(0:10,each=11),6),
      rep(0:10,11*6), 
      rep(betas, each=121),
      rep(mus, each=121),
      rep(alphs, each=121)
    ),
    dim=c(11,11,6)
  )

  neg.log.lik = -eval.s(lieLogisticBinom.pred, counts)
  neg.log.lik
}

fit <- function(datafr){
  LL = function(beta, 
                mu0.2_4, alph0.2_4,
                mu0.5_4, alph0.5_4,
                mu0.8_4, alph0.8_4,
                mu0.2_5, alph0.2_5,
                mu0.5_5, alph0.5_5,
                mu0.8_5, alph0.8_5){
    lieLL(datafr, 
          beta, 
          mu0.2_4, alph0.2_4,
          mu0.5_4, alph0.5_4,
          mu0.8_4, alph0.8_4,
          mu0.2_5, alph0.2_5,
          mu0.5_5, alph0.5_5,
          mu0.8_5, alph0.8_5)
  }
  summary(mle(LL, start=list(beta=rnorm(1, 0, 0.5),
                             mu0.2_4=rnorm(1, 5, 3),
                             alph0.2_4=rnorm(1, 0, 0.5),
                             mu0.5_4=rnorm(1, 5, 3),
                             alph0.5_4=rnorm(1, 0, 0.5),
                             mu0.8_4=rnorm(1, 5, 3),
                             alph0.8_4=rnorm(1, 0, 0.5),
                             mu0.2_5=rnorm(1, 5, 3),
                             alph0.2_5=rnorm(1, 0, 0.5),
                             mu0.5_5=rnorm(1, 5, 3),
                             alph0.5_5=rnorm(1, 0, 0.5),
                             mu0.8_5=rnorm(1, 5, 3),
                             alph0.8_5=rnorm(1, 0, 0.5)),
              method = "BFGS"))
}

