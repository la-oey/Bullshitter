condColors.diff = c("p = 0.2" = "blue",
                    "p = 0.5" = "purple",
                    "p = 0.8" = "red")

P.K = matrix(rep(p.k(0:10, 0.5), each=11), nrow=11)
# MLE functions for liar data

logitToProb <- function(logit){
  exp(logit) / (1+exp(logit))
}

probToLogit <- function(prob){
  log(prob / (1 - prob))
}

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

binom.p <- function(kstar, alph){
  alph = logitToProb(alph)
  dbinom(kstar, 10, alph)
}

df <- NULL
nLL <- function(beta, mu0.2_4, alph0.2_4,
                mu0.5_4, alph0.5_4,
                mu0.8_4, alph0.8_4,
                mu0.2_5, alph0.2_5,
                mu0.5_5, alph0.5_5,
                mu0.8_5, alph0.8_5){
  k = df$k
  ksay = df$ksay
  expt = df$expt
  p = df$p
  mu = case_when(
    p == 0.2 & expt == "expt4" ~ mu0.2_4,
    p == 0.5 & expt == "expt4" ~ mu0.5_4,
    p == 0.8 & expt == "expt4" ~ mu0.8_4,
    p == 0.2 & expt == "expt5" ~ mu0.2_5,
    p == 0.5 & expt == "expt5" ~ mu0.5_5,
    p == 0.8 & expt == "expt5" ~ mu0.8_5
  )
  alph = case_when(
    p == 0.2 & expt == "expt4" ~ alph0.2_4,
    p == 0.5 & expt == "expt4" ~ alph0.5_4,
    p == 0.8 & expt == "expt4" ~ alph0.8_4,
    p == 0.2 & expt == "expt5" ~ alph0.2_5,
    p == 0.5 & expt == "expt5" ~ alph0.5_5,
    p == 0.8 & expt == "expt5" ~ alph0.8_5
  )
  
  betas = ifelse(expt=="expt4", 1*beta, -1*beta)
  
  pred = lieLogisticBinom(k, ksay, betas, mu, alph)
  # likelihood of observed kstar for that k, given parameters
  neg.log.lik = -1*sum(log(pred))
  mus = c(mu0.2_4, mu0.5_4, mu0.8_4, mu0.2_5, mu0.5_5, mu0.8_5)
  alphas = c(alph0.2_4, alph0.5_4, alph0.8_4, alph0.2_5, alph0.5_5, alph0.8_5)
  #neg.log.prior = sum(.0001*(mus-5)^2)-abs(beta)+ sum(alphas^2)+u*10
  neg.log.prior = 0
  neg.log.lik+neg.log.prior
}