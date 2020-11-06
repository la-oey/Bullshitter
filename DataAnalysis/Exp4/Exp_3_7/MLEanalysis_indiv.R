# MLE analysis fit to individuals

setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7")

library(tidyverse)
library(stats4)
bs.final <- read.csv("bsfinal_anon.csv")

my_red = c("#ffd5d6","#fc7f81","#fd2428")
p_label = c(
  "p = 0.2" = "20% red",
  "p = 0.5" = "50% red",
  "p = 0.8" = "80% red"
)

sender_points_label = c(
  "expt4" = "Sender gets\npoints for red",
  "expt5" = "Sender gets\npoints for blue"
)

base_rate_label = c(
  "p = 0.2" = "20% red",
  "p = 0.5" = "50% red",
  "p = 0.8" = "80% red"
)


################################
###### Analysis Functions ######
################################

humanLie <- bs.final %>%
  filter(roleCurrent == "bullshitter") %>%
  mutate(condition = paste0(probabilityRed,"_",expt))

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

###################
## Analysis Loop ##
###################

set.seed(8888)
indivFits <- data.frame(subjID = NULL, 
                        expt = NULL, 
                        prob = NULL, 
                        beta.est = NULL, 
                        beta.se = NULL, 
                        mu.est = NULL, 
                        mu.se = NULL, 
                        alph.est = NULL, 
                        alph.se = NULL)
# subj041, subj166 fail to fit because they never lied
for(subjNum in unique(humanLie$subjID)){
  humanLie.subj <- filter(humanLie, subjID == subjNum)
  expt = unique(humanLie.subj$expt)
  prob = unique(humanLie.subj$probabilityRed)
  try({
    nLL.indiv <- function(beta, mu, alph){
      k = humanLie.subj$drawnRed
      kstar = humanLie.subj$reportedDrawn
      mu = mu
      alph = alph
      betas = ifelse(expt=="expt4", 1*beta, -1*beta)

      pred = lieLogisticBinom(k, kstar, betas, mu, alph)
      # likelihood of observed kstar for that k, given parameters
      neg.log.lik = -1*sum(log(pred))
      # neg.log.prior = abs(beta)
      neg.log.prior = .05*(mu-5)^2
      neg.log.lik + neg.log.prior
    }

    fit.subj <- summary(mle(nLL.indiv,
                            start=list(beta=rnorm(1, 0, 0.5),
                                       mu=rnorm(1, 5, 3),
                                       alph=rnorm(1, 0, 0.5)),
                            method = "BFGS"))
    newdf <- data.frame(subjID = subjNum,
                        expt = expt,
                        prob = prob,
                        beta.est = round(fit.subj@coef["beta","Estimate"],5),
                        beta.se = round(fit.subj@coef["beta","Std. Error"],5),
                        mu.est = round(fit.subj@coef["mu","Estimate"],5),
                        mu.se = round(fit.subj@coef["mu","Std. Error"],5),
                        alph.est = round(fit.subj@coef["alph","Estimate"],5),
                        alph.se = round(fit.subj@coef["alph","Std. Error"],5)
    )
    indivFits <- bind_rows(indivFits, newdf)
  }, TRUE)
}


errorSubj <- setdiff(humanLie$subjID, indivFits$subjID) #subjects that threw an error in fitting data
length(errorSubj)


nSubj <- length(unique(indivFits$subjID))
lieMLE.pred <- data.frame(subj=rep(indivFits$subjID,each=11*11),
                          p=as.factor(rep(indivFits$prob,each=11*11)),
                          expt=as.factor(rep(indivFits$expt,each=11*11)),
                          k=rep(rep(0:10,each=11),nSubj), 
                          kstar=rep(0:10, 11*nSubj),
                          beta=rep(indivFits$beta.est,each=11*11),
                          beta.se=rep(indivFits$beta.se,each=11*11),
                          mu=rep(indivFits$mu.est,each=11*11),
                          mu.se=rep(indivFits$mu.se,each=11*11),
                          alph=rep(indivFits$alph.est,each=11*11),
                          alph.se=rep(indivFits$alph.se,each=11*11)) %>%
  mutate(beta = ifelse(expt=="expt4", beta, -beta),
         p.lie.k = p.lie.k(k, beta, mu),
         p.kstar.k=lieLogisticBinom(k, kstar, beta, mu, alph),
         binom.kstar = binom.p(kstar, alph),
         logp.kstar.k = log(p.kstar.k))
#write_csv(lieMLE.pred, "individual_sender.csv")


lieMLE.pred %>%
  select(subj, p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(p_expt = paste0(p, "_", expt),
         p = as.factor(paste("p =", p)),
         meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=expt, y=meanAlph, fill=p)) +
  geom_dotplot(binaxis = "y", binwidth=0.2, stackdir = "center", alpha=0.5, position="dodge") +
  geom_pointrange(aes(fill=p),
                  stat="summary", 
                  fun.data = "mean_se", 
                  position =  position_dodge(width = 0.9), 
                  shape=21,
                  size=0.75) +
  scale_x_discrete("", labels=sender_points_label) +
  scale_y_continuous("Mean Lie", limits=c(0,10), expand=c(0,0.15,0,0), breaks=seq(0,10,2)) +
  scale_fill_manual("Base Rate", values=my_red, labels=base_rate_label) +
  theme_minimal()
ggsave("img/individual/meanLie.pdf", width=6, height=6)

lieMLE.pred %>%
  select(subj, p, expt, beta, beta.se) %>%
  unique() %>%
  filter(beta.se < 10) %>%
  ggplot(aes(x=expt, y=beta, fill=p)) +
  geom_dotplot(binaxis = "y", binwidth=0.1, stackdir = "center", alpha=0.25, position="dodge") +
  geom_pointrange(aes(fill=p),
                  stat="summary", 
                  fun.data = "mean_se", 
                  position =  position_dodge(width = 0.9), 
                  shape=21,
                  size=0.5) +
  scale_x_discrete("", labels=sender_points_label) +
  scale_fill_manual("Base Rate", values=my_red, labels=base_rate_label) +
  theme_minimal()
ggsave("img/individual/beta.pdf")

lieMLE.pred %>%
  select(subj, p, expt, mu, mu.se) %>%
  unique() %>%
  #filter(beta.se < 10) %>%
  ggplot(aes(x=expt, y=mu, fill=p)) +
  geom_dotplot(binaxis = "y", binwidth=0.5, stackdir = "center", alpha=0.25, position="dodge") +
  geom_pointrange(aes(fill=p),
                  stat="summary", 
                  fun.data = "mean_se", 
                  position =  position_dodge(width = 0.9), 
                  shape=21,
                  size=0.5) +
  scale_x_discrete("", labels=sender_points_label) +
  scale_fill_manual("Base Rate", values=my_red, labels=base_rate_label) +
  theme_minimal()
ggsave("img/individual/mu.pdf")

 lieMLE.pred %>%
  select(subj, p, expt, k, p.lie.k) %>%
  unique() %>%
  mutate(p = as.factor(paste("p =", p)),
         expt = factor(ifelse(expt=="expt4", "Sender gets points for red", "Sender gets points for blue"),
                       levels = c("Sender gets points for red", "Sender gets points for blue"))) %>%
  ggplot(aes(x=k, y=p.lie.k, colour=subj)) +
  geom_line(size=0.25) +
  scale_y_continuous("Probability of Lying") +
  guides(colour=FALSE) +
  facet_grid(p~expt) +
  theme_bw()
ggsave("img/individual/probLie.pdf")












humanLie %>%
  group_by(subjID, lie) %>%
  count() %>%
  group_by(subjID) %>%
  mutate(prop = n/sum(n)) %>%
  filter(!lie) %>%
  ggplot(aes(x=prop)) +
  geom_histogram()
