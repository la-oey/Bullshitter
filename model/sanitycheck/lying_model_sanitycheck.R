source("lying_modelFunctions.R")
source("lying_model_sanitycheck_functions.R")
library(tidyverse)
library(cowplot)
library(stats4)





ALPH = 0



# 0th level

Expt = 4
(detector0 = rep(0.1,11))
detect0.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                           expt=rep(c("expt4","expt5"), each=3*11),
                           ksay=rep(0:10,3*2), 
                           callBS=rep(detector0,3))
detect0 <- detect0.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=callBS)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()
cond.detect0 <- detect0.df %>%
  mutate(p = paste0("p = ", p)) %>%
  ggplot(aes(x=ksay, y=callBS, colour=p)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_colour_manual(values = condColors.diff) +
  facet_wrap(~expt) +
  theme_bw() +
  theme(legend.position = "none")


Expt = 4
liar0_0.2_expt4 = p.L_ksay.k.r(0.2, detector0)
liar0_0.5_expt4 = p.L_ksay.k.r(0.5, detector0)
liar0_0.8_expt4 = p.L_ksay.k.r(0.8, detector0)
Expt = 5
liar0_0.2_expt5 = p.L_ksay.k.r(0.2, detector0)
liar0_0.5_expt5 = p.L_ksay.k.r(0.5, detector0)
liar0_0.8_expt5 = p.L_ksay.k.r(0.8, detector0)
round(liar0_0.5_expt4, 4)
lie0.df <- data.frame(p=rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2), 
                      expt=rep(c("expt4","expt5"), each=3*11*11),
                      k=rep(rep(0:10, each=11),3*2), 
                      ksay=rep(0:10, 11*3*2), 
                      prob=c(as.vector(liar0_0.2_expt4),
                             as.vector(liar0_0.5_expt4),
                             as.vector(liar0_0.8_expt4),
                             as.vector(liar0_0.2_expt5),
                             as.vector(liar0_0.5_expt5),
                             as.vector(liar0_0.8_expt5)))
lie0 <- lie0.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #ggtitle("P(ksay|k)") +
  theme(legend.position = "none")
data.frame(k=rep(0:10, each=11), 
           ksay=rep(0:10, 11), 
           prob=as.vector(liar0_0.5_expt4*P.K)) %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle("P(ksay)")

df <- lie0.df %>%
  mutate(counts = floor(1000*prob*(0.8*dbinom(k, 10, p))+0.2*(1/11))) %>%
  uncount(weights=counts)
fit <- summary(mle(nLL,
                           start=list(beta=rnorm(1, 0, 0.5),
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
lieMLE.pred <- data.frame(k=rep(rep(0:10,each=11),3*2), 
                                  ksay=rep(0:10, 11*3*2),
                                  p=paste0("p = ", rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2)),
                                  expt=as.factor(c(rep("expt4",11*11*3), rep("expt5",11*11*3))),
                                  alph=c(rep(fit@coef["alph0.2_4","Estimate"],11*11),
                                         rep(fit@coef["alph0.5_4","Estimate"],11*11),
                                         rep(fit@coef["alph0.8_4","Estimate"],11*11),
                                         rep(fit@coef["alph0.2_5","Estimate"],11*11),
                                         rep(fit@coef["alph0.5_5","Estimate"],11*11),
                                         rep(fit@coef["alph0.8_5","Estimate"],11*11)),
                                  alph.se=c(rep(fit@coef["alph0.2_4","Std. Error"],11*11),
                                            rep(fit@coef["alph0.5_4","Std. Error"],11*11),
                                            rep(fit@coef["alph0.8_4","Std. Error"],11*11),
                                            rep(fit@coef["alph0.2_5","Std. Error"],11*11),
                                            rep(fit@coef["alph0.5_5","Std. Error"],11*11),
                                            rep(fit@coef["alph0.8_5","Std. Error"],11*11))) %>%
  mutate(p.kstar.k = p.kstar.k(k, ksay, alph),
         binom.kstar = binom.p(ksay, alph))
param.lie <- lieMLE.pred %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=expt, y=meanAlph, fill=p)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("") +
  scale_y_continuous("Lie", limits=c(0,10), breaks=c(0,2,4,6,8,10), expand=c(0,0)) +
  scale_fill_manual(values = condColors.diff) +
  guides(fill=guide_legend(title="", override.aes=list(size=2.5))) +
  theme_minimal() +
  theme(legend.position = "none")
fit.lie0 = lieMLE.pred
param.lie0 = param.lie


Expt = 4
truth0_0.2_expt4 = p_t.ksay.r(0.2, detector0)
(truth0_0.5_expt4 = p_t.ksay.r(0.5, detector0))
truth0_0.8_expt4 = p_t.ksay.r(0.8, detector0)
Expt = 5
truth0_0.2_expt5 = p_t.ksay.r(0.2, detector0)
truth0_0.5_expt5 = p_t.ksay.r(0.5, detector0)
truth0_0.8_expt5 = p_t.ksay.r(0.8, detector0)
truth0.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                        expt=rep(c("expt4","expt5"), each=3*11),
                        ksay=rep(0:10,3*2), 
                        lie=c(truth0_0.2_expt4,
                              truth0_0.5_expt4,
                              truth0_0.8_expt4,
                              truth0_0.2_expt5,
                              truth0_0.5_expt5,
                              truth0_0.8_expt5))
truth0.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=lie)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()














# 1st level

Expt = 4
detector1_0.2_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth0_0.2_expt4)
(detector1_0.5_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth0_0.5_expt4))
detector1_0.8_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth0_0.8_expt4)
Expt = 5
detector1_0.2_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth0_0.2_expt5)
detector1_0.5_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth0_0.5_expt5)
detector1_0.8_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth0_0.8_expt5)
detect1.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                         expt=rep(c("expt4","expt5"), each=3*11),
                         ksay=rep(0:10,3*2),
                         callBS=c(detector1_0.2_expt4,
                                  detector1_0.5_expt4,
                                  detector1_0.8_expt4,
                                  detector1_0.2_expt5,
                                  detector1_0.5_expt5,
                                  detector1_0.8_expt5))
detect1 <- detect1.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=callBS)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()
cond.detect1 <- detect1.df %>%
  mutate(p = paste0("p = ", p)) %>%
  ggplot(aes(x=ksay, y=callBS, colour=p)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_colour_manual(values = condColors.diff) +
  facet_wrap(~expt) +
  theme_bw() +
  theme(legend.position = "none")


Expt = 4
liar1_0.2_expt4 = p.L_ksay.k.r(0.2, detector1_0.2_expt4)
liar1_0.5_expt4 = p.L_ksay.k.r(0.5, detector1_0.5_expt4)
liar1_0.8_expt4 = p.L_ksay.k.r(0.8, detector1_0.8_expt4)
Expt = 5
liar1_0.2_expt5 = p.L_ksay.k.r(0.2, detector1_0.2_expt5)
liar1_0.5_expt5 = p.L_ksay.k.r(0.5, detector1_0.5_expt5)
liar1_0.8_expt5 = p.L_ksay.k.r(0.8, detector1_0.8_expt5)
round(liar1_0.5_expt4, 4) 
lie1.df <- data.frame(p=rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2), 
                      expt=rep(c("expt4","expt5"), each=3*11*11),
                      k=rep(rep(0:10, each=11),3*2), 
                      ksay=rep(0:10, 11*3*2), 
                      prob=c(as.vector(liar1_0.2_expt4),
                             as.vector(liar1_0.5_expt4),
                             as.vector(liar1_0.8_expt4),
                             as.vector(liar1_0.2_expt5),
                             as.vector(liar1_0.5_expt5),
                             as.vector(liar1_0.8_expt5)))
lie1 <- lie1.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #ggtitle("P(ksay|k)") +
  theme(legend.position = "none")
lie1
data.frame(k=rep(0:10, each=11), 
           ksay=rep(0:10, 11), 
           prob=as.vector(liar1_0.5_expt4*P.K)) %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle("P(ksay)")

df <- lie1.df %>%
  mutate(counts = floor(1000*prob*(0.8*dbinom(k, 10, p))+0.2*(1/11))) %>%
  uncount(weights=counts)
fit <- summary(mle(nLL,
                   start=list(beta=rnorm(1, 0, 0.5),
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
lieMLE.pred <- data.frame(k=rep(rep(0:10,each=11),3*2), 
                          ksay=rep(0:10, 11*3*2),
                          p=paste0("p = ", rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2)),
                          expt=as.factor(c(rep("expt4",11*11*3), rep("expt5",11*11*3))),
                          alph=c(rep(fit@coef["alph0.2_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.2_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_5","Estimate"],11*11)),
                          alph.se=c(rep(fit@coef["alph0.2_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.2_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_5","Std. Error"],11*11))) %>%
  mutate(p.kstar.k = p.kstar.k(k, ksay, alph),
         binom.kstar = binom.p(ksay, alph))
param.lie <- lieMLE.pred %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=expt, y=meanAlph, fill=p)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("") +
  scale_y_continuous("", limits=c(0,10), labels=rep("",6), breaks=c(0,2,4,6,8,10), expand=c(0,0)) +
  scale_fill_manual(values = condColors.diff) +
  guides(fill=guide_legend(title="", override.aes=list(size=2.5))) +
  theme_minimal() +
  theme(legend.position = "none")
fit.lie1 = lieMLE.pred
param.lie1 = param.lie


Expt = 4
truth1_0.2_expt4 = p_t.ksay.r(0.2, detector1_0.2_expt4)
(truth1_0.5_expt4 = p_t.ksay.r(0.5, detector1_0.5_expt4))
truth1_0.8_expt4 = p_t.ksay.r(0.8, detector1_0.8_expt4)
Expt = 5
truth1_0.2_expt5 = p_t.ksay.r(0.2, detector1_0.2_expt5)
truth1_0.5_expt5 = p_t.ksay.r(0.5, detector1_0.5_expt5)
truth1_0.8_expt5 = p_t.ksay.r(0.8, detector1_0.8_expt5)
truth1.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                        expt=rep(c("expt4","expt5"), each=3*11),
                        ksay=rep(0:10,3*2), 
                        lie=c(truth1_0.2_expt4,
                              truth1_0.5_expt4,
                              truth1_0.8_expt4,
                              truth1_0.2_expt5,
                              truth1_0.5_expt5,
                              truth1_0.8_expt5))
truth1.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=lie)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()














# 2nd level

Expt = 4
detector2_0.2_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth1_0.2_expt4)
(detector2_0.5_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth1_0.5_expt4))
detector2_0.8_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth1_0.8_expt4)
Expt = 5
detector2_0.2_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth1_0.2_expt5)
(detector2_0.5_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth1_0.5_expt5))
detector2_0.8_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth1_0.8_expt5)
detect2.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                         expt=rep(c("expt4","expt5"), each=3*11),
                         ksay=rep(0:10,3*2),
                         callBS=c(detector2_0.2_expt4,
                                  detector2_0.5_expt4,
                                  detector2_0.8_expt4,
                                  detector2_0.2_expt5,
                                  detector2_0.5_expt5,
                                  detector2_0.8_expt5))
detect2 <- detect2.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=callBS)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()
cond.detect2 <- detect2.df %>%
  mutate(p = paste0("p = ", p)) %>%
  ggplot(aes(x=ksay, y=callBS, colour=p)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_colour_manual(values = condColors.diff) +
  facet_wrap(~expt) +
  theme_bw() +
  theme(legend.position = "none")


Expt = 4
liar2_0.2_expt4 = p.L_ksay.k.r(0.2, detector2_0.2_expt4)
liar2_0.5_expt4 = p.L_ksay.k.r(0.5, detector2_0.5_expt4)
liar2_0.8_expt4 = p.L_ksay.k.r(0.8, detector2_0.8_expt4)
Expt = 5
liar2_0.2_expt5 = p.L_ksay.k.r(0.2, detector2_0.2_expt5)
liar2_0.5_expt5 = p.L_ksay.k.r(0.5, detector2_0.5_expt5)
liar2_0.8_expt5 = p.L_ksay.k.r(0.8, detector2_0.8_expt5)
round(liar2_0.5_expt4, 4) 
lie2.df <- data.frame(p=rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2), 
                      expt=rep(c("expt4","expt5"), each=3*11*11),
                      k=rep(rep(0:10, each=11),3*2), 
                      ksay=rep(0:10, 11*3*2), 
                      prob=c(as.vector(liar2_0.2_expt4),
                             as.vector(liar2_0.5_expt4),
                             as.vector(liar2_0.8_expt4),
                             as.vector(liar2_0.2_expt5),
                             as.vector(liar2_0.5_expt5),
                             as.vector(liar2_0.8_expt5)))
lie2 <- lie2.df %>%
  filter(p==0.5, expt=="expt4") %>%  
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #ggtitle("P(ksay|k)") +
  theme(legend.position = "none")
lie2
data.frame(k=rep(0:10, each=11), ksay=rep(0:10, 11), prob=as.vector(liar2_0.5_expt4*P.K)) %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle("P(ksay)")

df <- lie2.df %>%
  mutate(counts = floor(1000*prob*(0.8*dbinom(k, 10, p))+0.2*(1/11))) %>%
  uncount(weights=counts)
fit <- summary(mle(nLL,
                   start=list(beta=rnorm(1, 0, 0.5),
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
lieMLE.pred <- data.frame(k=rep(rep(0:10,each=11),3*2), 
                          ksay=rep(0:10, 11*3*2),
                          p=paste0("p = ", rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2)),
                          expt=as.factor(c(rep("expt4",11*11*3), rep("expt5",11*11*3))),
                          alph=c(rep(fit@coef["alph0.2_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.2_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_5","Estimate"],11*11)),
                          alph.se=c(rep(fit@coef["alph0.2_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.2_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_5","Std. Error"],11*11))) %>%
  mutate(p.kstar.k = p.kstar.k(k, ksay, alph),
         binom.kstar = binom.p(ksay, alph))
param.lie <- lieMLE.pred %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=expt, y=meanAlph, fill=p)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("") +
  scale_y_continuous("", limits=c(0,10), labels=rep("",6), breaks=c(0,2,4,6,8,10), expand=c(0,0)) +
  scale_fill_manual(values = condColors.diff) +
  guides(fill=guide_legend(title="", override.aes=list(size=2.5))) +
  theme_minimal() +
  theme(legend.position = "none")
fit.lie2 = lieMLE.pred
param.lie2 = param.lie


Expt = 4
truth2_0.2_expt4 = p_t.ksay.r(0.2, detector2_0.2_expt4)
(truth2_0.5_expt4 = p_t.ksay.r(0.5, detector2_0.5_expt4))
truth2_0.8_expt4 = p_t.ksay.r(0.8, detector2_0.8_expt4)
Expt = 5
truth2_0.2_expt5 = p_t.ksay.r(0.2, detector2_0.2_expt5)
truth2_0.5_expt5 = p_t.ksay.r(0.5, detector2_0.5_expt5)
truth2_0.8_expt5 = p_t.ksay.r(0.8, detector2_0.8_expt5)
truth2.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                        expt=rep(c("expt4","expt5"), each=3*11),
                        ksay=rep(0:10,3*2), 
                        lie=c(truth2_0.2_expt4,
                              truth2_0.5_expt4,
                              truth2_0.8_expt4,
                              truth2_0.2_expt5,
                              truth2_0.5_expt5,
                              truth2_0.8_expt5))
truth2.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=lie)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()












# 3rd level

Expt = 4
detector3_0.2_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth2_0.2_expt4)
(detector3_0.5_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth2_0.5_expt4))
detector3_0.8_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth2_0.8_expt4)
Expt = 5
detector3_0.2_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth2_0.2_expt5)
detector3_0.5_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth2_0.5_expt5)
detector3_0.8_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth2_0.8_expt5)
detect3.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                         expt=rep(c("expt4","expt5"), each=3*11),
                         ksay=rep(0:10,3*2),
                         callBS=c(detector3_0.2_expt4,
                                  detector3_0.5_expt4,
                                  detector3_0.8_expt4,
                                  detector3_0.2_expt5,
                                  detector3_0.5_expt5,
                                  detector3_0.8_expt5))
detect3 <- detect3.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=callBS)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()
cond.detect3 <- detect3.df %>%
  mutate(p = paste0("p = ", p)) %>%
  ggplot(aes(x=ksay, y=callBS, colour=p)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_colour_manual(values = condColors.diff) +
  facet_wrap(~expt) +
  theme_bw() +
  theme(legend.position = "none")


Expt = 4
liar3_0.2_expt4 = p.L_ksay.k.r(0.2, detector3_0.2_expt4)
liar3_0.5_expt4 = p.L_ksay.k.r(0.5, detector3_0.5_expt4)
liar3_0.8_expt4 = p.L_ksay.k.r(0.8, detector3_0.8_expt4)
Expt = 5
liar3_0.2_expt5 = p.L_ksay.k.r(0.2, detector3_0.2_expt5)
liar3_0.5_expt5 = p.L_ksay.k.r(0.5, detector3_0.5_expt5)
liar3_0.8_expt5 = p.L_ksay.k.r(0.8, detector3_0.8_expt5)
round(liar3_0.5_expt4, 4)
lie3.df <- data.frame(p=rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2), 
                      expt=rep(c("expt4","expt5"), each=3*11*11),
                      k=rep(rep(0:10, each=11),3*2), 
                      ksay=rep(0:10, 11*3*2), 
                      prob=c(as.vector(liar3_0.2_expt4),
                             as.vector(liar3_0.5_expt4),
                             as.vector(liar3_0.8_expt4),
                             as.vector(liar3_0.2_expt5),
                             as.vector(liar3_0.5_expt5),
                             as.vector(liar3_0.8_expt5)))
lie3 <- lie3.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #ggtitle("P(ksay|k)") +
  theme(legend.position = "none")
lie3
data.frame(k=rep(0:10, each=11), ksay=rep(0:10, 11), prob=as.vector(liar3_0.5_expt4*P.K)) %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle("P(ksay)")

df <- lie3.df %>%
  mutate(counts = floor(1000*prob*(0.8*dbinom(k, 10, p))+0.2*(1/11))) %>%
  uncount(weights=counts)
fit <- summary(mle(nLL,
                   start=list(beta=rnorm(1, 0, 0.5),
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
lieMLE.pred <- data.frame(k=rep(rep(0:10,each=11),3*2), 
                          ksay=rep(0:10, 11*3*2),
                          p=paste0("p = ", rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2)),
                          expt=as.factor(c(rep("expt4",11*11*3), rep("expt5",11*11*3))),
                          alph=c(rep(fit@coef["alph0.2_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.2_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_5","Estimate"],11*11)),
                          alph.se=c(rep(fit@coef["alph0.2_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.2_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_5","Std. Error"],11*11))) %>%
  mutate(p.kstar.k = p.kstar.k(k, ksay, alph),
         binom.kstar = binom.p(ksay, alph))
param.lie <- lieMLE.pred %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=expt, y=meanAlph, fill=p)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("") +
  scale_y_continuous("", limits=c(0,10), labels=rep("",6), breaks=c(0,2,4,6,8,10), expand=c(0,0)) +
  scale_fill_manual(values = condColors.diff) +
  guides(fill=guide_legend(title="", override.aes=list(size=2.5))) +
  theme_minimal() +
  theme(legend.position = "none")
fit.lie3 = lieMLE.pred
param.lie3 = param.lie


Expt = 4
truth3_0.2_expt4 = p_t.ksay.r(0.2, detector3_0.2_expt4)
(truth3_0.5_expt4 = p_t.ksay.r(0.5, detector3_0.5_expt4))
truth3_0.8_expt4 = p_t.ksay.r(0.8, detector3_0.8_expt4)
Expt = 5
truth3_0.2_expt5 = p_t.ksay.r(0.2, detector3_0.2_expt5)
truth3_0.5_expt5 = p_t.ksay.r(0.5, detector3_0.5_expt5)
truth3_0.8_expt5 = p_t.ksay.r(0.8, detector3_0.8_expt5)
truth3.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                        expt=rep(c("expt4","expt5"), each=3*11),
                        ksay=rep(0:10,3*2), 
                        lie=c(truth3_0.2_expt4,
                              truth3_0.5_expt4,
                              truth3_0.8_expt4,
                              truth3_0.2_expt5,
                              truth3_0.5_expt5,
                              truth3_0.8_expt5))
truth3.df %>%
  filter(p==0.5, expt=="expt4") %>%  ggplot(aes(x=ksay, y=lie)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()













# 4th level

Expt = 4
detector4_0.2_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth3_0.2_expt4)
(detector4_0.5_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth3_0.5_expt4))
detector4_0.8_expt4 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth3_0.8_expt4)
Expt = 5
detector4_0.2_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.2, truth3_0.2_expt5)
detector4_0.5_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.5, truth3_0.5_expt5)
detector4_0.8_expt5 = mapply(p.D_bs.ksay.r,0:10, 0.8, truth3_0.8_expt5)
detect4.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                         expt=rep(c("expt4","expt5"), each=3*11),
                         ksay=rep(0:10,3*2),
                         callBS=c(detector4_0.2_expt4,
                                  detector4_0.5_expt4,
                                  detector4_0.8_expt4,
                                  detector4_0.2_expt5,
                                  detector4_0.5_expt5,
                                  detector4_0.8_expt5))
detect4 <- detect4.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=callBS)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()
cond.detect4 <- detect4.df %>%
  mutate(p = paste0("p = ", p)) %>%
  ggplot(aes(x=ksay, y=callBS, colour=p)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_colour_manual(values = condColors.diff) +
  facet_wrap(~expt) +
  theme_bw() +
  theme(legend.position = "none")


Expt = 4
liar4_0.2_expt4 = p.L_ksay.k.r(0.2, detector4_0.2_expt4)
liar4_0.5_expt4 = p.L_ksay.k.r(0.5, detector4_0.5_expt4)
liar4_0.8_expt4 = p.L_ksay.k.r(0.8, detector4_0.8_expt4)
Expt = 5
liar4_0.2_expt5 = p.L_ksay.k.r(0.2, detector4_0.2_expt5)
liar4_0.5_expt5 = p.L_ksay.k.r(0.5, detector4_0.5_expt5)
liar4_0.8_expt5 = p.L_ksay.k.r(0.8, detector4_0.8_expt5)
round(liar4_0.5_expt4, 4)
lie4.df <- data.frame(p=rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2), 
                      expt=rep(c("expt4","expt5"), each=3*11*11),
                      k=rep(rep(0:10, each=11),3*2), 
                      ksay=rep(0:10, 11*3*2), 
                      prob=c(as.vector(liar4_0.2_expt4),
                             as.vector(liar4_0.5_expt4),
                             as.vector(liar4_0.8_expt4),
                             as.vector(liar4_0.2_expt5),
                             as.vector(liar4_0.5_expt5),
                             as.vector(liar4_0.8_expt5)))
lie4 <- lie4.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #ggtitle("P(ksay|k)") +
  theme(legend.position = "none")
lie4
data.frame(k=rep(0:10, each=11), ksay=rep(0:10, 11), prob=as.vector(liar4_0.5_expt4*P.K)) %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle("P(ksay)")

df <- lie4.df %>%
  mutate(counts = floor(1000*prob*(0.8*dbinom(k, 10, p))+0.2*(1/11))) %>%
  uncount(weights=counts)
fit <- summary(mle(nLL,
                   start=list(beta=rnorm(1, 0, 0.5),
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
lieMLE.pred <- data.frame(k=rep(rep(0:10,each=11),3*2), 
                          ksay=rep(0:10, 11*3*2),
                          p=paste0("p = ", rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2)),
                          expt=as.factor(c(rep("expt4",11*11*3), rep("expt5",11*11*3))),
                          alph=c(rep(fit@coef["alph0.2_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.2_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_5","Estimate"],11*11)),
                          alph.se=c(rep(fit@coef["alph0.2_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.2_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_5","Std. Error"],11*11))) %>%
  mutate(p.kstar.k = p.kstar.k(k, ksay, alph),
         binom.kstar = binom.p(ksay, alph))
param.lie <- lieMLE.pred %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=expt, y=meanAlph, fill=p)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("") +
  scale_y_continuous("", limits=c(0,10), labels=rep("",6), breaks=c(0,2,4,6,8,10), expand=c(0,0)) +
  scale_fill_manual(values = condColors.diff) +
  guides(fill=guide_legend(title="", override.aes=list(size=2.5))) +
  theme_minimal() +
  theme(legend.position = "none")
fit.lie4 = lieMLE.pred
param.lie4 = param.lie












# Recursive
set.seed(100)
numSims = 100
recursionDepth = 0.25


detect_recurse.vect <- c()
lie_recurse.vect <- c()
for(j in c(4, 5)){
  Expt = j
  for(i in c(0.2, 0.5, 0.8)){
    sim.D <- t(replicate(numSims, recurse.D(recursionDepth, i, rep(0.1,11))))
    prop <- colMeans(sim.D)
    #se <- prop * (1-prop) / sqrt(numSims)
    detect_recurse.vect <- c(detect_recurse.vect, prop)
    
    sim.L <- t(replicate(numSims, as.vector(p.L_ksay.k.r(i, recurse.D(recursionDepth, i, rep(0.1,11))))))
    prop <- colMeans(sim.L)
    #se <- prop * (1-prop) / sqrt(numSims)
    lie_recurse.vect <- c(lie_recurse.vect, prop)
  }
}

detect_recurse.df <- data.frame(p=rep(c(rep(0.2,11), rep(0.5,11), rep(0.8,11)),2), 
                                expt=rep(c("expt4","expt5"), each=3*11),
                                ksay=rep(0:10,3*2),
                                callBS=detect_recurse.vect)
detect_recurse <- detect_recurse.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=ksay, y=callBS)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw()
cond.detect_recurse <- detect_recurse.df %>%
  mutate(p = paste0("p = ", p)) %>%
  ggplot(aes(x=ksay, y=callBS, colour=p)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_colour_manual(values = condColors.diff) +
  facet_wrap(~expt) +
  theme_bw() +
  theme(legend.position = "none")


lie_recurse.df <- data.frame(p=rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2), 
                             expt=rep(c("expt4","expt5"), each=3*11*11),
                             k=rep(rep(0:10, each=11),3*2), 
                             ksay=rep(0:10, 11*3*2), 
                             prob=lie_recurse.vect)
lie_recurse <- lie_recurse.df %>%
  filter(p==0.5, expt=="expt4") %>%
  ggplot(aes(x=k, y=ksay, fill=prob)) + 
  geom_tile() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #ggtitle("P(ksay|k)") +
  theme(legend.position = "none")
df <- lie_recurse.df %>%
  mutate(counts = floor(1000*prob*(0.8*dbinom(k, 10, p))+0.2*(1/11))) %>%
  uncount(weights=counts)
fit <- summary(mle(nLL,
                   start=list(beta=rnorm(1, 0, 0.5),
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
lieMLE.pred <- data.frame(k=rep(rep(0:10,each=11),3*2), 
                          ksay=rep(0:10, 11*3*2),
                          p=paste0("p = ", rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2)),
                          expt=as.factor(c(rep("expt4",11*11*3), rep("expt5",11*11*3))),
                          alph=c(rep(fit@coef["alph0.2_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_4","Estimate"],11*11),
                                 rep(fit@coef["alph0.2_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_5","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_5","Estimate"],11*11)),
                          alph.se=c(rep(fit@coef["alph0.2_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_4","Std. Error"],11*11),
                                    rep(fit@coef["alph0.2_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_5","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_5","Std. Error"],11*11))) %>%
  mutate(p.kstar.k = p.kstar.k(k, ksay, alph),
         binom.kstar = binom.p(ksay, alph))
param.lie <- lieMLE.pred %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=expt, y=meanAlph, fill=p)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("") +
  scale_y_continuous("", limits=c(0,10), labels=rep("",6), breaks=c(0,2,4,6,8,10), expand=c(0,0)) +
  scale_fill_manual(values = condColors.diff) +
  guides(fill=guide_legend(title="", override.aes=list(size=2.5))) +
  theme_minimal() +
  theme(legend.position = "none")
fit.lie_recurse = lieMLE.pred
param.lie_recurse = param.lie





plot_grid(detect0, detect1, detect2, detect3, detect4, detect_recurse,
                        lie0, lie1, lie2, lie3, lie4, lie_recurse,
                        nrow=2) +
  draw_label("Level 0", fontface="bold", size=12, y=0.95, x=1/12, hjust=0) +
  draw_label("Level 1", fontface="bold", size=12, y=0.95, x=3/12, hjust=0) +
  draw_label("Level 2", fontface="bold", size=12, y=0.95, x=5/12, hjust=0) +
  draw_label("Level 3", fontface="bold", size=12, y=0.95, x=7/12, hjust=0) +
  draw_label("Level 4", fontface="bold", size=12, y=0.95, x=9/12, hjust=0) +
  draw_label("Recurse", fontface="bold", size=12, y=0.95, x=11/12-.005, hjust=0)
filename1 = paste0("img/alph",ALPH,"_fig1.png")
ggsave(filename1, height = 10, width = 18)

plot_grid(cond.detect0, cond.detect1, cond.detect2, cond.detect3, cond.detect4, cond.detect_recurse,
          param.lie0, param.lie1, param.lie2, param.lie3, param.lie4, param.lie_recurse,
          nrow=2) +
  draw_label("Level 0", fontface="bold", size=12, y=0.95, x=1/12, hjust=0) +
  draw_label("Level 1", fontface="bold", size=12, y=0.95, x=3/12, hjust=0) +
  draw_label("Level 2", fontface="bold", size=12, y=0.95, x=5/12, hjust=0) +
  draw_label("Level 3", fontface="bold", size=12, y=0.95, x=7/12, hjust=0) +
  draw_label("Level 4", fontface="bold", size=12, y=0.95, x=9/12, hjust=0) +
  draw_label("Recurse", fontface="bold", size=12, y=0.95, x=11/12-.005, hjust=0)
filename2 = paste0("img/alph",ALPH,"_fig2.png")
ggsave(filename2, height = 10, width = 18)










# attempt to turn into function

# filter.prevdf <- function(detect.df, prob, exp){
#   detect.df %>%
#     filter(p==prob, expt==paste0("expt",exp)) %>%
#     .[ncol(.)] %>% #get values in last column
#     as.vector()
# }
# 
# lie.level <- function(level, detect.df){
#   Expt = 4
#   liar_0.2_expt4 = p.L_ksay.k.r(0.2, filter.prevdf(detect.df, 0.2, Expt))
#   liar_0.5_expt4 = p.L_ksay.k.r(0.5, filter.prevdf(detect.df, 0.5, Expt))
#   liar_0.8_expt4 = p.L_ksay.k.r(0.8, filter.prevdf(detect.df, 0.8, Expt))
#   Expt = 5
#   liar_0.2_expt5 = p.L_ksay.k.r(0.2, filter.prevdf(detect.df, 0.2, Expt))
#   liar_0.5_expt5 = p.L_ksay.k.r(0.5, filter.prevdf(detect.df, 0.5, Expt))
#   liar_0.8_expt5 = p.L_ksay.k.r(0.8, filter.prevdf(detect.df, 0.8, Expt))
#   
#   data.frame(p=rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),2), 
#              expt=rep(c("expt4","expt5"), each=3*11*11),
#              k=rep(rep(0:10, each=11),3*2), 
#              ksay=rep(0:10, 11*3*2), 
#              prob=c(as.vector(liar_0.2_expt4),
#                     as.vector(liar_0.5_expt4),
#                     as.vector(liar_0.8_expt4),
#                     as.vector(liar_0.2_expt5),
#                     as.vector(liar_0.5_expt5),
#                     as.vector(liar_0.8_expt5)))
# }
