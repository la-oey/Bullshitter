library(tidyverse)
library(gridExtra)
library(grid)
library(ggpubr)
source('modelFunc.R')


#p.bs = 0.5+(0.5-0.5)*pbinom(0:N, N, P)
P = 0.5
p.bs = pbinom(0:N, N, P)
#p.bs = rep(0.5,11)
r.decay = 0.1

#ggsave("img/showState_expt1.png",state1)
p.lie = bluffer(p.bs, P)

# ------

ideal <- showState(150, 0.5, p.bs, r.decay)
idealFig <- annotate_figure(ideal, top=text_grob("Recursive Inference Model", size=12))
#ggsave("img/ideal.png", idealFig)

# ------

# Inference over our AI from Expt 1, no recursion, b/c our AI doesn't perform inference

# Model Computer Caller
p.world = 0.5
comp.caller(0:10, p.world)
comp_detect <- data.frame(ksay=0:10, p.bs.ksay=comp.caller(0:10, 0.5)) %>%
  ggplot(aes(x=ksay, y=p.bs.ksay)) +
  geom_line() +
  scale_x_continuous("Reported (k*)") +
  scale_y_continuous(bquote("" *~P[D]* "(BS | k*)")) +
  theme_minimal()
comp.bluffer(p.world)
comp_liar <- data.frame(k=0:10, ksay=c(colSums(KSAY*comp.bluffer(0.5)))) %>%
  ggplot(aes(x=k, y=ksay)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, colour="red", linetype=2) +
  scale_x_continuous("Actual (k)") +
  scale_y_continuous("Reported (k*)", limits=c(0,10.05)) +
  theme_minimal()
comp <- grid.arrange(comp_liar, comp_detect, nrow=2)
compFig <- annotate_figure(comp, top=text_grob("Non-Inferential Model (Expt AI)", size=12))
#ggsave("img/comp.png", compFig)



bluffer(comp.caller(0:10, p.world), p.world)
modelComp_liar <- data.frame(k=0:10, ksay=c(colSums(KSAY*bluffer(comp.caller(0:10, 0.5), 0.5)))) %>%
  ggplot(aes(x=k, y=ksay)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, colour="red", linetype=2) +
  scale_x_continuous("Actual (k)") +
  scale_y_continuous("", limits=c(0,10.05)) +
  #scale_y_continuous("Reported (k*)", limits=c(0,10.05)) +
  theme_minimal()
caller(comp.bluffer(p.world), p.world)
modelComp_detect <- data.frame(ksay=0:10, p.bs.ksay=c(caller(comp.bluffer(0.5), 0.5))) %>%
  ggplot(aes(x=ksay, y=p.bs.ksay)) +
  geom_line() +
  scale_x_continuous("Reported (k*)") +
  scale_y_continuous("") +
  #scale_y_continuous("P(BS | k*)") +
  theme_minimal()
modelComp <- grid.arrange(modelComp_liar, modelComp_detect, nrow=2)
modelCompFig <- annotate_figure(modelComp, top=text_grob("Oracle Model", size=12))
#ggsave("img/modelComp.png", modelCompFig)

# ------

pickLie <- function(prob.lie, real.val){
  expectedVal = prob.lie * real.val
  if(real.val < 10){
    lies = (real.val+1):10
    prob.eachLie = (1-prob.lie)/length(lies)
    for(l in lies){
      expectedVal = expectedVal + l*prob.eachLie
    }
  }
  else{
    expectedVal = 10
  }
  return(expectedVal)
}
dumbLiar <- data.frame(real=0:10) %>%
  mutate(reported = mapply(pickLie, 0.5, real)) %>%
  ggplot(aes(x=real, y=reported)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, colour="red", linetype=2) +
  scale_x_continuous("") +
  scale_y_continuous("Expected Reported (k*)", limits=c(0,10)) +
  #scale_x_continuous("Actual (k)") +
  #scale_y_continuous("Reported (k*)", limits=c(0,10)) +
  theme_minimal()

hypothesis.test <- function(x, p.world){
  as.numeric(binom.test(x, 10, p.world, alternative="two.sided")$p.value < 0.05)
}

dumbDetect.hypoth <- data.frame(reported=0:10) %>%
  mutate(p.bs.ksay = mapply(hypothesis.test, reported, 0.5)) %>%
  ggplot(aes(x=reported, y=p.bs.ksay)) +
  geom_line() +
  scale_x_continuous("") +
  #scale_x_continuous("Reported (k*)") +
  scale_y_continuous(bquote("Expected" *~P[D]* "(BS | k*)"), limits=c(0,1)) +
  theme_minimal()

dumbDetect.dbinom <- data.frame(reported=0:10, p.bs.ksay = 1-dbinom(0:10, 10, 0.5)) %>%
  ggplot(aes(x=reported, y=p.bs.ksay)) +
  geom_line() +
  scale_x_continuous("Reported (k*)") +
  scale_y_continuous("P(BS | k*)") +
  theme_minimal()

dumb <- grid.arrange(dumbLiar, dumbDetect.hypoth, nrow=2)
dumbFig <- annotate_figure(dumb, top=text_grob("No-Theory-of-Mind Model", size=12))
#ggsave("img/dumb.png", dumbFig)

modelPredict <- grid.arrange(dumbFig, modelCompFig, idealFig, ncol=3)
ggsave("img/modelPredict.png", modelPredict, width=8, height=5)

# ------

# Varying Mean of Prior Distribution (as in expt 2)

plots = lapply(c(0.2,0.5,0.8), function(x) showState(150, x, pbinom(0:N, N, x), r.decay))
fig2 <- do.call(grid.arrange, c(plots, ncol=3))
state2 <- annotate_figure(fig2, top=text_grob("Recursive Rational Model", size=18))
ggsave("img/showState_expt2.png",state2)

# ------

# Varying Mean of Prior Distribution (as in expt 2) w/ recursion decay at 0.50

plots = lapply(c(0.2,0.5,0.8), function(x) showState(150, x, pbinom(0:N, N, x), 0.50))
state3 <- do.call(grid.arrange, c(plots, ncol=3))
ggsave("img/img/showState_expt2_rdecay0.50.png",state3)

# ------

# Varying Mean of Prior Distribution (as in expt 2) w/ recursion decay at 0

plots = lapply(c(0.2,0.5,0.8), function(x) showState(150, x, pbinom(0:N, N, x), 0))
fig4 <- do.call(grid.arrange, c(plots, ncol=3))
state4 <- annotate_figure(fig4, top=text_grob("Non-Recursive Rational Model", size=18))
ggsave("img/showState_expt2_rdecay0.png",state4)

# ------

# Varying Prior Distribution About Lying (uniform vs binomial)

P = 0.5
p.bs = rep(0.5,11)
qunif <- showState(10, P, p.bs, r.decay)
qunif100 <- showState(100, P, p.bs, r.decay)
p.bs = pbinom(0:N, N, P)
qbinom <- showState(10, P, p.bs, r.decay)
qbinom100 <- showState(100, P, p.bs, r.decay)
manipPriors <- grid.arrange(qunif, qunif100, qbinom, qbinom100, ncol=4)
ggsave("img/varyPriors.png", manipPriors)


# -----

# No Recursion

ggsave("img/noRecursion.png", showState(150, 0.5, pbinom(0:N, N, 0.5), 0))

# ----

# Varying decay in recursion
plots = lapply(seq(0, 0.9, 0.2), function(x) showState(150, 0.5, pbinom(0:N, N, 0.5), x))
state_decayRecurse <- do.call(grid.arrange, c(plots, ncol=5))
ggsave("img/showState_decay.png",state_decayRecurse)


# ------


# sumMatrix <- matrix(rep(rep(0, (N+1)), (N+1)), nrow=N+1)
# sumP.bs.ksay.r <- rep(0,11)
# iterations <- 50000
# r.caller = function(DEPTH, p.world){
#   if(runif(1)<DEPTH){
#     return(pbinom(0:N, N, 0.5))
#   } else {
#     p.bs.ksay(0:N, r.bluffer(DEPTH, p.world))
#   }
# }
# for(i in 1:iterations){
#   p.bs.ksay.r = r.caller(0.1, 0.5)
#   sumP.bs.ksay.r = sumP.bs.ksay.r + p.bs.ksay.r
#   sumMatrix = sumMatrix + p.ksay.k(ksay = KSAY, k = K, p.bs.ksay = p.bs.ksay.r)
# }
# 
# avgMatrix <- sumMatrix/iterations
# avgP.bs.ksay.r <- sumP.bs.ksay.r/iterations
# 
# 
# sim <- stepPlot_sim(avgMatrix, avgP.bs.ksay.r, 0.5)
# #ggsave("img/simulate.png",sim)
# 
# 
# sumMatrix <- matrix(rep(rep(0, (N+1)), (N+1)), nrow=N+1)
# sumP.bs.ksay.r <- rep(0,11)
# iterations <- 50000
# r.caller = function(DEPTH, p.world){
#   if(runif(1)<DEPTH){
#     return(pbinom(0:N, N, 0.2))
#   } else {
#     p.bs.ksay(0:N, r.bluffer(DEPTH, p.world))
#   }
# }
# for(i in 1:iterations){
#   p.bs.ksay.r = r.caller(0.1, 0.2)
#   sumP.bs.ksay.r = sumP.bs.ksay.r + p.bs.ksay.r
#   sumMatrix = sumMatrix + p.ksay.k(ksay = KSAY, k = K, p.bs.ksay = p.bs.ksay.r)
# }
# 
# avgMatrix <- sumMatrix/iterations
# avgP.bs.ksay.r <- sumP.bs.ksay.r/iterations
# 
# sim0.2 <- stepPlot_sim(avgMatrix, avgP.bs.ksay.r, 0.2)
# 
# 
# sumMatrix <- matrix(rep(rep(0, (N+1)), (N+1)), nrow=N+1)
# sumP.bs.ksay.r <- rep(0,11)
# iterations <- 50000
# r.caller = function(DEPTH, p.world){
#   if(runif(1)<DEPTH){
#     return(pbinom(0:N, N, 0.8))
#   } else {
#     p.bs.ksay(0:N, r.bluffer(DEPTH, p.world))
#   }
# }
# for(i in 1:iterations){
#   p.bs.ksay.r = r.caller(0.1, 0.8)
#   sumP.bs.ksay.r = sumP.bs.ksay.r + p.bs.ksay.r
#   sumMatrix = sumMatrix + p.ksay.k(ksay = KSAY, k = K, p.bs.ksay = p.bs.ksay.r)
# }
# 
# avgMatrix <- sumMatrix/iterations
# avgP.bs.ksay.r <- sumP.bs.ksay.r/iterations
# 
# sim0.8 <- stepPlot_sim(avgMatrix, avgP.bs.ksay.r, 0.8)
# 
# simPriors<- grid.arrange(sim0.2, sim, sim0.8, nrow=3)
# 
# ggsave("img/sim_expt2.png", simPriors)
# 
# 
# 
# 
# 
# 
# sumMatrix <- matrix(rep(rep(0, (N+1)), (N+1)), nrow=N+1)
# sumP.bs.ksay.r <- rep(0,11)
# iterations <- 20000
# r.caller = function(DEPTH, p.world){
#   if(runif(1)<DEPTH){
#     return(pbinom(0:N, N, 0.5))
#   } else {
#     p.bs.ksay(0:N, r.bluffer(DEPTH, p.world))
#   }
# }
# for(i in 1:iterations){
#   p.bs.ksay.r = r.caller(1, 0.5)
#   sumP.bs.ksay.r = sumP.bs.ksay.r + p.bs.ksay.r
#   sumMatrix = sumMatrix + p.ksay.k(ksay = KSAY, k = K, p.bs.ksay = p.bs.ksay.r)
# }
# 
# avgMatrix <- sumMatrix/iterations
# avgP.bs.ksay.r <- sumP.bs.ksay.r/iterations
# 
# sim <- stepPlot_sim(avgMatrix, avgP.bs.ksay.r, 0.5)
# 
# sumMatrix <- matrix(rep(rep(0, (N+1)), (N+1)), nrow=N+1)
# sumP.bs.ksay.r <- rep(0,11)
# iterations <- 20000
# r.caller = function(DEPTH, p.world){
#   if(runif(1)<DEPTH){
#     return(pbinom(0:N, N, 0.2))
#   } else {
#     p.bs.ksay(0:N, r.bluffer(DEPTH, p.world))
#   }
# }
# for(i in 1:iterations){
#   p.bs.ksay.r = r.caller(1, 0.2)
#   sumP.bs.ksay.r = sumP.bs.ksay.r + p.bs.ksay.r
#   sumMatrix = sumMatrix + p.ksay.k(ksay = KSAY, k = K, p.bs.ksay = p.bs.ksay.r)
# }
# 
# avgMatrix <- sumMatrix/iterations
# avgP.bs.ksay.r <- sumP.bs.ksay.r/iterations
# 
# sim0.2 <- stepPlot_sim(avgMatrix, avgP.bs.ksay.r, 0.2)
# 
# 
# sumMatrix <- matrix(rep(rep(0, (N+1)), (N+1)), nrow=N+1)
# sumP.bs.ksay.r <- rep(0,11)
# iterations <- 20000
# r.caller = function(DEPTH, p.world){
#   if(runif(1)<DEPTH){
#     return(pbinom(0:N, N, 0.8))
#   } else {
#     p.bs.ksay(0:N, r.bluffer(DEPTH, p.world))
#   }
# }
# for(i in 1:iterations){
#   p.bs.ksay.r = r.caller(1, 0.8)
#   sumP.bs.ksay.r = sumP.bs.ksay.r + p.bs.ksay.r
#   sumMatrix = sumMatrix + p.ksay.k(ksay = KSAY, k = K, p.bs.ksay = p.bs.ksay.r)
# }
# 
# avgMatrix <- sumMatrix/iterations
# avgP.bs.ksay.r <- sumP.bs.ksay.r/iterations
# 
# sim0.8 <- stepPlot_sim(avgMatrix, avgP.bs.ksay.r, 0.8)
# 
# simPriors<- grid.arrange(sim0.2, sim, sim0.8, nrow=3)
# 
# ggsave("img/sim_expt2_norecurse.png", simPriors)






