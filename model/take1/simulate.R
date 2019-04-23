library(tidyverse)
library(gridExtra)
source("modelFunc.R")

numSims = 500

df <- data.frame(role=factor(),
                 p=numeric(), 
                 decay=numeric(),
                 ks=numeric(), # ksay in Detector, k in Liar
                 n=numeric(),
                 val=numeric(),
                 se=numeric()) # prop in Detector, expLie in Liar

for(i in c(0.2, 0.5, 0.8)){
  for(j in c(1, 0.5, 0.05)){
    sim.D <- t(replicate(numSims, r.caller(j, i)))
    prop <- colMeans(sim.D)
    se <- prop * (1-prop) / sqrt(numSims)
    df <- bind_rows(df, data.frame(role="Detector", p=i, decay=j, ks=0:10, n=numSims, val=prop, se=se))
    
    sim.L <- t(replicate(numSims, expect.ksay(r.caller(j, i))))
    expLie <- colMeans(sim.L)
    se <- apply(sim.L, 2, sd) / sqrt(numSims)
    df <- bind_rows(df, data.frame(role="Liar", p=i, decay=j, ks=0:10, n=numSims, val=expLie, se=se))
  }
}

#write.csv(df,"sims.csv")

detect <- df %>%
  filter(role=="Detector") %>%
  mutate(prob = paste("p =", p),
         decay.txt = factor(paste("decay =", round(decay,2)),  levels=c("decay = 1", "decay = 0.5", "decay = 0.05"))) %>%
  ggplot(aes(x=ks, y=val, colour=prob)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax=val+se, ymin=val-se), width=0.5) +
  ggtitle("Responder") +
  scale_x_continuous("Reported Marbles Drawn") +
  scale_y_continuous("Proportion BS Called", limits=c(0,1)) +
  facet_wrap(~decay.txt) +
  theme_minimal()

lie <- df %>%
  filter(role=="Liar") %>%
  mutate(prob = paste("p =", p),
         decay.txt = factor(paste("decay =", round(decay,2)),  levels=c("decay = 1", "decay = 0.5", "decay = 0.05"))) %>%
  ggplot(aes(x=ks, y=val, colour=prob)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax=val+se, ymin=val-se), width=0.5) +
  geom_abline(intercept = 0, slope = 1, colour="red", linetype=2) +
  ggtitle("Marble Drawer") +
  scale_x_continuous("Actual Marbles Drawn") +
  scale_y_continuous("Reported Marbles Drawn", limits=c(0,10)) +
  facet_wrap(~decay.txt) +
  theme_minimal()
ggsave("spikey.png", grid.arrange(lie, detect, nrow=2))


