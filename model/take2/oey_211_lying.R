library(tidyverse)
source("lying_modelFunctions.R")

numSims = 100

df <- data.frame(role=factor(),
                 p=numeric(), 
                 decay=numeric(),
                 ks=numeric(), # ksay in Detector, k in Liar
                 n=numeric(),
                 val=numeric(),
                 se=numeric()) # prop in Detector, expLie in Liar

for(i in c(0.2, 0.5, 0.8)){
  for(j in c(1, 0.25, 0.05)){
    sim.D <- t(replicate(numSims, recurse.D(j, i, rep(0.1,11))))
    prop <- colMeans(sim.D)
    se <- prop * (1-prop) / sqrt(numSims)
    df <- bind_rows(df, data.frame(role="Detector", p=i, decay=j, ks=0:10, n=numSims, val=prop, se=se))
    
    sim.L <- t(replicate(numSims, exp.ksay(i, recurse.D(j, i, rep(0.1,11)))))
    expLie <- colMeans(sim.L)
    se <- apply(sim.L, 2, sd) / sqrt(numSims)
    df <- bind_rows(df, data.frame(role="Liar", p=i, decay=j, ks=0:10, n=numSims, val=expLie, se=se))
  }
}

#write.csv(df,"sims.csv")

  
df %>%
  filter(role=="Liar") %>%
  mutate(p.txt = paste("p =", p),
         decay.txt = paste("decay =", round(decay,2))) %>%
  ggplot(aes(x=ks, y=val)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax=val+se, ymin=val-se), width=0.5) +
  geom_abline(intercept = 0, slope = 1, colour="red", linetype=2) +
  ggtitle("Marble Drawer") +
  scale_x_continuous("Actual Marbles Drawn") +
  scale_y_continuous("Reported Marbles Drawn", limits=c(0,10)) +
  facet_grid(decay.txt ~ p.txt) +
  theme_minimal()
ggsave("img/liar.png")

df %>%
  filter(role=="Detector") %>%
  mutate(p.txt = paste("p =", p),
         decay.txt = paste("decay =", round(decay,2))) %>%
  ggplot(aes(x=ks, y=val)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax=val+se, ymin=val-se), width=0.5) +
  ggtitle("Responder") +
  scale_x_continuous("Reported Marbles Drawn") +
  scale_y_continuous("Proportion BS Called", limits=c(0,1)) +
  facet_grid(decay.txt ~ p.txt) +
  theme_minimal()
ggsave("img/detector.png")

  