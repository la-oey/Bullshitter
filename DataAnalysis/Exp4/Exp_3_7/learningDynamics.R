setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7")

library(tidyverse)
bs.final <- read.csv("bsfinal_anon.csv")

glimpse(bs.final)
my_red = c("#ffd5d6","#fc7f81","#fd2428")
sender_points_label = c(
  "expt4" = "Sender gets\npoints for red",
  "expt5" = "Sender gets\npoints for blue"
)


# examine lie detecting learning
detectLearn <- bs.final %>%
  filter(roleCurrent == "bullshitDetector") %>%
  mutate(trialHalf = ifelse(trialNumber <= 50, "1st half", "2nd half"),
         correctCall = ifelse(lie, callBS == "True", callBS == "False"),
         expt = ifelse(expt=="expt4", "sender gets points for red", "sender gets points for blue"),
         probabilityRed = paste("p =", probabilityRed)) %>%
  group_by(probabilityRed, expt, trialHalf, lie) %>%
  summarise(callCorrProp = sum(correctCall)/n(),
            se = sqrt((callCorrProp*(1-callCorrProp)/n())),
            lower = callCorrProp - se,
            upper = callCorrProp + se) %>%
  mutate(lie = ifelse(lie, "Correct Rejection", "Correct Acceptance"))
ggplot(detectLearn, aes(x=lie, y=callCorrProp, fill=trialHalf)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(data=detectLearn, aes(ymin=lower, ymax=upper), width=0.5, position=position_dodge(.9)) +
  scale_x_discrete("") +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  facet_grid(expt~probabilityRed) +
  theme_bw()
ggsave("img/learning/learn_detect.png", width=10, height=6)



# examine lying learning
meanSenderPoints <- bs.final %>%
  filter(roleCurrent == "bullshitter") %>%
  mutate(trialHalf = ifelse(trialNumber <= 50, "1st half", "2nd half"),
         pointAdvantage = playerTrialScore - oppTrialScore,
         expt = factor(ifelse(expt=="expt4", "Sender gets points for red", "Sender gets points for blue")),
         expt = fct_relevel(expt, c("Sender gets points for red", "Sender gets points for blue")),
         probabilityRed = paste0(probabilityRed*100, "% red")) %>%
  group_by(probabilityRed, expt, subjID, trialHalf) %>%
  summarise(mean=mean(pointAdvantage))
meanSenderPoints %>%
  group_by(probabilityRed, expt, trialHalf) %>%
  summarise(meanMean = mean(mean)) %>%
  ggplot(aes(x=trialHalf, y=meanMean)) +
  geom_path(group=1) +
  geom_dotplot(data=meanSenderPoints, aes(x=trialHalf, y=mean, fill=probabilityRed), binaxis = "y", binwidth=0.5, stackdir = "center", alpha=0.8) +
  stat_summary(data=meanSenderPoints, aes(x=trialHalf, y=mean, fill=probabilityRed), size=0.1) +
  scale_x_discrete("") +
  scale_y_continuous("Sender's Mean Point Advantage", limits=c(-10,15), expand=c(0,0), breaks=seq(-10,10,5)) +
  scale_fill_manual(values=my_red) +
  guides(fill=FALSE) +
  facet_grid(expt ~ probabilityRed, labeller=labeller(sender_points_label)) +
  theme_bw()
ggsave("img/learning/learn_pointsSender.png", width=7, height=4.5)

# 1st 10 vs last 10 trials
meanSenderPointsTen <- bs.final %>%
  filter(roleCurrent == "bullshitter") %>%
  mutate(trialHalf = case_when(
    trialNumber <= 20 ~ "1st ten", 
    trialNumber >= 80 ~ "2nd ten"
  ),
  pointAdvantage = playerTrialScore - oppTrialScore,
  expt = factor(ifelse(expt=="expt4", "Sender gets points for red", "Sender gets points for blue")),
  expt = fct_relevel(expt, c("Sender gets points for red", "Sender gets points for blue")),
  probabilityRed = paste0(probabilityRed*100, "% red")) %>%
  filter(!is.na(trialHalf)) %>%
  group_by(probabilityRed, expt, subjID, trialHalf) %>%
  summarise(mean=mean(pointAdvantage))
meanSenderPointsTen %>%
  group_by(probabilityRed, expt, trialHalf) %>%
  summarise(meanMean = mean(mean)) %>%
  ggplot(aes(x=trialHalf, y=meanMean)) +
  geom_path(group=1) +
  geom_dotplot(data=meanSenderPoints, aes(x=trialHalf, y=mean, fill=probabilityRed), binaxis = "y", binwidth=0.5, stackdir = "center", alpha=0.8) +
  stat_summary(data=meanSenderPoints, aes(x=trialHalf, y=mean, fill=probabilityRed), size=0.1) +
  scale_x_discrete("") +
  scale_y_continuous("Sender's Mean Point Advantage", limits=c(-10,15), expand=c(0,0), breaks=seq(-10,10,5)) +
  scale_fill_manual(values=my_red) +
  guides(fill=FALSE) +
  facet_grid(expt ~ probabilityRed, labeller=labeller(sender_points_label)) +
  theme_bw()
ggsave("img/learning/learn_pointsSender.png", width=7, height=4.5)


bs.final %>%
  filter(roleCurrent == "bullshitter") %>%
  mutate(trialHalf = ifelse(trialNumber <= 50, "1st half", "2nd half"),
         pointAdvantage = playerTrialScore - oppTrialScore,
         expt = ifelse(expt=="expt4", "sender gets points for red", "sender gets points for blue"),
         probabilityRed = paste("p =", probabilityRed)) %>%
  group_by(probabilityRed, expt, subjID, trialHalf) %>%
  summarise(mean=mean(pointAdvantage)) %>%
  ggplot(aes(x=trialHalf, y=mean, colour=subjID)) +
  geom_point(size=0.5, alpha=0.5) +
  geom_path(aes(group=subjID, colour=subjID), size=0.2, alpha=0.5) +
  stat_summary(size=0.1) +
  scale_x_discrete("") +
  guides(colour=FALSE) +
  facet_grid(expt ~ probabilityRed) +
  theme_bw()

meanSenderPoints %>%
  ggplot(aes(x=trialHalf, y=mean, fill=trialHalf)) +
  geom_dotplot(binaxis = "y", binwidth=0.5, stackdir = "center", alpha=0.25) +
  stat_summary(size=0.1) +
  geom_path(group=1, size=0.1, alpha=0.25) +
  scale_x_discrete("") +
  guides(fill=FALSE) +
  facet_grid(expt ~ probabilityRed) +
  theme_bw()





detectLearn <- bs.final %>%
  filter(roleCurrent == "bullshitDetector") %>%
  mutate(trialHalf = case_when(
    trialNumber <= 20 ~ "1st ten", 
    trialNumber > 80 ~ "2nd ten"
  ),
  correctCall = ifelse(lie, callBS == "True", callBS == "False"),
  expt = factor(ifelse(expt=="expt4", "Sender gets points for red", "Sender gets points for blue")),
  expt = fct_relevel(expt, c("Sender gets points for red", "Sender gets points for blue")),
  probabilityRed = paste0(probabilityRed*100, "% red")) %>%
  filter(!is.na(trialHalf)) %>%
  group_by(probabilityRed, expt, trialHalf, lie) %>%
  summarise(callCorrProp = sum(correctCall)/n(),
            se = sqrt((callCorrProp*(1-callCorrProp)/n())),
            lower = callCorrProp - se,
            upper = callCorrProp + se) %>%
  mutate(lie = ifelse(lie, "Correct Rejection", "Correct Acceptance"))

detectLearn %>%
  filter(lie == "Correct Acceptance") %>%
  ggplot(aes(x=trialHalf, y=callCorrProp, fill=probabilityRed)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(data=filter(detectLearn, lie=="Correct Acceptance"), 
                aes(ymin=lower, ymax=upper), 
                width=0.3, position=position_dodge(.9)) +
  geom_path(group=1) +
  ggtitle("Correct Acceptances") +
  scale_x_discrete("") +
  scale_y_continuous("Proportion of Correct Acceptances", limits=c(0,1), expand=c(0,0)) +
  scale_fill_manual(values=my_red) +
  guides(fill=FALSE) +
  facet_grid(expt~probabilityRed) +
  theme_bw() +
  theme(axis.text.y = element_text(size=7))
ggsave("img/learning/learn_detect_accept.png", 
       width=7, 
       height=4.5)

detectLearn %>%
  filter(lie == "Correct Rejection") %>%
  ggplot(aes(x=trialHalf, y=callCorrProp, fill=probabilityRed)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(data=filter(detectLearn, lie=="Correct Rejection"), 
                aes(ymin=lower, ymax=upper), 
                width=0.3, position=position_dodge(.9)) +
  geom_path(group=1) +
  ggtitle("Correct Rejections") +
  scale_x_discrete("") +
  scale_y_continuous("Proportion of Correct Rejections", limits=c(0,1), expand=c(0,0)) +
  guides(fill=FALSE) +
  scale_fill_manual(values=my_red) +
  facet_grid(expt~probabilityRed) +
  theme_bw() +
  theme(axis.text.y = element_text(size=7))
ggsave("img/learning/learn_detect_reject.png", 
       width=7, 
       height=4.5)
