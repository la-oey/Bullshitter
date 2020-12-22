# plot models

setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7/mleFits/")

source("optimizeModels.R")
library(gridExtra)

modelEvals <- data.frame(
  model = c(
    "random sender",
    "always truth",
    "everybody lies",
    "some people lie",
    "no ToM",
    "recursive ToM",
    "random receiver",
    "signif testing",
    "no ToM",
    "recursive ToM"
  ),
  role = c(
    rep("sender", 6),
    rep("receiver", 4)
  ),
  AIC = c(
    2*0 + 2*randomSenderEval,
    2*1 + alwaysTruthEval@m2logL,
    2*2 + everybodyLiesEval@m2logL,
    2*3 + somePeopleLieEval@m2logL,
    2*2 + noToMeval.s,
    2*3 + recurseToMeval.s,
    2*0 + 2*randomReceiverEval,
    2*2 + signifTestingEval@m2logL,
    2*2 + noToMeval.r,
    2*3 + recurseToMeval.r
  )
)


senderEval <- modelEvals %>%
  filter(role == "sender", model != "random sender") %>%
  mutate(model = factor(model, levels=c("always truth",
                                        "everybody lies",
                                        "some people lie",
                                        "no ToM",
                                        "recursive ToM"))) %>%
  ggplot(aes(x=model, y=AIC)) +
  geom_point(stat="identity") +
  ggtitle("Sender") +
  scale_x_discrete("model") +
  scale_y_continuous(limits=c(24500, 26500)) +
  theme_minimal()
receiverEval <- modelEvals %>%
  filter(role == "receiver") %>%
  mutate(model = factor(model, levels=c("random receiver",
                                        "signif testing",
                                        "no ToM",
                                        "recursive ToM"))) %>%
  ggplot(aes(x=model, y=AIC)) +
  geom_point(stat="identity") +
  ggtitle("Receiver") +
  scale_x_discrete("model") +
  scale_y_continuous(limits=c(11000, 13000)) +
  theme_minimal()
grid.arrange(senderEval, receiverEval, ncol=1)
