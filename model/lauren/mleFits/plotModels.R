# plot models

setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/DataAnalysis/Exp4/Exp_3_7/mleFits/")

source("optimizeModels.R")
library(gridExtra)

modelEvals <- data.frame(
  model = c(
    "random sender",
    "truth or uniform lie",
    "everybody lies",
    "some people lie",
    "no ToM",
    "recursive ToM",
    "recursive ToM\n(2 alphas)",
    "random receiver",
    "signif testing",
    "no ToM",
    "recursive ToM",
    "recursive ToM\n(2 alphas)"
  ),
  role = c(
    rep("sender", 7),
    rep("receiver", 5)
  ),
  AIC = c(
    2*0 + 2*randomSenderEval,
    2*1 + alwaysTruthEval@m2logL,
    2*2 + everybodyLiesEval@m2logL,
    2*3 + somePeopleLieEval@m2logL,
    2*2 + noToMeval.s,
    2*3 + recurseToMeval.s,
    2*4 + recurseToMeval2.s,
    2*0 + 2*randomReceiverEval,
    2*2 + signifTestingEval@m2logL,
    2*2 + noToMeval.r,
    2*3 + recurseToMeval.r,
    2*4 + recurseToMeval2.r
  )
)

logitToProb(alwaysTruthEval@coef['pTrue','Estimate'])
senderEval <- modelEvals %>%
  filter(role == "sender", model != "random sender") %>%
  mutate(model = factor(model, levels=c("truth or uniform lie",
                                        "everybody lies",
                                        "some people lie",
                                        "no ToM",
                                        "recursive ToM",
                                        "recursive ToM\n(2 alphas)"))) %>%
  ggplot(aes(x=model, y=AIC)) +
  geom_point(stat="identity") +
  ggtitle("Sender") +
  scale_x_discrete("model") +
  scale_y_continuous(limits=c(24500, 26500)) +
  theme_bw()
receiverEval <- modelEvals %>%
  filter(role == "receiver") %>%
  mutate(model = factor(model, levels=c("random receiver",
                                        "signif testing",
                                        "no ToM",
                                        "recursive ToM",
                                        "recursive ToM\n(2 alphas)"))) %>%
  ggplot(aes(x=model, y=AIC)) +
  geom_point(stat="identity") +
  ggtitle("Receiver") +
  scale_x_discrete("model") +
  scale_y_continuous(limits=c(11000, 13000)) +
  theme_bw()
grid.arrange(senderEval, receiverEval, ncol=1)
ggsave("img/modelComparison.png", 
       grid.arrange(senderEval, receiverEval, ncol=1),
       width=8,
       height=8)
