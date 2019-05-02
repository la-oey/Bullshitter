library(tidyverse)
library(stats4)

bs <- read.csv("raw.csv")

bs <- bs %>%
  mutate(lie = drawnRed != reportedDrawn,
         player.bullshitter = ifelse(roleCurrent == "bullshitter", "human", "computer"),
         player.bullshitDetector = ifelse(roleCurrent == "bullshitDetector", "human", "computer"))

exclude.sampling <- bs %>%
  filter(reportedDrawn > 10) %>%
  .$subjID

catch <- bs %>%
  filter(catchKey != -1) %>%
  group_by(subjID) %>%
  summarise(catchCorrect = sum(catchKey == catchResponse))

exclude.catch <- catch %>%
  filter(catchCorrect <= 0.75*max(catchCorrect)) %>%
  .$subjID

bs.final <- bs %>%
  filter(!subjID %in% exclude.catch, !subjID %in% exclude.sampling, exptPart == "trial")


humanDetect <- bs.final %>%
  filter(roleCurrent == "bullshitDetector") %>%
  mutate(callBS = ifelse(callBS == "False", FALSE, TRUE),
         avgRed = probabilityRed * 10,
         reportedDrawn.sq = reportedDrawn ^ 2,
         probabilityRed.f = relevel(as.factor(probabilityRed), ref="0.5"))


logitToProb <- function(logit){
  exp(logit) / (1+exp(logit))
}

probToLogit <- function(prob){
  log(prob / (1 - prob))
}

quadratic <- function(x, a, b, c){
  a*(x-b)^2+c
}

logisticModel <- function(x, a, b, c){
  logitToProb(pmin(10, pmax(-10, quadratic(x, a, b, c))))
}

lapsePlusLogistic <- function(x, a, b, c, alph){
  alph2 = logitToProb(alph)
  a = exp(a)
  alph2/2 + (1-alph2)*logisticModel(x, a, b, c)
}

loglik <- function(x, y, a, b, c, alph){
  sum(
    dbinom(y, 1, lapsePlusLogistic(x, a, b, c, alph), log=T)
  )
}


brutefit <- function(tmp){
  nLL <- function(a, b, c, alph){
    -loglik(tmp$reportedDrawn, tmp$callBS, a, b, c, alph)+
      (a-1)^2+
      (b-5)^2+
      c^2+
      (alph+2)^2
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while(is.null(fits)){
    try(fit <- summary(mle(nLL,
                           start=list(a=rnorm(1, 0, 3),
                                      b=rnorm(1, 0, 3),
                                      c=rnorm(1, 0, 3),
                                      alph=rnorm(1, -2, 3)), method = 'BFGS'), TRUE))
    iter = iter+1
    
    if(! is.null(fit)){
      fits <- c(tmp$probabilityRed[1], -0.5*fit@m2logL, length(tmp$reportedDrawn), fit@coef[,"Estimate"], fit@coef[,"Std. Error"])
    } else {
      if(iter>100){
        fits <- c(tmp$probabilityRed[1], -9999, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      }
    }
  }
  names(fits) <- c("probabilityRed", "logL", "n", "est.a", "est.b", "est.c", "est.alph", "se.a", "se.b", "se.c", "se.alph")
  return(fits)
}

quadr.fits <- data.frame(do.call(rbind, by(humanDetect, humanDetect$probabilityRed, brutefit)))
print(paste("Failed quadratic fits:", sum(quadr.fits$logL==-9999)))


quadr.est <- quadr.fits %>%
  select(1:7) %>%
  gather("variable", "estimate", 4:7) %>%
  mutate(variable = gsub("est.", "", variable))
quadr.se <- quadr.fits %>%
  select(-c(4:7)) %>%
  gather("variable", "std.err", 4:7) %>%
  mutate(variable = gsub("se.", "", variable))
quadr.summ <- left_join(quadr.est, quadr.se, by=c("probabilityRed","logL","n","variable"))
quadr.summ %>%
  mutate(z.value = estimate/std.err,
         p.value = ifelse(pnorm(-abs(z.value)) > .000001, pnorm(-abs(z.value)), "<.000001")) %>%
  arrange(probabilityRed, variable)

# y = a(x-b)^2 + c
b_0.2 <- quadr.summ %>%
  filter(variable == "b", probabilityRed == 0.2)
b_0.5 <- quadr.summ %>%
  filter(variable == "b", probabilityRed == 0.5)
b_0.8 <- quadr.summ %>%
  filter(variable == "b", probabilityRed == 0.8)

t.test.summ <- function(m1, sd1, n1, m2, sd2, n2){
  s_p <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
  t <- (m1 - m2) / (s_p * sqrt(1/n1 + 1/n2))
  p <- pt(abs(t), n1 + n2 - 2, lower.tail=F)
  return(data.frame(m1, sd1, n1, m2, sd2, n2, t, p))
}

t.test.summ(b_0.5$estimate, b_0.5$std.err, b_0.5$n, b_0.2$estimate, b_0.2$std.err, b_0.2$n)
t.test.summ(b_0.5$estimate, b_0.5$std.err, b_0.5$n, b_0.8$estimate, b_0.8$std.err, b_0.8$n)

quadr.plot <- data.frame(x=rep(0:10,3), 
                         p=as.factor(c(rep(0.2,11), rep(0.5,11), rep(0.8,11))),
                         a=c(rep(quadr.fits$est.a[quadr.fits$probabilityRed==0.2],11),
                             rep(quadr.fits$est.a[quadr.fits$probabilityRed==0.5],11),
                             rep(quadr.fits$est.a[quadr.fits$probabilityRed==0.8],11)),
                         b=c(rep(quadr.fits$est.b[quadr.fits$probabilityRed==0.2],11),
                             rep(quadr.fits$est.b[quadr.fits$probabilityRed==0.5],11),
                             rep(quadr.fits$est.b[quadr.fits$probabilityRed==0.8],11)),
                         c=c(rep(quadr.fits$est.c[quadr.fits$probabilityRed==0.2],11),
                             rep(quadr.fits$est.c[quadr.fits$probabilityRed==0.5],11),
                             rep(quadr.fits$est.c[quadr.fits$probabilityRed==0.8],11)),
                         alph=c(rep(quadr.fits$est.alph[quadr.fits$probabilityRed==0.2],11),
                                rep(quadr.fits$est.alph[quadr.fits$probabilityRed==0.5],11),
                                rep(quadr.fits$est.alph[quadr.fits$probabilityRed==0.8],11))) %>%
  mutate(prob=lapsePlusLogistic(x, a, b, c, alph))
ggplot(quadr.plot, aes(x=x, y=prob, colour=p)) +
  geom_line() +
  scale_y_continuous(limits=c(0,1))


exptDetect <- bs.final %>%
  filter(roleCurrent == "bullshitDetector") %>%
  mutate(probabilityRed.txt = paste("p =", probabilityRed)) %>%
  group_by(probabilityRed.txt, reportedDrawn) %>%
  summarise(prop = (sum(callBS == "True")+1)/(n()+2),
            se = sqrt((prop*(1-prop)/n())),
            n = n()) %>%
  filter(n > 3) %>%
  mutate(fit="experiment")
quadr.plot2 <- data.frame(probabilityRed.txt = paste("p =", quadr.plot$p),
                          reportedDrawn = quadr.plot$x,
                          prop = quadr.plot$prob,
                          se = NA,
                          n = NA,
                          fit = "model")
quadr.plot_full <- bind_rows(exptDetect, quadr.plot2)
ggplot(data=quadr.plot_full, aes(x=reportedDrawn, y=prop, colour=probabilityRed.txt, linetype=fit)) +
  geom_point(data=filter(quadr.plot_full, fit=="experiment")) +
  geom_line() +
  geom_errorbar(data=filter(quadr.plot_full, fit=="experiment"), aes(x=reportedDrawn, colour=probabilityRed.txt, min=prop-se, max=prop+se), width=.3) +
  scale_x_continuous("Reported Marbles Drawn", limits=c(-0.2,10.2)) +
  scale_y_continuous("Proportion BS Called", limits=c(0,1)) +
  guides(colour=guide_legend(title="")) +
  theme_minimal()

