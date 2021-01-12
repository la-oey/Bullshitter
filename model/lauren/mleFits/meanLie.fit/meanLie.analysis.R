
# k counts

kmatrix <- bs.final %>%
  filter(roleCurrent == "bullshitter") %>%
  count(expt, probabilityRed, drawnRed) %>%
  pull(n) %>%
  rep(each=11) %>%
  array(dim=c(11,11,6))




meanLieEval = list(
  # # # # # # # #
  # # no ToM # #
  # # # # # # # #
  noToM = function(){
    print("no ToM")
    noToM.pred.kstar <- noToM.s.pred(
      noToMeval@coef['alph','Estimate'],
      noToMeval@coef['eta.S','Estimate'])
    noToM.predicted <- round(
      array(
        noToM.pred.kstar,
        dim=c(11,11,6)
      ) * kmatrix)
    fit(noToM.predicted)
  },
  
  # # # # # # # # # # #
  # # recursive ToM # #
  # # # # # # # # # # #
  recurseToM = function(){
    print("recursive ToM")
    recurseToM.pred.kstar <- recurseToM.pred(
      recurseToMeval@coef['alph','Estimate'],
      recurseToMeval@coef['eta.S','Estimate'],
      recurseToMeval@coef['eta.R','Estimate'],
      recurseToMeval@coef['lambda','Estimate'])[[2]]
    recurseToM.predicted <- round(recurseToM.pred.kstar * kmatrix)
    fit(recurseToM.predicted)
  },
  
  # # # # # # # # # # # #
  # # everybody lies # #
  # # # # # # # # # # # #
  everybodyLies = function(){
    print("everybody lies")
    everybodyLies.pred.kstar <- everybodyLies.pred(
      everybodyLiesEval@coef['lambda','Estimate'],
      everybodyLiesEval@coef['weight','Estimate'])
    everybodyLies.predicted <- round(
      array(
        everybodyLies.pred.kstar,
        dim=c(11,11,6)
      ) * kmatrix)
    fit(everybodyLies.predicted)
  },
  
  # # # # # # # # # # # #
  # # some people lie # #
  # # # # # # # # # # # #
  someLies = function(){
    print("some people lie")
    someLies.pred.kstar <- someLies.pred(
      somePeopleLieEval@coef['pTrue','Estimate'],
      somePeopleLieEval@coef['lambda','Estimate'],
      somePeopleLieEval@coef['weight','Estimate']
    )
    someLies.predicted <- round(
      array(
        someLies.pred.kstar,
        dim=c(11,11,6)
      ) * kmatrix)
    fit(someLies.predicted)
  }
)


noToMlie = meanLieEval$noToM()
recurseToMlie = meanLieEval$recurseToM()
everybodyLieslie = meanLieEval$everybodyLies()
someLieslie = meanLieEval$someLies()




###########
# figures #
###########
my_red = c("#ffd5d6","#fc7f81","#fd2428")

meanLieModels <- bind_rows(
  as.tibble(noToMlie@coef) %>% mutate(variable=row.names(noToMlie@coef), model="noToM"),
  as.tibble(recurseToMlie@coef) %>% mutate(variable=row.names(noToMlie@coef), model="recursiveToM"),
  as.tibble(everybodyLieslie@coef) %>% mutate(variable=row.names(noToMlie@coef), model="everybodyLies"),
  as.tibble(someLieslie@coef) %>% mutate(variable=row.names(noToMlie@coef), model="somePeopleLie")
) %>%
  rename(SE = `Std. Error`)

meanLieModels %>%
  filter(str_detect(variable, "alph")) %>%
  mutate(p = paste("p =", rep(c(0.2,0.5,0.8), 8)),
         expt = paste("sender gets points for",
                      rep(rep(c("red","blue"), each=3), 4)),
         meanLie = 10*logitToProb(Estimate),
         model=factor(model, 
                      levels=c("everybodyLies", "somePeopleLie", "noToM", "recursiveToM"))) %>%
  ggplot(aes(x=expt, y=meanLie, fill=p)) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  scale_x_discrete("") +
  scale_y_continuous(limits=c(0,10)) +
  scale_fill_manual("", values=my_red) +
  facet_wrap(~model) +
  theme_bw()





