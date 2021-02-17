setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/model/lauren/mleFits/")
load("recurseToMfit.Rdata")

source("models/recursiveToM_model.R")
source("models/somePeopleLie_model.R")



humanLieCounts <- humanLie %>%
  count(expt, probabilityRed, drawnRed, reportedDrawn) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), drawnRed=0:10, reportedDrawn=0:10, fill = list(n = 0)) %>%
  pull(n) %>%
  matrix(nrow=121)



# Functions

eval.s <- function(matr, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(matr)*ns)
}

getDiag <- function(arr){
  apply(arr, MARGIN=3, FUN=diag)
}

select_all_but_diag <- function(x) {
  matrix(x[lower.tri(x, diag = F) | upper.tri(x, diag = F)], 
         nrow = nrow(x) - 1, 
         ncol = ncol(x))
}
getLies <- function(arr){
  apply(arr, MARGIN=3, FUN=select_all_but_diag)
}

asTibble <- function(df){
  df %>%
    as_tibble() %>% 
    mutate(ksay = 0:10) %>% 
    pivot_longer(-ksay, names_to = 'k', values_to='probability') %>% 
    mutate(k = as.numeric(substr(k, 2, 10))-1,
           expt = ifelse(k < ceiling(max(k)/2), "red", "blue"),
           expt = factor(expt, levels=c("red","blue"))) %>%
    relocate(k, .before = ksay) %>%
    arrange(k, ksay) %>%
    mutate(p = rep(rep(c(0.2, 0.5, 0.8), each=121),2),
           p = as.factor(p),
           k = k %% 11,
           probTxt = paste0(round(probability*100),"%")) %>%
    relocate(c(expt,p), .before = k) %>%
    arrange(expt, p, k, ksay)
}



# Experiment Data

expt = read_csv("bsfinal_anon.csv") %>%
  mutate(roleCurrent = ifelse(roleCurrent=="bullshitter", "sender", "receiver")) %>%
  rename(p = probabilityRed, k = drawnRed, ksay = reportedDrawn) %>%
  mutate(expt = factor(ifelse(expt=="expt4", "red", "blue"), levels=c("red","blue")),
         p = as.factor(p))

expt.S.full <- expt %>%
  filter(roleCurrent == "sender")

expt.S <- expt.S.full %>%
  group_by(expt, p, k, ksay) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(expt, p, k, ksay, fill=list(n=0)) %>%
  group_by(expt, p, k) %>%
  mutate(prob = n / sum(n),
         probTxt = paste0(round(prob*100), "%"))


# Optimized Recurse ToM Prediction
origModelPred <- recurseToM.pred(
  recurseToMeval@coef['alph','Estimate'],
  recurseToMeval@coef['eta.S','Estimate'],
  recurseToMeval@coef['eta.R','Estimate'],
  recurseToMeval@coef['lambda','Estimate'])[[2]] 

asTibble(origModelPred)


origModel.diag = -2*eval.s(
  getDiag(
    origModelPred
  ),
  getDiag(
    array(
      humanLieCounts, 
      dim=c(11,11,6))
  )
)
origModel.lies = -2*eval.s(
  getLies(
    origModelPred
  ),
  getLies(
    array(
      humanLieCounts, 
      dim=c(11,11,6))
  )
)


# Some People Lie
someLies.LL <- function(pTrue, lambda, weight){
  ns.l = humanLieCounts
  
  -eval.s(
    someLies.pred(pTrue, lambda, weight),
    ns.l
  )
}
someLies.fit <- summary(mle(someLies.LL,
                            start=list(pTrue=rnorm(1,0,1),
                                       lambda=rnorm(1,0,1),
                                       weight=rnorm(1,0,1)),
                            method = "BFGS"))

someLies.preds <- someLies.pred(
  someLies.fit@coef['pTrue','Estimate'], 
  someLies.fit@coef['lambda','Estimate'],
  someLies.fit@coef['weight','Estimate']
)

someLies.preds.df <- someLies.preds %>%
  as_tibble() %>% 
  mutate(ksay = rep(0:10, 11)) %>%
  pivot_longer(-ksay, names_to = 'condition', values_to='probability') %>%
  mutate(condition = as.numeric(substr(condition, 2, 10))-1,
         expt = ifelse(condition < ceiling(max(condition)/2), "red", "blue"),
         expt = factor(expt, levels=c("red","blue")),
         p = condition %% 3,
         p = 0.2 + 0.3*p,
         k = rep(0:10, each=66),
         probTxt = paste0(round(probability*100), "%")) %>%
  select(-condition) %>%
  relocate(c(expt, p, k), .before = ksay) %>%
  arrange(expt, p, k, ksay)

somePeopleLie.diag = -2*eval.s(
  getDiag(
    array(
      someLies.preds,
      dim=c(11,11,6)
    )
  ),
  getDiag(
    array(
      humanLieCounts, 
      dim=c(11,11,6))
  )
)
somePeopleLie.lies = -2*eval.s(
  getLies(
    array(
      someLies.preds,
      dim=c(11,11,6)
    )
  ),
  getLies(
    array(
      humanLieCounts, 
      dim=c(11,11,6))
  )
)
  
