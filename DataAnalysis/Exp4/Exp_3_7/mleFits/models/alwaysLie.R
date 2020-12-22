

alwaysLie.func <- function(util){
  case_when(
    util == 1 ~ rep(c(rep(0.0001,10),1-0.0001*10),11),
    util == -1 ~ rep(c(1-0.0001*10,rep(0.0001,10)),11)
  )
}

alwaysLie.pred <- function(){
  matrix(
    mapply(alwaysLie.func, rep(c(1,-1), each=3)),
    nrow=121
  )
}

