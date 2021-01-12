

alwaysLie.func <- function(expt){
  case_when(
    expt$utilSign == 1 ~ rep(c(rep(0.0001,10),1-0.0001*10),11),
    expt$utilSign == -1 ~ rep(c(1-0.0001*10,rep(0.0001,10)),11)
  )
}

alwaysLie.pred <- function(){
  matrix(
    mapply(alwaysLie.func, rep(c(1,-1), each=3)),
    nrow=121
  )
}

