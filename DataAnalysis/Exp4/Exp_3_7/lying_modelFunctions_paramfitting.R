numMarbles = 10
KSAY = matrix(rep(0:numMarbles,numMarbles+1),nrow=numMarbles+1)
K = matrix(rep(0:numMarbles, each=numMarbles+1), nrow=numMarbles+1)
BET = 0.8
#ALPH = 0.25 #0.25
liePenalty = 10 # 10 # -5
faPenalty = 5 # 5 # 0
#Expt = 4
#moral = 0 #liar's internal penalty for lying
#moral_receiver = 0 #only use to calibrate BS caller's utility


util = 1
ksay = 6
# util = [-1, +1] "pts for red" = +1, "pts for blue" = -1

u.L(6, TRUE, FALSE, -1, 7, FALSE)
u.D(6, TRUE, FALSE, -1, 7, FALSE)

u.L <- function(ksay, lie, BS, utilStr, eta.L, lastLvl=FALSE) {
  utility = case_when(
    !BS ~ utilStr*2*ksay - utilStr*10,
    !lie ~ utilStr*2*ksay - utilStr*10 + faPenalty,
    lie ~ -liePenalty
  )
  return(ifelse(lie & lastLvl, utility - eta.L, utility))
}

u.D <- function(ksay, lie, BS, utilStr, eta.D, lastLvl=FALSE) {
  utility = case_when(
    !BS ~ utilStr*10 - utilStr*2*ksay,
    !lie ~ utilStr*10 - utilStr*2*ksay - faPenalty ,
    lie ~ liePenalty
  )
  return(ifelse(BS & lastLvl, utility - eta.D, utility))
}


softmax <- function(alph, allEV) { # allEV = vector of numerics
  mapply(function(i) exp(i*alph)/sum(exp(allEV*alph)), allEV)
}

p.k <- function(k, p) {
  BET*dbinom(k, numMarbles, p) + (1-BET)*1/(numMarbles+1)
  #dbinom(k, numMarbles, p)
}


EV.D_bs.ksay.r <- function(ksay, p, bs, util, eta.D, lastlvl=FALSE, p.L) { # both are vectors
  u.D(ksay, lie=TRUE, BS=bs, util, eta.D, lastLvl=lastlvl) * p.L + u.D(ksay, lie=FALSE, BS=bs, util, eta.D, lastLvl=lastlvl) * (1-p.L)
}
# EV.D_bs.ksay.r(0:10, 0.5, TRUE, 1, 7, TRUE, rep(0.1,11))
# EV.D_bs.ksay.r(0:10, 0.5, TRUE, 1, 7, TRUE, mapply(p_t.ksay.r, 0:10, 0.5, rep(0.1,11)))
# EV.D_bs.ksay.r(0:10, 0.5, TRUE, 1, 7, TRUE, 0.1)

p.D_bs.ksay.r <- function(ksay, p, util, alph, eta.D, lastlvl=FALSE, p.L) {
  EV.BS <- EV.D_bs.ksay.r(ksay, p, TRUE, util, eta.D, lastlvl=lastlvl, p.L)
  EV.noBS <- EV.D_bs.ksay.r(ksay, p, FALSE, util, eta.D, lastlvl=lastlvl, p.L)
  softmax(alph, c(EV.BS, EV.noBS))[1]
}
# mapply(p.D_bs.ksay.r, 0:10, 0.5, 1, 0.25, 7, FALSE, 0.4)
# mapply(p.D_bs.ksay.r, 0:10, 0.5, 1, 0.25, 7, FALSE, p_t.ksay.r(1, 0.25, 2, 0.5, rep(0.1,11)))
# p.D_bs.ksay.r(0:10, 0.5, 1, 1, 7, FALSE, 0.1)
# softmax(1, c(EV.D_bs.ksay.r(2, 0.5, TRUE, 1, 7, lastlvl=FALSE, 0.1), EV.D_bs.ksay.r(2, 0.5, FALSE, 1, 7, lastlvl=FALSE, 0.1)))[1]

EV.L_ksay.k.r <- function(k, ksay, util, eta.L, p, lastlvl=FALSE, p.D) {
  mapply(u.L, ksay, lie=ksay!=k, BS=TRUE, util, eta.L, lastLvl=lastlvl) * p.D + mapply(u.L, ksay, lie=ksay!=k, BS=FALSE, util, eta.L, lastLvl=lastlvl) * (1-p.D)
  #u.L(ksay, lie=ksay!=k, BS=TRUE, util, eta.L, lastLvl=lastlvl) * p.D + u.L(ksay, lie=ksay!=k, BS=FALSE, util, eta.L, lastLvl=lastlvl) * (1-p.D)
}
EV.L_ksay.k.r(5, 0:10, 1, 7, 0.5, lastlvl=F, 0.5)
# mapply(function(i) EV.L_ksay.k.r(i, 0:10, 1, 7, 0.5, lastlvl=F, rep(0.5,11)), 0:10)


p.L_ksay.k.r <- function(util, alph, eta.L, p, lastlvl=FALSE, p.D) { #look into this
  EV.all <- mapply(function(i) EV.L_ksay.k.r(i, 0:numMarbles, util, eta.L, p, lastlvl=lastlvl, p.D), 0:numMarbles)
  apply(EV.all,2,softmax, alph)
}

# round(p.L_ksay.k.r(1, 1, 7, 0.5, TRUE, rep(0.5,11)),5)
# round(p.L_ksay.k.r(0.5, rep(0.5,11)) * .8 + 1 / length(0:numMarbles) *.2,4)

returnVectVal <- function(vect, alph, ksay){
  #mapply(function(i) {softmax(vect, alph)[i+1]}, ksay)
  softmax(vect, alph)[ksay+1]
}
returnVectVal(matrix(0:11, ncol=2), 1, c(2,5))

p.L_ksay.k.r.i <- function(k, ksay, util, alph, eta.L, p, lastlvl=FALSE, p.D) {
  EV.all <- mapply(function(i) EV.L_ksay.k.r(i, 0:numMarbles, util, eta.L, p, lastlvl=lastlvl, p.D), k)
  diag(apply(EV.all,2,returnVectVal, alph, ksay))

  #mapply(function(j){matr[j,ksay[j]+1]}, length(k))
  #softmax(alph, mapply(function(i) EV.L_ksay.k.r(k, i, util, eta.L, p, lastlvl=lastlvl, p.D), 0:numMarbles))
  #softmax(alph, c(EV.L_ksay.k.r(k, 0:numMarbles, util, eta.L, p, lastlvl=lastlvl, p.D)))[ksay+1]
}
round(p.L_ksay.k.r.i(c(5,5,2,1, 0:8), c(5,9,8,2, 0:8), 1, 1, 7, 0.5, TRUE, 0.5), 5)

round(p.L_ksay.k.r.i(2, 0:10, 1, 1, 7, 0.5, TRUE, 0.5), 5)

p_t.ksay.r <- function(util, alph, eta.L, p, p.D) {
  P.K <- matrix(rep(p.k(0:numMarbles, p), each=numMarbles+1), nrow=numMarbles+1)
  P.L_KSAY.K <- p.L_ksay.k.r(util, alph, eta.L, p, lastlvl=FALSE, p.D)
  LIE = 1-diag(numMarbles+1)
  rowSums(P.K*P.L_KSAY.K*LIE)/rowSums(P.K*P.L_KSAY.K)
}
# p_t.ksay.r("expt4", 0.25, 2, 0.5, rep(0.1,11))



## this is different from recurse.L
iterate.L <- function(iter, util, alph, p, prior=rep(0.1,11)){
  eta.L = 0
  #print(iter)
  p_t.ksay.r(util, alph, eta.L, p, iterate.D(iter, util, alph, p, prior))
}

iterate.D <- function(iter, util, alph, p, prior=rep(0.1,11)){
  #print(iter)
  eta.D = 0
  ifelse(iter == 0, prior, mapply(p.D_bs.ksay.r,0:10, p, util, alph, eta.D, lastlvl=FALSE, iterate.L((iter - 1), util, alph, p, prior)))
  # if(iter == 0){
  #   return(prior)
  # } else{
  #   return(mapply(p.D_bs.ksay.r,0:10, p, util, alph, eta.D, lastlvl=FALSE, iterate.L((iter - 1), util, alph, p, prior)))
  # }
}
mapply(p.D_bs.ksay.r, 0:10, 0.5, "expt4", 0.25, 7, FALSE, p_t.ksay.r("expt4", 0.25, 2, 0.5, rep(0.1,11)))


iterate.D(1, "expt4", 0.25, 0.5)
iterate.L(1, "expt4", 0.25, 0.5)
round(p.L_ksay.k.r("expt4", 0.25, 2, 0.5, TRUE, iterate.D(0, "expt4", 0.25, 0.5)),4)
round(mapply(p.D_bs.ksay.r,0:10, 0.5, "expt4", 0.25, 7, lastlvl=TRUE, iterate.L(0,"expt4", 0.25, 0.5)),4)

p_t.ksay.r("expt4", 1, 0, 0.5, iterate.D(0, "expt4", 1, 0.5)) == iterate.L(0, "expt4", 1, 0.5)
mapply(p.D_bs.ksay.r,0:10, 0.5, "expt4", 1, 7, lastlvl=FALSE, iterate.L(0,"expt4", 1, 0.5)) == iterate.D(1, "expt4", 1, 0.5)

