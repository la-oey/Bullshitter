numMarbles = 10
KSAY = matrix(rep(0:numMarbles,numMarbles+1),nrow=numMarbles+1)
K = matrix(rep(0:numMarbles, each=numMarbles+1), nrow=numMarbles+1)
ALPH = 0.25
liePenalty = 10 # 10 # -5
faPenalty = 5 # 5 # 0
Expt = 1

# Depends on Expt #
u.L <- function(ksay, lie, BS) {
  if(Expt %in% 1:2){
    if(!BS){
        return(2*ksay - 10) # no BS + no lie, no BS + lie
      } else{
        if(!lie){
          return(2*ksay) # BS + no lie
        } else{
          return(-2*ksay) # BS + lie
        }
      }
  } else if(Expt %in% 3:4){
    if(!BS){
      return(2*ksay - 10) # no BS + no lie, no BS + lie
    } else{
      if(!lie){
        return(2*ksay - (10-faPenalty)) # BS + no lie
      } else{
        return(rep(-liePenalty, length(ksay))) # BS + lie
      }
    }
  } else if(Expt == 5){
    if(!BS){
      return(10 - 2*ksay) # no BS + no lie, no BS + lie
    } else{
      if(!lie){
        return(10 - 2*ksay + faPenalty) # BS + no lie
      } else{
        return(rep(-liePenalty, length(ksay))) # BS + lie
      }
    }
  }
}

u.D <- function(ksay, lie, BS) {
  if(Expt %in% 1:2){
    if(!BS){
      return(10 - 2*ksay) # no BS + no lie, no BS + lie
    } else{
      if(!lie){
        return(-2*ksay) # BS + no lie
      } else{
        return(2*ksay) # BS + lie
      }
    }
  } else if(Expt %in% 3:4){
    if(!BS){
      return(10 - 2*ksay) # no BS + no lie, no BS + lie
    } else{
      if(!lie){
        return((10-faPenalty) - 2*ksay) # BS + no lie
      } else{
        return(rep(liePenalty, length(ksay))) # BS + lie
      }
    }
  } else if(Expt == 5){
    if(!BS){
      return(2*ksay - 10) # no BS + no lie, no BS + lie
    } else{
      if(!lie){
        return(2*ksay - 10 - faPenalty) # BS + no lie
      } else{
        return(rep(liePenalty, length(ksay))) # BS + lie
      }
    }
  }
}

softmax <- function(allEV) { # allEV = vector of numerics
  mapply(function(i) exp(i*ALPH)/sum(exp(allEV*ALPH)), allEV)
}

p.k <- function(k, p) {
  dbinom(k, numMarbles, p)
}


EV.D_bs.ksay.r <- function(ksay, p, bs, p.L) { # both are vectors
  u.D(ksay, lie=TRUE, BS=bs) * p.L + u.D(ksay, lie=FALSE, BS=bs) * (1-p.L)
}
# EV.D_bs.ksay.r(0:10, 0.5, TRUE, rep(0.1,11))
# EV.D_bs.ksay.r(0:10, 0.5, TRUE, mapply(p_t.ksay.r, 0:10, 0.5, rep(0.1,11)))


p.D_bs.ksay.r <- function(ksay, p, p.L) {
  EV.BS <- EV.D_bs.ksay.r(ksay, p, TRUE, p.L)
  EV.noBS <- EV.D_bs.ksay.r(ksay, p, FALSE, p.L)
  softmax(c(EV.BS, EV.noBS))[1]
}
# mapply(p.D_bs.ksay.r, 0:10, 0.5, 0.4)
# mapply(p.D_bs.ksay.r, 0:10, 0.5, mapply(p_t.ksay.r, 0.5, rep(0.4,11)))


EV.L_ksay.k.r <- function(k, ksay, p, p.D) {
  mapply(u.L, ksay, lie=ksay!=k, BS=TRUE) * p.D + mapply(u.L, ksay, lie=ksay!=k, BS=FALSE) * (1-p.D)
}
# EV.L_ksay.k.r(5, 0:10, 0.5, rep(0.5,11))
# mapply(function(i) EV.L_ksay.k.r(i, 0:10, 0.5, rep(0.5,11)), 0:10)


p.L_ksay.k.r <- function(p, p.D) { #look into this
  EV.all <- mapply(function(i) EV.L_ksay.k.r(i, 0:numMarbles, p, p.D), 0:numMarbles)
  apply(EV.all,2,softmax)
}
# round(p.L_ksay.k.r(0.5, rep(0.5,11)),4) 
# round(p.L_ksay.k.r(0.5, rep(0.5,11)) * .8 + 1 / length(0:numMarbles) *.2,4)


p_t.ksay.r <- function(p, p.D) {
  P.K <- matrix(rep(p.k(0:numMarbles, p), each=numMarbles+1), nrow=numMarbles+1)
  if(Expt %in% 1:3){
    P.L_KSAY.K <- p.L_ksay.k.r(p, p.D)
  } else if(Expt %in% 4:5){
    P.L_KSAY.K <- .8*p.L_ksay.k.r(p, p.D) + .2* 1 / length(0:numMarbles)
  }
  LIE = 1-diag(numMarbles+1)
  rowSums(P.K*P.L_KSAY.K*LIE)/rowSums(P.K*P.L_KSAY.K)
}
# p_t.ksay.r(0.5, rep(0.1,11))
# p_t.ksay.r(0.5, pbinom(0:10, 10, 0.5))

exp.ksay <- function(p, p.D) {
  colSums(KSAY * p.L_ksay.k.r(p, p.D))
}
#exp.ksay(0.5,rep(0.5,11))

recurse.L <- function(decay, p, prior=rep(0.1,11)) {
  p_t.ksay.r(p, recurse.D(decay, p, prior))
}

recurse.D <- function(decay, p, prior=rep(0.1,11)){
  if(runif(1, 0, 1) < decay){
    return(prior)
  } else{
    return(mapply(p.D_bs.ksay.r,0:10, p, recurse.L(decay, p, prior)))
  }
}


recurse.D(0.1, 0.5)
(zeroth <- rep(0.5,11))
#(zeroth <- pbinom(0:numMarbles, numMarbles, 0.5))
(first <- p_t.ksay.r(0.5, zeroth))
(second <- mapply(p.D_bs.ksay.r, 0:numMarbles, 0.5, first))
(third <- p_t.ksay.r(0.5, second))
(fourth <- mapply(p.D_bs.ksay.r, 0:numMarbles, 0.5, third))
(fifth <- p_t.ksay.r(0.5, fourth))

