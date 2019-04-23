library(tidyverse)
library(gridExtra)
library(grid)
library(ggpubr)

P = 0.5
N = 10
r.decay = 0.2 # originally 0.1

SOFTMAX = 1 # originally 1

# neater to make them here
KSAY = matrix(rep(0:N, (N+1)), nrow=N+1)
K = matrix(rep(0:N, each=(N+1)), nrow=N+1)

# the world
soft = function(SOFTMAX){
  # print(SOFTMAX)
  force(SOFTMAX)
  return(function(u){exp(u*SOFTMAX)/sum(exp(u*SOFTMAX))})
}

FX = soft(SOFTMAX)

p.k = function(k, p.world){
  dbinom(k, N, p.world)
}

# the caller (needs p.lie.ksay from bluffer)
u.k.call = function(k){(N-k)-k}
#u.bs.call = function(k,lie){if(length(lie)==1){lie=rep(lie, length(k))}; ifelse(lie==1, 2*k,-2*k)}
u.bs.call = function(k,lie){if(length(lie)==1){lie=rep(lie, length(k))}; ifelse(lie==1, 10,(N-k)-k-5)}

eu.bs.ksay = function(ksay, p.lie.ksay){
  u.bs.call(ksay, rep(T, length(ksay)))*p.lie.ksay + u.bs.call(ksay, rep(F, length(ksay)))*(1-p.lie.ksay)
}

p.bs.ksay = function(ksay, p.lie.ksay){
  u.accept = u.k.call(ksay)
  u.bs = eu.bs.ksay(ksay, p.lie.ksay)
  apply(rbind(u.accept, u.bs), 2, FX)['u.bs',]
  #apply(rbind(u.accept, u.bs), 2, FX)
}
p.bs.ksay(0:10, 0.1)

# p.bs.ksay(pbinom(0:N, N, 0.5),p.lie.ksay(p.ksay.k(KSAY,K,pbinom(0:N, N, 0.5)),0.5))


p.lie.ksay = function(p.ksay.k, p.world){
  lie = 1-diag(N+1)
  P.K = matrix(rep(p.k(0:10, p.world), each=11), nrow=11)
  #print(p.ksay.k*P.K*lie)
  rowSums(p.ksay.k*P.K*lie)/rowSums(p.ksay.k*P.K)
}
p.lie.ksay(p.ksay.k(KSAY,K,pbinom(0:10,10,0.5)), 0.5)
p.lie.ksay(p.ksay.k(KSAY,K,rep(0.5,11)), 0.5)

caller = function(p.ksay.k, p.world){
  p.lie = p.lie.ksay(p.ksay.k, p.world)
  p.bs.ksay(0:N, p.lie)
}

# the bluffer (needs p.bs.ksay from caller)
u.k.bluff = function(k){((k)-(N-k))}
#u.bs.bluff = function(k,lie){ifelse(lie, -2*k, 2*k)}
u.bs.bluff = function(k,lie){ifelse(lie, -10, (k)-(N-k)+5)}

eu.ksay.k = function(ksay, k, p.bs.ksay){
  lie = ksay != k
  P.BS = matrix(rep(p.bs.ksay, length(k)), nrow = length(ksay))
  u.k.bluff(ksay)*(1-p.bs.ksay) + p.bs.ksay*u.bs.bluff(ksay, lie)
}
eu.ksay.k(KSAY, K, rep(0.5,11))
eu.ksay.k(KSAY, K, pbinom(0:10,10,0.5))

p.ksay.k = function(ksay, k, p.bs.ksay){
  apply(eu.ksay.k(ksay,k,p.bs.ksay), 2, FX)
}
round(p.ksay.k(KSAY,K,0.5),4)
round(p.ksay.k(KSAY,K,pbinom(0:10,10,0.5)),4)

# Output P(lie|ksay)
bluffer = function(p.bs.ksay, p.world){
  p.ksay.k(KSAY,
           K,
           p.bs.ksay)
  # p.lie.ksay(, 
  #            p.k(K, p.world))
}

expect.ksay <- function(p.D) {
  colSums(p.ksay.k(KSAY, K, p.D)*KSAY)
}
#expect.ksay(0.5)

r.bluffer = function(DEPTH, p.world){
  p.lie.ksay(p.ksay.k(KSAY,
                      K,
                      r.caller(DEPTH, p.world)), 
             p.world)
}



r.caller = function(DEPTH, p.world){
  if(runif(1)<DEPTH){
    return(pbinom(0:N, N, p.world))
    #return(rep(0.1,11))
  } else {
    p.bs.ksay(0:N, r.bluffer(DEPTH, p.world))
  }
}

#(zeroth <- rep(0.5,11))
(zeroth <- pbinom(0:10,10,0.5))
(first <- p.lie.ksay(p.ksay.k(KSAY, K, zeroth), 0.5))
(second <- p.bs.ksay(0:N, first))
(third <- p.lie.ksay(p.ksay.k(KSAY, K, second), 0.5)) #check why this is diff from the other
(fourth <- p.bs.ksay(0:N, third))
(fifth <- p.lie.ksay(p.ksay.k(KSAY, K, fourth), 0.5))



# keep all iterations -----------------------------------------------------

stepPlot = function(p.bs, p.lie, p.world){
  # P(ksay|k)
  g1 <- as.tibble(p.ksay.k(KSAY, K, p.bs)) %>%
    mutate(ksay = 1:n()) %>%
    gather(key = k, val = p, -ksay) %>%
    mutate(k = as.numeric(gsub("\\V", "", k))-1,
           ksay = ksay-1) %>%
    group_by(k) %>%
    mutate(mult = p*ksay) %>%
    summarise(expected = sum(mult)) %>%
    ggplot(aes(x=k, y=expected))+
    geom_line()+
    geom_abline(intercept = 0, slope = 1, colour="red", linetype=2) +
    #ggtitle("Recursive Social Liar Model") +
    scale_x_continuous("") +
    scale_y_continuous("", limits=c(0,10)) +
    #scale_x_continuous("Actual (k)") +
    #scale_y_continuous("Reported (k*)", limits=c(0,10)) +
    theme_minimal()
  #ggsave("img/recurse_liar.png",g1)
  # P(BS|ksay)
  g2 <- tibble(ksay=0:N, p.bs.ksay = p.bs) %>%
    ggplot(aes(x=ksay, y=p.bs.ksay))+
    geom_line()+
    #ggtitle("Recursive Social Detector Model") +
    scale_x_continuous("") +
    scale_y_continuous("", limits=c(0,1)) +
    #scale_x_continuous("Reported (k*)") +
    #scale_y_continuous(bquote("" *~P[D]* "(BS | k*)"), limits=c(0,1)) +
    theme_minimal()
  #ggsave("img/recurse_detect.png",g2)
  #return(grid.arrange(g1,g2, ncol=2))
  fig <- ggarrange(annotate_figure(g1, right=text_grob("Liar", rot=270)), 
                   annotate_figure(g2, right=text_grob("Lie Detector", rot=270)), nrow=2)
  #return(annotate_figure(fig, right=text_grob(paste("P(w) =", p.world), rot=270)))
  return(fig)
}


# i = # of iterations
# p.bs = distribution of prior probability of BS|ksay
# p.world = mean probability of k (e.g. 0.2, 0.5, 0.8)
# decay = probability of ending recursion X shift in weight
showState = function(i, p.world, p.bs, decay){
  p.lie = bluffer(p.bs, p.world)
  while(i >= 1){
    p.bs = p.bs*(1-decay) + decay*caller(p.lie, p.world)
    p.lie = p.lie*(1-decay) + decay*bluffer(p.bs, p.world)
    i = i-1
  }
  stepPlot(p.bs, p.lie, p.world)
}




comp.bluffer = function(p.world){
  p.sim = p.k(KSAY, p.world)
  idx = KSAY <= K  # turns into K
  p.kexact = colSums(p.sim*idx)
  p.sim[idx] = 0
  p.sim[KSAY == K] = p.kexact
  return(p.sim)
}


comp.caller = function(ksay, p.world){
  pbinom(ksay, N, p.world)
}



# Simulate

stepPlot_sim = function(conditional.matrix, vec.bs.ksay, p.world){
  g1 <- as.tibble(conditional.matrix) %>%
    mutate(ksay = 1:n()) %>%
    gather(key = k, val = p, -ksay) %>%
    mutate(k = as.numeric(gsub("\\V", "", k))-1,
           ksay = ksay-1) %>%
    group_by(k) %>%
    mutate(mult = p*ksay) %>%
    summarise(expected = sum(mult)) %>%
    ggplot(aes(x=k, y=expected))+
    geom_line()+
    geom_abline(intercept = 0, slope = 1, colour="red", linetype=2) +
    #ggtitle("Recursive Social Liar Model") +
    scale_x_continuous("Actual (w)") +
    scale_y_continuous("Reported (w')", limits=c(0,10)) +
    theme_minimal()+
    theme(legend.position = 'none')
  #ggsave("img/recurse_liar.png",g1)
  # P(BS|ksay)
  g2 <- tibble(ksay=0:N, p.bs.ksay = vec.bs.ksay) %>%
    ggplot(aes(x=ksay, y=p.bs.ksay))+
    geom_line()+
    #ggtitle("Recursive Social Detector Model") +
    scale_x_continuous("Reported (w')") +
    scale_y_continuous("P(BS | w')", limits=c(0,1)) +
    theme_minimal()
  #ggsave("img/recurse_detect.png",g2)
  #return(grid.arrange(g1,g2, ncol=2))
  fig <- ggarrange(g1,g2, ncol=2)
  return(annotate_figure(fig, left=text_grob(paste("P(w) =", p.world), rot=0)))
}



