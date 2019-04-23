library(tidyverse)
library(gridExtra)

P = 0.5
N = 50
r.decay = 0.1

SOFTMAX = 1
FX = softmax(SOFTMAX)

# neater to make them here
KSAY = matrix(rep(0:N, (N+1)), nrow=N+1)
K = matrix(rep(0:N, each=(N+1)), nrow=N+1)

# the world
softmax = function(SOFTMAX){
  # print(SOFTMAX)
  force(SOFTMAX)
  return(function(u){exp(u*SOFTMAX)/sum(exp(u*SOFTMAX))})
}

p.k = function(k){
  dbinom(k, N, P)
}

# the caller (needs p.lie.ksay from bluffer)
u.k.call = function(k){(N-k)-k}
u.bs.call = function(k,lie){if(length(lie)==1){lie=rep(lie, length(k))}; ifelse(lie==1, 2*k,-2*k)}

eu.bs.ksay = function(ksay, p.lie.ksay){
  u.bs.call(ksay, rep(T, length(ksay)))*p.lie.ksay + u.bs.call(ksay, rep(F, length(ksay)))*(1-p.lie.ksay)
}

p.bs.ksay = function(ksay, p.lie.ksay){
  u.accept = u.k.call(ksay)
  u.bs = eu.bs.ksay(ksay, p.lie.ksay)
  apply(rbind(u.accept, u.bs), 2, FX)['u.bs',]
}

caller = function(p.lie){
  p.bs.ksay(0:N, p.lie)
}

# the bluffer (needs p.bs.ksay from caller)
u.k.bluff = function(k){((k)-(N-k))}
u.bs.bluff = function(k,lie){ifelse(lie, -2*k, 2*k)}

eu.ksay.k = function(ksay, k, p.bs.ksay){
  lie = ksay != k
  P.BS = matrix(rep(p.bs.ksay, length(k)), nrow = length(ksay))
  u.k.bluff(ksay)*(1-p.bs.ksay) + p.bs.ksay*u.bs.bluff(ksay, lie)
}

p.ksay.k = function(ksay, k, p.bs.ksay){
  apply(eu.ksay.k(ksay,k,p.bs.ksay), 2, FX)
}

p.lie.ksay = function(p.ksay.k, p.k){
  lie = 1-diag(N+1)
  rowSums(p.ksay.k*p.k*lie)/rowSums(p.ksay.k*p.k)
}


bluffer = function(p.bs.ksay){
  p.lie.ksay(p.ksay.k(KSAY,
                      K,
                      p.bs.ksay), 
             p.k(K))
}

r.bluffer = function(DEPTH){
  p.lie.ksay(p.ksay.k(KSAY,
                      K,
                      r.caller(DEPTH)), 
             p.k(K))
}



r.caller = function(DEPTH){
  if(runif(1)<DEPTH){
    return(rep(0.1, N+1))
  } else {
    p.bs.ksay(0:N, r.bluffer(DEPTH))
  }
}



# keep all iterations -----------------------------------------------------

stepPlot = function(p.bs, p.lie){
  g0 <- as.tibble(p.ksay.k(KSAY, K, p.bs)*p.k(K)) %>%
    mutate(ksay = 1:n()) %>%
    gather(key = k, val = p, -ksay) %>%
    mutate(k = as.numeric(gsub("\\V", "", k))) %>%
    ggplot(aes(x=k, y=ksay, size=p, alpha=p))+
    geom_point()+
    scale_size_continuous(range=c(0,5))+
    theme_minimal()+
    theme(legend.position = 'none')
  g1 <- as.tibble(p.ksay.k(KSAY, K, p.bs)) %>%
    mutate(ksay = 1:n()) %>%
    gather(key = k, val = p, -ksay) %>%
    mutate(k = as.numeric(gsub("\\V", "", k))) %>%
    ggplot(aes(x=k, y=ksay, size=p, alpha=p))+
    geom_point()+
    scale_size_continuous(range=c(0,5))+
    theme_minimal()+
    theme(legend.position = 'none')
  g2 <- tibble(ksay=0:N, p.bs.ksay = p.bs, time='P(BS|ksay)') %>%
    bind_rows(tibble(ksay=0:N, p.bs.ksay = bluffer(p.bs), time='P(lie|ksay)')) %>%
    ggplot(aes(x=ksay, y=p.bs.ksay, color=time))+
    geom_line()+theme_minimal()
  return(grid.arrange(g0,g1,g2, ncol=3))
}


showState = function(i, p.bs){
  p.lie = bluffer(p.bs)
  while(i >= 1){
    p.bs = p.bs*(1-r.decay) + r.decay*caller(p.lie)
    p.lie = p.lie*(1-r.decay) + r.decay*bluffer(p.bs)
    i = i-1
  }
  stepPlot(p.bs)
}


p.bs = 0.5+(0.5-0.5)*pbinom(0:N, N, P)
showState(150, p.bs)
p.lie = bluffer(p.bs)

tibble(ksay = 0:N) %>% 
  mutate(u.ksay = u.k.call(ksay), u.bs= u.bs.call(ksay, T)) %>% 
  ggplot(aes(x=ksay, y=u.ksay))+geom_line()+geom_line(aes(y=u.bs), col='red')


tibble(ksay = 0:N) %>% 
  mutate(p.bs = caller(p.lie)) %>%
  ggplot(aes(x=ksay, y=p.bs))+geom_line()
