

signifTesting.detect <- function(kstar, p, a, b){
  a = logitToProb(pmin(10, pmax(-10, a)))
  b = logitToProb(pmin(10, pmax(-10, b)))
  n = 10
  pval = 1-ifelse(kstar <= n*p, pbinom(kstar, n, p, lower.tail=T), pbinom(kstar-1, n, p, lower.tail=F))
  pval.norm = (pval - min(pval))/(max(pval)-min(pval))
  (b-a)*pval.norm + a
}

signifTesting.pred <- function(a, b){
  matrix(
    rep(
      mapply(
        function(prob) {
          signifTesting.detect(0:10, prob, a, b)
        },
        c(0.2, 0.5, 0.8) #repeat for each base rate condition
      ), 
      2 #repeat for each utility structure condition
    ),
    nrow = 11
  )
}

