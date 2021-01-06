logistic = function(z){1/(1+exp(-z))}
logit = function(p){log(p/(1-p))}
logistic.lim = function(z){logistic(pmin(10, pmax(-10, z)))}
exp.lim = function(x){exp(pmin(5, pmax(-5, x)))}

normCenter = function(X){
  d = length(dim(X))
  
  norms = apply(X, 
                MARGIN = 1:(d-1),
                FUN = mean)
  return(X-norms)
}

normSum = function(X){
  d = length(dim(X))
  norms = apply(X, 
                MARGIN = 1:(d-1),
                FUN = sum)
  return(X/norms)
}

softmaxMat = function(alph, vals){
  Z = normCenter(alph*vals)
  aev = array(exp.lim(Z), dim = dim(vals))
  P = normSum(aev)
  P = normSum(P+1e-8)
  return(P)
}

softmax <- function(alph, allEV) { # allEV = vector of numerics
  # EV: I changed this, and may have broken something.
  aev = exp(alph * allEV)
  return(aev/sum(aev))
  # mapply(function(i) exp(i*alph)/sum(exp(allEV*alph)), allEV)
}

poissonAverage = function(input, lambda, d){
  dims = dim(input)
  if(length(dims) < d){
    return(input)
  } else {
    ndepths = dims[d]
    p.depth = dpois(0:(ndepths-1), lambda) / ppois(ndepths-1, lambda)
    apply(input*array(rep(p.depth, each = prod(dims[-d])), dim = dims),
          MARGIN = (1:(d-1)),
          FUN = sum)
    
  }
}


paramsMake = function(lst){
  params = list()
  params$lambda = 0
  params$detector = list('aversion' = 0,
                         'softmax' = 1,
                         'l0_prob' = 0.5)
  params$liar = list('aversion' = 0,
                     'softmax' = 1,
                     'd0_prob' = 0.1)
  for(pname in names(lst)){
    nameparts = strsplit(pname, '\\.')[[1]]
    if(length(nameparts)==1){
      n = nameparts[[1]]
      params[[n]] = lst[[pname]]
      # overrides liar/detector specific parameters if this is one.
      if(n %in% names(params$liar)){
        params$liar[[n]] = lst[[pname]]
      }
      if(n %in% names(params$detectpr)){
        params$detector[[n]] = lst[[pname]]
      }
    } else if(length(nameparts)==2){ 
      params[[nameparts[1]]][[nameparts[2]]] = lst[[pname]]
    } else {
      stop('parameter name format not recognized')
    }
  }
  
  return(params)
}

# TODO: transform: p.true p.unif


paramsTransform = function(lst){
  transforms = list()
  transforms[['lambda']] = exp.lim;
  transforms[['softmax']] = exp.lim;
  transforms[['prob']] = logistic.lim;
  for(pname in names(lst)){
    tmatch = str_detect(pname, names(transforms))
    if(any(tmatch)){
      tname = names(transforms)[which(tmatch)[1]]
      lst[[pname]] = transforms[[tname]](lst[[pname]])
    }
  }
  return(lst)
}

paramsPriors = function(lst){
  LP = 0
  for(pname in names(lst)){
    LP = LP + dnorm(lst[[pname]], log=T)
  }
  return(LP)
}

paramSeedFromFx = function(func){
  lst = as.list(args(func))
  lst[[length(lst)]] = NULL
  for(n in names(lst)){
    lst[[n]] = rnorm(1)
  }
  return(lst)
}

