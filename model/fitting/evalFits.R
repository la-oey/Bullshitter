rm(list=ls())
here::i_am('README.md')

require(tidyverse)
require(stats4)

savefile = here::here('model', 'fitting', 'fits', 'save.Rdata')
load(savefile)


source(here::here('model', 'fitting', 'loadExpts.R'))
source(here::here('model', 'fitting', 'models.R'))

#evalSubtle

getTruthTable = function(x){
  cbind(diag(x), rowSums(x)-diag(x))
}

evalPrediction = function(prediction, dat){
  LLs = c()
  if('detector' %in% names(prediction)){
    LLs['detector'] = sum(log(prediction[['detector']])*dat[['detector']])
  } else {
    LLs['detector'] = NA
  }
  
  if('liar' %in% names(prediction)){
    LLs['liar'] = sum(log(prediction[['liar']])*dat[['liar']])
    LLs['liar.truth'] = sum(log(getTruthTable(prediction[['liar']]))*getTruthTable(dat[['liar']]))
    diag(predictions[['liar']]) = 0
    diag(dat[['liar']]) = 0
    LLs['liar.lie'] = sum(log(normSum(prediction[['liar']]))*dat[['liar']])
  } else {
    LLs[c('liar', 'liar.truth','liar.lie')] = NA
  }
  return(LLs)
}



aLL = tibble()
for(model.name in c(names(fits), 'human', 'maxEntropy')){
  LL = c()
  for(i in 1:length(expts)){
    if(model.name %in% names(fits)){
      params = fits[[model.name]]@coef %>% as.list() %>% paramsTransform() %>% paramsMake()
      modelFx = eval(parse(text=paste0('predict', str_split(model.name, "\\.")[[1]][1])))
      prediction = modelFx(params, expts[[i]])
    } else if(model.name == 'human') {
      prediction = list('liar' = normSum(data[[i]][['liar']]+1e-9),
                        'detector' = normSum(data[[i]][['detector']]+1e-9))
    } else if(model.name == 'maxEntropy'){
      prediction = list('liar' = normSum(data[[i]][['liar']]*0+1e-1),
                        'detector' = normSum(data[[i]][['detector']]*0+1e-1))
      
    }
    LL = rbind(LL, evalPrediction(prediction, data[[i]]))
  }
  aLL = LL %>% as_tibble() %>% 
    mutate(expt = 1:n()) %>% 
    pivot_longer(-expt, names_to = 'role', values_to = 'LL') %>% 
    mutate(model = model.name, 
           n.params = length(as.list(args(modelFx)))-1) %>% 
    bind_rows(aLL)
}


dLL = aLL %>% group_by(role, model) %>% 
  summarize(Deviance = -2*sum(LL)) %>% 
  filter(!is.na(Deviance)) 

dLL %>% 
  ggplot(aes(x=model, y=Deviance))+
  facet_wrap(~role, ncol = 1, scales = "free")+
  geom_col() +
  coord_flip()


model.name = 'TOM'
i = 2
modelFx = eval(parse(text=paste0('predict', str_split(model.name, "\\.")[[1]][1])))
params = fits[[model.name]]@coef %>% as.list() %>% paramsTransform() %>% paramsMake()
prediction = modelFx(params, expts[[i]])

data[[i]]$liar %>% 
  normSum() %>% 
  as.vector() %>% 
  as_tibble() %>% 
  mutate(k = rep(0:10, 11), 
         ksay = rep(0:10, each=11)) %>% 
  ggplot(aes(x=k, y=ksay, fill=value))+
  geom_tile()


prediction$liar %>% 
  normSum() %>% 
  as.vector() %>% 
  as_tibble() %>% 
  mutate(k = rep(0:10, 11), 
         ksay = rep(0:10, each=11)) %>% 
  ggplot(aes(x=k, y=ksay, fill=value))+
  geom_tile()
