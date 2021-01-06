rm(list=ls())
here::i_am('README.md')

require(tidyverse)
require(stats4)


REFIT = FALSE
savefile = here::here('model', 'fitting', 'fits', 'save.Rdata')


if(!REFIT){
  load(savefile)
} else {
  fits = list()
}

source(here::here('model', 'fitting', 'loadExpts.R'))
source(here::here('model', 'fitting', 'models.R'))

for(model.name in names(modelWrappers)){
  if(!(model.name %in% names(fits))){
    tryCatch({fits[[model.name]] = mle(modelWrappers[[model.name]],
                             start = paramSeedFromFx(modelWrappers[[model.name]]),
                             method = "BFGS")},
             error = function(cond){print(model.name); message(cond)})
    save(file = savefile, list=ls())
  }
}


# 
# params = fits[['TOM']]@coef %>% as.list() %>% paramsTransform() %>% paramsMake()
# 
# LL = model_expt_LL(predictTOM, params) 
# 
# LL %>% as_tibble() %>% 
#   summarize_all(function(x){sum(x)*-2})
# 
# # how good is perfect?
# LLmax = matrix(NA, 
#             nrow=length(expts), 
#             ncol=2, 
#             dimnames = list(c(), c('liar', 'detector')))
# LLmin = LLmax
# for(i in 1:length(expts)){
#   for(a in names(data[[i]])){
#     LLmax[i,a] = sum(log(normSum(data[[i]][[a]]+1))*data[[i]][[a]])
#     LLmin[i,a] = sum(log(normSum(data[[i]][[a]]*0+1))*data[[i]][[a]])
#   }
# }
