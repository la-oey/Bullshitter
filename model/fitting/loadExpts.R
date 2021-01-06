require(tidyverse)


baseexpt = list()
baseexpt$BET = 0.8
baseexpt$liePenalty = 10 # 10 # -5
baseexpt$faPenalty = 5 # 5 # 0
baseexpt$numMarbles = 10

bs.final <- read.csv(here::here('model', 'fitting', 'data', 'bsfinal_anon.csv'))

humanLie <- bs.final %>%
  filter(roleCurrent == "bullshitter")
humanDetect <- bs.final %>%
  filter(roleCurrent == "bullshitDetector")

# 121 x 6 matrix
humanLieCounts <- humanLie %>%
  count(expt, probabilityRed, drawnRed, reportedDrawn) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), drawnRed=0:10, reportedDrawn=0:10, fill = list(n = 0))

# 22 x 6 matrix
humanDetectCounts <- humanDetect %>%
  count(expt, probabilityRed, reportedDrawn, callBS) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), reportedDrawn=0:10, callBS=c("True","False"), fill = list(n = 0))

expts = list()
data = list()
i = 1
for(ename in c("expt4","expt5")){
  for(p in c(0.2,0.5,0.8)){
    expts[[i]] = baseexpt
    expts[[i]]$utilSign = ifelse(ename == 'expt4', 1, -1)
    expts[[i]]$p = p
    data[[i]] = list()
    data[[i]]$liar = humanLieCounts %>% 
      filter(expt == ename, probabilityRed == p) %>% 
      pull(n) %>% matrix(ncol=baseexpt$numMarbles+1, byrow=T)
    # TODO: worry -- original may have had the wrong ordering on k and ksay wrt model.
    # here we have: k on d1 (row), ksay on d2 (col)
    data[[i]]$detector = humanDetectCounts %>% 
      filter(expt == ename, probabilityRed == p) %>% 
      pull(n) %>% 
      matrix(ncol=2, byrow=T) %>% 
      .[,c(2,1)]
    # here we have "BS" in col1, and "no BS" on col2
    i = i+1
  }
}
