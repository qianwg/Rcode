rm(list=ls())
library(rio)
library(tidyverse)
baseline2017_2019<-import('~/data/baseline2017-2019.sav')
gastroscopy<-import('~/data/胃镜检查名单2017-2019.xlsx')
names(baseline2017_2019)[5]<-'身份证号'
match<-inner_join(gastroscopy,baseline2017_2019,by='身份证号')
export(match,'match2017-2019.xlsx')
