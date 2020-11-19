rm(list=ls())
library(rio)
library(tidyverse)
find.gastr<-function(x){
  x<-ifelse(grepl(pattern="胃",x=x),x,NA)
  return(x)
}

gastroscopy<-import('~/data/住院和门诊合并.xlsx')
gastroscopy[,9:20]<-apply(gastroscopy[,9:20],2,find.gastr)
gastroscopy$NAN<-apply(gastroscopy[,9:20],1,function(x)sum(!is.na(x)))
export(gastroscopy,'~/data2019.xlsx')
