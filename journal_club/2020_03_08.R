rm(list=ls())
#Time:2021-03-05
##analysis gastroscopy data about screeening 2017-2020
#1、import package and data
library(rio)
library(openxlsx)
library(tidyverse)
gastroscopy17<-read.xlsx('~/data/journal_club/2020_03_08/gastroscopy2017-2020.xlsx',sheet=1)%>%transmute(inw17=1,ID_BLAST17=ID_BLAST,PersoID,gastrores17=GASTFIBD_GS_2017)
gastroscopy18<-read.xlsx('~/data/journal_club/2020_03_08/gastroscopy2017-2020.xlsx',sheet=2)%>%transmute(inw18=1,source18=项目来源,ID_BLAST18=ID_BLAST,PersoID,gastrores18=胃镜诊断)
gastroscopy19<-read.xlsx('~/data/journal_club/2020_03_08/gastroscopy2017-2020.xlsx',sheet=3)%>%transmute(inw19=1,source19=来源,ID_BLAST19=ID_BLAST,PersoID,gastrores19=胃镜诊断描述)
gastroscopy20<-read.xlsx('~/data/journal_club/2020_03_08/gastroscopy2017-2020.xlsx',sheet=4)%>%transmute(inw20=1,source20=来源,ID_BLAST20=筛查编号,PersoID,gastrores20=胃镜诊断描述)
gastroscopy<-Reduce(function(x,y)merge(x,y,by='PersoID',all=TRUE) ,
                    list(gastroscopy17,gastroscopy18,gastroscopy19,gastroscopy20))%>%mutate(inw17=ifelse(is.na(inw17),0,inw17),
                                                                                            inw18=ifelse(is.na(inw18),0,inw18),
                                                                                            inw19=ifelse(is.na(inw19),0,inw19),
                                                                                            inw20=ifelse(is.na(inw20),0,inw20))
###
for(i in paste0('inw',18:20)) {
  a<-table(gastroscopy[,'inw17'],gastroscopy[,i])[2,2]
  print(a)
}

for(i in paste0('inw',19:20)) {
  a<-table(gastroscopy[,'inw18'],gastroscopy[,i])[2,2]
  print(a)
}
with(gastroscopy,table(inw19,inw20))[2,2]
