rm(list=ls())
library(stringr)
library(rio)
library(tidyverse)
pathology1<-import('~/data/病理1-2017-2019.xlsx')
pathology2<-import('~/data/病理2-2014-2019.xlsx')
###处理pathology1
summary(pathology1)
apply(pathology1,2,function(x)sum(is.na(x)))
#去除无有效身份证件的
pathology1.1<-pathology1%>%filter(!is.na(身份证号))
#apply(pathology1.1,2,function(x)sum(is.na(x)))
####病理名单
pathology1<-import('~/data/pathology_inf.xlsx',sheet=1)
pathology2<-import('~/data/pathology_inf.xlsx',sheet=2)
apply(pathology1,2,function(x)sum(is.na(x)))
pathology1%>%filter(is.na(id_hos),is.na(id_cli))#无住院号和门诊号同时缺失的人
#id.hos<-pathology1[which(duplicated(pathology1$id_card)),'id_card']
pathology1.hos<-pathology1%>%filter(!is.na(id_hos),!duplicated(id_hos))
#id1.hos<-pathology1.hos[which(duplicated(pathology1.hos$id_card)),'id_card']
#pathology1.hos[which(pathology1.hos$id_card %in% id1.hos),]
pathology1.cli<-pathology1%>%filter(!is.na(id_cli),!duplicated(id_cli))
pathology1.3<-full_join(pathology1.hos,pathology1.cli,by='id_card')
####pathology2
apply(pathology2,2,function(x)sum(is.na(x)))
#id.hos<-pathology2[which(duplicated(pathology2$id_card)),'id_card']
pathology2.1<-pathology2%>%filter(!is.na(id_hos),!duplicated(id_card))
#pathology2[which(pathology2$id_card %in% id.hos),]
##combined 
pathology_total<-full_join(pathology1.3,pathology2.1,by='id_card')
export(pathology_total,'~/pathology_info.xlsx')





