rm(list=ls())
library(rio)
library(tidyverse)
library(survival)
library(rms)
library(survivalROC)
EGJ<-import('~/data/SEER_2020.xlsx')
summary(EGJ)
EGJ2<-EGJ%>%transmute(
  Age_group=factor(Age_group,levels=c(1,2),labels=c('<60','>=60')),
  Race=factor(Race,levels=c(1,2,3),labels=c('White','Black','Others')),
  Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
  time=`Survival months`,status,
  #status=factor(status,levels=c(0,1),labels=c('Alive','Dead')),
  stage=factor(stage,levels=c(1,2,3,4),labels=c('locaized','regional','distans','UNstage')),
  Grade=factor(Grade,levels=c(1,2,3,4),labels=c('I','II','III','IV')),
  Surgery=factor(Surgery,levels=c(0,1),labels=c('No','Yes')),
  Radiation=factor(Radiation,levels=c(0,1),labels=c('No','Yes')),
  Chemotherapy=factor(Chemotherapy,levels=c(0,1),labels=c('No','Yes')),
  tumor_size=factor(tumor_size,levels=c(1,2,3,4,5),labels=c('<=2','<=3','<=4','<=5','>5')),
  meta=factor(meta,levels=c(0,1),labels=c('No','Yes')),
  COD=`COD to site rec KM`,status_spec=`SEER cause-specific death classification`,
  status_other=`SEER other cause of death classification`
)