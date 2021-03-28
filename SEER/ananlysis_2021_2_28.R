rm(list=ls())
library(rio)
library(tidyverse)
#分析早期胃癌淋巴结转移的相关内容
gastric<-import('~/data/SEER/胃癌.xlsx')
names(gastric)
summary(gastric)
#数据集的构建
stomach2<-stomach%>%transmute(Age=`Age at diagnosis`,
                              Age_group=factor(ifelse(Age<50,1,2),levels=c(1,2),labels=c('<50','>=50')),
                              Race=factor(Race,levels=c(1,2,3),labels=c('White','Black','Others')),
                              Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
                              time=`Survival months`,status1,status2,Year_diag,Year_group=factor(case_when(
                                between(Year_diag,1975,1999) ~ 1,
                                between(Year_diag,2000,2010) ~ 2,
                                Year_diag>2010 ~ 3,
                              ),levels=c(1,2,3),labels=c('1975-1999','2000-2010','>2010')),
                              #status=factor(status,levels=c(0,1),labels=c('Alive','Dead')),
                              stage=factor(stage,levels=c(1,2,3,4),labels=c('locaized','regional','distanst','UNstage')),
                              COD=`COD to site rec KM`,status_spec=`SEER cause-specific death classification`,
                              #status_other=`SEER other cause of death classification`,
                              primary_site=factor(Primary_Site,levels=c(1,2,3),labels=c('Noncardia','Cardia','Others')),
                              correa=factor(Correa,levels=c(1,2,3),labels=c('intestinal','diffuse','others')),
                              stage_cli=as.numeric(stage_cli),stage_cli6=as.numeric(stage_cli6),stage_cli7=as.numeric(stage_cli7)
)