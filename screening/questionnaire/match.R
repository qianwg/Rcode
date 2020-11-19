rm(list=ls())
library(rio)
library(tidyverse)
source('~/Rcode/screening/questionnaire/questionnaire2020.R')
#重复编号及身份证号
mat_id<-screening2020[which(duplicated(screening2020$persoID)),'persoID']
mat_id2<-data_Hp2020[which(duplicated(data_Hp2020$persoID)),'persoID']
#screening2020[which(screening2020$persoID %in% mat_id),c('ID','name','persoID','初筛编号')]%>%arrange(persoID)
#screening2020[which(screening2020$persoID %in% mat_id2),c('ID','name','persoID','初筛编号')]%>%arrange(persoID)
#data_Hp2020[which(data_Hp2020$persoID %in% mat_id),c('初筛编号','name_Hp','persoID','街道','C14Value','Hp_pos')]%>%arrange(persoID)
#data_Hp2020[which(data_Hp2020$persoID %in% mat_id2),c('初筛编号','name_Hp','persoID','街道','C14Value','Hp_pos')]%>%arrange(persoID)
#screening2020[which(screening2020$初筛编号=='10301341'),c('初筛编号','name','persoID')]
#screening2020[which(screening2020$初筛编号=='10301342'),c('初筛编号','name','persoID')]
data_Hp2020[which(data_Hp2020$初筛编号=='10301341'),'persoID']<-'120103195111134517'
data_Hp2020[which(data_Hp2020$初筛编号=='10301342'),'persoID']<-'120103195112134543'
####
screening2020<-screening2020[-which(screening2020$ID %in% c(41070329,41080415,41080539,41030443,41030427)),]
data_Hp2020<-data_Hp2020[-which(data_Hp2020$初筛编号 %in% c(10701496,10700222,10800446,10304813,10301291,10303214,21032108,10303714,10303730,10301283,10901603,10300805,21081191,10304967)),]
###匹配
#1、问卷与Hp
match1<-inner_join(screening2020,data_Hp2020,by='persoID')
match1_not<-inner_join(s)




