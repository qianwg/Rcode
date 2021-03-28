rm(list=ls())
##PG合并病理，胃镜
library(rio)
library(tidyverse)
library(openxlsx)
##读取PG数据
path1<-read.xlsx('~/data/hospital/Path_PG20210323/病理1.xlsx',detectDates = TRUE)
path2<-read.xlsx('~/data/hospital/Path_PG20210323/病理2.xlsx',detectDates = TRUE)
HosPG<-import('~/data/hospital/Path_PG20210323/HospitalPG.sav')
gastroscopy<-read.xlsx("~/data/hospital/20210319/Gastroscpy(20210319版).xlsx",detectDates = TRUE)
##PG合并病理
########合并病理1与PG
#1、根据病理1的id_hos与PG的MRN6
path1$MRN6<-path1$id_hos
PGPath1.1<-left_join(HosPG,path1,by="MRN6")
#2、根据病理1的id_cli与PG的MRN8
path1$MRN8<-path1$id_cli
PGPath1.2<-left_join(HosPG,path1,by="MRN8")
##两者进行合并
PGPath1.2<-PGPath1.2%>%select(-MRN6.y)
names(PGPath1.2)[which(names(PGPath1.2)=="MRN6.x")]<-"MRN6"
PGPath<-rbind(PGPath1.1,PGPath1.2)
#apply(PGPath[,c('ExtIDPat','MRN1','MRN2','PtName','name','id_hos','id_cli','id_card')],2,function(x)sum(is.na(x)))
#PGPath[which(PGPath$id_card %in% PGPath$id_card[which(duplicated(PGPath$id_card))]),c('MRN1','MRN2','PtName','name','id_hos','id_cli','id_card')]%>%arrange(id_card)
PGPath2<-PGPath[which(!duplicated(PGPath$id_card)),]
###
rm(PGPath,PGPath1.1,PGPath1.2)
##PG合并胃镜
#MRN8与IID
PGPath3<-Path