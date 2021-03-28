rm(list=ls())
###2021-03-23：合并PG数据及病理数据
library(rio)
library(tidyverse)
library(openxlsx)
##读取PG数据
path1<-read.xlsx('~/data/hospital/Path_PG20210323/病理1.xlsx',detectDates = TRUE)
path2<-read.xlsx('~/data/hospital/Path_PG20210323/病理2.xlsx',detectDates = TRUE)
HosPG<-import('~/data/hospital/Path_PG20210323/HospitalPG.sav')


########合并病理1与PG
#1、根据病理1的id_hos与PG的MRN6
path1$MRN6<-path1$id_hos
PGPath1.1<-inner_join(HosPG,path1,by="MRN6")
#2、根据病理1的id_cli与PG的MRN8
path1$MRN8<-path1$id_cli
PGPath1.2<-inner_join(HosPG,path1,by="MRN8")
##两者进行合并
PGPath1.2<-PGPath1.2%>%select(-MRN6.y)
names(PGPath1.2)[which(names(PGPath1.2)=="MRN6.x")]<-"MRN6"
PGPath<-rbind(PGPath1.1,PGPath1.2)
#apply(PGPath[,c('ExtIDPat','MRN1','MRN2','PtName','name','id_hos','id_cli','id_card')],2,function(x)sum(is.na(x)))
#PGPath[which(PGPath$id_card %in% PGPath$id_card[which(duplicated(PGPath$id_card))]),c('MRN1','MRN2','PtName','name','id_hos','id_cli','id_card')]%>%arrange(id_card)
PGPath2<-PGPath[which(!duplicated(PGPath$id_card)),]
###
rm(PGPath,PGPath1.1,PGPath1.2)
########合并病理2与PG
##根据病理2的id_hos跟PG的MRN6进行合并
path2$MRN6<-path2$id_hos
PGPath3<-inner_join(HosPG,path2,by="MRN6")
########合并病理1与病理2
#创建变量
PGPath3$var_8<-NA
PGPath3$var_9<-NA
PGPath3$var_10<-NA
PGPath3$var_11<-NA
PGPath3$var_12<-NA
PGPath3$var_13<-NA
PGPath3$var_14<-NA
PGPath3$var1_8<-NA
PGPath3$var1_9<-NA
PGPath3$var1_10<-NA
PGPath3$var1_11<-NA
PGPath3$var1_12<-NA
PGPath3$var1_13<-NA
PGPath3$var1_14<-NA
##合并
PGPath4<-rbind(PGPath2,PGPath3)
##查看重复情况
PGPath4.1<-PGPath4%>%distinct()
PGPath4.2<-PGPath4[which(!duplicated(PGPath4$id_card)),]
PGPath4.1[which(PGPath4.1$id_card %in% PGPath4.1$id_card[which(duplicated(PGPath4.1$id_card))]),c('MRN1','MRN2','PtName','name','id_hos','id_cli','id_card')]%>%arrange(id_card)
export(PGPath4.1,"~/PGPath.xlsx")
export(PGPath2,"~/PGPath2017_2019.xlsx")
###针对2017-2019年数据分析
#创建变量：胃相关
PGPath2.1<-PGPath2%>%mutate(
  stomach=case_when(
    str_detect(var1_1,paste(c("胃","贲门","结合部"),collapse="|")) ~ 1,
    str_detect(var1_2,paste(c("胃","贲门","结合部"),collapse="|")) ~ 1,
    str_detect(var1_3,paste(c("胃","贲门","结合部"),collapse="|")) ~ 1,
    str_detect(var1_4,paste(c("胃","贲门","结合部"),collapse="|")) ~ 1,
    str_detect(var1_5,paste(c("胃","贲门","结合部"),collapse="|")) ~ 1,
    str_detect(var1_6,paste(c("胃","贲门","结合部"),collapse="|")) ~ 1,
  ),
  #是否术后
  postoperative=ifelse(str_detect(var1_1,paste(c("术后","残胃"),collapse="|")),1,0),
  #是否患癌
  cancer=ifelse(str_detect(var1_1,'癌'),1,0)
)
table(PGPath2.1$stomach,PGPath2.1$postoperative)

##创建变量：（1）Lauren分型；（2）癌前病变
#肠型胃癌:管状腺癌；
#弥漫型胃癌：印戒细胞癌
PGPath2.2<-PGPath2.1%>%mutate(
  lauren=case_when(
    str_detect(var1_1,'Lauren{0,1}(.*?)肠型') ~ 1,
    str_detect(var1_2,'Lauren{0,1}(.*?)肠型') ~ 1,
    str_detect(var1_3,'Lauren{0,1}(.*?)肠型') ~ 1,
    str_detect(var1_4,'Lauren{0,1}(.*?)肠型') ~ 1,
    str_detect(var1_5,'Lauren{0,1}(.*?)肠型') ~ 1,
    str_detect(var1_1,'Lauren{0,1}(.*?)弥漫型') ~ 2,
    str_detect(var1_2,'Lauren{0,1}(.*?)弥漫型') ~ 2,
    str_detect(var1_2,'lauren{0,1}(.*?)弥漫型') ~ 2,
    str_detect(var1_2,'LAUREN{0,1}(.*?)弥漫型') ~ 2,
    str_detect(var1_3,'Lauren{0,1}(.*?)弥漫型') ~ 2,
    str_detect(var1_4,'Lauren{0,1}(.*?)弥漫型') ~ 2,
    str_detect(var1_5,'Lauren{0,1}(.*?)弥漫型') ~ 2,
    
    str_detect(var1_1,'Lauren{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_1,'lauren{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_2,'Lauren{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_3,'Lauren{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_4,'Lauren{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_5,'Lauren{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_1,'LAUREN{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_2,'LAUREN{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_3,'LAUREN{0,1}(.*?)混合型') ~ 3,
    str_detect(var1_1,paste(c("管状腺癌","乳头状腺癌"),collapse="|")) ~ 1,
    str_detect(var1_2,paste(c("管状腺癌","乳头状腺癌"),collapse="|")) ~ 1,
    str_detect(var1_3,paste(c("管状腺癌","乳头状腺癌"),collapse="|")) ~ 1,
    
    str_detect(var1_1,'腺癌{0,1}(.*?)印戒细胞癌') ~ 3,
    str_detect(var1_2,'腺癌{0,1}(.*?)印戒细胞癌') ~ 3,
    str_detect(var1_3,'腺癌{0,1}(.*?)印戒细胞癌') ~ 3,
    str_detect(var1_1,paste(c("印戒细胞癌","弥漫性低分化腺癌"),collapse="|")) ~ 2,
    str_detect(var1_2,paste(c("印戒细胞癌","弥漫性低分化腺癌"),collapse="|")) ~ 2,
    str_detect(var1_3,paste(c("印戒细胞癌","弥漫性低分化腺癌"),collapse="|")) ~ 2,

  ),
  lauren=factor(lauren,levels=c(1,2,3),labels=c('肠型','弥漫型','混合型')),
  #癌前病变
  Precancerous=case_when(
    str_detect(var1_1,'萎缩') ~ 1,
    str_detect(var1_2,'萎缩') ~ 1,
    str_detect(var1_3,'萎缩') ~ 1,
    str_detect(var1_1,paste(c("肠化","肠上皮化生"),collapse="|")) ~ 2,
    str_detect(var1_2,paste(c("肠化","肠上皮化生"),collapse="|")) ~ 2,
    str_detect(var1_3,paste(c("肠化","肠上皮化生"),collapse="|")) ~ 2,
    str_detect(var1_1,'不典型增生') ~ 3,
    str_detect(var1_2,'不典型增生') ~ 3,
    str_detect(var1_3,'不典型增生') ~ 3,
    str_detect(var1_1,'上皮内瘤变') ~ 3,
    str_detect(var1_2,'上皮内瘤变') ~ 3,
    str_detect(var1_3,'上皮内瘤变') ~ 3
  ),
  Precancerous=factor(Precancerous,levels = c(1,2,3),labels=c('萎缩','肠化','上皮内瘤变')),
  #胃肠间质瘤
  GIST=case_when(
    str_detect(var1_1,paste(c("胃肠间质瘤","胃肠道间质瘤","GIST","间质瘤"),collapse="|")) ~ 1,
    str_detect(var1_2,paste(c("胃肠间质瘤","胃肠道间质瘤","GIST"),collapse="|")) ~ 1,
    str_detect(var1_3,paste(c("胃肠间质瘤","胃肠道间质瘤","GIST"),collapse="|")) ~ 1,
    
  ),
  #神经内分泌肿瘤
  神经内分泌肿瘤=case_when(
    str_detect(var1_1,paste(c("神经内分泌瘤"),collapse="|")) ~ 1,
    str_detect(var1_2,paste(c("神经内分泌瘤"),collapse="|")) ~ 1,
    str_detect(var1_3,paste(c("神经内分泌瘤"),collapse="|")) ~ 1,
    
  )
  
)
table(PGPath2.2$lauren)
table(PGPath2.2$Precancerous)

#PGPath2.2[which(PGPath2.2$lauren==1),c('var1_1','var1_2','var1_3')]  
export(PGPath2.2,"~/PGPath2.2.xlsx")




