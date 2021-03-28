rm(list=ls())
library(rio)
library(tidyverse)
#医院PG数据整理
#读取数据
data<-import('~/data/biomarker_hospital.xlsx')
#1、去掉无PG数据的(136685)
#2、去掉无个人信息的(姓名、MRN、EXtlDPat、DtValidate)--->136652
#3、将同一个人同一时间做的PG1、PG2合成一行(68378)
#4、将同一个人的数据合并成一行
##########
data1.1<-data%>%select(PtName,MRN,ExtIDPat,DtValidate,TestCode,TestResult)
#
data2.1<-data1.1%>%filter(!is.na(TestResult))%>%filter(is.na(PtName) & is.na(MRN) & 
                                                      is.na(ExtIDPat) & is.na(DtValidate))




#长变宽190735行变68370人次
data2<-data1.1%>%filter(!is.na(TestResult))%>%filter(!is.na(PtName) | !is.na(MRN) | 
                                                    !is.na(ExtIDPat) | !is.na(DtValidate))%>%
  pivot_wider(id_cols=c('PtName','MRN','ExtIDPat','DtValidate'),names_from = TestCode,values_from=TestResult)
sum(is.na(data2$PtName))#29缺失
sum(is.na(data2$MRN))#384缺失
sum(is.na(data2$ExtIDPat))#9238缺失
sum(is.na(data2$DtValidate))#5缺失
data2%>%filter(is.na(PtName) & is.na(MRN) & 
                 is.na(ExtIDPat) & is.na(DtValidate))
###
data3<-data2%>%filter(!is.na(MRN))%>%mutate(PG=paste(DtValidate,'|',PG1,'|',PG2))%>%
  select(PtName,MRN,ExtIDPat,PG)
data3.split<-split(data3,data3$MRN)
data3.split1<-lapply(data3.split,function(x){
  x[1:50,1:3]<-x[1,1:3]
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  return(x)
})
data3.split2<-lapply(data3.split1,function(x){
  x<-pivot_wider(x,names_from = seq,values_from = PG)
  names(x)[4:53]<-paste("第",1:50,'次诊断',sep='')
  #row.names(x)<-x$IDCARD
  return(x)
})
data4<-do.call(rbind,data3.split2)
export(data4,'~/data4.xlsx')


####
gastroscopy<-import('~/data/胃镜医院.xlsx')
sum(is.na(gastroscopy$MRN))#5927缺失
sum(is.na(gastroscopy$ExtIDPat))#209缺失
gastroscopy1<-gastroscopy%>%filter(!is.na(ExtIDPat))%>%mutate(inf=paste(MRN,'/',年龄,'|',主诉,'|',临床诊断,'|',
                                                                        镜下诊断,'|',报告时间2,sep=''))%>%
  select(姓名,ExtIDPat,性别,inf)
gastroscopy1.split<-split(gastroscopy1,gastroscopy1$ExtIDPat)  
gastroscopy1.split1<-lapply(gastroscopy1.split,function(x){
  x[1:20,1:3]<-x[1,1:3]
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  return(x)
})
gastroscopy1.split2<-lapply(gastroscopy1.split1,function(x){
  x<-pivot_wider(x,names_from = seq,values_from = inf)
  names(x)[4:24]<-paste("第",1:20,'次诊断',sep='')
  #row.names(x)<-x$IDCARD
  return(x)
})
gastroscopy2<-do.call(rbind,gastroscopy1.split2) 
export(gastroscopy2,'~/gastroscopy2.xlsx')
###筛查人群数据整理
rm(list=ls())
library(rio)
library(tidyverse)
data<-import('~/data/筛查人群数据.xlsx')
#str(data)
data1<-data%>%mutate(aaa=paste(住院号,检测日期,检测值,sep='|'))%>%select(-住院号,-检测日期,-检测值)
mag_bio<-function(data,biomarker){
  data.1<-data%>%filter(癌标==biomarker)%>%select(姓名,性别,身份证号,aaa)
  data.2<-split(data.1,data.1$身份证号)
  n<-max(data.frame(lapply(data.2, function(x)nrow(x))))
  data.3<-lapply(data.2,function(x){
    x[1:n,1:3]<-x[1,1:3]
    x$seq<-seq(nrow(x)) 
    row.names(x)<-NULL
    return(x)
  })
  data.4<-lapply(data.3,function(x){
    x<-pivot_wider(x,names_from = seq,values_from = aaa)
   # names(x)[4:n+3]<-paste("第",1:n,'次检测',sep='')
    row.names(x)<-NULL
    return(x)
  })
  data.5<-do.call(rbind,data.4)
  data.5$freq<-apply(data.5[,-1:-3],1,function(x)sum(!is.na(x)))
  return(data.5)
}
#CA125
ca125<-mag_bio(data1,'CA125')
#CA153
ca153<-mag_bio(data1,'CA153')
#CA199
ca199<-mag_bio(data1,'CA199')
#AFP
afp<-mag_bio(data1,'AFP')
#CEA
cea<-mag_bio(data1,'CEA')
#PG1 and PG2
data2<-data%>%filter(癌标=="PG1" | 癌标=="PG2")%>%pivot_wider(id_cols=c('姓名','性别','身份证号','住院号','检测日期'),names_from = 癌标,values_from=检测值)%>%
  muate(PG=paste(住院号,检测日期,'|',PG1,'|',PG2))%>%select(姓名,性别,身份证号,PG)
data2.1<-split(data2,data2$身份证号)
max(data.frame(lapply(data2.1, function(x)nrow(x))))
data2.2<-lapply(data2.1,function(x){
  x[1:18,1:3]<-x[1,1:3]
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  return(x)
})
data2.3<-lapply(data2.2,function(x){
  x<-pivot_wider(x,names_from = seq,values_from = PG)
  #names(x)[4:21]<-paste("第",1:50,'次诊断',sep='')
  #row.names(x)<-x$IDCARD
  return(x)
})
PG<-do.call(rbind,data2.3)
PG$freq<-apply(PG[,-1:-3],1,function(x)sum(!is.na(x)))
###
fulldata<-list('CA125'=ca125,'CA153'=ca153,'CA199'=ca199,'AFP'=afp,'CEA'=cea,'PG'=PG)
write.xlsx(fulldata, '~/data2.xlsx')

####################<<<<<<<<<<<<<2020-12-10分析>>>>>>>>>>>>>>>>>>>>>#####
rm(list=ls())
#读取数据
PG<-import('~/data/biomarker_hospital2.xlsx')
#查看数据
summary(PG)
apply(PG,2,function(x)sum(is.na(x)))

apply(PG1,2,function(x)sum(is.na(x)))
PG1%>%filter(is.na(MRN),is.na(ExtIDPat))
id1<-PG1[which(duplicated(PG1$MRN) & !is.na(PG1$MRN)),'MRN']
PG1[which(is.na(PG1$MRN)),'ExtIDPat']
#PG1[which(duplicated(PG1$MRN) & !is.na(PG1$MRN))
apply(PG1,2,function(x)sum(is.na(x)))

##########处理数据
PG1<-PG%>%transmute(PtName,Expression_1,MRN,ExtIDPat,TestCode,TestResult,
                    MRN2=ifelse(is.na(MRN),ExtIDPat,MRN))%>%filter(!is.na(TestResult),!is.na(PtName),!is.na(MRN2))
PG2<-PG1%>%pivot_wider(id_cols=c('PtName','MRN2','ExtIDPat','Expression_1'),names_from = TestCode,values_from=TestResult)%>%data.frame()
##MRN一样，ExtIDPat一定一样吗
#summary(PG2)
apply(PG2,2,function(x)sum(is.na(x)))
PG2.2<-PG2%>%transmute(MRN2,ExtIDPat)
PG2.2.split<-split(PG2.2,PG2.2$MRN2)
max(as.data.frame(lapply(PG2.2.split,function(x)nrow(x))))
PG2.3<-lapply(PG2.2.split,function(x){
  x[1:49,'MRN2']<-x[1,'MRN2']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from =ExtIDPat)
  return(x)
})
PG2.4<-do.call(rbind,PG2.3) 
export(PG2.4,'~/PG(住院号+门诊号).xlsx')
###MRN2和PG
PG3.2<-PG2%>%transmute(MRN2,PG=paste(Expression_1,'|',PG1,'|',PG2))
PG3.2.split<-split(PG3.2,PG3.2$MRN2)
max(as.data.frame(lapply(PG3.2.split,function(x)nrow(x))))
PG3.3<-lapply(PG3.2.split,function(x){
  x[1:49,'MRN2']<-x[1,'MRN2']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from =PG)
  return(x)
})
PG3.4<-do.call(rbind,PG3.3) 
export(PG3.4,'~/PG(PG).xlsx')
###MRN2和姓名
PG4.2<-PG2%>%transmute(MRN2,PtName)
PG4.2.split<-split(PG4.2,PG4.2$MRN2)
max(as.data.frame(lapply(PG4.2.split,function(x)nrow(x))))
PG4.3<-lapply(PG4.2.split,function(x){
  x[1:49,'MRN2']<-x[1,'MRN2']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from =PtName)
  return(x)
})
PG4.4<-do.call(rbind,PG4.3) 
export(PG4.4,'~/PG(name).xlsx')
############ExtIDPat和MRN2
PG3<-PG2%>%filter(!is.na(ExtIDPat))
PG3.1<-PG3%>%transmute(MRN2,ExtIDPat)
PG3.2.split<-split(PG3.1,PG3.1$ExtIDPat)
PG3.3<-lapply(PG3.2.split,function(x){
  x<-x[!duplicated(x[,'MRN2']),]
  x[1:2,'ExtIDPat']<-x[1,'ExtIDPat']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from =MRN2)
  return(x)
})
PG3.4<-do.call(rbind,PG3.3) 
export(PG3.4,'~/PG(住院号+门诊号2).xlsx')
############ExtIDPat和PG
PG3<-PG2%>%filter(!is.na(ExtIDPat))
PG3.1<-PG3%>%transmute(ExtIDPat,PG=paste(Expression_1,'|',PG1,'|',PG2))
PG3.2.split<-split(PG3.1,PG3.1$ExtIDPat)
max(as.data.frame(lapply(PG3.2.split,function(x)nrow(x))))
PG3.3<-lapply(PG3.2.split,function(x){
  x[1:49,'ExtIDPat']<-x[1,'ExtIDPat']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from =PG)
  return(x)
})
PG3.4<-do.call(rbind,PG3.3) 
export(PG3.4,'~/PG(PG2).xlsx')















