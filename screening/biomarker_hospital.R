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
sum(is.na(data1.1$PtName))#102缺失
sum(is.na(data1.1$MRN))#810缺失
sum(is.na(data1.1$ExtIDPat))#24603缺失
sum(is.na(data1.1$DtValidate))#55缺失
sum(is.na(data1.1$TestCode))#0缺失
sum(is.na(data1.1$TestResult))#54050缺失
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
 





