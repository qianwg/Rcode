library(RMySQL)
library(tidyverse)
library(stringr)
library(ggpubr)
#-------------------------------------MySQL数数据库连接---------------------------------------------------------------
cnn<-dbConnect(MySQL(),host='39.106.31.215',user='root',password='',dbname='screening')#与mysql进行连接
# dbListTables(cnn)#查看screening下的数据集
# dbListFields(cnn,'biomarker')#查看字段
#解决中文乱码问题
encoding <- if(grepl(pattern = 'utf8|utf-8',x = Sys.getlocale(),ignore.case = T)) 'utf8' else 'latin1'
dbSendQuery(cnn,paste("SET names",encoding))
#读取数据集biomark2019
biomark<-dbReadTable(cnn,'biomarker')
baseline<-dbReadTable(cnn,'baseline2019')
#通过sql语句查询数据
# dbGetQuery(cnn,"SELECT b.* FROM biomarker AS b WHERE PG1='>200.0'")#查询PG1>200的
dbDisconnect(cnn)#断开连接
# rm(list=ls())#清楚environment
# rm(biomark1)
#---------------------------------------基础性操作----------------------------------------------------------------
##对biomarker数据库进行操作
str_func<-function(x){
  b=table(str_detect(x,'>'))
  return(b[2])
  }
biomark1<-biomark%>%select(AFP,CA199,CA153,CA125,CEA,PG1,PG2,PG1.PG2,NSE,SCC,Fer)
apply(biomark3,2,str_func)#查看各个指标中的极大值数量
#重新赋值极大值
#1.将PG1＞200去除
biomark2<-biomark1%>%filter(PG1!='>200.0')
#2.将字符串中的‘>’去除,且改为数值型
str_func2<-function(x){
  b=as.numeric(str_replace_all(x,'>',''))
  return(b)
}
biomark3<-data.frame(apply(biomark2,2,str_func2))
# rm(biomark2)
summary(biomark3)
#--------------------------------------探索性分析(去除异常值)------------------------------------------------------------------
#1.分析数值分布
source('~/project/Rcode/screening/biomark_risk.R')
biomark4<-biomark3%>%select(AFP,CA199,CA125,CA153,CEA)
biomark5<-marker_func(biomark4)
biomark6<-data.frame(apply(biomark5[,c("AFP.risk","CA199.risk","CA125.risk","CA153.risk","CEA.risk")],2,risk_fuc))
apply(biomark6[,c("AFP.risk","CA199.risk","CA125.risk","CA153.risk","CEA.risk")],2,table)
#2.去除＞200的指标
fucn_200<-function(x){
  x[x>200]<-NA
  return(x)
}
biomark7<-data.frame(apply(biomark5,2,fucn_200))%>%select(AFP,CA199,CA125,CA153,CEA)
summary(biomark7)
#-------------------------------------探索性分析(画分布直方图)---------------------------------------------------------------------------
#AFP
gghistogram(biomark7,x='AFP',bins=100,y='..density..',rug=TRUE,add='median',add.params = list(color='red'))+
  scale_x_continuous(limits = c(0,25))+geom_vline(aes(xintercept=7),col='green')
#CA199
gghistogram(biomark7,x='CA199',bins=100,y='..density..',rug=TRUE,add='median',add.params = list(color='red'))+
  scale_x_continuous(limits = c(0,100))+geom_vline(aes(xintercept=27),col='green')
#CA125


