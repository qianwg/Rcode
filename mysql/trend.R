library(RMySQL)
library(tidyverse)
library(coin)
library()
#-------------------------------------MySQL数数据库连接---------------------------------------------------------------
cnn<-dbConnect(MySQL(),host='49.232.130.131',user='root',password='shengchao123',dbname='screening')#与mysql进行连接
#解决中文乱码问题
encoding <- if(grepl(pattern = 'utf8|utf-8',x = Sys.getlocale(),ignore.case = T)) 'utf8' else 'latin1'
dbSendQuery(cnn,paste("SET names",encoding))
#读取数据集
biomark<-dbGetQuery(cnn,'SELECT id,name,AFP,CA199,CEA FROM biomarker')#17+18+19的biomarker数据库
biomark<-dbGetQuery(cnn,'SELECT id,name,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,HBsAg 
                    FROM biomarker WHERE year=2019;')#19年tumor marker数据
biomark<-dbGetQuery(cnn,'SELECT id,name,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,HBsAg 
                    FROM biomarker WHERE year=2019 and source="示范区";')#19年示范区tumor marker数据
baseline<-dbReadTable(cnn,'baseline2019')#19年基线数据
baseline<-dbReadTable(cnn,'baseline')#读取17+18+19基线数据
#提取针对胃部肿瘤标志物相关研究的基线资料
baseline<-dbGetQuery(cnn,'SELECT id,name,sex,age,disea14,disea15,disea16,disea17,disea18,disea19,disea20,disea22,disea23,disea28,disea29,disea30,disea31,
                          cancerfh,catpfath,catpmoth,catpbrot1,catpbrot2,catpsist1,catpsist2,catpchil1,catpchil2,
                          smoking,quitsmkyrs,cpd,smkyrs,alcohol,menopause,agemenopau FROM baseline2019 WHERE source="示范区";')

#读取baseline1718
#dbClearResult(dbListResults(cnn)[[1]])#清空cnn的查询结果
baseline1718<-dbGetQuery(cnn,'SELECT * FROM baseline WHERE year=2017 OR year=2018;')
#读取所有baseline
baseline<-dbGetQuery(cnn,'SELECT * FROM baseline;')
#res<-dbSendQuery(cnn,'SELECT * FROM baseline WHERE year=2017 OR year=2018;')
#baseline1718<-dbFetch(res,n=-1) #取所有的数据
#dbClearResult(res)#清空res的查询
#通过sql语句查询数据
# dbGetQuery(cnn,"SELECT b.* FROM biomarker AS b WHERE PG1='>200.0'")#查询PG1>200的
dbDisconnect(cnn)#断开连接
# rm(list=ls())#清楚environment
# rm(biomark1)
#------------------------------------------------------------------------------------------------
#---------------------------------------1.基础性操作----------------------------------------------------------------
##对biomarker数据库进行操作
str_func<-function(x){
  b=table(str_detect(x,'>'))
  return(b[2])
}
biomark1.1<-biomark%>%select(AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,NSE,SCC,Fer)
apply(biomark,2,str_func)#查看各个指标中的极大值数量
apply(biomark1.1,2,str_func)#查看各个指标中的极大值数量
#重新赋值极大值
#1.将PG1＞200去除
biomark1.2<-biomark1.1%>%filter(PG1!='>200.0')
#2.将字符串中的‘>’去除,且改为数值型
str_func2<-function(x){
  b=as.numeric(str_replace_all(x,'>',''))
  return(b)
}
biomark1.3<-data.frame(apply(biomark1.2,2,str_func2))
# rm(biomark2)
summary(biomark3)
#-------------------------------------------趋势性检验--------------------------------------------------------
##linear-by-linear test for  contingency table




















