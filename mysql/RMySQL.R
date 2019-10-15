library(RMySQL)
library(tidyverse)
library(stringr)
library(ggpubr)
#-------------------------------------MySQL数数据库连接---------------------------------------------------------------
#cnn<-dbConnect(MySQL(),host='39.106.31.215',user='root',password='',dbname='screening')#与mysql进行连接
cnn<-dbConnect(MySQL(),host='49.232.130.131',user='root',password='',dbname='screening')#与mysql进行连接
# dbListTables(cnn)#查看screening下的数据集
# dbListFields(cnn,'biomarker')#查看字段
#解决中文乱码问题
encoding <- if(grepl(pattern = 'utf8|utf-8',x = Sys.getlocale(),ignore.case = T)) 'utf8' else 'latin1'
dbSendQuery(cnn,paste("SET names",encoding))
#读取数据集biomark2019
biomark<-dbReadTable(cnn,'biomarker')#17+18+19的biomarker数据库
biomark<-dbGetQuery(cnn,'SELECT id,name,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,HBsAg 
                    FROM biomarker WHERE year=2019;')#19年tumor marker数据
biomark<-dbGetQuery(cnn,'SELECT id,name,sex,age,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,HBsAg 
                    FROM biomarker WHERE year=2019 and source="示范区";')#19年示范区tumor marker数据
baseline<-dbReadTable(cnn,'baseline2019')
baseline<-dbReadTable(cnn,'baseline')#读取17+18+19基线数据
#读取baseline1718
#dbClearResult(dbListResults(cnn)[[1]])#清空cnn的查询结果
baseline1718<-dbGetQuery(cnn,'SELECT * FROM baseline WHERE year=2017 OR year=2018;')
#res<-dbSendQuery(cnn,'SELECT * FROM baseline WHERE year=2017 OR year=2018;')
#baseline1718<-dbFetch(res,n=-1) #取所有的数据
#dbClearResult(res)#清空res的查询
#通过sql语句查询数据
# dbGetQuery(cnn,"SELECT b.* FROM biomarker AS b WHERE PG1='>200.0'")#查询PG1>200的
dbDisconnect(cnn)#断开连接
# rm(list=ls())#清楚environment
# rm(biomark1)
#---------------------------------------1.基础性操作----------------------------------------------------------------
##对biomarker数据库进行操作
str_func<-function(x){
  b=table(str_detect(x,'>'))
  return(b[2])
  }
biomark1.1<-biomark%>%select(AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,NSE,SCC,Fer)
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
#--------------------------------------2.探索性分析(去除异常值)------------------------------------------------------------------
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
#-------------------------------------3.探索性分析(画分布直方图)---------------------------------------------------------------------------
#AFP
gghistogram(biomark7,x='AFP',bins=100,y='..density..',rug=TRUE,add='median',add.params = list(color='red'))+
  scale_x_continuous(limits = c(0,25))+geom_vline(aes(xintercept=7),col='green')
#CA199
gghistogram(biomark7,x='CA199',bins=100,y='..density..',rug=TRUE,add='median',add.params = list(color='red'))+
  scale_x_continuous(limits = c(0,100))+geom_vline(aes(xintercept=27),col='green')
#CA125
#------------------------------------4.2019示范区肿瘤标志物阳性鉴定--------------------------------------------------------
biomark1.1<-biomark%>%filter(year==2019,source=='示范区')%>%select(area,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PG1.PG2,HBsAg)
biomark1.1[,-1]<-data.frame(apply(biomark1.1[,-1],2,str_func2))
#1.AFP(>4*7)，CEA(4*5),ca199(4*27) ,CA125(4*35) CA153(4*30),PG(PG1<=70,PGR<=3),HBsAG(>0.05)
biomark1.1$afp.pos<-ifelse(biomark1.1$AFP>4*7 & !is.na(biomark1.1$AFP),2,1)
biomark1.1$cea.pos<-ifelse(biomark1.1$CEA>4*5 & !is.na(biomark1.1$CEA),2,1)
biomark1.1$ca199.pos<-ifelse(biomark1.1$CA199>4*27 & !is.na(biomark1.1$CA199),2,1)
biomark1.1$ca125.pos<-ifelse(biomark1.1$CA125>4*35 & !is.na(biomark1.1$CA125),2,1)
biomark1.1$ca153.pos<-ifelse(biomark1.1$CA153>4*30 & !is.na(biomark1.1$CA153),2,1)
biomark1.1$PG.pos<-ifelse(biomark1.1$PG1<=70 & biomark1.1$PG1.PG2<=3 & !is.na(biomark1.1$PGR),2,1)
biomark1.1$HBsAG.pos<-ifelse(biomark1.1$HBsAg>0.05 & !is.na(biomark1.1$HBsAg),2,1)
biomark1.1$postive<-ifelse(biomark1.1$afp.pos==2 | biomark1.1$cea.pos==2 | biomark1.1$ca199.pos==2 |
                             biomark1.1$ca125.pos==2 | biomark1.1$ca153.pos==2 | biomark1.1$PG.pos==2 |
                             biomark1.1$HBsAG.pos==2,2,1)
apply(biomark1.1[,c('afp.pos','cea.pos','ca199.pos','ca125.pos','ca153.pos','PG.pos','HBsAG.pos','postive')],2,table)
with(biomark1.1,table(area,postive))
#----------------------------------------5.2019胃蛋白酶原基础性分析----------------------------------------------------------------------------
#1.首先分析和排除极大值
biomark5.1<-biomark%>%select(PG1,PG2,PGR)#脱敏6030
apply(biomark5.1,2,str_func)#查看各个指标中的极大值数量
biomark5.3<-biomark5.1%>%filter(PG1!='>200.0')#去除PG1>200的，89个，剩5941个
biomark5.3[,c('PG1','PG2','PGR')]<-data.frame(apply(biomark5.3[,c('PG1','PG2','PGR')],2,as.numeric))
summary(biomark5.3)
#3.查看PG1分布
p1<-ggboxplot(biomark5.3,y='PG1',add='jitter',palette = 'jco')+geom_hline(yintercept = 70,color='red')
gghistogram(biomark5.3,x='PG1',bins=100,rug=TRUE,add='median',add.params = list(color='red'))
biomark5.3$PG1_range[biomark5.3$PG1<20]<-1
biomark5.3$PG1_range[biomark5.3$PG1<=70 & biomark5.3$PG1>=20]<-2
biomark5.3$PG1_range[biomark5.3$PG1<=200 & biomark5.3$PG1>70]<-3
biomark5.3$PG1_range<-factor(biomark5.3$PG1_range,levels = c(1,2,3),labels=c('严重受损','轻度受损','相对正常'))
table(biomark5.3$PG1_range)
PG1_freq<-data.frame(levels=c('严重受损','轻度受损','相对正常','应激升高'),freq=c(152,3964,1825,89))
PG1_freq$prop<-paste0(round(PG1_freq$freq/6030,2)*100,'%')
PG1_freq
p2<-ggpie(PG1_freq,x='freq',lab.pos='out',label='prop',fill='levels')+
  theme(legend.title = element_blank(),legend.position = 'right')
p3<-gghistogram(biomark5.3,x='PG1',bins=40,fill='PG1_range',palette = 'jco',rug=TRUE)+
  scale_x_continuous(breaks=c(0,20,40,70,100,150,200))+theme(legend.title=element_blank())
p4<-ggbarplot(PG1_freq,x='levels',y='freq',fill='levels',palette = 'jco',ylab='频数',xlab='')+
  theme(legend.position = 'none')+geom_text(aes(label=freq),)
ggarrange(p1,p3,p2,p4,nrow=2,ncol=2)
#4.PG2分布
summary(biomark5.3$PG2)
gghistogram(biomark5.3,x='PG2',bins=30,rug=TRUE,add='median',add.params = list(color='red'))
#gghistogram(biomark5.3,x='PG2',bins=30,rug=TRUE,add='median',color='PG1_range',add.params = list(color='red'))
#PG1 and PGR
ggscatter(biomark5.3,x='PG1',y='PGR',alpha=0.2,color='PG1_range',add='reg.line')+theme(legend.title = element_blank())


#5.PG2 and PGR
ggscatter(biomark5.3,x='PG2',y='PGR',alpha=0.2)
ggscatter(biomark5.3,x='PG2',y='PGR',alpha=0.2,color='PG1_range')
ggscatter(biomark5.3,x='PG2',y='PGR',alpha=0.2,facet.by='PG1_range')
mypanel <- function(x, y){
  panel.xyplot(x, y, pch=19)
  panel.rug(x,y)
  panel.grid(h=-1, v=-1) # 添加水平和垂直的网格线
  panel.lmline(x, y, col="red", lwd=1, lty=2) # 添加回归曲线
}


xyplot(PGR ~ PG2|PG1_range, data=biomark5.3,
       layout=c(3,1),
       aspect=1.5,
       main = "PGR vs. PG2 by PG1_range",
       xlab = "PG2",
       ylab = "PGR",
       panel = mypanel)

#6.PG2 and PG2 and PGR
ggplot(data=biomark5.3,aes(x=PG1,y=PGR,size=PG2))+geom_point(alpha=0.5,shape=21,colour='black',fill='cornsilk')+
  scale_size_area()+
  scale_colour_brewer(palette = "Set1")+
  geom_hline(aes(yintercept = 3))+geom_vline(aes(xintercept=70))+geom_vline(aes(xintercept=20))+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+scale_y_continuous(breaks=c(0,3,5,10,15,20))

#7.幽门螺旋杆菌感染史与PG2、PG2、PGR的关系如何
biomark$id<-as.numeric(biomark$id)
baseline<-dbGetQuery(cnn,'SELECT id,name,disea17,disea18,disea20 FROM baseline WHERE year=2019 and source="示范区";')
biomark7<-left_join(biomark,baseline,by=c('id','name'))
biomark7.1<-biomark7%>%select(sex,age,PG1,PG2,PGR,disea17,disea18,disea20)
biomark7.2<-biomark7.1%>%filter(PG1!='>200.0')#去除PG1>200的，89个，剩5941个
biomark7.2[,c('PG1','PG2','PGR')]<-data.frame(apply(biomark7.2[,c('PG1','PG2','PGR')],2,as.numeric))
summary(biomark7.2)
ggboxplot(data=subset(biomark7.2,!is.na(disea20)),x='disea20',y='PG2',add='jitter',color='disea20')+
  stat_compare_means(method='wilcox.test',label.x=1.5,label='p.signif',size=6,label.y=60)+
  theme(legend.position = 'none')
ggplot(data=biomark5.3,aes(x=PG1,y=PGR,size=PG2))+geom_point(alpha=0.5,shape=21,colour='black',fill='cornsilk')+
  scale_size_area()+
  scale_colour_brewer(palette = "Set1")+
  geom_hline(aes(yintercept = 3))+geom_vline(aes(xintercept=70))+geom_vline(aes(xintercept=20))+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+scale_y_continuous(breaks=c(0,3,5,10,15,20))













