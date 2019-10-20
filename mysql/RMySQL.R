library(RMySQL)
library(tidyverse)
library(stringr)
library(ggpubr)
library(lattice)
library(reshape2)
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
biomark<-dbGetQuery(cnn,'SELECT id,name,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,HBsAg 
                    FROM biomarker WHERE year=2019 and source="示范区";')#19年示范区tumor marker数据
baseline<-dbReadTable(cnn,'baseline2019')
baseline<-dbReadTable(cnn,'baseline')#读取17+18+19基线数据
#提取针对胃部肿瘤标志物相关研究的基线资料
baseline<-dbGetQuery(cnn,'SELECT id,name,sex,age,disea14,disea15,disea16,disea17,disea18,disea19,disea20,disea22,disea23,disea28,disea29,disea30,disea31,
                          cancerfh,catpfath,catpmoth,catpbrot1,catpbrot2,catpsist1,catpsist2,catpchil1,catpchil2,
                          smoking,quitsmkyrs,cpd,smkyrs,menopause,agemenopau FROM baseline2019 WHERE source="示范区";')

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
#2.合并基线资料
biomark5.2<-left_join(biomark,baseline,by=c('id','name'))
biomark5.3<-biomark5.2%>%filter(PG1!='>200.0')#去除PG1>200的，89个，剩5941个
biomark5.3[,c('PG1','PG2','PGR')]<-data.frame(apply(biomark5.3[,c('PG1','PG2','PGR')],2,as.numeric))
summary(biomark5.3)
#3.查看PG1分布
p1<-ggboxplot(biomark5.3,y='PG1',add='jitter',palette = 'jco',xlab='')+geom_hline(yintercept = 70,color='red')
gghistogram(biomark5.3,x='PG1',bins=100,rug=TRUE,add='median',add.params = list(color='red'))
biomark5.3$PG1_range[biomark5.3$PG1<20]<-1
biomark5.3$PG1_range[biomark5.3$PG1<=70 & biomark5.3$PG1>=20]<-2
biomark5.3$PG1_range[biomark5.3$PG1<=200 & biomark5.3$PG1>70]<-3
biomark5.3$PG1_range<-factor(biomark5.3$PG1_range,levels = c(1,2,3),labels=c('严重受损','轻度受损','相对正常'))
table(biomark5.3$PG1_range)
PG1_freq<-data.frame(levels=c('严重受损','轻度受损','相对正常','应激升高'),freq=c(152,3964,1825,89))
PG1_freq$prop<-paste0(round(PG1_freq$freq/6030,2)*100,'%')

PG1_freq
p2<-ggpie(PG1_freq,x='freq',lab.pos='out',label='prop',fill='levels',color='white',palette = 'jco')+
  theme(legend.title = element_blank(),legend.position = 'right')
p3<-gghistogram(biomark5.3,x='PG1',y='..density..',bins=40,palette = 'jco',rug=TRUE,xlab='')+
  scale_x_continuous(breaks=c(0,20,40,70,100,150,200))+theme(legend.title=element_blank())
p4<-ggbarplot(PG1_freq,x='levels',y='freq',fill='levels',palette ='jco',ylab='频数',xlab='')+
  geom_text(aes(label=freq),position=position_dodge(0.9),vjust=0)+theme(legend.position = 'right',legend.title = element_blank())+
  scale_fill_discrete(labels=c('严重受损(PG<20)','应急升高(PG>=200)','相对正常(20<=PG<=70)','轻度受损(70<PG<200)'))
ggarrange(p3,p4,ncol=2)
#4.PG2分布
summary(biomark5.3$PG2)
gghistogram(biomark5.3,x='PG2',bins=30,rug=TRUE,add='median',add.params = list(color='red'))
#gghistogram(biomark5.3,x='PG2',bins=30,rug=TRUE,add='median',color='PG1_range',add.params = list(color='red'))
#PG1 and PGR
ggscatter(biomark5.3,x='PG1',y='PGR',alpha=0.5,add='reg.line',add.params = list(color='red'))+stat_cor(method='spearman')
ggscatter(biomark5.3,x='PG1',y='PGR',alpha=0.2,color='PG1_range',facet.by = 'PG1_range',add='reg.line',palette = 'jco',add.params = list(color='red'))+theme(legend.title = element_blank())+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+stat_cor(method='spearman')+theme(legend.position = 'none')


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
#PG1 adn PG2
ggscatter(data=biomark5.3,x='PG1',y='PG2',alpha=0.5,add='reg.line',add.params = list(color='red'))+stat_cor(method='spearman')




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
#PG2 and PG1 and PGR
ggplot(data=biomark5.3,aes(x=PG1,y=PGR,size=PG2))+geom_point(alpha=0.5,shape=21,colour='black',fill='cornsilk')+
  scale_size_area()+
  scale_colour_brewer(palette = "Set1")+
  geom_hline(aes(yintercept = 3))+geom_vline(aes(xintercept=70))+geom_vline(aes(xintercept=20))+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+scale_y_continuous(breaks=c(0,3,5,10,15,20))
##age and PG1、PG2、PGR
biomark5.3$age_group[biomark5.3$age>=40 & biomark5.3$age<45]<-1
biomark5.3$age_group[biomark5.3$age>=45 & biomark5.3$age<50]<-2
biomark5.3$age_group[biomark5.3$age>=50 & biomark5.3$age<55]<-3
biomark5.3$age_group[biomark5.3$age>=55 & biomark5.3$age<60]<-4
biomark5.3$age_group[biomark5.3$age>=60 & biomark5.3$age<65]<-5
biomark5.3$age_group[biomark5.3$age>=65 & biomark5.3$age<70]<-6
biomark5.3$age_group[biomark5.3$age>=70 & biomark5.3$age<75]<-7
biomark5.3_age<-biomark5.3%>%select(id,sex,age_group,PG1,PG2,PGR)
biomark5.3_age.2<-melt(data=biomark5.3_age,id.var=c('id','age_group','sex'),variable.name='PG',value.name='value')
biomark5.3_age.2%>%filter(!is.na(age_group))%>%group_by(PG,age_group)%>%summarise(median=median(value,na.rm=T),P25=quantile(value,0.25,na.rm = TRUE),P75=quantile(value,0.75,na.rm = TRUE))%>%
  ggplot(aes(x=age_group,y=median,color=PG))+geom_errorbar(aes(ymin=P25,ymax=P75),width=0.1)+geom_point()+geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7),labels=c('40-44','45-49','50-54','55-59','60-64','65-69','>=70'))+labs(x='age',y='PG1')+
  facet_grid(PG~.,scales='free')+theme(legend.position = 'none')
## sex and PG1、PG2、PGR
biomark5.3$sex<-factor(biomark5.3$sex,levels = c(1,2),labels =c('男','女'))
m1<-ggbarplot(data=subset(biomark5.3,!is.na(sex)),x='sex',y='PG1',add='median_iqr',color='sex',palette = 'lancet',xlab='')+
  stat_compare_means(method='wilcox.test',label.x=1.5,label='p.signif',label.y=170)+theme(legend.position = 'none')
m2<-ggbarplot(data=subset(biomark5.3,!is.na(sex)),x='sex',y='PG2',add='median_iqr',color='sex',palette = 'lancet')+
  stat_compare_means(method='wilcox.test',label.x=1.5,label='p.signif',label.y=60)+theme(legend.position = 'none')
m3<-ggbarplot(data=subset(biomark5.3,!is.na(sex)),x='sex',y='PGR',add='median_iqr',color='sex',palette = 'lancet',xlab='')+
  stat_compare_means(method='wilcox.test',label.x=1.5,label='p.signif',label.y=17)+theme(legend.position = 'none')
ggarrange(m1,m2,m3,nrow=1)
#sex and age and PG1、PG2、PGR
biomark5.3_age.2%>%filter(!is.na(age_group))%>%group_by(PG,sex,age_group)%>%summarise(median=median(value,na.rm=T),P25=quantile(value,0.25,na.rm = TRUE),P75=quantile(value,0.75,na.rm = TRUE))%>%
  ggplot(aes(x=age_group,y=median,color=sex))+geom_errorbar(aes(ymin=P25,ymax=P75),width=0.1)+geom_point()+geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7),labels=c('40-44','45-49','50-54','55-59','60-64','65-69','>=70'))+labs(x='age',y='PG1')+
  facet_grid(PG~.,scales='free')+theme(legend.position = 'none')

#吸烟状态与PG水平的关系
biomark5.3$smoking<-factor(biomark5.3$smoking,levels=c(1,2,3),labels=c('Never','Current','Ago'))
#PG1
g1<-ggboxplot(data=subset(biomark5.3,!is.na(smoking)),x='smoking',y='PG1',add='jitter',add.params = list(size=0.4,jitter=0.2))+
         stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='..p.adj..')+
  stat_compare_means(label.y=260,label.x=0.7)
#PG2
g2<-ggboxplot(data=subset(biomark5.3,!is.na(smoking)),x='smoking',y='PG2',add='jitter',add.params = list(size=0.4,jitter=0.2))+
  stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='..p.adj..')
#PGR
g3<-ggboxplot(data=subset(biomark5.3,!is.na(smoking)),x='smoking',y='PGR',add='jitter',add.params = list(size=0.4,jitter=0.2))+
  stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='..p.adj..')
ggarrange(g1,g2,g3,nrow=1)
#sex 分层
biomark5.3_smoking<-biomark5.3%>%select(id,sex,smoking,PG1,PG2,PGR)
biomark5.3_smoking.2<-melt(data=biomark5.3_smoking,id.var=c('id','smoking','sex'),variable.name='PG',value.name='value')
x<-ggboxplot(data=subset(biomark5.3_smoking.2,!is.na(smoking)),x='smoking',y='value',add='jitter',add.params = list(size=0.4,jitter=0.2))+
  stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),size=3)
facet(x+theme_bw(),facet.by = c('PG','sex'),scales='free',panel.labs.background = list(fill = "steelblue", color = "steelblue"))
#胃癌家族史与PG水平表达的关系(无显著性差异)
biomark5.3[,c("catpfath"   ,"catpmoth" ,  "catpbrot1" , "catpbrot2" , 
              "catpsist1" ,"catpsist2" , "catpchil1" , "catpchil2" )]<-apply(biomark5.3[,c("catpfath"  
                                                                                           ,"catpmoth" ,  "catpbrot1" , "catpbrot2" , "catpsist1" ,"catpsist2" , "catpchil1" , "catpchil2" )],2,function(x)x<-ifelse(!is.na(x),x,99))

biomark5.3$gastric_family<-with(biomark5.3,ifelse(cancerfh==2 & !is.na(cancerfh),ifelse(catpfath==16 | catpmoth==16 | 
                                  catpbrot1==16 | catpbrot2==16 | catpsist1==16 | catpsist2==16 |
                                    catpchil1==16 | catpchil2==16,1,0),0))
biomark5.3$family<-with(biomark5.3,ifelse(cancerfh==2,1,0))
biomark5.3$gastric_family.2[biomark5.3$family==1 & biomark5.3$gastric_family==1]<-1
biomark5.3$gastric_family.2[biomark5.3$family==1 & biomark5.3$gastric_family==0]<-2
biomark5.3$gastric_family.2[biomark5.3$family==0 & biomark5.3$gastric_family==0]<-3
table(biomark5.3$gastric_family);table(biomark5.3$family);table(biomark5.3$gastric_family.2)
biomark5.3_gastric_family<-biomark5.3%>%select(id,sex,gastric_family.2,PG1,PG2,PGR)
biomark5.3_gastric_family.2<-melt(data=biomark5.3_gastric_family,id.vars = c('id','sex','gastric_family.2'),
                                  variable.name = 'PG',value.name = 'value')

ggboxplot(data=subset(biomark5.3_gastric_family.2,!is.na(gastric_family.2)),x='gastric_family.2',y='value',add='jitter',facet.by = 'PG',add.params = list(size=0.4,jitter=0.2))+
  stat_compare_means(comparisons = list(c('1','2'),c('1','3'),c('2','3')),label='..p.adj..')+
  stat_compare_means()
##



