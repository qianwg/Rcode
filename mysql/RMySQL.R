library(RMySQL)
library(tidyverse)
library(stringr)
library(ggpubr)
library(lattice)
library(reshape2)
#-------------------------------------MySQL数数据库连接---------------------------------------------------------------
#cnn<-dbConnect(MySQL(),host='39.106.31.215',user='root',password='',dbname='screening')#与mysql进行连接
cnn<-dbConnect(MySQL(),host='49.232.130.131',user='root',password='shengchao123',dbname='screening')#与mysql进行连接
# dbListTables(cnn)#查看screening下的数据集
# dbListFields(cnn,'biomarker')#查看字段
#解决中文乱码问题
encoding <- if(grepl(pattern = 'utf8|utf-8',x = Sys.getlocale(),ignore.case = T)) 'utf8' else 'latin1'
dbSendQuery(cnn,paste("SET names",encoding))
#读取数据集
biomark_res<-dbSendQuery(cnn,'SELECT id,name,AFP,CA199,CEA ,CA153,CA125 ,HBsAg FROM biomarker')#17+18+19的biomarker数据库
biomark<-dbFetch(biomark_res,n=-1)
dbClearResult(biomark_res)
biomark<-dbGetQuery(cnn,'SELECT id,name,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,HBsAg 
                    FROM biomarker WHERE year=2019;')#19年tumor marker数据
biomark<-dbGetQuery(cnn,'SELECT id,name,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR,HBsAg 
                    FROM biomarker WHERE year=2019 and source="示范区";')#19年示范区tumor marker数据
baseline<-dbReadTable(cnn,'baseline2019')
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
biomark5.1<-biomark%>%select(id,name,PG1,PG2,PGR)#脱敏6030
biomark5.1.2<-biomark5.1%>%filter(PG1!='>200.0')
biomark5.1.2[,c('PG1','PG2','PGR')]<-data.frame(apply(biomark5.1.2[,c('PG1','PG2','PGR')],2,str_func2))
summary(biomark5.1.2)
#2.合并基线资料
biomark5.2<-left_join(biomark,baseline,by=c('id','name'))
biomark5.2<-left_join(biomark5.1.2,baseline,by=c('id','name'))
#2.1基线资料分布
#2.2男女分布
biomark5.2$sex<-factor(biomark5.2$sex,levels = c(1,2),labels=c('男','女'))
k1<-biomark5.2%>%filter(!is.na(sex))%>%group_by(sex)%>%summarise(freq=n(),prop=paste0(freq,'(',round((freq/6028)*100,2),'%',')'))%>%ggpie(x='freq',
                    fill='sex',label='prop',palette = 'jco',lab.pos = 'in',color='white',xlab='男女分布')+theme(legend.position = 'right',legend.title = element_blank())

#年龄
biomark5.2$age_group[biomark5.2$age>=40 & biomark5.2$age<45]<-1
biomark5.2$age_group[biomark5.2$age>=45 & biomark5.2$age<50]<-2
biomark5.2$age_group[biomark5.2$age>=50 & biomark5.2$age<55]<-3
biomark5.2$age_group[biomark5.2$age>=55 & biomark5.2$age<60]<-4
biomark5.2$age_group[biomark5.2$age>=60 & biomark5.2$age<65]<-5
biomark5.2$age_group[biomark5.2$age>=65 & biomark5.2$age<70]<-6
biomark5.2$age_group[biomark5.2$age>=70 & biomark5.2$age<75]<-7
k2<-biomark5.2%>%filter(!is.na(age_group))%>%group_by(sex,age_group)%>%summarise(freq=n())%>%ggbarplot(x='age_group',y='freq',fill='sex',palette = 'jco',xlab='年龄组',ylab='频数')+
scale_x_discrete(breaks=c(1,2,3,4,5,6,7),labels=c('40-44','45-49','50-54','55-59','60-64','65-69','>=70'))

biomark5.3<-biomark5.2%>%filter(PG1!='>200.0')#去除PG1>200的，89个，剩5941个
biomark5.3[,c('PG1','PG2','PGR')]<-data.frame(apply(biomark5.3[,c('PG1','PG2','PGR')],2,as.numeric))
summary(biomark5.3)
#吸烟
biomark5.2$smoking<-factor(biomark5.2$smoking,levels = c(1,2,3),labels=c('从不','当前','以前'))
k3<-biomark5.2%>%filter(!is.na(smoking))%>%group_by(sex,smoking)%>%summarise(freq=n())%>%ggbarplot(x='smoking',y='freq',fill='sex',palette = 'jco',xlab='吸烟状态',ylab='频数')
###合并
k123<-ggarrange(k1,k2,k3,nrow=1,legend = 'top',common.legend = TRUE)
#自述胃部疾病
biomark5.2_disea<-biomark5.2%>%select(id,disea14,disea15,disea16,disea17,disea18,disea19,disea20,disea22,disea23)
biomark5.2_disea_1<-melt(biomark5.2_disea,id.vars = c('id'),variable.name = 'disea',value.name = 'value')
head(biomark5.2_disea_1)
k4<-biomark5.2_disea_1%>%filter(!is.na(value))%>%group_by(disea,value)%>%summarise(freq=n())%>%filter(value==2)%>%
  ggbarplot(x='disea',y='freq',label='freq',lab.pos = 'out',sort.val='desc',x.text.angle=50,xlab='胃部疾病史',ylab='频数')+
  scale_x_discrete(labels=c('胃溃疡','萎缩性胃炎','十二指肠溃疡','幽门螺旋杆菌感染史',
                             '胃息肉','胃粘膜异型或不典型增生','胃肠上皮化生','食管或胃上皮内瘤变','Barrett食管'))

ggarrange(k123,k4,nrow=2)
#3.查看PG1分布
p1<-ggboxplot(biomark5.3,y='PG1',add='jitter',palette = 'jco',xlab='')+geom_hline(yintercept = 70,color='red')
gghistogram(biomark5.3,x='PG1',bins=100,rug=TRUE,add='median',add.params = list(color='red'))
biomark5.3$PG1_range[biomark5.3$PG1<20]<-1
biomark5.3$PG1_range[biomark5.3$PG1<=70 & biomark5.3$PG1>=20]<-2
biomark5.3$PG1_range[biomark5.3$PG1<=200 & biomark5.3$PG1>70]<-3
biomark5.3$PG1_range<-factor(biomark5.3$PG1_range,levels = c(1,2,3),labels=c('严重受损','轻度受损','相对正常'))
table(biomark5.3$PG1_range)
PG1_freq<-data.frame(levels=c(1,2,3,4),freq=c(152,3964,1825,89))
PG1_freq$levels<-factor(PG1_freq$levels,levels=c(1,2,3,4),labels=c('PG<20','20<=PG<=70','70<PG<200','200<=PG'),order=TRUE)
PG1_freq$prop1<-round(PG1_freq$freq/6030,4)*100
PG1_freq$prop<-paste0(round(PG1_freq$freq/6030,4)*100,'%')

PG1_freq
p2<-ggpie(PG1_freq,x='freq',lab.pos='out',label='prop',fill='levels',color='white',palette = 'jco')+
  theme(legend.title = element_blank(),legend.position = 'right')
p3<-gghistogram(biomark5.3,x='PG1',y='..density..',bins=40,fill='green',palette = 'jco',rug=TRUE,xlab='PG1',ylab='密度')+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+theme(legend.title=element_blank())+
  stat_function(fun = dnorm, args = list(mean = mean(biomark5.3$PG1),sd = sd(biomark5.3$PG1)))
p3<-ggplot(data=biomark5.3,aes(x=PG1))+geom_histogram(aes(y=..density..),bins=40,fill='green',color='black')+scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+
  stat_function(fun = dnorm, args = list(mean = mean(biomark5.3$PG1),sd = sd(biomark5.3$PG1)),color='red')+
  labs(y='密度')+theme_classic()

p4<-ggbarplot(PG1_freq,x='levels',y='prop1',palette ='jco',fill='lightblue',ylab='百分比',xlab='PG1范围',label = 'prop1')

p4

p<-ggarrange(p3,p4,ncol=2)
###粘膜受损程度与年龄段分布
biomark5.4<-biomark%>%filter(year==2019,source=='示范区')%>%select(id,PG1,PGR)
biomark5.5<-inner_join(biomark5.4,baseline,by='id')
biomark5.5$PG1<-as.numeric(str_replace_all(biomark5.5$PG1,'>',''))
biomark5.5$PGR<-as.numeric(str_replace_all(biomark5.5$PGR,'>',''))
summary(biomark5.5$PG1)
biomark5.5$age_group[biomark5.5$age>=40 & biomark5.5$age<45]<-1
biomark5.5$age_group[biomark5.5$age>=45 & biomark5.5$age<50]<-2
biomark5.5$age_group[biomark5.5$age>=50 & biomark5.5$age<55]<-3
biomark5.5$age_group[biomark5.5$age>=55 & biomark5.5$age<60]<-4
biomark5.5$age_group[biomark5.5$age>=60 & biomark5.5$age<65]<-5
biomark5.5$age_group[biomark5.5$age>=65 & biomark5.5$age<70]<-6
biomark5.5$age_group[biomark5.5$age>=70 & biomark5.5$age<75]<-7
biomark5.5$PG1_range[biomark5.5$PG1<20]<-1
biomark5.5$PG1_range[biomark5.5$PG1>=20 & biomark5.5$PG1<=70]<-2
biomark5.5$PG1_range[biomark5.5$PG1>=70 & biomark5.5$PG1<200]<-3
biomark5.5$PG1_range[biomark5.5$PG1>=200]<-4
biomark5.5$PG1_range<-factor(biomark5.5$PG1_range,levels = c(1,2,3,4),labels=c('PG<20','20<=PG<=70','70<PG<200','PG>=200'))
biomark5.5$PGR_range[biomark5.5$PGR>3]<-1
biomark5.5$PGR_range[biomark5.5$PGR<=3]<-2
biomark5.5$PGR_range[biomark5.5$PGR==3 & biomark5.5$PG1==200]<-1
biomark5.5$PGR_range[biomark5.5$PGR<=3]<-2
biomark5.5$PGR_range<-factor(biomark5.5$PGR_range,levels = c(1,2),labels=c('PGR>3','PGR<=3'))
d2<-biomark5.5%>%group_by(age_group,PG1_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))
ggplot(d2, aes(x = age_group, y = perc*100, fill =PG1_range)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=round(perc*100,2)),vjust=-2)
v1<-d2%>%filter(!is.na(age_group))%>%ggbarplot(x='age_group',y='perc',fill='PG1_range',xlab='年龄组',ylab='百分比',label='perc',lab.pos = 'out',lab.vjust=-0.5,lab.size = 3,position=position_dodge(0.7))+theme(legend.title=element_blank())+
  scale_x_discrete(labels=c('40-44','45-49','50-54','55-59','60-64','65-79','70-74'))




d3<-biomark5.5%>%filter(!is.na(PGR_range) & !is.na(PG1_range))%>%group_by(PG1_range,PGR_range) %>%
  summarise(count=n())
d3$perp<-round((d3$count/6022)*100,2)
#d3[8,]<-c('PG>=200','PGR<=3','0','0')
d3
v2<-ggbarplot(data=d3,x='PG1_range',y='perp',fill='PGR_range',label='perp',xlab='PG1范围',ylab='百分比',
          position = position_dodge(0.7))+theme(legend.title = element_blank())
v<-ggarrange(v1,v2,nrow=1)
ggarrange(p,v,nrow=2)
  #4.PG2分布
summary(biomark5.3$PG2)
gghistogram(biomark5.3,x='PG2',bins=30,rug=TRUE,add='median',add.params = list(color='red'))
#gghistogram(biomark5.3,x='PG2',bins=30,rug=TRUE,add='median',color='PG1_range',add.params = list(color='red'))
#PG1 and PGR
n1<-ggscatter(biomark5.3,x='PG1',y='PGR',alpha=0.5,add='reg.line',add.params = list(color='red'))+stat_cor(method='spearman')
n2<-ggscatter(biomark5.3,x='PG1',y='PGR',alpha=0.2,color='PG1_range',facet.by = 'PG1_range',add='reg.line',palette = 'jco',add.params = list(color='red'))+theme(legend.title = element_blank())+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+stat_cor(method='spearman')+theme(legend.position = 'none')
n3<-ggarrange(n1,i1,nrow=1)
ggarrange(n3,n2,nrow=2)
#5.PG2 and PGR
i2<-ggscatter(biomark5.3,x='PG2',y='PGR',alpha=0.2,add='reg.line')+stat_cor(method='spearman')
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
i1<-ggscatter(data=biomark5.3,x='PG1',y='PG2',alpha=0.5,add='reg.line',add.params = list(color='red'))+stat_cor(method='spearman')
ggarrange(i2,i1,nrow=1)


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
a<-biomark5.3_age.2%>%filter(!is.na(age_group))%>%group_by(PG,age_group)%>%summarise(median=median(value,na.rm=T),P25=quantile(value,0.25,na.rm = TRUE),P75=quantile(value,0.75,na.rm = TRUE))%>%
  ggplot(aes(x=age_group,y=median,color=PG))+geom_errorbar(aes(ymin=P25,ymax=P75),width=0.1)+geom_point()+geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7),labels=c('40-44','45-49','50-54','55-59','60-64','65-69','>=70'))+labs(x='age',y='')+
  facet_grid(PG~.,scales='free')+theme(legend.position = 'none')
## sex and PG1、PG2、PGR
biomark5.3$sex<-factor(biomark5.3$sex,levels = c(1,2),labels =c('男','女'))
m1<-ggbarplot(data=subset(biomark5.3,!is.na(sex)),x='sex',y='PG1',add='median_iqr',color='sex',palette = 'lancet',xlab='')+
  stat_compare_means(method='wilcox.test',label.x=1.5,label='p.signif',label.y=170)+theme(legend.position = 'none')
m2<-ggbarplot(data=subset(biomark5.3,!is.na(sex)),x='sex',y='PG2',add='median_iqr',color='sex',palette = 'lancet')+
  stat_compare_means(method='wilcox.test',label.x=1.5,label='p.signif',label.y=60)+theme(legend.position = 'none')
m3<-ggbarplot(data=subset(biomark5.3,!is.na(sex)),x='sex',y='PGR',add='median_iqr',color='sex',palette = 'lancet',xlab='')+
  stat_compare_means(method='wilcox.test',label.x=1.5,label='p.signif',label.y=17)+theme(legend.position = 'none')
m<-ggarrange(m1,m2,m3,nrow=1)
ggarrange(m,a,nrow=2)
#sex and age and PG1、PG2、PGR
biomark5.3_age.2$sex<-factor(biomark5.3_age.2$sex,levels=c(1,2),labels = c('男','女'))
biomark5.3_age.2%>%filter(!is.na(age_group))%>%group_by(PG,sex,age_group)%>%summarise(median=median(value,na.rm=T),P25=quantile(value,0.25,na.rm = TRUE),P75=quantile(value,0.75,na.rm = TRUE))%>%
  ggplot(aes(x=age_group,y=median,color=sex))+geom_errorbar(aes(ymin=P25,ymax=P75),width=0.1)+geom_point()+geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7),labels=c('40-44','45-49','50-54','55-59','60-64','65-69','>=70'))+labs(x='age',y='PG1')+
  facet_grid(PG~.,scales='free')+theme(legend.position = 'top')

#吸烟状态与PG水平的关系
biomark5.3$smoking<-factor(biomark5.3$smoking,levels=c(1,2,3),labels=c('Never','Current','Ago'))
#PG1
g1<-ggboxplot(data=subset(biomark5.3,!is.na(smoking)),x='smoking',y='PG1',add='jitter',add.params = list(size=0.4,jitter=0.2))+
         stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='..p.adj..')+
  stat_compare_means(label.y=260,label.x=0.7)
#PG2
g2<-ggboxplot(data=subset(biomark5.3,!is.na(smoking)),x='smoking',y='PG2',add='jitter',add.params = list(size=0.4,jitter=0.2))+
  stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='..p.adj..')+
  stat_compare_means(label.y=110,label.x=0.7)
#PGR
g3<-ggboxplot(data=subset(biomark5.3,!is.na(smoking)),x='smoking',y='PGR',add='jitter',add.params = list(size=0.4,jitter=0.2))+
  stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='..p.adj..')+
  stat_compare_means(label.y=30,label.x=0.7)
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
#疾病史与PG1、PG2、PGR的分布情况
biomark5.3_disea<-biomark5.3%>%select(id,disea14,disea15,disea16,disea17,disea18,disea19,disea20,disea22,disea23,PG1,PG2,PGR)
biomark5.3_disea_1<-melt(biomark5.3_disea,id.vars = c('id','PG1','PG2','PGR'),variable.name = 'disea')

biomark5.3_disea_1$value<-factor(biomark5.3_disea_1$value,levels = c(1,2),labels = c('否','是'))
head(biomark5.3_disea_1)
#PG1
ggboxplot(data=subset(biomark5.3_disea_1,!is.na(value)),x='value',y='PG1',alpha=0.4,facet.by = 'disea',add='jitter',add.params = list(size=0.4,jitter=0.2))
##------------------------------------------胃镜检查结果的描述性分析--------------------------------------
gastroscogy<-dbGetQuery(cnn,'SELECT source,id,name,gastroscogy pathlogy FROM gastroscogy')#19年胃镜检查结果
head(gastroscogy)
gastroscogy$recieve<-ifelse(gastroscogy$gastroscogy!='',1,0)
gastroscogy$recieve<-factor(gastroscogy$recieve,levels=c(1,0),labels=c('接受','拒绝'))
#检查情况
gastroscogy%>%group_by(source,recieve)%>%summarise(freq=n())%>%
ggbarplot(x='source',y='freq',fill='recieve',xlab='来源',ylab='频数',label='freq',lab.pos = 'in',lab.vjust =8)+
  theme(legend.position = 'right',legend.title = element_blank())
#胃镜检查结果分布轻量
gastroscogy$source<-factor(gastroscogy$source)
y2<-gastroscogy%>%filter(gastroscogy!='')%>%group_by(gastroscogy)%>%summarise(freq=n())%>%ggbarplot(x='gastroscogy',y='freq',
                            ylab='频数',xlab='胃镜检查结果',title='全部',sort.val='desc',x.text.angle=50,label='freq',lab.pos = 'out')
y3<-gastroscogy%>%filter(gastroscogy!='',source=='示范区')%>%group_by(gastroscogy)%>%summarise(freq=n())%>%ggbarplot(x='gastroscogy',y='freq',
                        ylab='频数',xlab='胃镜检查结果',title='示范区',sort.val='desc',x.text.angle=50,label='freq',lab.pos = 'out')
ggarrange(y2,y3)
ggarrange(y1,y23,nrow=2)
##PG与饮酒的关系
biomark5.2$alcohol<-factor(biomark5.2$alcohol,levels = c(1,2),labels=c('否','是'))
ggboxplot(data=subset(biomark5.2,!is.na(alcohol)),x='alcohol',y='PG1',add='jitter',palette='jco')+
  stat_compare_means(method='wilcox.test',label='p.signif',label.x=1.5,size=4)
#alcohol and ses
ggboxplot(data=subset(biomark5.2,!is.na(alcohol)),x='alcohol',y='PG1',add='jitter',palette='jco',facet.by = 'sex')+
  stat_compare_means(method='wilcox.test',label='p.signif',label.x=1.5,size=4)


##PG1<20的人群(严重萎缩的情况分布)
gastroscogy2<-gastroscogy%>%filter(source=='示范区')
gastroscogy_20<-inner_join(gastroscogy2,biomark5.2,by=c('id','name'))
summary(gastroscogy_20)
gastroscogy_20[,c('PG1','PG2','PGR')]<-apply(gastroscogy_20[,c('PG1','PG2','PGR')],2,str_func2)
nrow(gastroscogy_20%>%filter(PG1<20 & gastroscogy!=''))#98,55
nrow(gastroscogy_20%>%filter(PG1>=20 & PG1<=70 ))#54,27
nrow(gastroscogy_20%>%filter(PG1>70 & PG1<200))#25,12
nrow(gastroscogy_20%>%filter(PG1>=200))#29,14
table<-data.frame(PG=c('<20','20<=PG<70','70<PG<200','PG>=200'),
                  拒绝=c(43,27,13,15),
                  接受=c(55,27,12,14))
table.2<-melt(table,id.vars = 'PG',variable.name = 'variable')
head(table.2)
ggbarplot(data=table.2,x='PG',y='value',fill='variable',label='value',lab.pos = 'in',xlab='PG范围',ylab='频数')+
  theme(legend.title = element_blank())
gastroscogy_20$PG1_range[gastroscogy_20$PG1<20]<-1
gastroscogy_20$PG1_range[gastroscogy_20$PG1>=20 & gastroscogy_20$PG1<=70]<-2
gastroscogy_20$PG1_range[gastroscogy_20$PG1>=70 & gastroscogy_20$PG1<200]<-3
gastroscogy_20$PG1_range[gastroscogy_20$PG1>=200]<-4
gastroscogy_20$PG1_range<-factor(gastroscogy_20$PG1_range,levels = c(1,2,3,4),
                                 labels = c('PG<20','20<=PG<=70','70<PG<200','PG>=200'))
table(gastroscogy_20$PG1_range)

aaa<-gastroscogy_20%>%filter(gastroscogy!='')%>%group_by(PG1_range,gastroscogy)%>%
  summarise(freq=n())
head(aaa)
summary(aaa)
aaa1<-import('~/胃镜结果pg.xlsx')
s1<-ggbarplot(aaa1,x='gastroscogy',y='freq',fill='PG1_range',
  ylab='频数',xlab='胃镜检查结果',title='示范区',x.text.angle=50,sort.val = 'desc',
   sort.by.groups=TRUE,position = position_dodge(),label = 'freq',lab.pos = 'out')+
  theme(legend.title = element_blank())
ggarrange(y2,s1)
#6----------------------------------------------17+18+19基础肿瘤标志物高危分布情况------------------------------------------------
biomark6.1<-biomark%>%select(CEA,AFP,CA199,CA153,CA125)
biomark6.2<-data.frame(apply(biomark6.1,2,str_func2))
summary(biomark6.2)
risk<-function(x){
  x2<-factor(x,levels = c(0,1,2,3,4),labels=c('正常','超出截值1-2倍','超出截值2-3倍','超出截值3-4倍','超出截值4倍'))
  return(x2)
}
   biomark6.3<-within(biomark6.2,{
    CEA.risk<-vector()
    AFP.risk<-vector()
    CA199.risk<-vector()
    CA153.risk<-vector()
    CA125.risk<-vector()
    #CEA
    CEA.risk[CEA<=5]<-0
    CEA.risk[CEA>5 & CEA<=10]<-1
    CEA.risk[CEA>10 & CEA<=15]<-2
    CEA.risk[CEA>15 & CEA<=20]<-3
    CEA.risk[CEA>20]<-4
    #AFP
    AFP.risk[AFP<=7]<-0
    AFP.risk[AFP>7 & AFP<=14]<-1
    AFP.risk[AFP>14 & AFP<=21]<-2
    AFP.risk[AFP>21 & AFP<=28]<-3
    AFP.risk[AFP>28]<-4
    #CA199
    CA199.risk[CA199<=27]<-0
    CA199.risk[CA199>27 & CA199<=27*2]<-1
    CA199.risk[CA199>27*2 & CA199<=27*3]<-2
    CA199.risk[CA199>27*3 & CA199<=27*4]<-3
    CA199.risk[CA199>27*4]<-4
    #CA153
    CA153.risk[CA153<=25]<-0
    CA153.risk[CA153>25 & CA153<=50]<-1
    CA153.risk[CA153>50 & CA153<=75]<-2
    CA153.risk[CA153>75 & CA153<=100]<-3
    CA153.risk[CA153>100 ]<-4
    #CA125
    CA125.risk[CA125<=35]<-0
    CA125.risk[CA125>35 & CA125<=70]<-1
    CA125.risk[CA125>70 & CA125<=105]<-2
    CA125.risk[CA125>105 & CA125<=140]<-3
    CA125.risk[CA125>140]<-4
  })
biomark6.3[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')]<-apply(biomark6.3[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,risk)
table_risk<-as.data.frame(apply(biomark6.3[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,table))
table_risk1<-rbind(apply(table_risk,2,sum),table_risk)
table_prop<-list()
for(i in 1:5){
  table_prop[[i]]<-round(table_risk1[,i]/table_risk1[1,i],4)*100
}
table_prop1<-data.frame(t(do.call(rbind,table_prop)))
table<-list()
for(i in 1:5){
  table[[i]]<-paste(table_risk1[,i],'(',table_prop1[,i],'%)',sep='')
}
table2<-data.frame(t(do.call(rbind,table)))
table3<-cbind(c('完成人数','<1倍上限','1-2倍上限',
                '2-3倍上限','3-4倍上限','>4倍上限'),table2)
names(table3)<-c('指标','CEA','AFP','CA199','CA153(女性)','CA125(女性)')
ggtable<-ggtexttable(table3,rows=NULL,theme = ttheme('mBlue'))
##--------------------------------------------CEA,AFP.CA199----------------------------------------
#1 去除>号
biomark[,c('AFP','CA199','CEA')]<-apply(biomark[,c('AFP','CA199','CEA')],2,str_func2)
summary(biomark)
#2 筛出大于节点值3倍以上的人群
biomark7.1<-within(biomark,{
  CEA.risk<-vector()
  AFP.risk<-vector()
  CA199.risk<-vector()
  #CEA
  CEA.risk[CEA<=5]<-0
  CEA.risk[CEA>5 & CEA<=10]<-1
  CEA.risk[CEA>10 & CEA<=15]<-2
  CEA.risk[CEA>15 & CEA<=20]<-3
  CEA.risk[CEA>20]<-4
  #AFP
  AFP.risk[AFP<=7]<-0
  AFP.risk[AFP>7 & AFP<=14]<-1
  AFP.risk[AFP>14 & AFP<=21]<-2
  AFP.risk[AFP>21 & AFP<=28]<-3
  AFP.risk[AFP>28]<-4
  #CA199
  CA199.risk[CA199<=27]<-0
  CA199.risk[CA199>27 & CA199<=27*2]<-1
  CA199.risk[CA199>27*2 & CA199<=27*3]<-2
  CA199.risk[CA199>27*3 & CA199<=27*4]<-3
  CA199.risk[CA199>27*4]<-4

})
apply(biomark7.1[,c('AFP.risk','CA199.risk','CEA.risk')],2,table)
biomark7.2<-biomark7.1%>%filter(AFP.risk>=3 | CA199.risk>=3 | CEA.risk>=3)
biomark7.3<-left_join(biomark7.2,baseline,by=c('id','name'))
export(biomark7.3,'~/screening/肿瘤标志物高危17-19.xlsx')



