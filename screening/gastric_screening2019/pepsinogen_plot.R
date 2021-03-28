rm(list=ls())
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
library(rms)
library(ggsignif)
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
source('~/Rcode/screening/gastric_screening2020/data2020.R')
source('~/Rcode/statistics/two_y_axis.R')
mytheme<-theme(plot.title=element_text(hjust=0.5),
               axis.title=element_text(face="bold",size=14),
               axis.text=element_text(face="bold",size=16),
               axis.text.x  = element_text(face="bold",size=14),
               #panel.grid.major = element_line(colour=NA),
               #panel.grid.major.x = element_line(color='grey'),
               #panel.grid.major.y = element_line(color='grey'),
               panel.background = element_blank(),
               axis.line = element_line(color='grey'),
               #legend.title=element_text(face="bold",size=14),
               legend.title=element_blank(),
               legend.text = element_text(face='bold',size=13),
               legend.position = 'top',
               #strip.text.x =element_text(face='blod',color='red')
)
mytheme2<-theme(plot.title=element_text(hjust=0.5,face="bold"),
                axis.title=element_text(family="serif",size=14,face="bold"),
                axis.text=element_text(family="serif",size=14,face="bold"),
                panel.grid.major = element_line(colour=NA),
                panel.grid.minor = element_blank(),
                panel.background=element_rect(fill=NA),
                axis.line = element_line(color='black'),
                legend.title = element_text(family='serif',size=15,face='bold'),
                legend.text = element_text(family = 'serif',size=15,face='bold'),
                legend.position = 'top'
)
#性别和年龄的基本分别
make.table(dat=pepsinogen2020,
           strat        = "性别",
           cat.rmstat   = c(""),
           cat.varlist  = c('年龄分组2','年龄分组3'),
           cat.ptype    = c("chisq"))
pepsinogen2020%>%group_by(性别)%>%summarise(mean=mean(年龄))
pepsinogen2019%>%group_by(性别,年龄分组2)%>%filter%>%summarise(n=n())%>%ggplot()+
  geom_bar(aes(x=年龄分组2,y=n,fill=性别),stat='identity',position = 'dodge')+mytheme2+
  labs(x='Age groups',fill='Sex',y='N')

#问题1：PG本身发生何种变动？
#1.1---PG的基本分布
#PG1
PG1.normal<-pepsinogen2019%>%ggplot(aes(x=PG1))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 57.10,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019$PG1),sd=sd(pepsinogen2019$PG1)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+
  annotate('text',label='N=6009\nMEAN=64.02\nSD=32.8',x=150,y=0.015,size=4,color='black')
PG2.normal<-pepsinogen2019%>%ggplot(aes(x=PG2))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 9.80,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019$PG2),sd=sd(pepsinogen2019$PG2)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(0,6,10,15,20,50),limits = c(0,51),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+
  annotate('text',label='N=6009\nMEAN=11.99\nSD=7.89',x=40,y=0.072,size=4,color='black')+labs(y=" ")
PGR.normal<-pepsinogen2019%>%ggplot(aes(x=PGR))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 6.00,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019$PGR),sd=sd(pepsinogen2019$PGR)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(0,1,2,3,6,10,15,20),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+
  annotate('text',label='N=6009\nMEAN=6.126\nSD=2.49',x=18,y=0.15,size=4,color='black')+labs(y=" ")
PG1.normal | PG2.normal | PGR.normal





#基本分布情况
#PG1与PGR的累计分布图
#pg1.1<-pepsinogen%>%ggplot()+geom_histogram(aes(x=PG1),binwidth=1,fill='lightblue',color='black')+mytheme2+
#  scale_x_continuous(breaks=c(0,20,30,40,50,60,70,100,150,200),expand = c(0,0))+
#  scale_y_continuous(expand = c(0,0))
#
pg1.1<-pepsinogen2019%>%ggplot()+geom_histogram(aes(x=PG1),binwidth=10,fill='lightblue',color='black')+mytheme2+
  scale_x_continuous(breaks=c(0,20,30,40,50,60,70,100,150,200),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
pg1.1.1<-pepsinogen2019%>%ggplot()+geom_histogram(aes(x=PG1,y=..density..),binwidth=10,fill='lightblue',color='black')+mytheme2+
  scale_x_continuous(breaks=c(0,20,30,40,50,60,70,100,150,200),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
#
pg1.2<-pepsinogen2019%>%group_by(PG1)%>%summarise(n=n())%>%mutate(sumn=cumsum(n))%>%mutate(sumperc=round(sumn/sum(n),4)*100)%>%
  ggplot(aes(x=PG1))+geom_line(aes(y=sumperc),size=1)+mytheme+scale_x_continuous(breaks = c(0,20,30,40,50,70,100,120,150,180,200),expand = c(0,0))+scale_y_continuous(breaks = seq(0,100,5),expand = c(0,0))+
  labs(y='Cumulative percentage')
pg1.2.1<-pepsinogen2019%>%group_by(PG1)%>%summarise(n=n())%>%mutate(sumn=cumsum(n))%>%mutate(sumperc=round(sumn/sum(n),2))%>%
  ggplot(aes(x=PG1))+geom_line(aes(y=sumperc),size=1)+mytheme+scale_x_continuous(breaks = c(0,20,30,40,50,70,100,120,150,180,200),expand = c(0,0))+scale_y_continuous(breaks = seq(0,100,5),expand = c(0,0))+
  labs(y='Cumulative percentage')

ggplot2.two_y_axis(pg1.1,pg1.2)
ggplot2.two_y_axis(pg1.1.1,pg1.2.1)

#PGR
perc<-pepsinogen2019%>%filter(PGR!=30)%>%group_by(PGR)%>%summarise(n=n())%>%mutate(sumperc=round(n/sum(n),4)*100)
print(perc,n=158)
pgr.1<-pepsinogen2019%>%filter(PGR!=30)%>%ggplot()+geom_histogram(aes(x=PGR),binwidth=0.5,fill='lightblue',color='black')+mytheme2+
  scale_x_continuous(breaks=c(0,2,3,4,5,6,7,10,15,20,30),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
pgr.2<-pepsinogen2019%>%filter(PGR!=30)%>%group_by(PGR)%>%summarise(n=n())%>%mutate(sumn=cumsum(n))%>%mutate(sumperc=round(sumn/sum(n),4)*100)%>%
  ggplot(aes(x=PGR))+geom_line(aes(y=sumperc),size=1)+mytheme+scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,14,16,18,20,25,30),expand = c(0,0))+
  scale_y_continuous(breaks=seq(0,100,10),expand = c(0,0))+
  labs(y='Cumulative percentage')
ggplot2.two_y_axis(pgr.1,pgr.2)

#PG2
pg2.1<-pepsinogen2019%>%filter(PGR!=100)%>%ggplot()+geom_histogram(aes(x=PG2),binwidth=5,fill='lightblue',color='black')+mytheme2+
  scale_x_continuous(breaks=c(0,5,10,12,15,20,25,30,35,40,50,60),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
pg2.2<-pepsinogen2019%>%group_by(PG2)%>%summarise(n=n())%>%mutate(sumn=cumsum(n))%>%mutate(sumperc=round(sumn/sum(n),4)*100)%>%
  ggplot(aes(x=PG2))+geom_line(aes(y=sumperc),size=1)+mytheme+scale_x_continuous(breaks = c(0,2,4,6,8,10,14,18,20,25,30,50,75,100),expand = c(0,0))+
  scale_y_continuous(breaks=seq(0,100,10),expand = c(0,0))+
  labs(y='Cumulative percentage')

ggplot2.two_y_axis(pg2.1,pg2.2)

#异常值情况
pepsinogen2019%>%filter(PG1==200)%>%summary()
##PG1 AND PGR
#d<-data.frame(x=c(0,20,30,50,70),y=c(0,3,3,3,3),group=c('a','a','b','b','c'))
p1<-pepsinogen2019%>%ggplot(aes(x=PG1,y=PGR))+geom_point(alpha=0.3)+mytheme+
  scale_x_continuous(breaks = c(0,20,25,30,50,70,100,150,200))+scale_y_continuous(breaks=c(0,2,3,6,10,20,30))
ggMarginal(p1,type='histogram')



#PG1  and PG2
p2<-pepsinogen2019%>%filter(PG1!=200)%>%ggplot(aes(x=PG1,y=PG2))+geom_point(alpha=0.3)+mytheme+
  scale_x_continuous(breaks = c(0,20,25,30,50,70,100,150,200))#+scale_y_continuous(breaks=c(0,2,3,6,10,20,30))
#PG2 AND PGR
p3<-pepsinogen2019%>%filter(PG1!=200)%>%ggplot(aes(x=PG2,y=PGR))+geom_point(alpha=0.3)+mytheme#+
#scale_x_continuous(breaks = c(0,20,25,30,50,70,100,150,200))#+scale_y_continuous(breaks=c(0,2,3,6,10,20,30))
p1 / p2 / p3
##年龄分布1
pg1.age<-pepsinogen2019%>%filter(年龄>=40,年龄<=74)%>%group_by(年龄)%>%
  summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75),median.pg2=median(PG2),Q1.pg2=quantile(PG2,0.25),Q3.pg2=quantile(PG2,0.75))%>%
  ggplot(aes(x=年龄))+geom_ribbon(aes(ymin=Q1,ymax=Q3), fill="#6699CC", alpha=.4)+mytheme+
  geom_line(aes(y=median,color="PGI"),size=1)+
  geom_ribbon(aes(ymin=Q1.pg2,ymax=Q3.pg2),fill="#FFCC00",alpha=0.4)+
  geom_line(aes(y=median.pg2,colour="PGII"),size=1)+
  labs(x = "Age",y='Median(Q1-Q3)',colour='PG')+scale_x_continuous(breaks=seq(42,74,5))+
  scale_y_log10()+scale_color_manual(values=c("PGI"="#003366","PGII"="#FFCC00"))
pg2.age<-pepsinogen2019%>%filter(年龄>=40,年龄<=74,!is.na(PGR))%>%group_by(年龄)%>%
  summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_ribbon(aes(x=年龄,ymin=Q1,ymax=Q3), fill="#FF9933", alpha=.6)+
  mytheme+geom_line(aes(x=年龄,y=median,colour='PGR'),size=1)+
  labs(x = "Age",y='',colour='')+scale_x_continuous(breaks=seq(42,74,5))+scale_color_manual(values=c('PGR'='#FF9933'))
(pg1.age | pg2.age) + plot_layout(guides='collect') & theme(legend.position = 'top')
##PG分组后在年龄组中的分布
pepsinogen2019%>%filter(!is.na(年龄分组3))%>%group_by(年龄分组3,PG_pos4) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%mutate(lab=paste0(count,"\n","(",perc,'%)'))%>%
  ggplot(aes(x = 年龄分组3, y = perc, fill =PG_pos4)) +
  geom_bar(stat="identity", width = 0.7,position = 'dodge') +
  mytheme+geom_text(aes(label=lab,fontface='bold',vjust=0.1,hjust=0.5),position = position_dodge(width = 0.8))+
  labs(x='Age',y='Percent',fill='PG_pos')+scale_y_continuous(limits=c(0,105))+
  scale_fill_discrete(labels=c('Normal','Mild','Severe'))
##PG与年龄分另一种画法
pg1.age<-pepsinogen2019%>%group_by(年龄分组2)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75),
                                             median2=median(PG2),Q12=quantile(PG2,0.25),Q32=quantile(PG2,0.75),
                                             medianr=median(PGR),Q1r=quantile(PGR,0.25),Q3r=quantile(PGR,0.75)
                                             )%>%filter(!is.na(年龄分组2))%>%
 ggplot()+geom_errorbar(aes(x=年龄分组2,ymin=Q1,ymax=Q3),width=0.1,size=0.5,color='blue')+geom_point(aes(x=年龄分组2,y=median),size=2,color='blue')+
  geom_line(aes(x=年龄分组2,y=median,group=1,color='PGI'))+
  geom_errorbar(aes(x=年龄分组2,ymin=Q12,ymax=Q32),width=0.1,size=0.5,color='#FF6600')+geom_point(aes(x=年龄分组2,y=median2),size=2,color='#FF6600')+
  geom_line(aes(x=年龄分组2,y=median2,group=1,color='PGII'))+scale_y_log10()+mytheme+
  scale_color_manual(values=c("PGI"="blue","PGII"="#FF6600"))+labs(x='',fill=' ',y="Median(Q1-Q3)")+
  guides(shape=guide_legend(override.aes=list(size=10)))


pg2.age<-pepsinogen2019%>%group_by(年龄分组2)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%filter(!is.na(年龄分组2))%>%
  ggplot()+geom_errorbar(aes(x=年龄分组2,ymin=Q1,ymax=Q3),width=0.1,size=0.5,color='#FF6600')+geom_point(aes(x=年龄分组2,y=median),size=2,color='#FF6600')+
  geom_line(aes(x=年龄分组2,y=median,group=1,color='PGR'))+scale_y_log10()+mytheme+
  scale_color_manual(values=c('PGR'='#FF6600'))+labs(x='',fill='',y="Median(Q1-Q3)")+
  guides(shape=guide_legend(override.aes=list(size=5)))
(pg1.age | pg2.age) + plot_layout(guides='collect') & theme(legend.position = 'top')


#性别分布
PG1.sex<-pepsinogen2019%>%group_by(性别)%>%summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGI',x='')+
  geom_line(aes(x=as.numeric(性别),y=c(87,87)),size=0.8)+geom_line(aes(x=c(1,1),y=c(86,87)),size=0.8)+geom_line(aes(x=c(2,2),y=c(86,87)),size=0.8)+
  annotate('text',label="***",x=1.5,y=88,size=10,color='black')

PG2.sex<-pepsinogen2019%>%group_by(性别)%>%summarise(median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGII',x=' ')+
  geom_line(aes(x=as.numeric(性别),y=c(16,16)),size=0.8)+geom_line(aes(x=c(1,1),y=c(15.7,16)),size=0.8)+geom_line(aes(x=c(2,2),y=c(15.7,16)),size=0.8)+
  annotate('text',label="***",x=1.5,y=17,size=10,color='black')


PGR.sex<-pepsinogen2019%>%group_by(性别)%>%summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGI/II ratio',x=' ')+
  geom_line(aes(x=as.numeric(性别),y=c(8.2,8.2)),size=0.8)+geom_line(aes(x=c(1,1),y=c(8.1,8.2)),size=0.8)+geom_line(aes(x=c(2,2),y=c(8.1,8.2)),size=0.8)+
  annotate('text',label="***",x=1.5,y=8.3,size=10,color='black')

(PG1.sex | PG2.sex | PGR.sex) +plot_layout(guides='collect') & theme(legend.position = 'top')
#



