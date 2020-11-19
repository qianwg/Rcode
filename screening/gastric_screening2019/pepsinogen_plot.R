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

##BMI and PG1
#BMI分布
bmi.freq<-pepsinogen2019%>%ggplot(aes(x=BMI,y=..density..))+geom_histogram(binwidth = 1,fill='lightblue',color='black')+mytheme+
  stat_overlay_normal_density(color = "red", linetype = "dashed")
pg1.1<-pepsinogen2019%>%filter(PG1!=200)%>%ggplot(aes(x=BMI,y=PG1))+geom_point(alpha=0.3)+geom_smooth(method='loess')+mytheme
bmi.freq | pg1.1  
bmi_pg1.2<-pepsinogen2019%>%filter(PG1!=200)%>%
  ggbetweenstats(
    x = BMI_group,
    y =PG1,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  )  

bmi_pg1.3<-pepsinogen2019%>%filter(PG1!=200)%>%
  ggbetweenstats(
    x = BMI_group2,
    y =PG1,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  )  
bmi_pg1.1<-pepsinogen2019%>%filter(PG1!=200)%>%
  ggbetweenstats(
    x = BMI_risk3,
    y =PG1,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  ) 
bmi_pg1.3 | bmi_pg1.2 | bmi_pg1.1
pg1.3<-pepsinogen2019%>%filter(PG1!=200)%>%ggline(x='BMI_risk3',y='PG1',add='median_iqr')
pg1.2<-pepsinogen2019%>%filter(PG1!=200)%>%group_by(BMI)%>%summarise(n=n(),median=median(PG1,na.rm = TRUE))%>%
  filter(n>100)%>%ggplot(aes(x=BMI,y=median))+geom_point()+geom_line()+mytheme#+scale_x_continuous(
#    breaks=c(18.5,19,20,21,22,23,24,25,26,27,28,30,40),labels=c('偏瘦','19','20','21','22','23','正常','25','26','27','超重','肥胖','中度肥胖')
#  )
pg1.1 | pg1.2
#BMI and PG2
pg2.1<-pepsinogen2019%>%filter(PG1!=200)%>%ggplot(aes(x=BMI,y=PG2))+geom_point(alpha=0.3)+geom_smooth(method='loess')+mytheme
pepsinogen2019%>%filter(PG1!=200)%>%ggplot(aes(x=BMI_risk3,y=PG2))+geom_boxplot()+mytheme2
pg2.2<-pepsinogen2019%>%filter(PG1!=200)%>%transmute(BMI=round(BMI,0),PG2)%>%group_by(BMI)%>%summarise(n=n(),median=median(PG2,na.rm = TRUE))%>%
  filter(n>100)%>%ggplot(aes(x=BMI,y=median))+geom_point()+geom_line()+mytheme+scale_x_continuous(
    breaks=c(18.5,19,20,21,22,23,24,25,26,27,28,30,40),labels=c('偏瘦','19','20','21','22','23','正常','25','26','27','超重','肥胖','中度肥胖')
  )
pg2.3<-pepsinogen2019%>%filter(PG1!=200)

bmi.freq | pg1.1  | pg2.1

##分类数据
bmi_pg2.2<-pepsinogen2019%>%filter(PG1!=200)%>%
  ggbetweenstats(
    x = BMI_group,
    y =PG2,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  )  

bmi_pg2.3<-pepsinogen2019%>%filter(PG1!=200)%>%
  ggbetweenstats(
    x = BMI_group2,
    y =PG2,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  )  
bmi_pg2.1<-pepsinogen2019%>%filter(PG1!=200)%>%
  ggbetweenstats(
    x = BMI_risk3,
    y =PG2,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  ) 
bmi_pg2.3 | bmi_pg2.2 | bmi_pg2.1


pepsinogen2019%>%transmute(BMI_group=factor(case_when(
  BMI<22.49 ~ 1,
  BMI>=22.49 & BMI<24.61 ~ 2,
  BMI>=24.61 & BMI<26.89 ~ 3,
  BMI>26.89 ~ 4
),levels = c(1,2,3,4),labels=c('Q1','Q2','Q3','Q4')),PG1,PG2)%>%ggboxplot(x='BMI_group',y='PG1')

##BMI and OR
dd <- datadist(pepsinogen2019) 
options(datadist='dd')
fit<- lrm(PG_pos ~ rcs(BMI,4),data=pepsinogen2019) 
#dd$limits$age[2] <- 50 ###这里是设置参考点，也就是HR为1的点，常见的为中位数或者临床有意义的点 
#fit=update(fit)
RR<-Predict(fit, BMI,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="BMI", y="RR (95%CI)") 
anova(fit)
##吸烟和PG
pepsinogen2019%>%pivot_longer(cols=c("PG1","PG2","PGR"),names_to = 'PG',values_to = 'value')%>%
   group_by(PG,吸烟1)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))

pepsinogen2019%>%pivot_longer(cols=c("PG1","PG2","PGR"),names_to = 'PG',values_to = 'value')%>%
  group_by(PG,吸烟2)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))%>%
  ggplot(aes(x=PG,y=median,fill=吸烟2))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='smoking',x=' ')+
  annotate('text',label="***",x=1,y=88.2,size=10,color='black')+
  annotate('text',label="***",x=2,y=17.1,size=10,color='black')+
  annotate('text',label="***",x=3,y=9.0,size=10,color='black')+
  scale_fill_discrete(labels=c('Never','smoker'))


##针对吸烟或过去吸烟的人群
pepsinogen2019%>%group_by(吸烟3)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
pepsinogen2019%>%group_by(吸烟3)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))
pepsinogen2019%>%group_by(吸烟3)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))

#包年
pepsinogen2019%>%filter(包年<100)%>%ggplot(aes(x=包年,y=PG1))+geom_point()+geom_smooth(method='lm')


#吸烟年数
pepsinogen2019%>%filter(吸烟1=="目前吸烟")%>%ggplot(aes(x=smkyrs,y=PG1))+geom_point()+geom_smooth(method='lm')
pepsinogen2019%>%filter(吸烟1=="目前吸烟")%>%ggplot(aes(x=smkyrs,y=PG2))+geom_point()+geom_smooth(method='lm')
pepsinogen2019%>%filter(吸烟1=="目前吸烟")%>%ggplot(aes(x=smkyrs,y=PGR))+geom_point()+geom_smooth(method='lm')
table(pepsinogen2019$吸烟年数分组)
pepsinogen2019%>%group_by(吸烟年数分组)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
pepsinogen2019%>%group_by(吸烟年数分组)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))
pepsinogen2019%>%group_by(吸烟年数分组)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))

pepsinogen2019%>%filter(!is.na(吸烟年数分组))%>%ggline(x='吸烟年数分组2',y='PG1',add='median_iqr',error.plot = 'pointrange')
pepsinogen2019%>%filter(!is.na(吸烟年数分组))%>%ggline(x='吸烟年数分组2',y='PG2',add='median_iqr',error.plot = 'pointrange')
pepsinogen2019%>%filter(!is.na(吸烟年数分组))%>%ggline(x='吸烟年数分组2',y='PGR',add='median_iqr',error.plot = 'pointrange')

pepsinogen2019%>%pivot_longer(cols=c('PG1','PG2','PGR'),names_to = 'PG',values_to = 'value')%>%
  ggplot()
#每日吸烟指数
pepsinogen2019%>%ggplot(aes(x=cpd,y=PG1))+geom_point()+geom_smooth(method='lm')


###饮酒与PG水平
pepsinogen2019%>%pivot_longer(cols=c("PG1","PG2","PGR"),names_to = 'PG',values_to = 'value')%>%
  group_by(PG,饮酒)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))

pepsinogen2019%>%pivot_longer(cols=c("PG1","PG2","PGR"),names_to = 'PG',values_to = 'value')%>%
  group_by(PG,饮酒)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))%>%
  ggplot(aes(x=PG,y=median,fill=饮酒))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='alcohol',x=' ')+
  annotate('text',label="***",x=1,y=81.5,size=9,color='black')+
  annotate('text',label="***",x=2,y=16.1,size=9,color='black')+
  annotate('text',label="NS",x=3,y=12.0,size=4,color='black')+
  scale_fill_discrete(labels=c('Occasional','Regular'))+scale_y_continuous(limits=c(0,83))



###根据PG与年龄对人群进行划分 
pepsinogen2019%>%ggplot(aes(x=PG1,y=PGR,color=年龄分组5))+geom_point(alpha=0.2)+mytheme+
  scale_x_continuous(breaks = c(0,20,25,30,50,70,100,150,200))+scale_y_continuous(breaks=c(0,2,3,6,10,20,30))+
  geom_hline(yintercept = 3)+geom_vline(xintercept = c(30,70))



##对于所有有胃镜/病理结果的人，其PG的分布与病变的分布曲线关系
gastroscopy<-import('~/data/示范区胃镜结果--2020-8-17.xlsx')
match<-left_join(gastroscopy,pepsinogen2019,by='ID')
table(match$type)
match$type<-factor(match$type,levels=1:9,labels=c('正常','消化性溃疡','胃息肉','胃切除术','慢性胃炎','萎缩性胃炎','肠上皮化生','不典型增生','胃癌'))
match%>%filter%>%filter(!is.na(type))%>%group_by(type)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
##PG1
pg1_match<-match%>%group_by(type)%>%summarise(n=n(),median=median(PG1,na.rm = TRUE),Q1=quantile(PG1,na.rm = TRUE,0.25),Q3=quantile(PG1,na.rm = TRUE,0.75),
                                              median.pg2=median(PG2,na.rm = TRUE),Q1.pg2=quantile(PG2,na.rm = TRUE,0.25),Q3.pg2=quantile(PG2,na.rm = TRUE,0.75))%>%
  filter(type!='正常' & type!='胃息肉' & !is.na(type) & type!='胃切除术')%>%
  ggplot(aes(x=type,y=median))+geom_point(color='red')+scale_x_discrete(limits=c('慢性胃炎','消化性溃疡','萎缩性胃炎','肠上皮化生','不典型增生','胃癌'),
                                                                        labels=c('慢性胃炎\n(N=155)','消化性溃疡\n(N=4)','萎缩性胃炎\n(N=12)','肠上皮化生\n(N=9)','不典型增生\n(N=13)','胃癌\n(N=3)'))+
  mytheme+labs(x='',y='Median(Q-Q3)')+geom_point(aes(x=type,y=Q1),color='blue')+geom_point(aes(x=type,y=Q3),color='blue')+
  stat_summary(fun.y=median,geom="line",lwd=1,aes(group=1),color='blue')+geom_pointrange(aes(ymin=Q1,ymax=Q3),color='blue')+
  scale_y_continuous(breaks=c(0,30,50,70,100))+geom_hline(aes(yintercept=30),linetype='dashed',color='red')

#PG2
pg2_match<-match%>%group_by(type)%>%summarise(n=n(),median=median(PG2,na.rm = TRUE),Q1=quantile(PG2,na.rm = TRUE,0.25),Q3=quantile(PG2,na.rm = TRUE,0.75))%>%
  filter(type!='正常' & type!='胃息肉' & !is.na(type) & type!='胃切除术')%>%
  ggplot(aes(x=type,y=median))+geom_point(color='red')+scale_x_discrete(limits=c('慢性胃炎','消化性溃疡','萎缩性胃炎','肠上皮化生','不典型增生','胃癌'),
                                                                        labels=c('慢性胃炎\n(N=155)','消化性溃疡\n(N=4)','萎缩性胃炎\n(N=12)','肠上皮化生\n(N=9)','不典型增生\n(N=13)','胃癌\n(N=3)'))+
  mytheme+labs(x='',y='Median(Q-Q3)')+geom_point(aes(x=type,y=Q1),color='orange')+geom_point(aes(x=type,y=Q3),color='orange')+
  stat_summary(fun.y=median,geom="line",lwd=1,aes(group=1),color='orange')+geom_pointrange(aes(ymin=Q1,ymax=Q3),color='orange')+scale_y_continuous(breaks=c(0,5,10,15,20,30))
  
##PGR
match%>%group_by(type)%>%summarise(n=n(),median=median(PGR,na.rm = TRUE),Q1=quantile(PGR,na.rm = TRUE,0.25),Q3=quantile(PGR,na.rm = TRUE,0.75))%>%
  filter(type!='正常' & type!='胃息肉' & !is.na(type) & type!='胃切除术')%>%
  ggplot(aes(x=type,y=median))+geom_point(color='#FF00FF')+scale_x_discrete(limits=c('慢性胃炎','消化性溃疡','萎缩性胃炎','肠上皮化生','不典型增生','胃癌'),
                                                                        labels=c('慢性胃炎\n(N=155)','消化性溃疡\n(N=4)','萎缩性胃炎\n(N=12)','肠上皮化生\n(N=9)','不典型增生\n(N=13)','胃癌\n(N=3)'))+
  mytheme+labs(x='',y='Median(Q-Q3)')+geom_point(aes(x=type,y=Q1),color='#FF00FF')+geom_point(aes(x=type,y=Q3),color='#FF00FF')+
  stat_summary(fun.y=median,geom="line",lwd=1,aes(group=1),color='#FF00FF')+geom_pointrange(aes(ymin=Q1,ymax=Q3),color='#FF00FF')+scale_y_continuous(breaks=c(2,3,4,6))+
  geom_hline(aes(yintercept=3),linetype='dashed',color='red')
(pg1_match / pg2_match)




w###BMI与PG
pepsinogen2019%>%group_by(BMI_group)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
pepsinogen2019%>%pivot_longer(cols=c("PG1","PG2","PGR"),names_to = 'PG',values_to = 'value')%>%
  group_by(PG,BMI_group)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))%>%
  ggplot(aes(x=PG,y=median,fill=BMI_group))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='BMI_group',x=' ')#+
  annotate('text',label="***",x=1,y=88.2,size=10,color='black')+
  #annotate('text',label="***",x=2,y=17.1,size=10,color='black')+
  #annotate('text',label="***",x=3,y=9.0,size=10,color='black')+
  #scale_fill_discrete(labels=c('Never','Current','Ever'))
pepsinogen2019%>%ggplot(aes(x=BMI_group,y=PG1))+geom_boxplot()+stat_compare_means(
  comparisons = list(c(1,2),c(1,3),c(2,3))
)#正常 vs 肥胖=0.029 ; 超重 vs 肥胖= 0.0056
pepsinogen2019%>%ggplot(aes(x=BMI_group,y=PG2))+geom_boxplot()+stat_compare_means(
  comparisons = list(c(1,2),c(1,3),c(2,3))
)#肥胖 vs 正常=0.02 ; 超重 vs 肥胖= 0.0074
pepsinogen2019%>%ggplot(aes(x=BMI_group,y=PGR))+geom_boxplot()+stat_compare_means(
  comparisons = list(c(1,2),c(1,3),c(2,3))
)

##慢性病史
pepsinogen2019%>%pivot_longer(cols=c('PG1','PG2',"PGR"),names_to = 'PG',values_to = 'value')%>%
  group_by(PG,糖尿病)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))%>%
  ggplot(aes(x=PG,y=median,fill=糖尿病))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='糖尿病',x=' ')#+
  #annotate('text',label="***",x=1,y=88.2,size=10,color='black')+
  #annotate('text',label="***",x=2,y=17.1,size=10,color='black')+
  #annotate('text',label="***",x=3,y=9.0,size=10,color='black')+
  #scale_fill_discrete(labels=c('Never','Current','Ever'))
  


##
pepsinogen2019%>%group_by(吸烟年数分组2)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75),
                                             median2=median(PG2),Q12=quantile(PG2,0.25),Q32=quantile(PG2,0.75),
                                             medianr=median(PGR),Q1r=quantile(PGR,0.25),Q3r=quantile(PGR,0.75)
)%>%filter(!is.na(吸烟年数分组2))%>%
  ggplot()+geom_errorbar(aes(x=吸烟年数分组2,ymin=Q1,ymax=Q3),width=0.1,size=0.5,color='blue')+geom_point(aes(x=吸烟年数分组2,y=median),size=2,color='blue')+
  geom_line(aes(x=吸烟年数分组2,y=median,group=1,color='PGI'))+
  geom_errorbar(aes(x=吸烟年数分组2,ymin=Q12,ymax=Q32),width=0.1,size=0.5,color='#FF6600')+geom_point(aes(x=吸烟年数分组2,y=median2),size=2,color='#FF6600')+
  geom_line(aes(x=吸烟年数分组2,y=median2,group=1,color='PGII'))+scale_y_log10()+mytheme+
  geom_errorbar(aes(x=吸烟年数分组2,ymin=Q1r,ymax=Q3r),width=0.1,size=0.5,color='red')+geom_point(aes(x=吸烟年数分组2,y=medianr),size=2,color='red')+
  geom_line(aes(x=吸烟年数分组2,y=medianr,group=1,color='PGR'))+
  scale_color_manual(values=c("PGI"="blue","PGII"="#FF6600",'PGR'='red'))+labs(x='Years',fill=' ',y="Median(Q1-Q3)")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6),labels=c('<10','10-19','20-29','30-39','40-49','>=50'))
  
  
  guides(shape=guide_legend(override.aes=list(size=10)))


###汇总
summary(lm(PG1~年龄,data=pepsinogen2019))



