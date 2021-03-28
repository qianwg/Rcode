rm(list=ls())
library(readr)
library(rio)
library(tidyverse)
library(survival)
library(survminer)
library(forestmodel)
library(openxlsx)
source('~/Rcode/statistics/two_y_axis.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
source('~/Rcode/statistics/linear_regression.R')
mytheme<-theme(plot.title=element_text(hjust=0.5),
  axis.title=element_text(family="serif",size=12,face="bold"),
  axis.text=element_text(family="serif",size=12,face="bold"),
  panel.grid.major = element_line(colour=NA),
  panel.grid.minor = element_blank(),
  panel.background=element_rect(fill=NA),
  axis.line = element_line(color='black'),
  legend.title = element_text(family='serif',size=12),
  legend.text = element_text(family = 'serif',size=10),
  legend.key = element_blank(),
  #legend.background = element_rect(colour = 'black')
  
)
data<-import('~/data/医院PG.xlsx')

###PLot1
pg1.1<-data%>%ggplot()+geom_histogram(aes(x=PG1),binwidth=10,fill='lightblue',color='black')+mytheme+
  scale_x_continuous(breaks=c(0,20,30,40,50,60,70,100,150,200),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
pg1.1.1<-data%>%ggplot()+geom_histogram(aes(x=PG1,y=..density..),binwidth=10,fill='lightblue',color='black')+mytheme+
  scale_x_continuous(breaks=c(0,20,30,40,50,60,70,100,150,200),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
#
pg1.2<-data%>%group_by(PG1)%>%summarise(n=n())%>%mutate(sumn=cumsum(n))%>%mutate(sumperc=round(sumn/sum(n),4)*100)%>%
  ggplot(aes(x=PG1))+geom_line(aes(y=sumperc),size=1)+mytheme+scale_x_continuous(breaks = c(0,20,30,40,50,70,100,120,150,180,200),expand = c(0,0))+scale_y_continuous(breaks = seq(0,100,5),expand = c(0,0))+
  labs(y='Cumulative percentage')
pg1.2.1<-data%>%group_by(PG1)%>%summarise(n=n())%>%mutate(sumn=cumsum(n))%>%mutate(sumperc=round(sumn/sum(n),2))%>%
  ggplot(aes(x=PG1))+geom_line(aes(y=sumperc),size=1)+mytheme+scale_x_continuous(breaks = c(0,20,30,40,50,70,100,120,150,180,200),expand = c(0,0))+scale_y_continuous(breaks = seq(0,100,5),expand = c(0,0))+
  labs(y='Cumulative percentage')

ggplot2.two_y_axis(pg1.1,pg1.2)
ggplot2.two_y_axis(pg1.1.1,pg1.2.1)


##
##PG1
pg1<-data%>%group_by(第1次病理情况)%>%summarise(n=n(),median=median(PG1,na.rm = TRUE),Q1=quantile(PG1,na.rm = TRUE,0.25),Q3=quantile(PG1,na.rm = TRUE,0.75),
                                              median.pg2=median(PG2,na.rm = TRUE),Q1.pg2=quantile(PG2,na.rm = TRUE,0.25),Q3.pg2=quantile(PG2,na.rm = TRUE,0.75))%>%
   ggplot(aes(x=第1次病理情况,y=median))+geom_point(color='red')+mytheme+labs(x='',y='Median(Q-Q3)')+geom_point(aes(x=第1次病理情况,y=Q1),color='blue')+geom_point(aes(x=第1次病理情况,y=Q3),color='blue')+
  stat_summary(fun.y=median,geom="line",lwd=1,aes(group=1),color='blue')+geom_pointrange(aes(ymin=Q1,ymax=Q3),color='blue')+labs(x="PGI level")+
  scale_x_discrete(limits=c('慢性胃炎','肠上皮化生','异型增生','胃癌'),labels=c('慢性胃炎\n(N=46)','肠上皮化生\n(N=17)','不典型增生\n(N=31)','胃癌\n(N=316)'))+
  geom_hline(aes(yintercept=70),color='blue')

#PG2
pg2<-data%>%group_by(第1次病理情况)%>%summarise(n=n(),median=median(PG2,na.rm = TRUE),Q1=quantile(PG2,na.rm = TRUE,0.25),Q3=quantile(PG2,na.rm = TRUE,0.75))%>%
  ggplot(aes(x=第1次病理情况,y=median))+geom_point(color='red')+scale_x_discrete(limits=c('慢性胃炎','肠上皮化生','异型增生','胃癌'),
                                                                        labels=c('慢性胃炎\n(N=46)','肠上皮化生\n(N=17)','不典型增生\n(N=31)','胃癌\n(N=316)'))+
  mytheme+labs(x='PGII水平',y='Median(Q-Q3)')+geom_point(aes(x=第1次病理情况,y=Q1),color='orange')+geom_point(aes(x=第1次病理情况,y=Q3),color='orange')+
  stat_summary(fun.y=median,geom="line",lwd=1,aes(group=1),color='orange')+geom_pointrange(aes(ymin=Q1,ymax=Q3),color='orange')+scale_y_continuous(limits=c(0,30))

pgr<-data%>%mutate(PGR=round(PG1/PG2,2))%>%group_by(第1次病理情况)%>%summarise(n=n(),median=median(PGR,na.rm = TRUE),Q1=quantile(PGR,na.rm = TRUE,0.25),Q3=quantile(PGR,na.rm = TRUE,0.75))%>%
  ggplot(aes(x=第1次病理情况,y=median))+geom_point(color='red')+scale_x_discrete(limits=c('慢性胃炎','肠上皮化生','异型增生','胃癌'),
                                                                           labels=c('慢性胃炎\n(N=46)','肠上皮化生\n(N=17)','不典型增生\n(N=31)','胃癌\n(N=316)'))+
  mytheme+labs(x='PGI/II ratio',y='Median(Q-Q3)')+geom_point(aes(x=第1次病理情况,y=Q1),color='red')+geom_point(aes(x=第1次病理情况,y=Q3),color='red')+
  stat_summary(fun.y=median,geom="line",lwd=1,aes(group=1),color='red')+geom_pointrange(aes(ymin=Q1,ymax=Q3),color='red')+scale_y_continuous(limits=c(0,30))+
  geom_hline(aes(yintercept=3),color='blue')
(pg1 / pg2) | pgr



#####
data<-import('~/data/hospital/医院PG.xlsx')
data<-data%>%filter(Age>39)%>%mutate(PGR=round(PG1/PG2,2),性别=factor(ifelse(性别=="男",1,2),levels=c(1,2),labels=c('男','女')),
                     年龄分组=factor(ifelse(Age<=49,0,ifelse(Age>60,2,1)),levels=c(0,1,2),labels=c('<50','50-60','>60')),
                    age_risk2=case_when(
                      between(Age,40,44) ~ 1,
                      between(Age,45,49) ~ 2,
                      between(Age,50,54) ~ 3,
                      between(Age,55,59) ~ 4,
                      between(Age,60,64) ~ 5,
                      between(Age,65,69) ~ 6,
                      Age>=70 ~ 7
                    ),
                    年龄分组2=factor(age_risk2,levels=seq(7),labels=c('40-44','45-49','50-54','55-59',
                                                                      '60-64','65-69','>=70')),
                    PG1_range3=case_when(
                      PG1<=30 ~ 1,
                      PG1>30 & PG1<=50.0 ~ 2,
                      PG1>50.0 & PG1<=70.0 ~ 3,
                      PG1>70 ~ 4
                    ),
                    PG1_range3=factor(PG1_range3,levels=c(1,2,3,4),labels=c('<=30','30.01-50','50.01-70','>70')),
                    PG2_range=case_when(
                      PG2<6.51 ~ 1,
                      PG2>=6.51 & PG2<9.8 ~ 2,
                      PG2>=9.8 & PG2<15.3 ~ 3,
                      PG2>=15.30 ~ 4
                    ),
                    PG2_range=factor(PG2_range,levels = c(1,2,3,4),labels=c('<6.5','6.51-9.79','9.8-15.29','>=15.30')),
                    PG2_range2=case_when(
                      PG2<6 ~ 1,
                      PG2>=6 & PG2<8.4 ~ 2,
                      PG2>=8.4 & PG2<11.7 ~ 3,
                      PG2>=11.7 & PG2<16.8 ~ 4,
                      PG2>=16.8 ~ 5
                    ),
                    PG2_range2=factor(PG2_range2,levels = c(1,2,3,4,5),labels=c('<6','6-8.39','8.4-11.69','11.7-16.8','>=16.8')),
                  
                    PGR_range2=case_when(
                      PGR<=3 ~ 1,
                      PGR>3 & PGR<=6 ~ 2,
                      PGR>6 & PGR<=9~ 3,
                      PGR>9~ 4
                    ),
                    PGR_range2=factor(PGR_range2,levels=c(1,2,3,4),labels=c('<=3','3.01-6','6.01-9','>9'))
)
#年龄
PG12<-data%>%group_by(年龄分组2)%>%summarise(PG1.med=median(PG1),PG1.Q1=quantile(PG1,0.25),PG1.Q3=quantile(PG1,0.75),
                                             PG2.med=median(PG2),PG2.Q1=quantile(PG2,0.25),PG2.Q3=quantile(PG2,0.75),
                                             PGR.med=median(PGR),PGR.Q1=quantile(PGR,0.25),PGR.Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_errorbar(aes(x=年龄分组2,ymin=PG1.Q1,ymax=PG1.Q3),width=0.1,size=0.5,color='blue')+geom_point(aes(x=年龄分组2,y=PG1.med),size=2,color='blue')+
  geom_line(aes(x=年龄分组2,y=PG1.med,group=1,color='PGI'))+
  geom_errorbar(aes(x=年龄分组2,ymin=PG2.Q1,ymax=PG2.Q3),width=0.1,size=0.5,color='#FF6600')+geom_point(aes(x=年龄分组2,y=PG2.med),size=2,color='#FF6600')+
  geom_line(aes(x=年龄分组2,y=PG2.med,group=1,color='PGII'))+scale_y_log10()+mytheme+
  scale_color_manual(values=c("PGI"="blue","PGII"="#FF6600"))+labs(x='Age',fill=' ',y="Median(Q1-Q3)")+
  guides(shape=guide_legend(override.aes=list(size=10)))

PGR<-data%>%group_by(年龄分组2)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%filter(!is.na(年龄分组2))%>%
  ggplot()+geom_errorbar(aes(x=年龄分组2,ymin=Q1,ymax=Q3),width=0.1,size=0.5,color='#FF6600')+geom_point(aes(x=年龄分组2,y=median),size=2,color='#FF6600')+
  geom_line(aes(x=年龄分组2,y=median,group=1,color='PGR'))+scale_y_log10()+mytheme+
  scale_color_manual(values=c('PGR'='#FF6600'))+labs(x='Age',fill=' ',y="Median(Q1-Q3)")+
  guides(shape=guide_legend(override.aes=list(size=5)))

PG12 | PGR
#性别
PG1.sex<-data%>%group_by(性别)%>%summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGI',x='')


PG2.sex<-data%>%group_by(性别)%>%summarise(median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGII',x=' ')+
  geom_line(aes(x=as.numeric(性别),y=c(23,23)),size=0.8)+geom_line(aes(x=c(1,1),y=c(22.5,23)),size=0.8)+geom_line(aes(x=c(2,2),y=c(22.5,23)),size=0.8)+
  annotate('text',label="***",x=1.5,y=23.5,size=10,color='black')


PGR.sex<-data%>%group_by(性别)%>%summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGI/II ratio',x=' ')
  
(PG1.sex | PG2.sex | PGR.sex) +plot_layout(guides='collect') & theme(legend.position = 'top')
#分布
#PGI
make.table(dat= data,
           strat        = "PG1_range3",
           cat.varlist  = c('性别','年龄分组'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
data%>%pivot_longer(cols=c('性别','年龄分组'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG1),2),Q1=round(quantile(PG1,0.25),2),Q3=round(quantile(PG1,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
p<-list()
for(i in c('性别','年龄分组')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(data[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=data)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=data)$p.value,4)
  }
}
do.call(rbind,p)

#PGII Level
make.table(dat= data,
           strat        = "PG2_range2",
           cat.varlist  = c('性别','年龄分组'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
data%>%pivot_longer(cols=c('性别','年龄分组'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG2),2),Q1=round(quantile(PG2,0.25),2),Q3=round(quantile(PG2,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))

p<-list()
for(i in c('性别','年龄分组')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(data[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=data)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=data)$p.value,4)
  }
}
do.call(rbind,p)

#PGI/II ratio 
make.table(dat= data,
           strat        = "PGR_range2",
           cat.varlist  = c('性别','年龄分组'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")

data%>%pivot_longer(cols=c('性别','年龄分组'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PGR),2),Q1=round(quantile(PGR,0.25),2),Q3=round(quantile(PGR,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PGR,'~/continous_PGR.xlsx')
p<-list()
for(i in c('性别','年龄分组')){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(data[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=data)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=data)$p.value,4)
  }
}
do.call(rbind,p)
