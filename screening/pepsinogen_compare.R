rm(list=ls())
library(rio)
library(tidyverse)
library(ggpubr)
library(blandr)
library(htmlTable)
library(patchwork)
#读取数据
pepsinogen2019_2020<-import('~/data/2020/PG检测比较.xlsx')%>%
  transmute(ID,PG1.2019=PG1.y,PG1.2020=PG1.x,PG2.2019=PG2.y,PG2.2020=PG2.x,
            PGR.2019=round(PG1.2019/PG2.2019,2),PGR.2020=round(PG1.2020/PG2.2020,2))

mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
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
summary(pepsinogen2019_2020)

with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PG1.2019,PG1.2020,type='p'))
with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PG2.2019,PG2.2020,type='p'))
with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PGR.2019,PGR.2020,type='p'))
#2019年与2020年PG1结果的一致性检验
#PG1
#cor(pepsinogen2019_2020$PG1.x,pepsinogen2019_2020$PG1.y)
#cor.test(pepsinogen2019_2020$PG1.x,pepsinogen2019_2020$PG1.y)
cor11<-ggplot(data=subset(pepsinogen2019_2020,PG1.2019!=200),aes(x=PG1.2019,y=PG1.2020))+geom_point(alpha=0.5)+
  geom_abline(slope=1,size=1)+scale_x_continuous(limits=c(0,250))+scale_y_continuous(limits=c(0,250))+mytheme+labs(title='Plot of identity',x='PG-I tested in 2019',y='PG-I tested in 2020')
#ggplot
statistics.results<-with(subset(pepsinogen2019_2020,PG1.2019!=200),blandr.statistics(PG1.2019, PG1.2020))
cor12<-blandr.plot.ggplot( statistics.results , plotTitle = "Bland-Altman  plot")+labs(x='Average of the two PG-I measurements',y='Difference')+
  mytheme
cor11 | cor12
##PGR
cor(pepsinogen2019_2020$PGR.2019,pepsinogen2019_2020$PGR.2020)
cor.test(pepsinogen2019_2020$PGR.2019,pepsinogen2019_2020$PGR.2020)
corr1<-ggplot(data=pepsinogen2019_2020,aes(x=PGR.2019,y=PGR.2020))+geom_point(alpha=0.5)+
  geom_abline(slope=1,size=1)+mytheme+labs(title='Plot of identity',x='PGI/II tested in 2019',y='PGI/II tested in 2020')
#ggplot
statistics.results<-with(pepsinogen2019_2020,blandr.statistics(PGR.2019, PGR.2020))
corr2<-blandr.plot.ggplot( statistics.results , plotTitle = "Bland-Altman  plot")+labs(x='Average of the two PGI/II measurements',y='Difference')+
  mytheme
corr1 | corr2
#PG2
cor(pepsinogen2019_2020$PG2.2019,pepsinogen2019_2020$PG2.2020)
cor.test(pepsinogen2019_2020$PG2.2019,pepsinogen2019_2020$PG2.2020)
cor21<-ggplot(data=pepsinogen2019_2020,aes(x=PG2.2019,y=PG2.2020))+geom_point(alpha=0.5)+
  geom_abline(slope=1,size=1)+mytheme+labs(title='Plot of identity',x='PG-II tested in 2019',y='PG-II tested in 2020')
#ggplot
statistics.results<-with(pepsinogen2019_2020,blandr.statistics(PG2.2019, PG2.2020))
cor22<-blandr.plot.ggplot( statistics.results , plotTitle = "Bland-Altman  plot")+labs(x='Average of the two PG-II measurements',y='Difference')+
  mytheme
cor21 | cor22

(cor11 | cor12) / (corr1 | corr2) / (cor21 | cor22)
