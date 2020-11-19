library(rio)
library(tidyverse)
library(ggpubr)
library(blandr)
#读取数据
source('~/Rcode/screening/gastric_screening2020/data2020.R')
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
pepsinogen2019<-pepsinogen2019%>%
  filter(ID!=31030159,ID!=31060461,自身癌!='是',残胃!='是')
source('~/Rcode/statistics/data_summary.R')
pepsinogen2019_2020<-inner_join(pepsinogen2019,pepsinogen2020,by='persoID')
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(face="bold",size=10),
               axis.text=element_text(face="bold",size=9),
               panel.grid.major = element_line(colour=NA),
               panel.grid.major.y = element_line(color='grey'),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black'),
               legend.position = 'none'
)
summary(pepsinogen2019_2020)

with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PG1.2019,PG1.2020,type='p'))
with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PG2.2019,PG2.2020,type='p'))
with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PGR.2019,PGR.2020,type='p'))
with(pepsinogen2019_2020,table(C14_pos))
#2019年与2020年PG1结果的一致性检验
#PG1
#cor(pepsinogen2019_2020$PG1.x,pepsinogen2019_2020$PG1.y)
#cor.test(pepsinogen2019_2020$PG1.x,pepsinogen2019_2020$PG1.y)
cor11<-ggplot(data=subset(pepsinogen2019_2020,PG1.x!=200),aes(x=PG1.x,y=PG1.y))+geom_point(alpha=0.5)+
  geom_abline(slope=1,size=1)+scale_x_continuous(limits=c(0,250))+mytheme+labs(title='Plot of identity',x='PG-I tested in 2019',y='PG-I tested in 2020')
#ggplot
statistics.results<-with(subset(pepsinogen2019_2020,PG1.x!=200),blandr.statistics(PG1.x, PG1.y))
cor12<-blandr.plot.ggplot( statistics.results , plotTitle = "Bland-Altman  plot")+labs(x='Average of the two PG-I measurements',y='Difference')+
  mytheme
cor1 | cor2
##PGR
cor(pepsinogen2019_2020$PGR.x,pepsinogen2019_2020$PGR.y)
cor.test(pepsinogen2019_2020$PGR.x,pepsinogen2019_2020$PGR.y)
corr1<-ggplot(data=pepsinogen2019_2020,aes(x=PGR.x,y=PGR.y))+geom_point(alpha=0.5)+
  geom_abline(slope=1,size=1)+mytheme+labs(title='Plot of identity',x='PGI/II tested in 2019',y='PGI/II tested in 2020')
#ggplot
statistics.results<-with(pepsinogen2019_2020,blandr.statistics(PGR.x, PGR.y))
corr2<-blandr.plot.ggplot( statistics.results , plotTitle = "Bland-Altman  plot")+labs(x='Average of the two PGI/II measurements',y='Difference')+
  mytheme
corr1 | corr2
#PG2
cor(pepsinogen2019_2020$PG2.x,pepsinogen2019_2020$PG2.y)
cor.test(pepsinogen2019_2020$PG2.x,pepsinogen2019_2020$PG2.y)
cor21<-ggplot(data=pepsinogen2019_2020,aes(x=PG2.x,y=PG2.y))+geom_point(alpha=0.5)+
  geom_abline(slope=1,size=1)+mytheme+labs(title='Plot of identity',x='PG-II tested in 2019',y='PG-II tested in 2020')
#ggplot
statistics.results<-with(pepsinogen2019_2020,blandr.statistics(PG2.x, PG2.y))
cor22<-blandr.plot.ggplot( statistics.results , plotTitle = "Bland-Altman  plot")+labs(x='Average of the two PG-II measurements',y='Difference')+
  mytheme
cor21 | cor22

(cor11 | cor12) / (corr1 | corr2) / (cor21 | cor22)



#查看2020年肿标分布
do.call(rbind,apply(biomarker2020[,c('PG1','PG2','PGR','AFP')],2,data_summary2))
table(biomarker2020[,c('C14_pos')])
##
biomarker2020%>%ggboxplot(x="C14_pos", y="PG1", fill = "C14_pos", 
                               palette = c("#00AFBB", "#E7B800"))+ 
  stat_compare_means(label.y = 260,label.x=1)+theme(legend.position = 'none')+labs(x='',title='PGI')



####2020-09-01
rm(list=ls())
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
source('~/Rcode/screening/gastric_screening2020/data2020.R')
pepsinogen_2020new<-anti_join(pepsinogen2020,pepsinogen2019,by='persoID')
pepsinogen_2020new2<-pepsinogen_2020new[,names(pepsinogen2019)]
pepsinogen2019_2020<-rbind(pepsinogen_2020new2,pepsinogen2019)
#rm(c(datat_Hp,data_PG))
#export(pepsinogen2019_2020,'~/pepsinogen2019_2020.xlsx')
###
pepsinogen2019_2020$year<-ifelse(substr(pepsinogen2019_2020$ID,1,1)==4,2020,2019)
table(pepsinogen2019_2020$year)
with(pepsinogen2019_2020,table(year,自身癌))
with(pepsinogen2019_2020,table(year,残胃))

pepsinogen2019_2020<-pepsinogen2019_2020%>%
  filter(ID!=31030159,ID!=31060461,自身癌!='是',残胃!='是')



















