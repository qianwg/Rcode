rm(list=ls())
library(rio)
library(tidyverse)
library(ggpubr)
seer<-import('~/data/SEER1973_2015.sav')
seer$CA_TYPE<-factor(seer$CA_TYPE,levels=c(0,1,2,3,4,6,7,8,9),
                     labels=c('LYMYLEUK','BREAST','COLRECT','DIGOTHR','FEMGEN',
                              'MALEGEN','OTHER','RESPIR','URINARY'))
seer$Survival_years_group<-with(seer,case_when(
  between(Survival_years,1,4) ~ 1,
  between(Survival_years,5,9) ~ 2,
  between(Survival_years,10,19) ~ 3,
  Survival_years>=20 ~ 4,

  
))
seer$Survival_years_group<-factor(seer$Survival_years_group,levels=c(1,2,3,4),labels=c('1-year','5-year','10-year','20-year'))
mytheme<-theme(plot.title=element_text(hjust=0.5),
               #axis.title=element_text(face="bold",size=10),
               #axis.text=element_text(face="bold",size=9),
               #panel.grid.major = element_line(colour=NA),
               #panel.grid.major.x = element_line(color='grey'),
               #panel.grid.major.y = element_line(color='grey'),
               panel.background = element_blank(),
               axis.line = element_line(color='grey'),
               legend.title=element_blank(),
               legend.position='top',
               #legend.position = 'none',
               #strip.text.x =element_text(face='blod',color='red')
)
#summary(seer)
##各个癌症的数量
summary(seer$CA_TYPE);table(seer$CA_TYPE)
##随访年
summary(seer$SRV_TIME_MON/12)#平均随访6.9年
##CVD死亡人数及比例
with(seer,table(CODPUB))
#nrow(seer[which(seer$CODPUB==50060 | seer$CODPUB==50070 | seer$CODPUB==50080 | 
#                  seer$CODPUB==50090 | seer$CODPUB==50050),])
with(seer,table(CVD_Death))
563848/sum(seer$SRV_TIME_MON)
with(subset(seer,CVD_Death!=0),prop.table(table(CVD_Death)))#占全死因死亡的比例
with(subset(seer,CA_CVD_Death!=0 | CA_CVD_Death!=1),prop.table(table(CA_CVD_Death)))#占除癌症以外死因死亡的比例
#Fifure.1
#诊断后，随年份，CVD死亡、癌症死亡、其他死亡的比率


##Overall
seer%>%filter(CA_CVD_Death!=0)%>%group_by(Survival_years,CA_CVD_Death)%>%summarise(n=n())%>%
  group_by(Survival_years)%>%mutate(percent=round(n/sum(n),4)*100)%>%filter(CA_CVD_Death!=3,Survival_years<=30)%>%
  ggplot()+geom_line(aes(x=Survival_years,y=percent,color=factor(CA_CVD_Death,labels=c('Cancer Death','CVD Death'))),size=2)+mytheme+
  scale_x_continuous(breaks=seq(30,2))
#各个癌症中CVD死亡和癌症死亡的趋势
seer%>%filter(CA_CVD_Death!=0)%>%group_by(CA_TYPE,Survival_years,CA_CVD_Death)%>%summarise(n=n())%>%
  group_by(CA_TYPE,Survival_years)%>%mutate(percent=round(n/sum(n),4)*100)%>%filter(CA_CVD_Death!=3,Survival_years<=30)%>%
  ggplot()+geom_line(aes(x=Survival_years,y=percent,color=factor(CA_CVD_Death,labels=c('Cancer Death','CVD Death'))),size=2)+mytheme+
  scale_x_continuous()+facet_wrap(.~CA_TYPE,nrow=3,scales='free')

##Figure.2
#CVD死亡的比例随诊断年份的趋势
#Overall
seer%>%filter(CA_CVD_Death!=0)%>%group_by(CA_TYPE,YEAR_DX,CVD_Death)%>%summarise(n=n())%>%
  group_by(CA_TYPE,YEAR_DX)%>%mutate(percent=round(n/sum(n),4)*100,
                                     Survival_years_group=factor(case_when(
                                       between(Survival_years,1,4) ~ 1,
                                       between(Survival_years,5,9) ~ 2,
                                       between(Survival_years,10,19) ~ 3,
                                       Survival_years>=20 ~ 4),levels=c(1,2,3,4),labels=c('1-year','5-year','10-year','20-year'))
                                     )%>%filter(CVD_Death==1)%>%
  ggplot()+geom_line(aes(x=YEAR_DX,y=percent),size=1.5,color=Survival_years_group)+mytheme












