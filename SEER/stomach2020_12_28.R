rm(list=ls())
library(rio)
library(tidyverse)
library(survival)
library(rms)
library(survivalROC)
library(patchwork)
library(survminer)
mytheme<-theme(
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
stomach<-import('~/data/SEER/stomach_2020_12_28.xlsx')
stomach2<-stomach%>%transmute(Age=`Age at diagnosis`,
  Age_group=factor(ifelse(Age<50,1,2),levels=c(1,2),labels=c('<50','>=50')),
  Race=factor(Race,levels=c(1,2,3),labels=c('White','Black','Others')),
  Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
  time=`Survival months`,status1,status2,Year_diag,Year_group=factor(case_when(
    between(Year_diag,1975,1999) ~ 1,
    between(Year_diag,2000,2010) ~ 2,
    Year_diag>2010 ~ 3,
  ),levels=c(1,2,3),labels=c('1975-1999','2000-2010','>2010')),
  #status=factor(status,levels=c(0,1),labels=c('Alive','Dead')),
  stage=factor(stage,levels=c(1,2,3,4),labels=c('locaized','regional','distanst','UNstage')),
  COD=`COD to site rec KM`,status_spec=`SEER cause-specific death classification`,
  #status_other=`SEER other cause of death classification`,
  primary_site=factor(Primary_Site,levels=c(1,2,3),labels=c('Noncardia','Cardia','Others')),
  correa=factor(Correa,levels=c(1,2,3),labels=c('intestinal','diffuse','others')),
  stage_cli=as.numeric(stage_cli),stage_cli6=as.numeric(stage_cli6),stage_cli7=as.numeric(stage_cli7)
)%>%filter(time>1)
#summary(stomach2)
#K-M about Correa
kms<-survfit(Surv(time,status1)~correa,data=subset(stomach2,correa!="others"))
survdiff(Surv(time,status1)~correa,data=subset(stomach2,correa!="others"))
plot<-ggsurvplot(kms, data = subset(stomach2,correa!="Others"),pval = T, break.x.by = 60,legend.title="Correa",    
                 risk.table = T,legend.labs=c("intestinal type",'diffuse type'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial',conf.int = FALSE)
plot(kms)

####stage=locaized
kms1<-survfit(Surv(time,status1)~correa,data=subset(stomach2,correa!="others" & stage=="locaized"))
plot1<-ggsurvplot(kms1, data = subset(stomach2,correa!="Others" & stage=="locaized"),pval = T, break.x.by = 60,legend.title="Correa",    
           risk.table = T,legend.labs=c("intestinal type",'diffuse type'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='stage=locaized')
##stage=regional
kms2<-survfit(Surv(time,status1)~correa,data=subset(stomach2,correa!="others" & stage=="regional"))
plot2<-ggsurvplot(kms2, data = subset(stomach2,correa!="Others" & stage=="regional"),pval = T, break.x.by = 60,legend.title="Correa",    
           risk.table = T,legend.labs=c("intestinal type",'diffuse type'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='stage=regional')
##
kms3<-survfit(Surv(time,status1)~correa,data=subset(stomach2,correa!="others" & stage=="distanst"))
plot3<-ggsurvplot(kms3, data = subset(stomach2,correa!="Others" & stage=="distanst"),pval = T, break.x.by = 30,legend.title="Correa",    
           risk.table = T,legend.labs=c("intestinal type",'diffuse type'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='stage=distanst')
arrange_ggsurvplots(list(plot,plot1,plot2,plot3),nrow=2,ncol=2,risk.table.height=0.3)
##


#####Age
#Age_group="<50"
kms_age1<-survfit(Surv(time,status1)~correa,data=subset(stomach2,correa!="others" &  Age_group=="<50"))
plot.age1<-ggsurvplot(kms_age1, data = subset(stomach2,correa!="Others" & Age_group=="<50"),pval = T, break.x.by = 60,legend.title="Correa",    
                  risk.table = T,legend.labs=c("intestinal type",'diffuse type'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='stage=distanst')
#Age_group=">=50"
kms_age2<-survfit(Surv(time,status1)~correa,data=subset(stomach2,correa!="others" &  Age_group==">=50"))
plot.age2<-ggsurvplot(kms_age2, data = subset(stomach2,correa!="Others" & Age_group==">=50"),pval = T, break.x.by = 60,legend.title="Correa",    
                     risk.table = T,legend.labs=c("intestinal type",'diffuse type'),xlab = "Time in months",risk.table.height = 0.25,
                     ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                     tables.theme=clean_theme(),title='stage=distanst')
arrange_ggsurvplots(list(plot.age1,plot.age2),ncol=2,risk.table.height=0.3)


####
kms_age2<-survfit(Surv(time,status1)~correa,data=subset(stomach2,correa!="others" &  Age_group==""))
plot.age2<-ggsurvplot(kms_age2, data = subset(stomach2,correa!="Others" & Age_group==">=50"),pval = T, break.x.by = 60,legend.title="Correa",    
                      risk.table = T,legend.labs=c("intestinal type",'diffuse type'),xlab = "Time in months",risk.table.height = 0.25,
                      ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                      tables.theme=clean_theme(),title='stage=distanst')




