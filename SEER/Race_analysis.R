rm(list=ls())
library(rio)
library(tidyverse)
library(survival)
library(rms)
library(survminer)
library(cmprsk)
library(splines)
library(htmlTable)
library(forestmodel)
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/HR.R')
stomach<-import('~/data/SEER/数据(分析种族用).xlsx')
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
stomach2<-stomach%>%transmute(ID=ID,Year_diagnosis,Age=Age,
                              Year_group=factor(case_when(
                                between(Year_diagnosis,1998,2003)~1,
                                between(Year_diagnosis,2004,2009)~2,
                                between(Year_diagnosis,2010,2016)~3,
                              ),levels=c(1,2,3),labels=c('1998-2003','2004-2009','2010-2016')),
                              Age_group=factor(ifelse(Age>=60,2,1),levels=c(1,2),labels=c('<60','>=60')),
                              Race1=factor(Race1,levels=c(1,2,3),labels=c('White','Black','Others')),
                              Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
                              time,status1,status2,
                              stage=factor(Summary_stage,levels=c(1,2,3,4),labels=c('locaized','regional','Distant','Unknown/Unstage')),
                              Primary_Site=factor(Primary_Site,levels=c(1,2,3),labels=c('Noncardia','Cardia','Others')),
                              Race2=factor(Race2,levels=c(1,2,3,4),labels=c('White','Black','Asian or Pacific Islander','American Indian/Alaska Native')),
                              Grade=factor(Grade,levels=c(1,2,3,4),labels=c('I','II','III','IV')),
                              Surgery=factor(Surgery,levels=c(1,2),labels=c('No','Yes')),
                              Radiation=factor(Radiation,levels=c(1,2),labels=c('No','Yes')),
                              Chemotherapy=factor(Chemotherapy,levels=c(1,2),labels=c('No','Yes')),
                              Marital=factor(Marital,levels=c(1,2,3),labels=c('Married','Divorced/Separated/Widowed','Single')),
                              Correa=factor(Correa,levels = c(1,2,3),labels=c('intestinal','Diffuse','Others')),
                              ethnicity
                              #tumor_size=factor(ifelse(size_group<=1,1,ifelse(size_group<=4,2,3)),levels=c(1,2,3),labels=c('<=2','>2-5','>5')),
                              #meta=factor(meta,levels=c(1,2),labels=c('No','Yes')),
                              #T=factor(T,levels=c(1,2,3,4),labels=c('T1','T2','T3','T4')),
                              #N=factor(N,levels=c(1,2,3,4),labels=c('N0','N1','N2','N3')),
                              #M=factor(M,levels=c(1,2),labels=c('M0','M1')),
                              #pos_lN_rate,Ln_group=factor(ifelse(pos_lN_rate<=0.59,1,2),levels=c(1,2),labels=c('<=0.59','>=6.0')),
                              #stage_cli1=factor(Stage_cli1,levels=c(1,2,3,4),labels=c('I','II','III','IV')),
                              #stage_cli2=factor(Stage_cli3,levels=c(1,2,3),labels=c('I','II/III','IV')),
                              
)#%>%filter(time>1)
#########特征在种族中的分布情况
make.table(dat=stomach2,
           strat        = "Race2",
           cat.rmstat   = c("row"),
           cat.varlist  = c('Year_group',"Age_group","Sex","Marital","Correa","stage","Grade",
                            "Primary_Site",'Surgery',"Radiation","Chemotherapy"
                            ),
           cat.ptype    = c("chisq"),
           output       = "html"
)
###5-year survival rate
options(digits = 5)
#Year_group
summary(survfit(Surv(time,status1)~Year_group,data=stomach2),times=60)
survdiff(Surv(time,status1)~Year_group,data=stomach2)
#Age
summary(survfit(Surv(time,status1)~Age_group,data=stomach2),times=60)
survdiff(Surv(time,status1)~Age_group,data=stomach2)
#Sex
summary(survfit(Surv(time,status1)~Sex,data=stomach2),times=60)
survdiff(Surv(time,status1)~Sex,data=stomach2)
#stage
summary(survfit(Surv(time,status1)~stage,data=stomach2),times=60)
survdiff(Surv(time,status1)~stage,data=stomach2)
#Primary_Site
summary(survfit(Surv(time,status1)~Primary_Site,data=stomach2),times=60)
survdiff(Surv(time,status1)~Primary_Site,data=stomach2)
#Grade
summary(survfit(Surv(time,status1)~Grade,data=stomach2),times=60)
survdiff(Surv(time,status1)~Grade,data=stomach2)
#Surgery
summary(survfit(Surv(time,status1)~Surgery,data=stomach2),times=60)
survdiff(Surv(time,status1)~Surgery,data=stomach2)
#Radiation
summary(survfit(Surv(time,status1)~Radiation,data=stomach2),times=60)
survdiff(Surv(time,status1)~Radiation,data=stomach2)
#Chemotherapy
summary(survfit(Surv(time,status1)~Chemotherapy,data=stomach2),times=60)
survdiff(Surv(time,status1)~Chemotherapy,data=stomach2)
#Marital
summary(survfit(Surv(time,status1)~Marital,data=stomach2),times=60)
survdiff(Surv(time,status1)~Marital,data=stomach2)
#Correa
summary(survfit(Surv(time,status1)~Correa,data=stomach2),times=60)
survdiff(Surv(time,status1)~Correa,data=stomach2)
#Race
summary(survfit(Surv(time,status1)~Race2,data=stomach2),times=60)
survdiff(Surv(time,status1)~Race2,data=stomach2)
##Cox regression
variables<-c('Year_group','Age_group','Sex','stage','Primary_Site','Race2',
             'Grade','Surgery','Radiation','Chemotherapy','Marital','Correa')
cox(y='Surv(time,status1)',x=variables,data=stomach2)
###p for interaction between Race and stage
#Age and stage_cli
stomach2$Race_stage<-factor(paste(stomach2$Race2,stomach2$stage,sep='|'))
forest_model(coxph(Surv(time,status1)~relevel(Race_stage,ref="White|locaized"),data=stomach2))
anova(coxph(Surv(time,status)~Age*stage_cli1+Race+Sex+Grade+Surgery+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))



####K-M曲线
kms<-survfit(Surv(time,status1)~Race2,data=stomach2)
ggsurvplot(kms,stomach2,pval = T, break.x.by = 30,legend.title="Race",    
           risk.table = T,legend.labs =
             c("White", "Black",'Asian or Pacific Islander','American Indian/Alaska Native'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600','#FF0033'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall Survial')

##Noncardia
kms2<-survfit(Surv(time,status1)~Race2,data=subset(stomach2,Primary_Site=="Noncardia"))
ggsurvplot(kms2,subset(stomach2,Primary_Site=="Noncardia"),pval = T, break.x.by = 30,legend.title="Race",    
           risk.table = T,legend.labs =
             c("White", "Black",'Asian or Pacific Islander','American Indian/Alaska Native'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600','#FF0033'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall Survial')
#Cardia
kms3<-survfit(Surv(time,status1)~Race2,data=subset(stomach2,Primary_Site=="Cardia"))
ggsurvplot(kms3,subset(stomach2,Primary_Site=="Cardia"),pval = T, break.x.by = 30,legend.title="Race",    
           risk.table = T,legend.labs =
             c("White", "Black",'Asian or Pacific Islander','American Indian/Alaska Native'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600','#FF0033'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall Survial')
####cummalutive curve
crmod <- with(stomach2,cuminc(time,status2,Race2))
plot(crmod,xlab = 'Month', ylab = 'CIF',lwd=2,lty=1,
     col = c('red','yellow','blue','lightblue','black','orange','forestgreen','green'))













