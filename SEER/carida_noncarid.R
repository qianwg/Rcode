rm(list=ls())
library(rio)
library(tidyverse)
library(survival)
library(rms)
library(survminer)
library(cmprsk)
library(splines)
library(htmlTable)
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/HR.R')
library(forestmodel)
stomach<-import('~/data/noncardia.xlsx')
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
#cut-off is 0.59 according to the softwale X-tile
stomach2<-stomach%>%transmute(ID=`Patient ID`,
  Age=`Age at diagnosis`,
  Age_group=factor(ifelse(Age>=60,2,1),levels=c(1,2),labels=c('<60','>=60')),
  Race=factor(Race,levels=c(1,2,3),labels=c('Black','White','Others')),
  Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
  time,status,status2,
  stage=factor(Stage,levels=c(1,2,3),labels=c('locaized','regional','Distant')),
  Grade=factor(Grade,levels=c(1,2),labels=c('I/II','III/IV')),
  Surgery=factor(surergy,levels=c(1,2),labels=c('No','Yes')),
  Radiation=factor(Radiation,levels=c(1,2),labels=c('No','Yes')),
  Chemotherapy=factor(`Chemotherapy recode (yes, no/unk)`,levels=c(1,2),labels=c('No','Yes')),
  tumor_size=factor(ifelse(size_group<=1,1,ifelse(size_group<=4,2,3)),levels=c(1,2,3),labels=c('<=2','>2-5','>5')),
  meta=factor(meta,levels=c(1,2),labels=c('No','Yes')),
  T=factor(T,levels=c(1,2,3,4),labels=c('T1','T2','T3','T4')),
  N=factor(N,levels=c(1,2,3,4),labels=c('N0','N1','N2','N3')),
  M=factor(M,levels=c(1,2),labels=c('M0','M1')),
  Marital=factor(Marital,levels=c(1,2,3),labels=c('Married','Divorced/Separated/Widowed','Single')),
  Primary_Site=factor(Primary_Site,levels=c(0,1,2),labels=c('Others','Noncardia','Cardia')),
  Correa=factor(Correa,levels = c(0,1,2),labels=c('Others','Diffuse','intestinal')),
  pos_lN_rate,Ln_group=factor(ifelse(pos_lN_rate<=0.59,1,2),levels=c(1,2),labels=c('<=0.59','>=6.0')),
  stage_cli1=factor(Stage_cli1,levels=c(1,2,3,4),labels=c('I','II','III','IV')),
  stage_cli2=factor(Stage_cli3,levels=c(1,2,3),labels=c('I','II/III','IV')),
  Race2=factor(Race2,levels=c(1,2,3,4),labels=c('White','Black','Asian or Pacific Islander','American Indian/Alaska Native'))
)%>%filter(time>1)
summary(stomach2)
###Positive Ln rate
kms<-survfit(Surv(time,status)~Ln_group,data=subset(stomach2,Primary_Site!="Others"))
survdiff(Surv(time,status)~Ln_group,data=subset(stomach2,Primary_Site!="Others"))
ggsurvplot(kms, subset(stomach2,Primary_Site!="Others"),pval = T, break.x.by = 30,legend.title="Primary Site",    
           risk.table = T,legend.labs =
             c("<=0.59", ">=6.0"),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall Survial')
###Cardia and Noncardia
#K-M
kms<-survfit(Surv(time,status)~Primary_Site,data=subset(stomach2,Primary_Site!="Others"))
survdiff(Surv(time,status)~Primary_Site,data=subset(stomach2,Primary_Site!="Others"))
ggsurvplot(kms, subset(stomach2,Primary_Site!="Others"),pval = T, break.x.by = 30,legend.title="Primary Site",    
                 risk.table = T,legend.labs =
                   c("Noncardia", "Cardia"),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
##cummalutive curve
crmod <- with(subset(stomach2,Primary_Site!="Others"),cuminc(time,status2,Primary_Site))
plot(crmod,xlab = 'Month', ylab = 'CIF',lwd=2,lty=1,
     col = c('red','blue','black','forestgreen'))
##table
make.table(dat=subset(stomach2,Primary_Site!="Others"),
           strat        = "Primary_Site",
           cat.rmstat   = c("col"),
           cat.varlist  = c("Age_group","Race","Sex","stage","Grade","Surgery","Radiation","Chemotherapy",
                             "tumor_size","meta","T","N","M","Marital","Correa"),
           cat.ptype    = c("chisq"),
           output       = "html"
)
##Nomogram for noncardia gastric cancer
stomach.noncardia<-stomach2[which(stomach2$Primary_Site=='Noncardia'),]
#univariate cox 
variables<-c("Age_group","Race","Sex","stage","Grade","Surgery","Radiation","Chemotherapy",
  "tumor_size","meta","T","N","Marital","Correa")
cox(x=variables,y='Surv(time,status)',data=stomach.noncardia)
cox.model<-coxph(Surv(time,status)~Age_group+Race+Sex+stage+Grade+Surgery+Radiation+Chemotherapy+tumor_size+
                   meta+T+N+Marital+Ln_group,data=stomach.noncardia)
step(cox.model,direction = 'backward')
cox.model2<-coxph(Surv(time,status)~Age_group+Sex+Grade+Surgery+Chemotherapy+tumor_size+
                   meta+T+N+Marital+Ln_group,data=stomach.noncardia)
summary(cox.model)
summary(cox.model2)
#
step(cox.model,direction = 'both')
#make nomogram
ddist<-datadist(stomach.noncardia)
options(datadist='ddist')
cox2<-cph(Surv(time,status)~Age_group+Race+Sex+stage+Grade+Surgery+Radiation+Chemotherapy+tumor_size+
            meta+T+N+Marital+Ln_group,data=stomach.noncardia,surv=T,x=T,y=T) 
surv<-Survival(cox2)
sur_3_year<-function(x)surv(1*12*3,lp=x)
sur_5_year<-function(x)surv(1*12*5,lp=x)
nom_sur <- nomogram(cox2,fun=list(sur_3_year,sur_5_year),lp= F,funlabel=c('3-Year Survival','5-Year survival'),maxscale=100,fun.at=c('0.9','0.8','0.7','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nom_sur,xfrac=0.25)
####Calibration
#3_year
cox3<-cph(Surv(time,status)~Age_group+Race+Sex+stage+Grade+Surgery+Radiation+Chemotherapy+tumor_size+
            meta+T+N+Marital,data=stomach.noncardia,surv=T,x=T,y=T,time.inc = 1*12*3) 
cal<-calibrate(cox3,cmethod="KM",method="boot",u=1*12*3,m=2453,B=100)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 3-Year Survival",ylab="Actual 3-Year Survival",col="blue")
box(lwd = 1)
abline(0,1,lty = 3,lwd = 1,col = "black")
#5_year
cox1 <- cph(Surv(time,status)~Age_group+Race+Sex+stage+Grade+Surgery+Radiation+Chemotherapy+tumor_size+
              meta+T+N+Marital,surv=T,x=T, y=T,time.inc = 1*12*5,data=stomach.noncardia) 
cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*12*5, m= 2453, B=100)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 5-Year Survival",ylab="Actual 5-Year Survival",col="blue")
box(lwd = 1)
abline(0,1,lty = 3,lwd = 1,col = "black")
#######################<<<<<<<<<<2020-12-17:贲门和非贲门的病理特征及预后比较>>>>>>>>>>>>>>>##########################3
stomach3<-stomach2%>%filter(Primary_Site!="Others")
###
variables<-c("Age_group","Race","Sex","Grade","Surgery","Radiation","Chemotherapy",
             "tumor_size","meta","T","N","M","Marital" ,
             "Correa","Ln_group",'stage_cli1','stage_cli2')
make.table(dat= stomach3,
           strat        = "Primary_Site",
           cat.varlist  =variables,
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
####K-M
kms<-survfit(Surv(time,status)~Primary_Site,data=stomach3)
survdiff(Surv(time,status)~Primary_Site,data=stomach3)
ggsurvplot(kms,data = stomach3,pval = T, break.x.by = 30,legend.title="Primary Site",    
                  risk.table = T,legend.labs =
                    c("Noncardia", "Cardia"),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Overall survival')
###3-Year survival
options(scipen = 100)
summary(kms,times=36)
#for noncardia
kms<-survfit(Surv(time,status)~stage_cli1,data=subset(stomach3,Primary_Site=="Noncardia"))
ggsurvplot(kms,data = subset(stomach3,Primary_Site=="Noncardia"),pval = T, break.x.by = 30,legend.title="Primary Site",    
           risk.table = T,legend.labs =
             c("I", "II",'III','IV'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600','#FF0033'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall survival')

summary(survfit(Surv(time,status)~stage_cli1,data=subset(stomach3,Primary_Site=="Noncardia")),times=36)
summary(survfit(Surv(time,status)~stage_cli1,data=subset(stomach3,Primary_Site=="Noncardia")),times=60)
#for cardia
kms<-survfit(Surv(time,status)~stage_cli1,data=subset(stomach3,Primary_Site=="Cardia"))
ggsurvplot(kms,data = subset(stomach3,Primary_Site=="Cardia"),pval = T, break.x.by = 30,legend.title="Primary Site",    
           risk.table = T,legend.labs =
             c("I", "II",'III','IV'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600','#FF0033'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall survival')
summary(survfit(Surv(time,status)~stage_cli1,data=subset(stomach3,Primary_Site=="Cardia")),times=36)
summary(survfit(Surv(time,status)~stage_cli1,data=subset(stomach3,Primary_Site=="Cardia")),times=60)
####univariate COx
##for noncardia
cox(y='Surv(time,status)',x=variables,data=subset(stomach3,Primary_Site=="Noncardia"))
forest_model(coxph(Surv(time,status)~relevel(Correa,ref='intestinal'),data=subset(stomach3,Primary_Site=="Noncardia")))
##for cardia
cox(y='Surv(time,status)',x=variables,data=subset(stomach3,Primary_Site=="Cardia"))
forest_model(coxph(Surv(time,status)~relevel(Correa,ref='intestinal'),data=subset(stomach3,Primary_Site=="Cardia")))
###multivariate cox
forest_model(coxph(Surv(time,status)~Age_group+Race+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+stage_cli1+relevel(Correa,ref='intestinal'),data=subset(stomach3,Primary_Site=="Noncardia")))
##for cardia
forest_model(coxph(Surv(time,status)~Age_group+Race+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+stage_cli1+relevel(Correa,ref='intestinal'),data=subset(stomach3,Primary_Site=="Cardia")))

####multi-cox by stage_cli
stomach4<-stomach3%>%mutate(Age_stage_cli1=factor(paste(Age_group,stage_cli1,sep="|")),
                            Race_stage_cli1=factor(paste(Race,stage_cli1,sep="|")),
                            Sex_stage_cli1=factor(paste(Sex,stage_cli1,sep="|")),
                            Grade_stage_cli1=factor(paste(Grade,stage_cli1,sep="|")),
                            Surgery_stage_cli1=factor(paste(Surgery,stage_cli1,sep="|")),
                            Radiation_stage_cli1=factor(paste(Radiation,stage_cli1,sep="|")),
                            Chemotherapy_stage_cli1=factor(paste(Chemotherapy,stage_cli1,sep="|")),
                            tumor_size_stage_cli1=factor(paste(tumor_size,stage_cli1,sep="|")),
                            meta_stage_cli1=factor(paste(meta,stage_cli1,sep="|")),
                            T_stage_cli1=factor(paste(T,stage_cli1,sep="|")),
                            N_stage_cli1=factor(paste(N,stage_cli1,sep="|")),
                            Marital_stage_cli1=factor(paste(Marital,stage_cli1,sep="|")),
                            Ln_group_stage_cli1=factor(paste(Ln_group,stage_cli1,sep="|"))
                            )
summary(stomach4)
#Age and stage_cli
forest_model(coxph(Surv(time,status)~Age_stage_cli1+Race+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Age*stage_cli1+Race+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#Race and stage_cli
forest_model(coxph(Surv(time,status)~Race_stage_cli1+Age_group+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Race*stage_cli1+Age_group+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#Sex and stage_cli
forest_model(coxph(Surv(time,status)~Sex_stage_cli1+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Sex*stage_cli1+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#Grade and stage_cli
forest_model(coxph(Surv(time,status)~Grade_stage_cli1+Sex+Race+Age_group+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Grade*stage_cli1+Sex+Race+Age_group+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#surgery and stage_cli
forest_model(coxph(Surv(time,status)~Surgery_stage_cli1+Sex+Race+Age_group+Grade+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Surgery*stage_cli1+Sex+Race+Age_group+Grade+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#Radiation and stage_cli
forest_model(coxph(Surv(time,status)~Radiation_stage_cli1+Sex+Race+Age_group+Grade+Surgery+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Radiation*stage_cli1+Sex+Race+Age_group+Grade+Surgery+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#Chemotherapy and stage_cli
forest_model(coxph(Surv(time,status)~Chemotherapy_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Chemotherapy*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#tumor_size and stage_cli
forest_model(coxph(Surv(time,status)~tumor_size_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~tumor_size*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#meta and stage_cli
forest_model(coxph(Surv(time,status)~meta_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#T and stage_cli
forest_model(coxph(Surv(time,status)~T_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
#N and stage_cli
forest_model(coxph(Surv(time,status)~N_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
#Marital and stage_cli
forest_model(coxph(Surv(time,status)~relevel(Marital_stage_cli1,ref='Married|I')+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Marital*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

#Ln_group and stage_cli
forest_model(coxph(Surv(time,status)~Ln_group_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))
anova(coxph(Surv(time,status)~Ln_group*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Cardia")))

######for noncardia cancer

#Age and stage_cli
forest_model(coxph(Surv(time,status)~Age_stage_cli1+Race+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Age*stage_cli1+Race+Sex+Grade+Surgery+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#Race and stage_cli
forest_model(coxph(Surv(time,status)~Race_stage_cli1+Age_group+Sex+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Race*stage_cli1+Age_group+Sex+Grade+Surgery+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#Sex and stage_cli
forest_model(coxph(Surv(time,status)~Sex_stage_cli1+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Sex*stage_cli1+Race+Age_group+Grade+Surgery+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#Grade and stage_cli
forest_model(coxph(Surv(time,status)~Grade_stage_cli1+Sex+Race+Age_group+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Grade*stage_cli1+Sex+Race+Age_group+Surgery+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#surgery and stage_cli
forest_model(coxph(Surv(time,status)~Surgery_stage_cli1+Sex+Race+Age_group+Grade+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Surgery*stage_cli1+Sex+Race+Age_group+Grade+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#Radiation and stage_cli
forest_model(coxph(Surv(time,status)~Radiation_stage_cli1+Sex+Race+Age_group+Grade+Surgery+
                     Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Radiation*stage_cli1+Sex+Race+Age_group+Grade+Surgery+
              Chemotherapy+tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#Chemotherapy and stage_cli
forest_model(coxph(Surv(time,status)~Chemotherapy_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Chemotherapy*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
              tumor_size+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#tumor_size and stage_cli
forest_model(coxph(Surv(time,status)~tumor_size_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~tumor_size*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
              Chemotherapy+meta+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#meta and stage_cli
forest_model(coxph(Surv(time,status)~meta_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+T+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#T and stage_cli
forest_model(coxph(Surv(time,status)~T_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+N+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
#N and stage_cli
forest_model(coxph(Surv(time,status)~N_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+Marital+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
#Marital and stage_cli
forest_model(coxph(Surv(time,status)~relevel(Marital_stage_cli1,ref='Married|I')+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Marital*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Ln_group+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))

#Ln_group and stage_cli
forest_model(coxph(Surv(time,status)~Ln_group_stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
                     Chemotherapy+tumor_size+meta+T+N+Marital+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))
anova(coxph(Surv(time,status)~Ln_group*stage_cli1+Sex+Race+Age_group+Grade+Surgery+Radiation+
              Chemotherapy+tumor_size+meta+T+N+Marital+relevel(Correa,ref='intestinal'),data=subset(stomach4,Primary_Site=="Noncardia")))


###################<<<<<<<<<<2020-12-19:针对做过手术之后,不同种族对胃癌预后的影响>>>>>>>>>>>>###########
stomach5<-stomach2%>%filter(Surgery=="Yes")#做过手术
make.table(dat=subset(stomach2,Primary_Site!="Others"),
           strat        = "Race2",
           cat.rmstat   = c("col"),
           cat.varlist  = c("Age_group","Race","Sex","stage","Grade","Surgery","Radiation","Chemotherapy",
                            "tumor_size","meta","T","N","M","Marital","Correa",'Primary_Site'),
           cat.ptype    = c("chisq"),
           output       = "html"
)




