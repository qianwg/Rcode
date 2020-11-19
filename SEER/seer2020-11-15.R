rm(list=ls())
library(rio)
library(tidyverse)
library(survival)
library(rms)
library(survivalROC)
EGJ<-import('~/data/SEER_2020.xlsx')
summary(EGJ)
EGJ2<-EGJ%>%transmute(
  Age_group=factor(Age_group,levels=c(1,2),labels=c('<60','>=60')),
  Race=factor(Race,levels=c(1,2,3),labels=c('White','Black','Others')),
  Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
  time=`Survival months`,status,
  #status=factor(status,levels=c(0,1),labels=c('Alive','Dead')),
  stage=factor(stage,levels=c(1,2,3,4),labels=c('locaized','regional','distans','UNstage')),
  Grade=factor(Grade,levels=c(1,2,3,4),labels=c('I','II','III','IV')),
  Surgery=factor(Surgery,levels=c(0,1),labels=c('No','Yes')),
  Radiation=factor(Radiation,levels=c(0,1),labels=c('No','Yes')),
  Chemotherapy=factor(Chemotherapy,levels=c(0,1),labels=c('No','Yes')),
  tumor_size=factor(tumor_size,levels=c(1,2,3,4,5),labels=c('<=2','<=3','<=4','<=5','>5')),
  meta=factor(meta,levels=c(0,1),labels=c('No','Yes')),
  COD=`COD to site rec KM`,status_spec=`SEER cause-specific death classification`,
  status_other=`SEER other cause of death classification`
)
summary(EGJ2)
###multi-cox
cox<-coxph(Surv(time,status)~Age_group+Race+Sex+stage+
             Grade+Surgery+Radiation+Chemotherapy+tumor_size+meta,data=EGJ2)
summary(cox)
cox_new<-coxph(Surv(time,status)~Age_group+Race+stage+
                 Grade+Surgery+Chemotherapy+tumor_size+meta,data=EGJ2)
##nomogram
ddist <- datadist(EGJ2)
options(datadist='ddist')
units(EGJ2$time)<-'Months'
cox2<-cph(Surv(time,status)~Age_group+Race+Sex+stage+
             Grade+Surgery+Radiation+Chemotherapy+tumor_size+meta,surv=T,x=T,y=T,data=EGJ2) 
#cox_new<-cph(Surv(time,status)~Age_group+Race+stage+
#               Grade+Surgery+Chemotherapy+tumor_size+meta,surv=T,x=T,y=T,data=EGJ2) 

surv<-Survival(cox2)
#surv<-Survival(cox_new)
sur_3_year<-function(x)surv(1*12*3,lp=x)
sur_5_year<-function(x)surv(1*12*5,lp=x)
nom_sur <- nomogram(cox2,fun=list(sur_3_year,sur_5_year),lp= F,funlabel=c('3-Year Survival','5-Year survival'),maxscale=100,fun.at=c('0.9','0.8','0.7','0.6','0.5','0.4','0.3','0.2','0.1'))
#nom_sur <- nomogram(cox_new,fun=list(sur_3_year,sur_5_year),lp= F,funlabel=c('3-Year Survival','5-Year survival'),maxscale=100,fun.at=c('0.9','0.8','0.7','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nom_sur,xfrac=0.25)
#
#计算C-index
coxpe<-predict(cox2)
c_index=1-rcorr.cens(coxpe,Surv(EGJ2$time,EGJ2$status))
c_index
#
summary(cox)
##计算ROC
cox_1<-step(cox,direction = "both")
EGJ2$risk_score<-predict(cox_1,type="risk",newdata=EGJ2)
EGJ2$risk_level<-as.vector(ifelse(EGJ2$risk_score>median(EGJ2$risk_score),"High","Low"))
#3_year
predict_time<-12*3 
myroc<-survivalROC(Stime=EGJ2$time, status=EGJ2$status, marker=EGJ2$risk_score, predict.time=predict_time,method="KM")
plot(myroc$FP,myroc$TP,type="l",xlim=c(0,1),ylim=c(0,1),col="blue", 
     xlab="FP",ylab="TP",main=paste("3-year Survival","AUC=",round(myroc$AUC,3)))
abline(0,1)
#5_year
predict_time<-12*5 #如果是天数365*5
myroc<-survivalROC(Stime=EGJ2$time, status=EGJ2$status, marker=EGJ2$risk_score, predict.time=predict_time,method="KM")
plot(myroc$FP,myroc$TP,type="l",xlim=c(0,1),ylim=c(0,1),col="blue", 
     xlab="FP",ylab="TP",main=paste("5-year Survival","AUC=",round(myroc$AUC,3)))
abline(0,1)
##Calibration
#3_year
cox3<-cph(Surv(time,status)~Age_group+Race+Sex+stage+
            Grade+Surgery+Radiation+Chemotherapy+tumor_size+meta,surv=T,x=T,y=T,time.inc = 1*12*3,data=EGJ2) 
cal<-calibrate(cox3,cmethod="KM",method="boot",u=1*12*3,m=2453,B=100)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 3-Year Survival",ylab="Actual 3-Year Survival",col="blue")
box(lwd = 1)
abline(0,1,lty = 3,lwd = 1,col = "black")
#5_year
cox1 <- cph(Surv(time,status)~Age_group+Race+Sex+stage+
              Grade+Surgery+Radiation+Chemotherapy+tumor_size+meta,surv=T,x=T, y=T,time.inc = 1*12*5,data=EGJ2) 
cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*12*5, m= 2453, B=100)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 5-Year Survival",ylab="Actual 5-Year Survival",col="blue")
box(lwd = 1)
abline(0,1,lty = 3,lwd = 1,col = "black")

##K-M
#by risk
kms<-survfit(Surv(time,status)~risk_level,data=EGJ2)
kmdffexp=survdiff(Surv(time,status)~risk_level,data=EGJ2)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
plot(kms,lty="solid",col=c("red","green"),
     xlab="Survival time in months",ylab="Survival probabilities",
     main=paste("Surival curve of risk score(P=",'<0.01',")",sep=""))
legend("topright",c("High risk","Low risk"),lty="solid",col=c("red","green"))
#by age
kms<-survfit(Surv(time,status)~Age_group,data=EGJ2)
kmdffexp=survdiff(Surv(time,status)~Age_group,data=EGJ2)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
plot(kms,lty="solid",col=c("red","green"),
     xlab="Survival time in months",ylab="Survival probabilities",
     main=paste("Surival curve of age (P", '<0.01' ,")",sep=""))
legend("topright",c("<60",">=60"),lty="solid",col=c("red","green"))
##by gender
kms<-survfit(Surv(time,status)~Sex,data=EGJ2)
kmdffexp=survdiff(Surv(time,status)~Sex,data=EGJ2)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
plot(kms,lty="solid",col=c("red","green"),
     xlab="Survival time in months",ylab="Survival probabilities",
     main=paste("Surival curve of gender (P=", pValue ,")",sep=""))
legend("topright",c("Male","Female"),lty="solid",col=c("red","green"))
#by race
kms<-survfit(Surv(time,status)~Race,data=EGJ2)
kmdffexp=survdiff(Surv(time,status)~Race,data=EGJ2)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
plot(kms,lty="solid",col=c("red","green","blue"),
     xlab="Survival time in months",ylab="Survival probabilities",
     main=paste("Surival curve of race (P",'<0.01',")",sep=""))
legend("topright",c("White","Black","Other"),lty="solid",col=c("red","green","blue"))



