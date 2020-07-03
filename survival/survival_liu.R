rm(list=ls())
library(rio)
source('~/Rcode/statistics/HR.R')
survival<-import('~/data/survival_liu.xlsx')
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
survival2<-survival%>%transmute(
  gender=factor(Sex...7,levels = c(1,2),labels=c('male','female')),
  age=factor(ifelse(age...8>=60,2,1),levels=c(1,2),labels=c('<60','>=60')),
  smoke=factor(smoke...10),
  alcohol=factor(alcohol),
  path=factor(path_type),
  Differentiation=factor(Differentiation),
  location=factor(Location...14),
  size=factor(size),
  ln=factor(LN...16),
  clinical_stage=factor(stage),
  mir_125b=factor(ifelse(miR_125b>quantile(survival$miR_125b,0.5,na.rm=TRUE),2,1),levels=c(1,2),labels=c('low','high')),
  caspase_6=`caspase-6分组后`,
  BIK=BIK分组,
  AR=factor(ifelse(`AR expression`>median(`AR expression`,na.rm=TRUE),2,1),levels=c(1,2),labels=c('low','high')),
  AR_125b=factor(ifelse(mir_125b=="high" & AR=="high",2,1),levels = c(1,2),labels=c('low','high')),
  OS=`2016OS`,
  status=ifelse(`2016event`==2,1,0),
  )%>%filter(!is.na(status),!is.na(smoke))
#survival2[which(is.na(survival2$OS)),c('OS','status')]
#summary(survival2)
table1::table1(~gender+age+smoke+alcohol+path+Differentiation+location+size+ln+clinical_stage+
               mir_125b+caspase_6+BIK+AR+AR_125b | status,data=survival2,render.categorical=my.render.cat)
cox(y='Surv(OS,status)',
    x=c('gender','age','smoke','alcohol','path','Differentiation','location','size','ln','clinical_stage',
        'mir_125b','caspase_6','BIK','AR','AR_125b'),data=survival2)
coxph(Surv())
