library(rio)
library(tidyverse)
#biomarker<-read.xlsx('~/data/biomarker/Biomarker+baseline(2017+18+19).xlsx',detectDates=TRUE,sheet=1)
biomarker<-import('~/data/biomarker/Biomarker+baseline(2017+18+19).sav')
#剔除自身癌
biomarker$ca[biomarker$CASelf==2 | !is.na(biomarker$CATPSelf)]<-2
biomarker<-biomarker%>%filter(is.na(ca))
#age
biomarker$age_group[biomarker$age>=40 & biomarker$age<45]<-1
biomarker$age_group[biomarker$age>=45 & biomarker$age<50]<-2
biomarker$age_group[biomarker$age>=50 & biomarker$age<55]<-3
biomarker$age_group[biomarker$age>=55 & biomarker$age<60]<-4
biomarker$age_group[biomarker$age>=60 & biomarker$age<65]<-5
biomarker$age_group[biomarker$age>=65 & biomarker$age<70]<-6
biomarker$age_group[biomarker$age>=70]<-7
biomarker$age_group2[biomarker$age>=40 & biomarker$age<50]<-1
biomarker$age_group2[biomarker$age>=50 & biomarker$age<60]<-2
biomarker$age_group2[biomarker$age>=60 & biomarker$age<70]<-3
biomarker$age_group2[biomarker$age>=70]<-4
#baonian
biomarker$baonian<-(biomarker$cpd*biomarker$smkyrs)/20
biomarker$baonian2<-ifelse(biomarker$baonian>quantile(biomarker$baonian,0.75,na.rm = TRUE)+IQR(biomarker$baonian,na.rm = TRUE),NA,biomarker$baonian)
#---BMI
biomarker$bmi<-with(biomarker,weight/((height/100)^2))
#中国人标准
biomarker$bmi_group<-with(biomarker,case_when(
  bmi<18.5 ~ 1,
  between(bmi,18.5,23.9) ~ 2,
  between(bmi,24,27.9) ~ 3,
  bmi>=28 ~ 4
))
#亚洲人标准
biomarker$bmi_group3<-with(biomarker,case_when(
  bmi<=22.9 ~ 1,
  between(bmi,23,27.4) ~2,
  bmi>=27.5 ~3
))
biomarker$bmi_group2<-with(biomarker,ifelse(bmi<25,1,2))
#CEA
biomarker$CEA_pos<-factor(ifelse(biomarker$CEA>5,2,1),labels=c('Negative','Positive'))
#AFP
biomarker$AFP_pos<-factor(ifelse(biomarker$AFP>7,2,1),labels=c('Negative','Positive'))
#CA199
biomarker$CA199_pos<-factor(ifelse(biomarker$CA199>27,2,1),labels=c('Negative','Positive'))
#CA153
biomarker$CA153_pos<-factor(ifelse(biomarker$CA153>25,2,1),labels=c('Negative','Positive'))
biomarker$CA153<-ifelse(biomarker$CA153>0,biomarker$CA153,NA)
#HBsAg
#=0为阴性，>0为阳性
biomarker$HBsAg_group<-ifelse(biomarker$HBsAg>0,2,1)
#数据集
#biomarker2<-biomarker%>%filter(AFP<=quantile(AFP,0.75)+IQR(AFP))#剔除极端值
#fucntion
percent_value<-function(x){
  p1<-round(quantile(x,0.01,na.rm=TRUE),2)
  p2.5<-round(quantile(x,0.025,na.rm=TRUE),2)
  p5<-round(quantile(x,0.05,na.rm=TRUE),2)
  p10<-round(quantile(x,0.1,na.rm=TRUE),2)
  p25<-round(quantile(x,0.25,na.rm=TRUE),2)
  p50<-round(quantile(x,0.5,na.rm=TRUE),2)
  p75<-round(quantile(x,0.75,na.rm=TRUE),2)
  p90<-round(quantile(x,0.9,na.rm=TRUE),2)
  p95<-round(quantile(x,0.95,na.rm=TRUE),2)
  p97.5<-round(quantile(x,0.975,na.rm=TRUE),2)
  p98<-round(quantile(x,0.98,na.rm=TRUE),2)
  p99<-round(quantile(x,0.99,na.rm=TRUE),2)
  table<-c(p1,p2.5,p5,p10,p25,p50,p75,p90,p95,p97.5,p98,p99)
  return(table)
}
