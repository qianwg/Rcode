library(rio)
library(tidyverse)
#biomarker<-read.xlsx('~/data/biomarker/Biomarker+baseline(2017+18+19).xlsx',detectDates=TRUE,sheet=1)
biomarker<-import('~/data/biomarker/Biomarker+baseline(2017+18+19).sav')
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
#AFP
biomarker$AFP_pos<-factor(ifelse(biomarker$AFP>7,2,1),labels=c('Negative','Positive'))
#CA199
biomarker$CA199_pos<-factor(ifelse(biomarker$CA199>27,2,1),labels=c('Negative','Positive'))
#CA153
biomarker$CA199_pos<-factor(ifelse(biomarker$CA199>25,2,1),labels=c('Negative','Positive'))
#HBsAg
#=0为阴性，>0为阳性
biomarker$HBsAg_group<-ifelse(biomarker$HBsAg>0,2,1)
#数据集
#biomarker2<-biomarker%>%filter(AFP<=quantile(AFP,0.75)+IQR(AFP))#剔除极端值
