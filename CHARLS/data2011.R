rm(list=ls())
library(rio)
library(tidyverse)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#读取数据
#
biomarkers<-import('~/data/CHARLS/CHARLS2011/biomarkers.dta')
demograph<-import('~/data/CHARLS/CHARLS2011/demographic_background.dta')
health<-import('~/data/CHARLS/CHARLS2011/health_status_and_functioning.dta')
blood<-import('~/data/CHARLS/CHARLS2011/Blood_20140429.dta')
health_care<-import('~/data/CHARLS/CHARLS2011/health_care_and_insurance.dta')
#individual_income<-import('~/data/CHARLS/individual_income.dta')
#work_retirement<-import('~/data/CHARLS/work_retirement_and_pension.dta')
household<-import('~/data/CHARLS/CHARLS2011/household_roster.dta')
##
demograph2<-demograph%>%transmute(
  ID2=ID,
  #ID=paste(householdID,'0',substr(ID,-2,2),sep=""),
  #人口学信息
  age=(2011-ba002_1),age2=ba004,sex=factor(rgender,levels=c(1,2),labels=c('Male','Female')),
  education=factor(case_when(
    bd001==1 ~ 1,
    between(bd001,2,4) ~ 2,
    between(bd001,5,6) ~ 3,
    bd001>=7 ~ 4),levels=c(1,2,3,4),labels=c('No formal education','Primary school','Middle or high school','College or above')),
  marriag=factor(case_when(be001<=2 ~ 1,
                    between(be001,3,5) ~ 2,
                    be001==6 ~ 3),levels=c(1,2,3),labels=c('Married','Separated/Divorced/Widowed','Never married'))
)
health2<-health%>%transmute(
  ID2=ID,
  #ID=paste(householdID,'0',substr(ID,-2,2),sep=""),
  #疾病史
  Hypertension=factor(ifelse(da007_1_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Dyslipidemia=factor(ifelse(da007_2_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Diabetes=factor(ifelse(da007_3_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Cancer=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('否','是')),
  lung_diseases=factor(ifelse(da007_5_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Liver_disease=factor(ifelse(da007_6_==1,2,1),levels=c(1,2),labels=c('否','是')),
  heart_disease=factor(ifelse(da007_7_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Stroke=factor(ifelse(da007_8_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Kidney_disease=factor(ifelse(da007_9_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Stomach_disease=factor(ifelse(da007_10_==1,2,1),levels=c(1,2),labels=c('否','是')),
  psychiatric_problems=factor(ifelse(da007_11_==1,2,1),levels=c(1,2),labels=c('否','是')),
  Memory_disease=factor(ifelse(da007_12_==1,2,1),levels=c(1,2),labels=c('否','是')),
  #女性生理生育因素
  menopause=da027,age_menarche1=da026_1,age_menarche2=da026_2,
  #癌症诊断时间
  cancer_dig_time1=da009_1_4_,cancer_dig_time2=da009_2_4_,
  #癌症诊断类型
  #respir(呼吸)、breast(乳房)、colorect(结肠&直肠)、urinary(泌尿)、digothr(消化道)、
  #malegen(男性生殖)、femgen(女性生殖)、lymyleuk(淋巴&白血病)、other(其他)
  cancer_brain=factor(ifelse(da017s1==1 & !is.na(da017s1),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_oral=factor(ifelse(da017s2==2 & !is.na(da017s2),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Larynx=factor(ifelse(da017s3==3 & !is.na(da017s3),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_pharynx=factor(ifelse(da017s4==4 & !is.na(da017s4),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Thyroid=factor(ifelse(da017s5==5 & !is.na(da017s5),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Lung=factor(ifelse(da017s6==6 & !is.na(da017s6),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Breast=factor(ifelse(da017s7==7 & !is.na(da017s7),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Oesophagus=factor(ifelse(da017s8==8 & !is.na(da017s8),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Stomach=factor(ifelse(da017s9==9 & !is.na(da017s9),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Liver=factor(ifelse(da017s10==10 & !is.na(da017s10),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Pancreas=factor(ifelse(da017s11==11 & !is.na(da017s11),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Kidney=factor(ifelse(da017s12==12 & !is.na(da017s12),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Prostate=factor(ifelse(da017s13==13 & !is.na(da017s13),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Testicle=factor(ifelse(da017s14==14 & !is.na(da017s14),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Ovary=factor(ifelse(da017s15==15 & !is.na(da017s15),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Cervix=factor(ifelse(da017s16==16 & !is.na(da017s16),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Endometrium=factor(ifelse(da017s17==17 & !is.na(da017s17),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Colon=factor(ifelse(da017s18==18 & !is.na(da017s18),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Bladder=factor(ifelse(da017s19==19 & !is.na(da017s19),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Skin=factor(ifelse(da017s20==20 & !is.na(da017s20),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Non_Hodgkin=factor(ifelse(da017s21==21 & !is.na(da017s21),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Leukemia=factor(ifelse(da017s22==22 & !is.na(da017s22),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Other=factor(ifelse(da017s23==23 & !is.na(da017s23),2,1),levels = c(1,2),labels=c('NO','YES')),
  respir_cancer=factor(ifelse(cancer_oral=="YES" | cancer_Larynx=="YES" | cancer_Larynx=="YES" | cancer_Lung=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  breast_cacner=cancer_Breast,
  colorect_cacner=cancer_Colon,
  urinary_cancer=factor(ifelse(cancer_Kidney=="YES" | cancer_Prostate=="YES" | cancer_Testicle=="YES" | cancer_Bladder=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  lymyleuk_cacner=factor(ifelse(cancer_Non_Hodgkin=="YES" | cancer_Leukemia=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  femgen_cancer=factor(ifelse(cancer_Ovary=="YES" | cancer_Cervix=="YES" | cancer_Endometrium=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  Other=factor(ifelse(cancer_Thyroid=="YES" | cancer_Skin=="YES" | cancer_Other=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  
  #Receiving treatment
  da020s1,da020s2,da020s3,da020s4,
  #smoking
  smoking=factor(case_when(
    da059==2 ~ 1,
    da059==1 & da061==2 ~ 2,
    da059==1 & da061==1 ~ 3,
  ),levels=c(1,2,3),labels=c('Never','Ever','Current')),
  #alcohol
  smoking=factor(case_when(
    da067==3 & da069==1 ~ 1,
    da067==3 & da069>1 ~ 2,
    da067<=2 ~ 3,
  ),levels=c(1,2,3),labels=c('Never','Ever','Current')),
  #抑郁(≥12)
  dep1=dc009-1,dep2=dc010-1,dep3=dc011-1,dep4=dc012-1,
  dep6=dc014-1,dep7=dc015-1,dep8=dc017-1,dep10=dc018-1,
  dep5=abs(dc013-4),dep8=abs(dc016-4),
)
biomarkers2<-biomarkers%>%transmute(
  ID2=ID,
  #ID=paste(householdID,'0',substr(ID,-2,2),sep=""),
  #身高(无法进行测试，请填写993。如果受试者选择不进行测试，请填写999)
  height=ifelse(qi002==993,NA,qi002),
  #体重(显示错误信息，请填写993)
  weight=ql002,
  #腰围(显示错误信息，请填写993。如果受试者选择不进行测试，请填写999)
  waist=ifelse(qm002>=993,NA,qm002),
  BMI=10000*height/(weight*weight),
  BMI_group1=case_when(
    BMI<24  ~ 1,#正常
    BMI>=24 & BMI<28 ~ 2,#超重
    BMI>=28 ~ 3#肥胖
  ),
  BMI_group1=factor(BMI_group1,levels = c(1,2,3),labels=c('正常','超重','肥胖')),
  BMI_group2=case_when(
    BMI<18.5 ~ 1, #偏瘦
    BMI<24 & BMI>=18.5  ~ 2,#正常
    BMI<28 & BMI>=24 ~ 3,#超重
    BMI>=28 ~ 4#肥胖
  ),
  BMI_group2=factor(BMI_group2,levels = c(1,2,3,4),labels=c('偏瘦','正常',"超重",'肥胖')),
)
blood2<-blood%>%transmute(
  ID2=ID,
  #ID=paste(householdID,'0',substr(ID,-2,2),sep=""),
  bloodweight,qc1_va003,White=qc1_vb002,MCV=qc1_vb006,Platelet=qc1_vb009,
  BUN=newbun,Glucose=newglu,Creatinine=newcrea,TC=newcho,Triglycerides=newtg,HDL=newhdl,LDL=newldl,CRP=newcrp,Hemoglobin=newhba1c,
  Uric_Acid=newua,Hematocrit=qc1_vb005,Hemoglobin=qc1_vb004,cystatinc
)
health_care2<-health_care%>%transmute(
  ID2=ID,
  #ID=paste(householdID,'0',substr(ID,-2,2),sep=""),
)
#individual_income2<-individual_income%>%transmute(
#  householdID6=householdID,ID=ID,communityID6=communityID
#)
data<-Reduce(function(x,y)merge(x,y,by='ID2',all=TRUE),
  list(demograph2,health2,biomarkers2,blood2,health_care2))%>%
  mutate(ID=paste0(substr(ID2,1,9),'0',substrRight(ID2,2)),
         )
#export(data,'~/CHarls.xlsx')
#biomarkers,demograph,health,blood,health_care
###个人数
#summary(data[,c('ID','householdID')])
#家庭数量
#nrow(data[which(!duplicated(data$householdID)),])
##验证
#summary(data[,1:12])
rm(biomarkers,biomarkers2,blood,blood2,demograph,demograph2,health2,health,health_care,health_care2,household,)

#####+随访信息
source('~/Rcode/CHARLS/follow-up.R')
data_followup<-left_join(data,followup,by='ID')





