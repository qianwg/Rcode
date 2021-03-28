rm(list=ls())
library(rio)
library(tidyverse)
#读取数据
#
biomarkers<-import('~/data/CHARLS/CHARLS2015/Biomarker.dta')
demograph<-import('~/data/CHARLS/CHARLS2015/Demographic_Background.dta')
health<-import('~/data/CHARLS/CHARLS2015/Health_Status_and_Functioning.dta')
health_care<-import('~/data/CHARLS/CHARLS2015/Health_Care_and_Insurance.dta')
#Exit_interview<-import('~/data/CHARLS/CHARLS2013/Exit_Interview.dta')
#individual_income<-import('~/data/CHARLS/individual_income.dta')
#work_retirement<-import('~/data/CHARLS/work_retirement_and_pension.dta')
#household<-import('~/data/CHARLS/household_roster.dta')
weight<-import('~/data/CHARLS/CHARLS2015/Weights.dta')
Blood<-import('~/data/CHARLS/CHARLS2015/Blood.dta')
Sample_infor<-import('~/data/CHARLS/CHARLS2015/Sample_Infor.dta')
##
##
demograph2<-demograph%>%transmute(
  ID=ID,
  #受访者类型(回访受访者/新受访者)
  xrtype_demograph=factor(xrtype,levels = c(1,2),labels = c('New Interview','Followup')),
  #人口学信息
  sex_demograph_2013=factor(ba000_w2_3,levels=c(1,2),labels=c('Male','Female')),
  #出生日期更新
  #birth_date_2013=paste(ba002_1,ba002_2,ba002_3,sep='-'),
  #公历还是农历
  #ba003_2013,
  #学历更新
  education_2013=factor(case_when(
    bd001_w2_4==1 ~ 1,
    between(bd001_w2_3,2,4) ~ 2,
    between(bd001_w2_3,5,6) ~ 3,
    between(bd001_w2_3,7,11) ~ 4,
    bd001_w2_4==12 ~ 5),levels=c(1,2,3,4,5),
    labels=c('No formal education','Primary school','Middle or high school','College or above','No changing')),
  
  marriag_2013=factor(case_when(be001<=2 ~ 1,
                                between(be001,3,5) ~ 2,
                                be001>=6 ~ 3),levels=c(1,2,3),labels=c('Married','Separated/Divorced/Widowed','Never married'))
)

health2<-health%>%transmute(
  ID=ID,xrtype_health=factor(xrtype,levels = c(1,2),labels = c('New Interview','Followup')),
  sex_health=factor(xrgender,levels=c(1,2),labels=c('Male','Female')),
  #新的受访者的疾病史
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
  ##回访者上次疾病史记录是否正确
  Hypertension_agree_2011=factor(da007_w2_1_1_,levels=c(1,2),labels=c('Agree','Disagree')),
  Dyslipidemia_agree_2011=factor(da007_w2_1_2_,levels=c(1,2),labels=c('Agree','Disagree')),
  Diabetes_agree_2011=factor(da007_w2_1_3_,levels=c(1,2),labels=c('Agree','Disagree')),
  Cancer_agree_2011=factor(da007_w2_1_4_,levels=c(1,2),labels=c('Agree','Disagree')),
  lung_diseases_agree_2011=factor(da007_w2_1_5_,levels=c(1,2),labels=c('Agree','Disagree')),
  Liver_disease_agree_2011=factor(da007_w2_1_6_,levels=c(1,2),labels=c('Agree','Disagree')),
  heart_disease_agree_2011=factor(da007_w2_1_7_,levels=c(1,2),labels=c('Agree','Disagree')),
  Stroke_agree_2011=factor(da007_w2_1_8_,levels=c(1,2),labels=c('Agree','Disagree')),
  Kidney_disease_agree_2011=factor(da007_w2_1_9_,levels=c(1,2),labels=c('Agree','Disagree')),
  Stomach_disease_agree_2011=factor(da007_w2_1_10_,levels=c(1,2),labels=c('Agree','Disagree')),
  psychiatric_problems_agree_2011=factor(da007_w2_1_11_,levels=c(1,2),labels=c('Agree','Disagree')),
  Memory_disease_agree_2011=factor(da007_w2_1_12_,levels=c(1,2),labels=c('Agree','Disagree')),
  #正确是否近两年有医生告诉你患有次疾病
  da007_w2_2=factor(da007_w2_2,levels=c(1,2),labels=c('Yes','No')),
  #癌症诊断时间
  cancer_dig_time1=da009_1_4_,cancer_dig_time2=da009_2_4_,
  #癌症诊断类型
  #respir(呼吸)、breast(乳房)、colorect(结肠&直肠)、urinary(泌尿)、digothr(消化道)、
  #malegen(男性生殖)、femgen(女性生殖)、lymyleuk(淋巴&白血病)、other(其他)
  cancer_brain_2013=factor(ifelse(da017s1==1 & !is.na(da017s1),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_oral_2013=factor(ifelse(da017s2==2 & !is.na(da017s2),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Larynx_2013=factor(ifelse(da017s3==3 & !is.na(da017s3),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_pharynx_2013=factor(ifelse(da017s4==4 & !is.na(da017s4),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Thyroid_2013=factor(ifelse(da017s5==5 & !is.na(da017s5),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Lung_2013=factor(ifelse(da017s6==6 & !is.na(da017s6),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Breast_2013=factor(ifelse(da017s7==7 & !is.na(da017s7),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Oesophagus_2013=factor(ifelse(da017s8==8 & !is.na(da017s8),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Stomach_2013=factor(ifelse(da017s9==9 & !is.na(da017s9),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Liver_2013=factor(ifelse(da017s10==10 & !is.na(da017s10),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Pancreas_2013=factor(ifelse(da017s11==11 & !is.na(da017s11),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Kidney_2013=factor(ifelse(da017s12==12 & !is.na(da017s12),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Prostate_2013=factor(ifelse(da017s13==13 & !is.na(da017s13),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Testicle_2013=factor(ifelse(da017s14==14 & !is.na(da017s14),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Ovary_2013=factor(ifelse(da017s15==15 & !is.na(da017s15),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Cervix_2013=factor(ifelse(da017s16==16 & !is.na(da017s16),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Endometrium_2013=factor(ifelse(da017s17==17 & !is.na(da017s17),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Colon_2013=factor(ifelse(da017s18==18 & !is.na(da017s18),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Bladder_2013=factor(ifelse(da017s19==19 & !is.na(da017s19),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Skin_2013=factor(ifelse(da017s20==20 & !is.na(da017s20),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Non_Hodgkin_2013=factor(ifelse(da017s21==21 & !is.na(da017s21),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Leukemia_2013=factor(ifelse(da017s22==22 & !is.na(da017s22),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Other_2013=factor(ifelse(da017s23==23 & !is.na(da017s23),2,1),levels = c(1,2),labels=c('NO','YES')),
  respir_cancer_2013=factor(ifelse(cancer_oral_2013=="YES" | cancer_Larynx_2013=="YES" | cancer_Larynx_2013=="YES" | cancer_Lung_2013=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  breast_cacner_2013=cancer_Breast_2013,
  colorect_cacner_2013=cancer_Colon_2013,
  urinary_cancer_2013=factor(ifelse(cancer_Kidney_2013=="YES" | cancer_Prostate_2013=="YES" | cancer_Testicle_2013=="YES" | cancer_Bladder_2013=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  lymyleuk_cacner_2013=factor(ifelse(cancer_Non_Hodgkin_2013=="YES" | cancer_Leukemia_2013=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  femgen_cancer_2013=factor(ifelse(cancer_Ovary_2013=="YES" | cancer_Cervix_2013=="YES" | cancer_Endometrium_2013=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  Other_2013=factor(ifelse(cancer_Thyroid_2013=="YES" | cancer_Skin_2013=="YES" | cancer_Other_2013=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
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
  ID=ID,
  #身高(无法进行测试，请填写993。如果受试者选择不进行测试，请填写999)
  height=ifelse(qi002>=993,NA,qi002),
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
health_care2<-health_care%>%transmute(
  ID=ID,
)
Exit<-Exit_interview%>%transmute(
  ID=ID,exda007_w2_1_4_,exda007_w2_1_5_,exda007_w2_1_7_,exda007_w2_1_8_,
  exda007_w2_1_11_,
  exda007_w2_2_4_,exda007_w2_2_5_,exda007_w2_2_7_,exda007_w2_2_8_,exda007_w2_2_11_,
  exda009_1_4_,exda009_2_4_,
  exda009_1_5_,exda009_2_5_,
  exda009_1_7_,exda009_2_7_,
  exda009_1_8_,exda009_2_8_,
  exda009_1_11_,exda009_2_11_,
)
weight2<-weight%>%transmute(
  ID,crosssection,INDV_weight,longitudinal,INDV_L_weight,
  HH_L_Died,INDV_L_Died,imonth,iyear
)%>%filter(ID!="")
#individual_income2<-individual_income%>%transmute(
#  householdID6=householdID,ID=ID,communityID6=communityID
#)
data<-Reduce(function(x,y)merge(x,y,by='ID',all=TRUE),
             list(demograph2,health2,biomarkers2,health_care2,Exit,weight2))%>%
  mutate(householdID=str_sub(ID,1,10),
         xrtype=ifelse(is.na(xrtype_demograph),xrtype_health,xrtype_demograph)
  )