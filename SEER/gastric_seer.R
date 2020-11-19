rm(list=ls())
library(rio)
library(tidyverse)
library(survival)
library(survminer)
stomach<-import('~/data/GASTRIC_SEER.sav')
#survival_months_flag
#1-Complete dates are available and there are 0 days of survival
#2-Complete dates are available and there are more than 0 days of survival
#3-Incomplete dates are available and there cannot be zero days of follow-up
#4-Incomplete dates are available and there could be zero days of follow-up
#5-Not calculated because a Death Certificate Only or Autopsy Only case
######
#summary(stomach)
  stomach_surv<-stomach%>%transmute(
  Age=Age_at_diagnosis,
  Race=factor(Race_recode_White_Black_Other,levels=c(1,2,3),labels=c('Black','Other','White')),
  Race2=factor(Race_recode_W_B_AI_API,levels=c(1,2,3,4,5),labels=c('American Indian/Alaska Native','Asian or Pacific Islander','Black','Unknown','White')),
    Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
  Year_diag=Year_of_diagnosis,
  #婚姻状况
  Marital_status=factor(case_when(
    Marital_status_at_diagnosis==2 | Marital_status_at_diagnosis==6 ~ 1,
    Marital_status_at_diagnosis==1 | Marital_status_at_diagnosis==3 ~ 2,
    Marital_status_at_diagnosis==4 ~ 3,
    Marital_status_at_diagnosis==7 ~ 4,
    Marital_status_at_diagnosis==5 ~ 5
    ),levels=seq(5),labels=c('Married or partner','Separated/divorced','Single','Widowed','Unknown')),
  #发病部位信息
  primary_site=factor(
    case_when(
      Primary_Site==160 ~ 1,
      Primary_Site==161 ~ 2,
      Primary_Site==162 ~ 3,
      Primary_Site==163 ~ 4,
      Primary_Site==164 ~ 5,
      Primary_Site==165 ~ 6,
      Primary_Site==166 ~ 7,
      Primary_Site==168 ~ 8,
      Primary_Site==169 ~ 9
    ),levels = seq(9),labels=c('Cardia','Fundus of stomach',
                               'Body of stomach','Gastric antrum','Pylorus','Lesser curvature of stomach',
                               'Greater curvature of stomach','Overlapping lesion of stomach','Stomach')),
  #组织学类型
  histologic_type=Histologic_Type_ICD_O_3,
  #肿瘤分级
  grade=factor(Grade,levels = seq(5),labels=c('Grade I','Grade II','Grade III','Grade IV','Unknown')),
  grade=factor(ifelse(Grade<=2,1,ifelse(Grade<=4,2,3)),levels=c(1,2,3),labels=c('Grade I-II','Grade III-IV','Unknown')),
  #手术信息、放化疗
  Surg_Prim_Site=factor(RX_Summ_Surg_Prim_Site_1998),chemo=factor(Chemotherapy_recode_yes_no_unk,levels = seq(2),labels=c('No/Unknown','Yes')),
  radiation=factor(ifelse(Radiation_recode<=7,2,1),levels=c(1,2),labels=c('None/Unknown','Yes')),
  #淋巴结信息
  nodes_examined=ifelse(Regional_nodes_examined_1988<=90,Regional_nodes_examined_1988,NA),nodes_pos=ifelse(Regional_nodes_positive_1988<=90,Regional_nodes_positive_1988,NA),
  nodes_group=factor(ifelse(nodes_examined<=14,1,2),levels=c(1,2),labels=c('<=14','>=15')),
  metastasis_Ln=factor(Mets_at_DX_Distant_LN_2016,levels=c(1,2,3),labels=c('None','Yes','Unknown')),
  #淋巴结阳性率
  LPR=nodes_pos/nodes_examined,
  #生存时间、随访时间等信息
  OS=Survival_months,Survival_months_flag,Type_of_follow_up_expected,Type_of_Reporting_Source,
  #多元发
  Sequence_number=factor(
    case_when(Sequence_number==1 ~ 1,
              Sequence_number==2 ~ 2,
              Sequence_number==3 ~ 3,
              Sequence_number>=4 ~ 4
              ),levels=seq(4),labels=c('One primary only','1st of 2 or more primaries','2nd of 2 or more primaries','nth of n or more primaries')),
  #诊断信息
  Diagnostic_Confirmation=factor(
    ifelse(Diagnostic_Confirmation==4,2,1),levels=c(1,2),labels=c('Others','Positive histology')
  ),COD=COD_to_site_rec_KM,
  #癌症特异性死亡
  end1=factor(SEERcausespecificdeathclassifi,levels=c(0,1,2),labels=c('Alive or dead of other cause','Dead (attributable to this cancer dx)','Dead (missing/unknown COD)')),
  status1=ifelse(SEERcausespecificdeathclassifi==1,1,0),
  #死亡相关结局
  end2=factor(SEERothercauseofdeathclassific,levels=c(0,1,2),labels=c('Alive or dead due to cancer','Dead (attributable to causes other than this cancer dx)','Dead (missing/unknown COD)')),
  #全死因死亡
  status2=factor(ifelse(Vitalstatusrecodestudycutoffus==2,1,0),levels=c(0,1),labels=c('Alive','Dead')),
   #转移
  metastasis=factor(ifelse(CS_mets_at_dx_2004_2015==0,1,ifelse(CS_mets_at_dx_2004_2015==99,3,2)),levels = c(1,2,3),labels=c('No','Yes','Unknown')),
  metastasis_bone=factor(SEERCombinedMetsatDX_bone_2010,levels=c(1,2,3),labels=c('No','Yes','Unknown')),
  metastasis_brain=factor(SEERCombinedMetsatDXbrain_2010,levels=c(1,2,3),labels=c('No','Yes','Unknown')),
  metastasis_liver=factor(SEERCombinedMetsatDXliver_2010,levels=c(1,2,3),labels=c('No','Yes','Unknown')),
  metastasis_lung=factor(SEERCombinedMetsatDX_lung_2010,levels=c(1,2,3),labels=c('No','Yes','Unknown')),
  #CS
  CS1=CSsitespecificfactor12004varyi,CS6=CSsitespecificfactor62004varyi,CS25=CSsitespecificfactor252004vary,
  #肿瘤大小、浸润程度
  tumor_size=CS_tumor_size_2004_2015,extension=CS_extension_2004_2015,
  tumor_size2=factor(
    case_when(
      tumor_size<=2 ~ 1,
      tumor_size<=3 & tumor_size>2 ~ 2,
      tumor_size<=5 & tumor_size>3 ~ 3,
      tumor_size>5 & tumor_size<999~ 4
    ),levels=seq(4),labels=c('<=2','<=3','<=5','>5')
  )
  #TNM分期
  
  
)
summary(stomach_surv)
#
#1、Sequence_number==1
#1、Positive histology:Diagnostic_Confirmation="Positive histology"
#2、Survival_months_flag==2
#3、Type_of_follow_up_expected==1
#4、Type_of_Reporting_Source>=3
#5、"Unknown":!is.na(OS)
stomach_surv2<-stomach_surv%>%filter(Sequence_number=='One primary only' & Survival_months_flag==2 & 
                                       Diagnostic_Confirmation=="Positive histology" & Type_of_follow_up_expected==1 & Type_of_Reporting_Source>=3 & !is.na(OS) & OS>0)
summary(stomach_surv2)
########topic：Luaren Classification
#Lauren Classification:histologic type---
#intestinal type(M8140、M8211、M8010、M8144)；diffuse type(M8145、M8490、M8142)
stomach_surv3<-stomach_surv2%>%filter(histologic_type==8140 | histologic_type==8211 | histologic_type==8010 |
                         histologic_type==8144 | histologic_type==8145 | histologic_type==8490 | histologic_type==8142)%>%
  mutate(Lauren=factor(ifelse(histologic_type==8140 | histologic_type==8211 | histologic_type==8010 |
                         histologic_type==8144,2,1),levels = c(1,2),labels=c('diffuse','intestinal')))
summary(stomach_surv3)
#survival--cancer-special death
fit<-survfit(Surv(OS,status1)~Lauren,data=stomach_surv3)
ggsurvplot(fit,linetype = c('solid', 'dashed'),pval = T, risk.table = T, palette = 'Set2',data=stomach_surv3)

##去除种族、分级、淋巴结信息、肿瘤大小不全的
stomach_surv4<-stomach_surv3%>%filter(!is.na(Race),grade!='Unknown',!is.na(nodes_examined),!is.na(nodes_pos),!is.na(tumor_size))
fit<-survfit(Surv(OS,status1)~Lauren,data=stomach_surv4)
ggsurvplot(fit,linetype = c('solid', 'dashed'),pval = T, risk.table = T, palette = 'Set2',data=stomach_surv4)





