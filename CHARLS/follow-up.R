###Follow up 


##2013年随访
health2013<-import('~/data/CHARLS/CHARLS2013/Health_Status_and_Functioning.dta')
Exit_interview2013<-import('~/data/CHARLS/CHARLS2013/Exit_Interview.dta')
weight2013<-import('~/data/CHARLS/CHARLS2013/Weights.dta')
health2013<-health2013%>%transmute(
  ID=ID,xrtype_health_2013=factor(xrtype,levels = c(1,2),labels = c('New Interview','Followup')),
  #疾病史
  Cancer_2013=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('否','是')),
  ##上次疾病史记录是否正确
  Cancer_agree__2013=factor(da007_w2_1_4_,levels=c(1,2),labels=c('Agree','Disagree')),
  #正确是否近两年有医生告诉你患有癌症
  dignosis_2013=factor(da007_w2_2_4_,levels=c(1,2),labels=c('Yes','No')),
  #癌症诊断时间
  cancer_dig_time1_2013=da009_1_4_,cancer_dig_time2_2013=da009_2_4_,
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
  #抑郁(≥12)
  dep1_2013=dc009-1,dep2_2013=dc010-1,dep3_2013=dc011-1,dep4_2013=dc012-1,
  dep6_2013=dc014-1,dep7_2013=dc015-1,dep8_2013=dc017-1,dep10_2013=dc018-1,
  dep5_2013=abs(dc013-4),dep8_2013=abs(dc016-4),
)

Exit2013<-Exit_interview2013%>%transmute(
  ID=ID,exda007_w2_1_4_,exda007_w2_1_5_,exda007_w2_1_7_,exda007_w2_1_8_,
  exda007_w2_1_11_,
  exda007_w2_2_4_,exda007_w2_2_5_,exda007_w2_2_7_,exda007_w2_2_8_,exda007_w2_2_11_,
  exda009_1_4_,exda009_2_4_,
  exda009_1_5_,exda009_2_5_,
  exda009_1_7_,exda009_2_7_,
  exda009_1_8_,exda009_2_8_,
  exda009_1_11_,exda009_2_11_,
)
weight2013<-weight2013%>%transmute(
  ID,crosssection2013=crosssection,INDV_weight_2013=INDV_weight,longitudinal_2013=longitudinal,INDV_L_weight_2013=INDV_L_weight,
  HH_L_Died_2013=HH_L_Died,INDV_L_Died_2013=INDV_L_Died,imonth_2013=imonth,iyear_2013=iyear
)%>%filter(ID!="")
###2015年随访
health2015<-import('~/data/CHARLS/CHARLS2015/Health_Status_and_Functioning.dta')
sample_inf2015<-import('~/data/CHARLS/CHARLS2015/Sample_Infor.dta')
weight2015<-import('~/data/CHARLS/CHARLS2015/Weights.dta')
health2015<-health2015%>%transmute(
  ID=ID,xrtype_health_2015=factor(xrtype,levels = c(1,2),labels = c('New Interview','Followup')),
  #新的受访者的疾病史
  Cancer_2015=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('否','是')),
  ##回访者上次疾病史记录是否正确
  Cancer_agree_2015=factor(da007_w2_1_4_,levels=c(1,2),labels=c('Agree','Disagree')),
  #不正确是否近两年有医生告诉你患有次疾病
  dignosis_2015=factor(da007_w2_2_4_,levels=c(1,2),labels=c('Yes','No')),
  #癌症诊断时间
  cancer_dig_time1_2015=da009_1_4_,cancer_dig_time2_2015=da009_2_4_,
  #癌症诊断类型
  #respir(呼吸)、breast(乳房)、colorect(结肠&直肠)、urinary(泌尿)、digothr(消化道)、
  #malegen(男性生殖)、femgen(女性生殖)、lymyleuk(淋巴&白血病)、other(其他)
  cancer_brain_2015=factor(ifelse(da017s1==1 & !is.na(da017s1),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_oral_2015=factor(ifelse(da017s2==2 & !is.na(da017s2),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Larynx_2015=factor(ifelse(da017s3==3 & !is.na(da017s3),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_pharynx_2015=factor(ifelse(da017s4==4 & !is.na(da017s4),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Thyroid_2015=factor(ifelse(da017s5==5 & !is.na(da017s5),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Lung_2015=factor(ifelse(da017s6==6 & !is.na(da017s6),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Breast_2015=factor(ifelse(da017s7==7 & !is.na(da017s7),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Oesophagus_2015=factor(ifelse(da017s8==8 & !is.na(da017s8),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Stomach_2015=factor(ifelse(da017s9==9 & !is.na(da017s9),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Liver_2015=factor(ifelse(da017s10==10 & !is.na(da017s10),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Pancreas_2015=factor(ifelse(da017s11==11 & !is.na(da017s11),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Kidney_2015=factor(ifelse(da017s12==12 & !is.na(da017s12),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Prostate_2015=factor(ifelse(da017s13==13 & !is.na(da017s13),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Testicle_2015=factor(ifelse(da017s14==14 & !is.na(da017s14),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Ovary_2015=factor(ifelse(da017s15==15 & !is.na(da017s15),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Cervix_2015=factor(ifelse(da017s16==16 & !is.na(da017s16),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Endometrium_2015=factor(ifelse(da017s17==17 & !is.na(da017s17),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Colon_2015=factor(ifelse(da017s18==18 & !is.na(da017s18),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Bladder_2015=factor(ifelse(da017s19==19 & !is.na(da017s19),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Skin_2015=factor(ifelse(da017s20==20 & !is.na(da017s20),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Non_Hodgkin_2015=factor(ifelse(da017s21==21 & !is.na(da017s21),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Leukemia_2015=factor(ifelse(da017s22==22 & !is.na(da017s22),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Other_2015=factor(ifelse(da017s23==23 & !is.na(da017s23),2,1),levels = c(1,2),labels=c('NO','YES')),
  respir_cancer_2015=factor(ifelse(cancer_oral_2015=="YES" | cancer_Larynx_2015=="YES" | cancer_Larynx_2015=="YES" | cancer_Lung_2015=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  breast_cacner_2015=cancer_Breast_2015,
  colorect_cacner_2015=cancer_Colon_2015,
  urinary_cancer_2015=factor(ifelse(cancer_Kidney_2015=="YES" | cancer_Prostate_2015=="YES" | cancer_Testicle_2015=="YES" | cancer_Bladder_2015=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  lymyleuk_cacner_2015=factor(ifelse(cancer_Non_Hodgkin_2015=="YES" | cancer_Leukemia_2015=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  femgen_cancer_2015=factor(ifelse(cancer_Ovary_2015=="YES" | cancer_Cervix_2015=="YES" | cancer_Endometrium_2015=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  Other_2015=factor(ifelse(cancer_Thyroid_2015=="YES" | cancer_Skin_2015=="YES" | cancer_Other_2015=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  #抑郁(≥12)
  dep1_2015=dc009-1,dep2_2015=dc010-1,dep3_2015=dc011-1,dep4_2015=dc012-1,
  dep6_2015=dc014-1,dep7_2015=dc015-1,dep8_2015=dc017-1,dep10_2015=dc018-1,
  dep5_2015=abs(dc013-4),dep8_2015=abs(dc016-4),
)
weight2015<-weight2015%>%transmute(
  ID,HH_weight_2015=HH_weight,HH_weight_ad1_2015=HH_weight_ad1,INDV_weight_2015=INDV_weight,
  INDV_weight_ad1_2015=INDV_weight_ad1,INDV_weight_ad2_2015=INDV_weight_ad2,Biomarker_weight_2015=Biomarker_weight,
  versionID_2015=versionID
)
##2018年随访
health2018<-import('~/data/CHARLS/CHARLS2018/Health_Status_and_Functioning.dta')
sample_inf2018<-import('~/data/CHARLS/CHARLS2018/Sample_Infor.dta')
cognition2018<-import('~/data/CHARLS/CHARLS2018/Cognition.dta')
weight2018<-import('~/data/CHARLS/CHARLS2018/Weights.dta')
health2018<-health2018%>%transmute(
  ID=ID,xrtype_health_2018=factor(xrtype,levels = c(1,2),labels = c('Followup','New Interview')),
  #受访者的疾病史
  Cancer_2018=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('否','是')),
  #癌症诊断时间
  cancer_dig_time1_2018=da009_1_4_,cancer_dig_time2_2018=da009_2_4_,
  #癌症诊断类型
  #respir(呼吸)、breast(乳房)、colorect(结肠&直肠)、urinary(泌尿)、digothr(消化道)、
  #malegen(男性生殖)、femgen(女性生殖)、lymyleuk(淋巴&白血病)、other(其他)
  cancer_brain_2018=factor(ifelse(da017_s1==1 & !is.na(da017_s1),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_oral_2018=factor(ifelse(da017_s2==2 & !is.na(da017_s2),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Larynx_2018=factor(ifelse(da017_s3==3 & !is.na(da017_s3),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_pharynx_2018=factor(ifelse(da017_s4==4 & !is.na(da017_s4),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Thyroid_2018=factor(ifelse(da017_s5==5 & !is.na(da017_s5),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Lung_2018=factor(ifelse(da017_s6==6 & !is.na(da017_s6),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Breast_2018=factor(ifelse(da017_s7==7 & !is.na(da017_s7),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Oesophagus_2018=factor(ifelse(da017_s8==8 & !is.na(da017_s8),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Stomach_2018=factor(ifelse(da017_s9==9 & !is.na(da017_s9),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Liver_2018=factor(ifelse(da017_s10==10 & !is.na(da017_s10),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Pancreas_2018=factor(ifelse(da017_s11==11 & !is.na(da017_s11),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Kidney_2018=factor(ifelse(da017_s12==12 & !is.na(da017_s12),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Prostate_2018=factor(ifelse(da017_s13==13 & !is.na(da017_s13),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Testicle_2018=factor(ifelse(da017_s14==14 & !is.na(da017_s14),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Ovary_2018=factor(ifelse(da017_s15==15 & !is.na(da017_s15),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Cervix_2018=factor(ifelse(da017_s16==16 & !is.na(da017_s16),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Endometrium_2018=factor(ifelse(da017_s17==17 & !is.na(da017_s17),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Colon_2018=factor(ifelse(da017_s18==18 & !is.na(da017_s18),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Bladder_2018=factor(ifelse(da017_s19==19 & !is.na(da017_s19),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Skin_2018=factor(ifelse(da017_s20==20 & !is.na(da017_s20),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Non_Hodgkin_2018=factor(ifelse(da017_s21==21 & !is.na(da017_s21),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Leukemia_2018=factor(ifelse(da017_s22==22 & !is.na(da017_s22),2,1),levels = c(1,2),labels=c('NO','YES')),
  cancer_Other_2018=factor(ifelse(da017_s23==23 & !is.na(da017_s23),2,1),levels = c(1,2),labels=c('NO','YES')),
  respir_cancer_2018=factor(ifelse(cancer_oral_2018=="YES" | cancer_Larynx_2018=="YES" | cancer_Larynx_2018=="YES" | cancer_Lung_2018=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  breast_cacner_2018=cancer_Breast_2018,
  colorect_cacner_2018=cancer_Colon_2018,
  urinary_cancer_2018=factor(ifelse(cancer_Kidney_2018=="YES" | cancer_Prostate_2018=="YES" | cancer_Testicle_2018=="YES" | cancer_Bladder_2018=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  lymyleuk_cacner_2018=factor(ifelse(cancer_Non_Hodgkin_2018=="YES" | cancer_Leukemia_2018=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  femgen_cancer_2018=factor(ifelse(cancer_Ovary_2018=="YES" | cancer_Cervix_2018=="YES" | cancer_Endometrium_2018=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  Other_2018=factor(ifelse(cancer_Thyroid_2018=="YES" | cancer_Skin_2018=="YES" | cancer_Other_2018=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  
 )
cognition2018<-cognition2018%>%transmute(
  ID,
  #抑郁(≥12)
  dep1_2018=dc009-1,dep2_2018=dc010-1,dep3_2018=dc011-1,dep4_2018=dc012-1,
  dep6_2018=dc014-1,dep7_2018=dc015-1,dep8_2018=dc017-1,dep10_2018=dc018-1,
  dep5_2018=abs(dc013-4),dep8_2018=abs(dc016-4)
)
weight2018<-weight2018%>%transmute(
  ID,HH_weight_2018=HH_weight,HH_weight_ad1_2018=HH_weight_ad1,INDV_weight_2018=INDV_weight,
  INDV_weight_ad2_2018=INDV_weight_ad2,versionID_2018=versionID
)
followup<-Reduce(function(x,y)merge(x,y,by='ID',all=TRUE),
             list(health2013,health2015,health2018,sample_inf2015,sample_inf2018,weight2013,weight2015,weight2018))
rm(Exit2013,Exit_interview2013,cognition2018,health2013,health2015,health2018,sample_inf2015,sample_inf2018,weight2013,weight2015,weight2018)




