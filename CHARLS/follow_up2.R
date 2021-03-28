rm(list=ls())
##
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
###2011基线
biomarkers2011<-import('~/data/CHARLS/CHARLS2011/biomarkers.dta')%>%
  transmute(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
demograph2011<-import('~/data/CHARLS/CHARLS2011/demographic_background.dta')%>%
  transmute(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)),
            age=(2011-ba002_1),age2=ba004)
health2011<-import('~/data/CHARLS/CHARLS2011/health_status_and_functioning.dta')%>%
  transmute(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)),
            cancer_dignosis_2011=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
            #癌症诊断时间
            cancer_dig_time1_2011=da009_1_4_,cancer_dig_time2_2011=da009_2_4_,
            #癌症诊断类型
            #respir(呼吸)、breast(乳房)、colorect(结肠&直肠)、urinary(泌尿)、digothr(消化道)、
            #malegen(男性生殖)、femgen(女性生殖)、lymyleuk(淋巴&白血病)、other(其他)
            cancer_brain_2011=factor(ifelse(da017s1==1 & !is.na(da017s1),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_oral_2011=factor(ifelse(da017s2==2 & !is.na(da017s2),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Larynx_2011=factor(ifelse(da017s3==3 & !is.na(da017s3),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_pharynx_2011=factor(ifelse(da017s4==4 & !is.na(da017s4),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Thyroid_2011=factor(ifelse(da017s5==5 & !is.na(da017s5),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Lung_2011=factor(ifelse(da017s6==6 & !is.na(da017s6),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Breast_2011=factor(ifelse(da017s7==7 & !is.na(da017s7),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Oesophagus_2011=factor(ifelse(da017s8==8 & !is.na(da017s8),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Stomach_2011=factor(ifelse(da017s9==9 & !is.na(da017s9),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Liver_2011=factor(ifelse(da017s10==10 & !is.na(da017s10),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Pancreas_2011=factor(ifelse(da017s11==11 & !is.na(da017s11),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Kidney_2011=factor(ifelse(da017s12==12 & !is.na(da017s12),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Prostate_2011=factor(ifelse(da017s13==13 & !is.na(da017s13),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Testicle_2011=factor(ifelse(da017s14==14 & !is.na(da017s14),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Ovary_2011=factor(ifelse(da017s15==15 & !is.na(da017s15),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Cervix_2011=factor(ifelse(da017s16==16 & !is.na(da017s16),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Endometrium_2011=factor(ifelse(da017s17==17 & !is.na(da017s17),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Colon_2011=factor(ifelse(da017s18==18 & !is.na(da017s18),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Bladder_2011=factor(ifelse(da017s19==19 & !is.na(da017s19),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Skin_2011=factor(ifelse(da017s20==20 & !is.na(da017s20),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Non_Hodgkin_2011=factor(ifelse(da017s21==21 & !is.na(da017s21),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Leukemia_2011=factor(ifelse(da017s22==22 & !is.na(da017s22),2,1),levels = c(1,2),labels=c('NO','YES')),
            cancer_Other_2011=factor(ifelse(da017s23==23 & !is.na(da017s23),2,1),levels = c(1,2),labels=c('NO','YES')),
            respir_cancer_2011=factor(ifelse(cancer_oral_2011=="YES" | cancer_Larynx_2011=="YES" | cancer_Larynx_2011=="YES" | cancer_Lung_2011=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
            breast_cacner_2011=cancer_Breast_2011,
            colorect_cacner_2011=cancer_Colon_2011,
            urinary_cancer_2011=factor(ifelse(cancer_Kidney_2011=="YES" | cancer_Prostate_2011=="YES" | cancer_Testicle_2011=="YES" | cancer_Bladder_2011=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
            lymyleuk_cacner_2011=factor(ifelse(cancer_Non_Hodgkin_2011=="YES" | cancer_Leukemia_2011=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
            femgen_cancer_2011=factor(ifelse(cancer_Ovary_2011=="YES" | cancer_Cervix_2011=="YES" | cancer_Endometrium_2011=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
            Other_2011=factor(ifelse(cancer_Thyroid_2011=="YES" | cancer_Skin_2011=="YES" | cancer_Other_2011=="YES",2,1),levels=c(1,2),labels=c('NO','YES')),
  )
blood2011<-import('~/data/CHARLS/CHARLS2011/Blood_20140429.dta')%>%
  transmute(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
health_care2011<-import('~/data/CHARLS/CHARLS2011/health_care_and_insurance.dta')%>%
  transmute(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
weight2011<-import('~/data/CHARLS/CHARLS2011/weight.dta')%>%
  transmute(
    ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)),
    imonth_2011=imonth,iyear_2011=iyear,urID,mainr
  )

baseline<-Reduce(function(x,y)merge(x,y,by='ID',all=TRUE),
             list(demograph2011,health2011,weight2011,biomarkers2011,blood2011,health_care2011))

rm(biomarkers2011,demograph2011,health2011,blood2011,health_care2011)
##2013年随访
health2013<-import('~/data/CHARLS/CHARLS2013/Health_Status_and_Functioning.dta')%>%
  transmute(ID,xrtype=factor(xrtype,levels = c(1,2),labels=c('New Interview','Followup')),
            #2011年癌症诊断情况
            cancer_dignosis_2011_2=factor(ifelse(zda007_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
            #2013年新随访者癌症诊断情况
            cancer_dignosis_2013=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
            #2011年癌症诊断情况是否记录正确
            cancer_dignosis_2011_agree=factor(da007_w2_1_4_,levels=c(1,2),labels=c('Agree','Disagree')),
            #继上次访问之后是否被诊断为癌症
            cancer_dignosis_2011_2013=factor(ifelse(da007_w2_2_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
            #癌症诊断时间
            cancer_dig_time1_2013=da009_1_4_,cancer_dig_time2_2013=da009_2_4_,
            #癌症类型
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
            
           )
weight2013<-import('~/data/CHARLS/CHARLS2013/Weights.dta')%>%
  transmute(ID,crossection_2013=factor(crosssection,levels=c(0,1),labels=c('NO','YES')),
            longitudinal_2013=factor(longitudinal,levels=c(0,1),labels=c('NO','YES')),
            INDV_L_Died_2013=factor(INDV_L_Died,levels=c(0,1),labels=c('NO','YES')),
            imonth_2013=imonth,iyear_2013=iyear,versionID_2013=versionID
            )
###2015年随访
health2015<-import('~/data/CHARLS/CHARLS2015/Health_Status_and_Functioning.dta')%>%
  transmute(
    ID,xrtype_2015=factor(xrtype,levels = c(1,2),labels=c('New Interview','Followup')),
    #2011年癌症诊断情况
    cancer_dignosis_2011_3=factor(ifelse(zda007_4_==1 & !is.na(zda007_4_),2,1),levels=c(1,2),labels=c('NO','Yes')),
    #2015年新随访者癌症诊断情况
    cancer_dignosis_2015=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
    #回访者2013年癌症诊断情况是否记录正确
    cancer_dignosis_2013_agree=factor(da007_w2_1_4_,levels=c(1,2),labels=c('Agree','Disagree')),
    #继上次访问之后是否被诊断为癌症
    cancer_dignosis_2013_2015=factor(ifelse(da007_w2_2_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
    #癌症诊断时间
    cancer_dig_time1_2015=da009_1_4_,cancer_dig_time2_2015=da009_2_4_,
    #癌症类型
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
    )
sample_inf2015<-import('~/data/CHARLS/CHARLS2015/Sample_Infor.dta')%>%
  transmute(ID,crossection_2015=factor(crosssection,levels=c(0,1),labels=c('NO','YES')),
            died_2015=factor(died,levels=c(0,1),labels=c('NO','YES')),
            imonth_2015=imonth,iyear_2015=iyear,versionID_2015=versionID)

##2018年随访
health2018<-import('~/data/CHARLS/CHARLS2018/Health_Status_and_Functioning.dta')%>%
  transmute(
    ID,xrtype_2018=factor(xrtype,levels = c(1,2),labels=c('Followup','New Interview')),
    #2015年癌症诊断情况
    cancer_dignosis_2018=factor(ifelse(zdiagnosed_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
    #2018年新和旧随访者癌症诊断情况
    cancer_dignosis_2015_2018=factor(ifelse(da007_4_==1,2,1),levels=c(1,2),labels=c('NO','Yes')),
    #癌症诊断时间
    cancer_dig_time1_2018=da009_1_4_,cancer_dig_time2_2018=da009_2_4_,
    #癌症类型
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
sample_inf2018<-import('~/data/CHARLS/CHARLS2018/Sample_Infor.dta')%>%
  transmute(ID,crossection_2018=factor(crosssection,levels=c(0,1),labels=c('NO','YES')),
            died_2018=factor(died,levels=c(0,1),labels=c('NO','YES')),
            imonth_2018=imonth,iyear_2018=iyear,versionID_2018=versionID
            )

##
followup<-Reduce(function(x,y)merge(x,y,by='ID',all=TRUE),
    list(health2013,weight2013,health2015,sample_inf2015,health2018,sample_inf2018))
##
baseline_followup<-left_join(baseline,followup,by='ID')
#export(baseline_followup,'~/baseline_followup.xlsx')