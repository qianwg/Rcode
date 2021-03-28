rm(list=ls())
#
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
##看一下2011年和2013年数据的合并
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

rm(biomarkers2011,demograph2011,health2011,blood2011,health_care2011,weight2011)
##2013年数据
health2013<-import('~/data/CHARLS/CHARLS2013/Health_Status_and_Functioning.dta')%>%select(-householdID,-communityID)




