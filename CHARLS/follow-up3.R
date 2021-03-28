rm(list=ls())
library(rio)
library(tidyverse)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
##2011年基线
biomarkers2011<-import('~/data/CHARLS/CHARLS2011/biomarkers.dta')%>%select(-householdID,-communityID)%>%
  mutate(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
demograph2011<-import('~/data/CHARLS/CHARLS2011/demographic_background.dta')%>%select(-householdID,-communityID)%>%
  mutate(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
health2011<-import('~/data/CHARLS/CHARLS2011/health_status_and_functioning.dta')%>%select(-householdID,-communityID)%>%
  mutate(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
blood2011<-import('~/data/CHARLS/CHARLS2011/Blood_20140429.dta')%>%mutate(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
health_care2011<-import('~/data/CHARLS/CHARLS2011/health_care_and_insurance.dta')%>%select(-householdID,-communityID)%>%
  mutate(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
weight2011<-import('~/data/CHARLS/CHARLS2011/weight.dta')%>%select(-householdID,-communityID)%>%
  mutate(ID=paste0(substr(ID,1,9),'0',substrRight(ID,2)))
psu2011<-import('~/data/CHARLS/CHARLS2011/PSU.dta')

export(biomarkers2011,'~/biomarkers2011.sav')
export(demograph2011,'~/demograph2011.sav')
export(health2011,'~/health2011.sav')
export(blood2011,'~/blood2011.sav')
export(health_care2011,'~/health_care2011.sav')
export(weight2011,'~/weight2011.sav')
export(psu2011,'~/psu2011.sav')
##2013年随访
biomarkers2013<-import('~/data/CHARLS/CHARLS2013/Biomarker.dta')%>%select(-householdID,-communityID)
demograph2013<-import('~/data/CHARLS/CHARLS2013/Demographic_Background.dta')%>%select(-householdID,-communityID)
health2013<-import('~/data/CHARLS/CHARLS2013/Health_Status_and_Functioning.dta')%>%select(-householdID,-communityID)
weight2013<-import('~/data/CHARLS/CHARLS2013/Weights.dta')%>%select(-householdID,-communityID,-versionID)
Exit_interview2013<-import('~/data/CHARLS/CHARLS2013/Exit_Interview.dta')%>%select(-householdID,-communityID,-versionID)
export(biomarkers2013,'~/biomarkers2013.sav')
export(demograph2013,'~/demograph2013.sav')
export(health2013,'~/health2013.sav')
export(weight2013,'~/weight2013.sav')
export(Exit_interview2013,'~/Exit_interview2013.sav')
data_id<-Reduce(function(x,y)merge(x,y,by='ID',all=TRUE),
                list(demograph2013,health2013,weight2013,biomarkers2013,Exit_interview2013))%>%transmute(ID)
export(data_id,'~/data_id.sav')



###2015年随访
health2015<-import('~/data/CHARLS/CHARLS2015/Health_Status_and_Functioning.dta')%>%select(-householdID,-communityID,-versionID)
sample_inf2015<-import('~/data/CHARLS/CHARLS2015/Sample_Infor.dta')%>%select(-householdID,-communityID)
weight2015<-import('~/data/CHARLS/CHARLS2015/Weights.dta')%>%select(-householdID,-communityID,-versionID)
biomarkers2015<-import('~/data/CHARLS/CHARLS2015/Biomarker.dta')%>%select(-householdID,-communityID,-versionID)
blood2015<-import('~/data/CHARLS/CHARLS2015/Blood.dta')%>%select(-householdID,-communityID,-versionID)
demograph2015<-import('~/data/CHARLS/CHARLS2015/Demographic_Background.dta')%>%select(-householdID,-communityID,-versionID)

export(biomarkers2015,'~/biomarkers2015.sav')
export(demograph2015,'~/demograph2015.sav')
export(health2015,'~/health2015.sav')
export(weight2015,'~/weight2015.sav')
export(blood2015,'~/blood2015.sav')
export(sample_inf2015,'~/sample_inf2015.sav')
data_id<-Reduce(function(x,y)merge(x,y,by='ID',all=TRUE),
                list(demograph2015,health2015,weight2015,biomarkers2015,blood2015,sample_inf2015))%>%transmute(ID)
export(data_id,'~/data_id.sav')

###2018年随访
demograph2018<-import('~/data/CHARLS/CHARLS2018/Demographic_Background.dta')%>%select(-householdID,-communityID,-versionID)
health2018<-import('~/data/CHARLS/CHARLS2018/Health_Status_and_Functioning.dta')%>%select(-householdID,-communityID,-versionID)
sample_inf2018<-import('~/data/CHARLS/CHARLS2018/Sample_Infor.dta')%>%select(-householdID,-communityID,-versionID)
cognition2018<-import('~/data/CHARLS/CHARLS2018/Cognition.dta')%>%select(-householdID,-communityID,-versionID)
weight2018<-import('~/data/CHARLS/CHARLS2018/Weights.dta')%>%select(-householdID,-communityID,-versionID)

export(demograph2018,'~/demograph2018.sav')
export(health2018,'~/health2018.sav')
export(weight2018,'~/weight2018.sav')
export(sample_inf2018,'~/sample_inf2018.sav')
export(cognition2018,'~/cognition2018.sav')

data_id<-Reduce(function(x,y)merge(x,y,by='ID',all=TRUE),
                list(demograph2018,health2018,cognition2018,weight2018,sample_inf2018))%>%transmute(ID)
export(data_id,'~/data_id.sav')





