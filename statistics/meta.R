rm(list=ls())
library(meta)
library(tidyverse)
 #subgroub<-openxlsx::read.xlsx('C:/Users/sheng/Desktop/subgroup.xlsx')
 subgroup<-openxlsx::read.xlsx('~/data/subgroup.xlsx')
#1,infection complication
#1.1 data
infection<-data.frame(
  Authors=c('Kotzampassi','Sadahiro','Kanazawa','Usami',
  'Polakowski','Lages','Zhang','Liu','Sommacal','Tanaka'),
  Years=c(2015,2014,2005,2011,2018,2017,2012,2010,2013,2012),
  Event_I=c(10,24,4,0,1,11,3,7,6,3),
  Total_I=c(84,100,21,32,36,18,30,50,23,30),
  Event_C=c(23,24,12,5,7,7,10,23,16,10),
  Total_C=c(80,95,23,29,37,18,30,50,23,34)
)
infection$Authors<-as.character(infection$Authors)
infection
#1.2combination of effect value
combination1<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection,sm='RR',
                      method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
combination1
#1.3.forest plot
forest(combination1)
#1.3.1 subgroup analysis(pro/synbiotics,Geography,surgery type,treatment duration,
#study quailty,composition of pro/synbiotics,)
infection_sub<-left_join(infection,subgroup,by=c('Authors','Years'))
infection_sub$followup<-ifelse(infection_sub$the.length.of.followed.up>=15,2,1)
infection_sub$followup<-factor(infection_sub$followup,levels = c(1,2),labels = c('<15','>=15'))
combination1_sub<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')

combination1_sub1<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=infection_sub$The.type.of.surgery2,
                      method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
combination1_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=type.of.treatment,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
combination1_sub3<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=duration.of.treatment,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
combination1_sub4<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=region,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
combination1_sub5<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=infection_sub$`num.of.pro/synbiotics`,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
combination1_sub6<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=followup,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
combination1_sub7<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=infection_sub$study.quailty,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')


forest(combination1_sub1)
forest(combination1_sub2)
forest(combination1_sub3)
forest(combination1_sub4)
forest(combination1_sub5)
forest(combination1_sub6)
forest(combination1_sub7)
#p for interaction and meta regression
metareg(combination1_sub,The.type.of.surgery)#手术方式
metareg(combination1_sub,type.of.treatment)#治疗方式
metareg(combination1_sub,duration.of.treatment)#治疗时间
metareg(combination1_sub,region)#地区
metareg(combination1_sub,infection_sub$`num.of.pro/synbiotics`)#干预方式
metareg(combination1_sub,followup)#随访时间
metareg(combination1_sub,study.quailty)#研究质量

#1.4 publication bias
funnel(combination1)
metabias(combination1)
#sensitivity analysis
metainf(combination1,pooled = 'random')
forest(metainf(combination1,pooled = 'random'))
##Omitting Lages 2017
infection_2<-infection[which(infection$Authors!='Lages'),]
combination1_2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_2,sm='RR',
                      method='MH',studlab=paste(Authors,Years),comb.random = TRUE,comb.fixed = FALSE,
                      label.e = 'Pro/Synbiotics')
forest(combination1_2)
funnel(combination1_2)
metabias(combination1_2,method.bias = 'linreg')
#


#2，surgical site infection
infection_surg<-data.frame(
  Authors=c('Kotzampassi','Sadahiro','Kanazawa','Flesch','Liu','Lages','Zhang','Liu',
            'Yang','Tanaka','Yokoyama','Reddy'),
  Years=c(2015,2014,2005,2017,2015,2017,2012,2010,2015,2012,2014,2007),
  Event_I=c(6,22,3,1,6,3,1,3,1,0,2,2),
  Total_I=c(84,100,21,49,66,18,30,50,30,30,21,20),
  Event_C=c(16,22,6,9,8,4,4,5,1,2,1,3),
  Total_C=c(80,95,23,42,68,18,30,50,30,34,21,22)
)
infection_surg
combination2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg,sm='RR',
                      method='MH',studlab=paste(Authors,Years),comb.random = TRUE,comb.fixed = FALSE,
                      label.e = 'Pro/Synbiotics')
combination2
forest(combination2)
funnel(combination2)
metabias(combination2)#无发表偏倚
infection_surg$Authors<-as.character(infection_surg$Authors)
infection_surg_sub<-left_join(infection_surg,subgroup,by=c('Authors','Years'))
infection_surg_sub$followup<-ifelse(infection_surg_sub$the.length.of.followed.up>=15,2,1)
infection_surg_sub$followup<-factor(infection_surg_sub$followup,levels = c(1,2),labels = c('<15','>=15'))
infection_surg_sub1<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=The.type.of.surgery,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=type.of.treatment,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub3<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=infection_surg_sub$duration.of.treatment,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub4<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=region,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub5<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=infection_surg_sub$`num.of.pro/synbiotics`,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub6<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=followup,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub7<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=infection_surg_sub$study.quailty,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')

forest(infection_surg_sub1)
forest(infection_surg_sub2)
forest(infection_surg_sub3)
forest(infection_surg_sub4)
forest(infection_surg_sub5)
forest(infection_surg_sub6)
forest(infection_surg_sub7)
#
infection_surg_sub<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',
                          method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')

metareg(infection_surg_sub,The.type.of.surgery)#手术方式
metareg(infection_surg_sub,type.of.treatment)#治疗方式
metareg(infection_surg_sub,duration.of.treatment)#治疗时间
metareg(infection_surg_sub,region)#地区
metareg(infection_surg_sub,study.quailty)#研究质量

##pneumonia
infection_pne<-data.frame(
  Authors=c('Kotzampassi','Kanazawa','Flesch','Liu','Lages','Zhang','Liu',
            'Yang','Liu','Tanaka','Yokoyama'),
  Years=c(2015,2005,2017,2015,2017,2012,2010,2015,2013,2012,2014),
  Event_I=c(2,0,0,6,1,1,2,3,3,2,7),
  Total_I=c(84,21,49,66,18,30,50,30,75,30,21),
  Event_C=c(9,1,4,8,2,4,5,5,10,7,5),
  Total_C=c(80,23,42,68,18,30,50,30,75,34,21))
infection_pne
combination3<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_pne,sm='RR',
                      method='MH',studlab=paste(Authors,Years),comb.random = TRUE,comb.fixed = FALSE,
                      label.e = 'Pro/Synbiotics')
combination3
forest(combination3)
funnel(combination3)
metabias(combination3,method.bias = 'linreg')
#
infection_pne$Authors<-as.character(infection_pne$Authors)
infection_pne_sub<-left_join(infection_pne,subgroup,by=c('Authors','Years'))
infection_pne_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_pne_sub,sm='RR',byvar=infection_pne_sub$type.of.treatment,
                             method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_pne_sub2)

####
#Urinary tract infection
infection_urinary<-data.frame(
  Authors=c('Kotzampassi','Liu','Liu','Yang','Liu'),
  Years=c(2015,2015,2010,2015,2013),
  Event_I=c(4,1,1,2,2),
  Total_I=c(84,66,50,30,75),
  Event_C=c(6,9,6,2,10),
  Total_C=c(80,68,50,30,75)
)
infection_urinary$Authors<-as.character(infection_urinary$Authors)
infection_urinary_sub<-left_join(infection_urinary,subgroup,by=c('Authors','Years'))
infection_urinary_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_urinary_sub,sm='RR',
                            method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_urinary_sub2)

##Bacteremia
infection_bacter<-data.frame(
  Authors=c('Kotzampassi','Kanazawa','Lages','Zhang','Yang'),
  Years=c(2015,2005,2017,2012,2015),
  Event_I=c(6,1,0,2,3),
  Total_I=c(84,21,18,30,30),
  Event_C=c(8,4,1,9,9),
  Total_C=c(80,23,18,30,30)
)
infection_bacter$Authors<-as.character(infection_bacter$Authors)
infection_bacter_sub<-left_join(infection_bacter,subgroup,by=c('Authors','Years'))
infection_bacter_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_bacter_sub,sm='RR',byvar=infection_bacter_sub$type.of.treatment,
                                method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_bacter_sub2)

##Anastomosis leakage
infection_leak<-data.frame(
  Authors=c('Kotzampassi','Zhang','Yang','Tanaka','Yokoyama'),
  Years=c(2015,2012,2015,2012,2014),
  Event_I=c(1,0,2,0,3),
  Total_I=c(84,30,30,30,21),
  Event_C=c(7,2,1,2,2),
  Total_C=c(80,30,30,34,21)
)
infection_leak$Authors<-as.character(infection_leak$Authors)
infection_leak_sub<-left_join(infection_leak,subgroup,by=c('Authors','Years'))
infection_leak_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_leak_sub,sm='RR',byvar=infection_leak_sub$type.of.treatment,
                               method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_leak_sub2)

##intra-abdominal abscess
infection_abscess<-data.frame(
  Authors=c('Kanazawa','Flesch','Zhang'),
  Years=c(2005,2017,2012),
  Event_I=c(2,0,2),
  Total_I=c(21,49,30),
  Event_C=c(4,4,1),
  Total_C=c(23,42,30)
)
infection_abscess$Authors<-as.character(infection_abscess$Authors)
infection_abscess_sub<-left_join(infection_abscess,subgroup,by=c('Authors','Years'))
infection_abscess_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_abscess_sub,sm='RR',#byvar=infection_abscess_sub$type.of.treatment,
                             method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_abscess_sub2)

#diarrhea
infection_diarrhea<-data.frame(
  Authors=c('Liu','Liu','Yang','Liu','Tanaka'),
  Years=c(2015,2010,2015,2013,2012),
  Event_I=c(16,17,8,11,1),
  Total_I=c(66,50,30,75,30),
  Event_C=c(31,9,16,22,8),
  Total_C=c(68,50,30,75,34)
)
infection_diarrhea$Authors<-as.character(infection_diarrhea$Authors)
infection_diarrhea_sub<-left_join(infection_diarrhea,subgroup,by=c('Authors','Years'))
infection_diarrhea_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_diarrhea_sub,sm='RR',#byvar=infection_abscess_sub$type.of.treatment,
                                method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_diarrhea_sub2)

##postoperative hospital stay
infection_hosp<-data.frame(
  Authors=c('Kanazawa','Usami','Liu','Zhang','Liu','Yang','Sommacal'),
  Years=c(2005,2011,2015,2012,2010,2015,2013),
  Total_I=c(21,32,66,30,50,30,23),
  Mean_I=c(36.9,18.5,11.26,12,12.7,15.86,12),
  SD_I=c(16.4,3.2,2.52,3,2.2,4.92,5),
  Total_C=c(23,29,68,30,50,30,23),
  Mean_C=c(47,20.3,12.96,14,12.6,15,12),
  SD_C=c(19.2,4.2,3.06,3,3.3,4.31,14)
)
infection_hosp$Authors<-as.character(infection_hosp$Authors)
infection_hosp_sub<-left_join(infection_hosp,subgroup,by=c('Authors','Years'))
infection_hosp_sub2<-metacont(Total_I,Mean_I,SD_I,Total_C,Mean_C,SD_C,data=infection_hosp_sub,sm='MD',byvar=infection_hosp_sub$type.of.treatment,
                                studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_hosp_sub2)

##antibiotic useage time
infection_antib<-data.frame(
  Authors=c('Kanazawa','Polakowski','Liu','Liu','Yang','Liu','Sommacal','Tanaka'),
  Years=c(2005,2018,2015,2010,2015,2013,2013,2012),
  Total_I=c(21,36,66,50,30,75,23,30),
  Mean_I=c(10.4,1.42,6.22,5.9,6.6,5.69,9,6.4),
  SD_I=c(7.4,0.5,1.96,1.6,2.81,2.16,3.6,4.6),
  Total_C=c(23,37,68,50,30,75,23,34),
  Mean_C=c(15.7,3.74,7.56,7,7.33,7.29,15,6.6),
  SD_C=c(15.7,3.74,7.56,7,7.33,7.29,15,6.6)
)
infection_antib$Authors<-as.character(infection_antib$Authors)
infection_antib_sub<-left_join(infection_antib,subgroup,by=c('Authors','Years'))
infection_antib_sub2<-metacont(Total_I,Mean_I,SD_I,Total_C,Mean_C,SD_C,data=infection_antib_sub,sm='MD',byvar=infection_antib_sub$type.of.treatment,
                              studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_antib_sub2)

##the length of ICU stays
infection_ICU<-data.frame(
  Authors=c('Kanazawa','Usami','Tanaka'),
  Years=c(2005,2011,2012),
  Total_I=c(21,32,30),
  Mean_I=c(1.3,2.4,2.9),
  SD_I=c(0.9,0.9,1.1),
  Total_C=c(23,29,34),
  Mean_C=c(1.3,2.3,3.3),
  SD_C=c(0.7,1,1.7)
)
infection_ICU$Authors<-as.character(infection_ICU$Authors)
infection_ICU_sub<-left_join(infection_ICU,subgroup,by=c('Authors','Years'))
infection_ICU_sub2<-metacont(Total_I,Mean_I,SD_I,Total_C,Mean_C,SD_C,data=infection_ICU_sub,sm='MD',byvar=infection_ICU_sub$type.of.treatment,
                               studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
forest(infection_ICU_sub2)






