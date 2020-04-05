rm(list=ls())
library(meta)
library(tidyverse)
subgroub<-openxlsx::read.xlsx('C:/Users/sheng/Desktop/subgroup.xlsx')
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
infection_sub<-left_join(infection,subgroub,by=c('Authors','Years'))
infection_sub$followup<-ifelse(infection_sub$the.length.of.followed.up>=15,2,1)
infection_sub$followup<-factor(infection_sub$followup,levels = c(1,2),labels = c('<15','>=15'))
combination1_sub<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')

combination1_sub1<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_sub,sm='RR',byvar=The.type.of.surgery,
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
            'Yang','Bajramagic','Tanaka','Yokoyama','Reddy'),
  Years=c(2015,2014,2005,2017,2015,2017,2012,2010,2015,2019,2012,2014,2007),
  Event_I=c(6,22,3,1,6,3,1,3,1,11,0,2,2),
  Total_I=c(84,100,21,49,66,18,30,50,30,39,30,21,20),
  Event_C=c(16,22,6,9,8,4,4,5,1,14,2,1,3),
  Total_C=c(80,95,23,42,68,18,30,50,30,39,34,21,22)
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
infection_surg_sub<-left_join(infection_surg,subgroub,by=c('Authors','Years'))
infection_surg_sub$followup<-ifelse(infection_surg_sub$the.length.of.followed.up>=15,2,1)
infection_surg_sub$followup<-factor(infection_surg_sub$followup,levels = c(1,2),labels = c('<15','>=15'))
infection_surg_sub1<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=The.type.of.surgery,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub2<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=type.of.treatment,
                           method='MH',studlab=paste(Authors,Years),label.e = 'Pro/Synbiotics')
infection_surg_sub3<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_surg_sub,sm='RR',byvar=duration.of.treatment,
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
            'Yang','Liu','Xie','Tanaka','Yokoyama'),
  Years=c(2015,2005,2017,2015,2017,2012,2010,2015,2013,2018,2012,2014),
  Event_I=c(2,0,0,6,1,1,2,3,3,4,2,7),
  Total_I=c(84,21,49,66,18,30,50,30,75,70,30,21),
  Event_C=c(9,1,4,8,2,4,5,5,10,6,7,5),
  Total_C=c(80,23,42,68,18,30,50,30,75,70,34,21))
infection_pne
combination3<-metabin(Event_I,Total_I,Event_C,Total_C,data=infection_pne,sm='RR',
                      method='MH',studlab=paste(Authors,Years),comb.random = TRUE,comb.fixed = FALSE,
                      label.e = 'Pro/Synbiotics')
combination3
forest(combination3)
funnel(combination3)
metabias(combination3,method.bias = 'linreg')



