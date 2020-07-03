library(tidyverse)
library(openxlsx)
library(ggpubr)
library(rio)
rm(list=ls())
screening<-read.xlsx('~/data/screening2019.xlsx',detectDates = TRUE)
source('~/Rcode/screening/riskscore2020/riskscore2020_2.R')

screening2<-risk_function(screening)
screening_PAD<-screening2[which(screening=='PAD'),]
screening_epidata<-screening2[which(screening2$source=='epidata'),]
write.xlsx(screening2,'~/20年风险评分.xlsx')
#
score<-screening2[,193:215]
score<-screening_epidata[,193:215]
score<-screening_PAD[,193:215]
rm(screening2)
#common_risk------->check
gghistogram(data=score,x='common_risk',y='..density..',bins=39)
#family
family<-score[,c("lung_family",'breast_family','liver_family','gastric_family')]%>%
pivot_longer(cols=ends_with('family'),names_to='family',values_to = 'score' )%>%
  group_by(family,score)%>%summarise(n=n())
facet(ggbarplot(data=family,x='score',y='n',label='n',lab.pos='out'),facet.by = 'family',scales='free_x')
#score
cancer_score<-screening2[,c("lung_score",'breast_score','liver_score','gastric_score')]%>%
  pivot_longer(cols=ends_with('score'),names_to='cancer',values_to = 'score' )%>%
   group_by(cancer,score)%>%summarise(n=n())
plot1<-facet(ggbarplot(data=subset(cancer_score,!is.na(score)),x='score',xlab='',y='n'),
      facet.by = 'cancer',scales='free')+theme(
        axis.text.x = element_text(
          color = "black",
          size = 6,
          vjust = 0.5,
          hjust = 0.5
        ),plot.title=element_text(hjust=0.5)
      )
plot1
table<-data.frame(t(apply(screening2[,c("lung_score",'breast_score','liver_score','gastric_score')],2,percent_value)))
table2<-cbind(c('lung_score','breast_score',
                'liver_score','gastric_score'),table)
names(table2)<-c('percent','1%','2.5%','5%','10%','25%','50%','75%','90%','95%','97%','97.5','98%','99%')
ggtable<-ggtexttable(table2,rows=NULL,theme = ttheme('mBlue'))
ggarrange(plot1,ggtable,nrow=2,widths=c(1,20),heights=c(1,0.25))
rm(list=ls())
#
table<-data.frame(t(apply(screening2[which(screening2$sex==1),c("lung_score",'breast_score','liver_score','gastric_score')],2,percent_value)))
table2<-cbind(c('lung_score','breast_score',
                'liver_score','gastric_score'),table)
names(table2)<-c('percent','1%','2.5%','5%','10%','25%','50%','75%','90%','95%','97%','97.5','98%','99%')
ggtexttable(table2,rows=NULL,theme = ttheme('mBlue'))
##分地区
screening2$qu2<-factor(ifelse(area=="东二营镇 " | area=="下营镇" | area=="别山镇" | area=="官庄镇" | area=="桑梓镇" |   area=="马伸桥镇" ,2,1 ),levels=c(1,2),labels=c('市内六区','蓟州区'))
table<-data.frame(t(apply(screening2[which(screening2$qu2=='蓟州区'),c("lung_score",'breast_score','liver_score','gastric_score')],2,percent_value)))
table2<-cbind(c('lung_score','breast_score',
                'liver_score','gastric_score'),table)
names(table2)<-c('percent','1%','2.5%','5%','10%','25%','50%','75%','90%','95%','97%','97.5','98%','99%')
ggtexttable(table2,rows=NULL,theme = ttheme('mBlue'))

###
screening2020<-import('~/data/1_250.xlsx')
str(screening2020)
##共性因素
screening2020_2<-risk_function2(screening2020)
screening2020_2[which(screening2020_2$id=="46040001"),c('id','name','age_risk','smoking_risk','passivesmk_risk','bmi_risk','food_risk','body_risk','stress_risk','common_risk')]
screening2020_2[which(screening2020_2$id=="42010003"),c('id','name','age_risk','smoking_risk','passivesmk_risk','bmi_risk','food_risk','body_risk','stress_risk','common_risk')]
screening2020_2[which(screening2020_2$id=="42010013"),c('id','name','age_risk','smoking_risk','passivesmk_risk','bmi_risk','food_risk','body_risk','stress_risk','common_risk')]
screening2020_2[which(screening2020_2$id=="42010017"),c('id','name','age_risk','smoking_risk','passivesmk_risk','bmi_risk','food_risk','body_risk','stress_risk','common_risk')]
screening2020_2[which(screening2020_2$id=="42010028"),c('id','name','age_risk','smoking_risk','passivesmk_risk','bmi_risk','food_risk','body_risk','stress_risk','common_risk')]



#总分
screening202_3<-risk_function(screening2020)
screening202_3[which(screening202_3$id=="46040001"),c('id','name','lung_score','breast_score','liver_score','gastric_score')]
screening202_3[which(screening202_3$id=="42010003"),c('id','name','lung_score','breast_score','liver_score','gastric_score')]
screening202_3[which(screening202_3$id=="42010013"),c('id','name','lung_score','breast_score','liver_score','gastric_score')]
screening202_3[which(screening202_3$id=="42010017"),c('id','name','lung_score','breast_score','liver_score','gastric_score')]
screening202_3[which(screening202_3$id=="42010028"),c('id','name','lung_score','breast_score','liver_score','gastric_score')]


###所有
cancer_score<-screening202_3[,c("lung_score",'breast_score','liver_score','gastric_score')]%>%
  pivot_longer(cols=ends_with('score'),names_to='cancer',values_to = 'score' )%>%
  group_by(cancer,score)%>%summarise(n=n())
plot1<-facet(ggbarplot(data=subset(cancer_score,!is.na(score)),x='score',xlab='',y='n'),
             facet.by = 'cancer',scales='free')+theme(
               axis.text.x = element_text(
                 color = "black",
                 size = 6,
                 vjust = 0.5,
                 hjust = 0.5
               ),plot.title=element_text(hjust=0.5)
             )
plot1
table<-data.frame(t(apply(screening202_3[,c("lung_score",'breast_score','liver_score','gastric_score')],2,percent_value)))
table2<-cbind(c('lung_score','breast_score',
                'liver_score','gastric_score'),table)
names(table2)<-c('percent','1%','2.5%','5%','10%','25%','50%','75%','90%','95%','97%','97.5','98%','99%')
ggtable<-ggtexttable(table2,rows=NULL,theme = ttheme('mBlue'))
ggarrange(plot1,ggtable,nrow=2,widths=c(1,20),heights=c(1,0.25))
###
#按地区
write.xlsx(screening202_3,'~/screening202_3.xlsx')

screening4<-import("~/screening202_3.xlsx")
table<-data.frame(t(apply(screening4[which(screening4$街道=='蓟州区'),c("lung_score",'breast_score','liver_score','gastric_score')],2,percent_value)))
table2<-cbind(c('lung_score','breast_score',
                'liver_score','gastric_score'),table)
names(table2)<-c('percent','1%','2.5%','5%','10%','25%','50%','75%','90%','95%','97%','97.5','98%','99%')
ggtable<-ggtexttable(table2,rows=NULL,theme = ttheme('mBlue'))



