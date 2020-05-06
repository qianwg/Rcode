library(tidyverse)
library(openxlsx)
library(ggpubr)
library(rio)
rm(list=ls())
screening<-read.xlsx('~/data/screening2019.xlsx',detectDates = TRUE)
source('~/Rcode/screening/riskscore2020/riskscore2020.R')
screening2<-risk_function(screening)
screening_PAD<-screening2[which(screening2$source=='PAD'),]
screening_epidata<-screening2[which(screening2$source=='epidata'),]
export(screening2,'~/20年风险评分.xlsx')
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
      )+labs(title='Epidata')
plot1
table<-data.frame(t(apply(score[,c("lung_score",'breast_score','liver_score','gastric_score')],2,percent_value)))
table2<-cbind(c('lung_score','breast_score',
                'liver_score','gastric_score'),table)
names(table2)<-c('percent','1%','2.5%','5%','10%','25%','50%','75%','90%','95%','97%','97.5','98%','99%')
ggtable<-ggtexttable(table2,rows=NULL,theme = ttheme('mBlue'))
ggarrange(plot1,ggtable,nrow=2,widths=c(1,20),heights=c(1,0.25))
rm(list=ls())
