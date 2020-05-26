rm(list=ls())
library(rio)
library(ggpubr)
library(table1)
library(tidyverse)
source('~/Rcode/statistics/data_summary.R')
#读取数据
source('~/Rcode/biomarker/female_data.R')
source('~/Rcode/screening/screening_female/screening_female_data.R')
screening_female<-inner_join(screening3,biomarker_female_risk,by='ID_BLAST')
#查看肿瘤标志物
apply(screening_female[,c('CA125','CA153','CEA')],2,data_summary2)
#查看妇科手术史
apply(screening_female[,c('绝育手术','子宫摘除术','卵巢摘除术')],2,table)
#初潮年龄
ggscatter(data=screening_female,x='初潮年龄',y='CA125',add='reg.line',aadd.params = list(color='red'))
screening_female%>%transmute(初潮年龄=初潮年龄,CA125=log(CA125))%>%ggboxplot(x='初潮年龄',y='CA125')
screening_female%>%group_by(初潮年龄)%>%summarise(mean=mean(CA125,na.rm=T))%>%
  filter(!is.na(初潮年龄))%>%ggscatter(x='初潮年龄',y='mean')+geom_line()
screening_female%>%ggplot(aes(x=出生年份,y))
#BMI
screening_female%>%transmute(BMI=BMI,CA125=log(CA125))%>%ggscatter(x='BMI','CA125',add='reg.line',add.params = list(color='red'))+stat_cor()
















