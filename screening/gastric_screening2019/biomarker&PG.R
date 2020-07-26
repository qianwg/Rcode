rm(list=ls())
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
library(corrplot)
library(PerformanceAnalytics)
##2020-7-06
#数据读取
source('~/Rcode/screening/gastric_screening/stomach_data.R')
source('~/Rcode/statistics/data_summary.R')
#基本分布
do.call(rbind,apply(pepsinogen[,c('AFP','CA199','CA125','CA153','CEA','PG1','PG2','PGR')],2,data_summary2))
#相关性
cor<-cor(pepsinogen[,c('AFP','CA199','CEA','PG1','PG2','PGR')],use='complete.obs',method='spearman')
chart.Correlation(pepsinogen[,c('AFP','CA199','CEA','PG1','PG2','PGR')], histogram=TRUE, pch=19,method = 'spearman')
#与胃萎缩的关系分析
table1(~AFP+CA199+CEA+CA199_pos+AFP_pos+CEA+CEA_pos+HBsAg_pos | PG_pos,data=pepsinogen,render.categorical=my.render.cat,render.continuous=c(.="Median [Q1,Q3]"))
chisq(x=c('CEA_pos','CA199_pos','AFP_pos','HBsAg_pos'),y='PG_pos',data=pepsinogen)
table1(~CA125_pos+CA153_pos | PG_pos,data=subset(pepsinogen,性别=='Female'),render.categorical=my.render.cat,render.continuous=c(.="Median [Q1,Q3]"))
chisq(x=c('CA125_pos','CA153_pos'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))

#
table1(~AFP+CA199+CEA+AFP_pos+CEA+CEA_pos+HBsAg_pos | PG_pos1,data=pepsinogen,render.categorical=my.render.cat,render.continuous=c(.="Median [Q1,Q3]"))
chisq(x=c('CEA_pos','CA199_pos','AFP_pos','HBsAg_pos'),y='PG_pos1',data=pepsinogen)
#
table1(~AFP+CA199+CEA+AFP_pos+CEA+CEA_pos+HBsAg_pos | PG_pos2,data=pepsinogen,render.categorical=my.render.cat,render.continuous=c(.="Median [Q1,Q3]"))
chisq(x=c('CEA_pos','CA199_pos','AFP_pos','HBsAg_pos'),y='PG_pos2',data=pepsinogen)

##AFP
table1(~CA199_pos | PG_pos,data=subset(pepsinogen,性别=='Female'),render.categorical=my.render.cat,render.continuous=c(.="Median [Q1,Q3]"))
chisq(x=c('CA199_pos'),y='PG_pos1',data=subset(pepsinogen,性别=='Female'))
#
table1(~CA199_pos | PG_pos,data=subset(pepsinogen,性别=='Male'),render.categorical=my.render.cat,render.continuous=c(.="Median [Q1,Q3]"))
chisq(x=c('CA199_pos'),y='PG_pos1',data=subset(pepsinogen,性别=='Male'))



###趋势性检验
CochranArmitageTest(with(pepsinogen,table(CEA_pos,PG_pos3)))
CochranArmitageTest(with(pepsinogen,table(AFP_pos,PG_pos3)))#有意义
CochranArmitageTest(with(pepsinogen,table(CA199_pos,PG_pos3)))
CochranArmitageTest(with(pepsinogen,table(HBsAg_pos,PG_pos3)))
CochranArmitageTest(with(subset(pepsinogen,性别=='Female'),table(CA125_pos,PG_pos3)))
CochranArmitageTest(with(subset(pepsinogen,性别=='Female'),table(CA153_pos,PG_pos3)))
##






