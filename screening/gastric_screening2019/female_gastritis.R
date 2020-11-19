rm(list = ls())
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
library(rms)
library(ggstatsplot)
library(htmlTable)
library(nnet)
library(effects)
library(VGAM)
source('~/Rcode/screening/gastric_screening2019/sensitivity_data.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
##萎缩性胃炎
make.table(dat=pepsinogen,
           strat        = "PG_pos",
           cat.varlist  = c("性别",'绝经分类','口服避孕药','雌激素代替治疗','子宫摘除术','卵巢摘除术','雌激素影响时间分组'),
           cat.rmstat   = c("row"),
           cat.ptype    = c("chisq"),
           output       = "html")
#一般萎缩及重度萎缩
make.table(dat=pepsinogen,
           strat        = "PG_pos4",
           cat.varlist  = c("性别",'绝经分类','口服避孕药','雌激素代替治疗','子宫摘除术','卵巢摘除术','雌激素影响时间分组'),
           cat.rmstat   = c("col"),
           cat.ptype    = c("chisq"),
           output       = "html")
###女性人群中的分布
make.table(dat=subset(pepsinogen,性别=='Female'),
           strat        = "PG_pos",
           cat.varlist  = c("初潮年龄分组",'生育','首次生育年龄分组','绝经年龄分组',
                            '绝经','口服避孕药','雌激素代替治疗','子宫摘除术','卵巢摘除术','妇科手术史',
                            '雌激素影响时间分组'),
           cat.rmstat   = c("col"),
           cat.ptype    = c("chisq"),
           output       = "html")

##多因素矫正
logit(x=c('绝经','年龄','BMI_group','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('生育','年龄','BMI_group','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('初潮年龄分组','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('首次生育年龄分组','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('绝经年龄分组','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('口服避孕药','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('雌激素代替治疗','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('雌激素影响时间分组','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))
logit(x=c('妇科手术史','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen,性别=='Female'))








