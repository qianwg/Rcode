rm(list=ls())
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
##2020-7-06
#数据读取
source('~/Rcode/screening/gastric_screening2020/stomach_data.R')
summary(pepsinogen)
#油炸
wilcox.test(PG1~油炸,data=pepsinogen)
wilcox.test(PG1~性别,data=pepsinogen)
kruskal.test(PG1~年龄分组,data=pepsinogen)


table1(~饮酒+喝茶+鲜奶+酸奶+咖啡+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+
         坚果+大蒜+菌类+油炸 | PG_pos,data=pepsinogen,render.categorical=my.render.cat)
chisq(x=variables9,y='PG_pos',data=pepsinogen)
table1(~油炸 | PG_pos,data=subset(pepsinogen,性别=='Female'),render.categorical=my.render.cat)
chisq(x='油炸',y='PG_pos',data=subset(pepsinogen,性别=='Female'))
table1(~油炸 | PG_pos,data=subset(pepsinogen,性别=='Male'),render.categorical=my.render.cat)
chisq(x='油炸',y='PG_pos',data=subset(pepsinogen,性别=='Male'))
forest_model(glm(PG_pos~油炸,data=pepsinogen,family = 'binomial'))
forest_model(glm(PG_pos~油炸+性别+年龄分组,data=pepsinogen,family = 'binomial'))
forest_model(glm(PG_pos~油炸+性别+年龄分组+BMI_group+吸烟1+饮酒+就业状况+被动吸烟1+糖尿病,data=pepsinogen2,family = 'binomial'))
forest_model(glm(PG_pos~性别,data=pepsinogen,family = 'binomial'))
forest_model(glm(PG_pos~年龄分组,data=pepsinogen,family = 'binomial'))
forest_model(glm(PG_pos~Hp_pos,data=pepsinogen,family = 'binomial'))
forest_model(glm(PG_pos~糖尿病,data=pepsinogen,family = 'binomial'))



#性别分层

##幽门螺杆菌
table1(~ | PG_pos,data=pepsinogen,render.categorical=my.render.cat)
chisq(x=variables9,y='PG_pos',data=pepsinogen)
table1(~油炸 | PG_pos,data=subset(pepsinogen,性别=='Female'),render.categorical=my.render.cat)
chisq(x='油炸',y='PG_pos',data=subset(pepsinogen,性别=='Female'))
table1(~油炸 | PG_pos,data=subset(pepsinogen,性别=='Male'),render.categorical=my.render.cat)
chisq(x='油炸',y='PG_pos',data=subset(pepsinogen,性别=='Male'))
forest_model(glm(PG_pos~油炸,data=pepsinogen,family = 'binomial'))
forest_model(glm(PG_pos~油炸+性别+年龄分组,data=pepsinogen,family = 'binomial'))
forest_model(glm(PG_pos~油炸+性别+年龄分组+BMI_group+吸烟1+饮酒+就业状况+被动吸烟1+糖尿病,data=pepsinogen2,family = 'binomial'))
