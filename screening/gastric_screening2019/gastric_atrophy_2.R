#敏感性分析，将患有残胃、萎缩性胃炎、胃肠上皮化石、异型增生、食管或胃上皮内瘤变的、
#门螺杆菌感染史、消化性溃疡的人去除，做一遍敏感性分析(共5537人，去除490人)
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
source('~/Rcode/screening/gastric_screening2019/stomach_data.R')
source('~/Rcode/statistics/OR.R')
#性别和年龄
table1(~性别+年龄分组+年龄分组2 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x=c('性别','年龄分组'),y='PG_pos',data=pepsinogen2)
logit(x=c('年龄分组','性别'),y='PG_pos',data=pepsinogen2)
logit(x=c('年龄分组2'),y='PG_pos',data=pepsinogen2)

#性别分层年龄
#Female
table1(~年龄分组 | PG_pos,data=subset(pepsinogen2,性别=='Female'),render.categorical=my.render.cat)
chisq.test(with(subset(pepsinogen2,性别=='Female'),table(PG_pos,年龄分组)))
logit(x='年龄分组',y='PG_pos',data=subset(pepsinogen2,性别=='Female'))
#Male
table1(~年龄分组 | PG_pos,data=subset(pepsinogen2,性别=='Male'),render.categorical=my.render.cat)
chisq.test(with(subset(pepsinogen2,性别=='Male'),table(PG_pos,年龄分组)))
logit(x='年龄分组',y='PG_pos',data=subset(pepsinogen2,性别=='Male'))
#家庭收入、教育水平、婚姻、就业状况、血型
table1(~家庭收入+教育+婚姻+就业状况+血型 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x=variables1,y='PG_pos',data=pepsinogen2)
logit(x=variables1,y='PG_pos',data=pepsinogen2)
logit(x=c('家庭收入'),y='PG_pos',data=pepsinogen2)
logit(x=variables1,y='PG_pos',data=subset(pepsinogen2,性别=='Female'))
logit(x='血型2',y='PG_pos',data=subset(pepsinogen2,性别=='Female'))
#多因素矫正
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','家庭收入'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','教育'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','婚姻'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','就业状况'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','血型'),y='PG_pos',data=pepsinogen2)


#癌症家族史
table1(~胃癌家族史 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x='胃癌家族史',y='PG_pos',data=pepsinogen2)
logit(x=c('胃癌家族史','BMI_group','性别','年龄分组','吸烟1',
          '就业状况','饮酒','糖尿病','咖啡','油炸','偏酸'),y='PG_pos',data=pepsinogen2)


##血型
#
with(pepsinogen2,table(血型,PG_pos))
with(pepsinogen2,round(prop.table(table(血型,PG_pos),margin = 1),4)*100)
with(pepsinogen2,chisq.test(血型,PG_pos))
###饮食、饮茶、饮酒、饮食偏好
#
table1(~饮酒+喝茶+鲜奶+酸奶+咖啡+碳酸饮料+果味饮料+茶味饮料+蔬菜+水果+谷类+肉类+鸡蛋+
         水产品+薯类+杂粮+豆类+
         坚果+大蒜+菌类+油炸+烧烤+熏制+腌制+酱制+晒制+偏咸+偏辣+偏烫+偏酸+偏甜+偏硬 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x=variables4,y='PG_pos',data=pepsinogen2)
logit(x=variables4,y='PG_pos',data=pepsinogen2)
##
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','喝茶'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','鲜奶'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','酸奶'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','碳酸饮料'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','果味饮料'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','茶味饮料'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','蔬菜'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','水果'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','谷类'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','肉类'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','水产品'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','鸡蛋'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','杂粮'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','薯类'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','豆类'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','坚果'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','大蒜'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','菌类'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','油炸'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','烧烤'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','熏制'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','酱制'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','晒制'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','腌制'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','熏制'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','晒制'),y='PG_pos',data=pepsinogen2)

logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏咸'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏辣'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏烫'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏甜'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏硬'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸'),y='PG_pos',data=pepsinogen2)


#油炸
table1(~油炸 | PG_pos,data=subset(pepsinogen2,性别=='Female'),render.categorical=my.render.cat)
chisq(x='油炸',y='PG_pos',data=subset(pepsinogen2,性别=='Female'))
table1(~油炸 | PG_pos,data=subset(pepsinogen2,性别=='Male'),render.categorical=my.render.cat)
chisq(x='油炸',y='PG_pos',data=subset(pepsinogen2,性别=='Male'))
forest_model(glm(PG_pos~油炸,data=pepsinogen2,family = 'binomial'))
forest_model(glm(PG_pos~油炸+性别+年龄分组,data=pepsinogen2,family = 'binomial'))
forest_model(glm(PG_pos~油炸+性别+年龄分组+BMI_group+吸烟1+饮酒+就业状况+被动吸烟1+糖尿病,data=pepsinogen22,family = 'binomial'))

forest_model(glm(PG_pos~蔬菜,data=pepsinogen2,family = 'binomial'))
forest_model(glm(PG_pos~咖啡+性别+年龄分组,data=pepsinogen2,family = 'binomial'))
forest_model(glm(PG_pos~+性别+年龄分组+BMI_group+吸烟1+饮酒+就业状况+被动吸烟1+糖尿病+教育,data=pepsinogen22,family = 'binomial'))
summary(glm(PG_pos~油炸+性别+年龄分组+BMI_group+吸烟1+饮酒+就业状况+被动吸烟1+糖尿病+教育+吸烟1*油炸,data=pepsinogen22,family = 'binomial'))

###吸烟

table1(~吸烟1+吸烟2+吸烟3+被动吸烟1+被动吸烟2 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x=c('吸烟1','吸烟2','吸烟3','被动吸烟1','被动吸烟2'),y='PG_pos',data=pepsinogen2)
##OR
#多因素矫正
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟1'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟2'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟3'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','被动吸烟1'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','被动吸烟2'),y='PG_pos',data=pepsinogen2)

###BMI

table1(~BMI_group+BMI_group2 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x=c('BMI_group','BMI_group2'),y='PG_pos',data=pepsinogen2)
##多因素矫正
logit(x=c('性别','年龄分组','就业状况','饮酒','糖尿病','吸烟1','咖啡','油炸','偏酸','BMI_group'),y='PG_pos',data=pepsinogen2)
modelit_uni<-glm(PG_pos~性别+年龄分组+就业状况+饮酒+糖尿病+吸烟1+咖啡+油炸+偏酸+relevel(BMI_group2,ref='正常'),data=pepsinogen2,family = 'binomial')
tableit_uni<-data.frame(summary(modelit_uni)$coef)
tableit_uni$OR<-exp(tableit_uni$Estimate)
tableit_uni$LCL <- exp(tableit_uni$Estimate - tableit_uni$Std..Error * 1.96 )
tableit_uni$UCL <- exp(tableit_uni$Estimate + tableit_uni$Std..Error * 1.96 )
tableit_uni$`p-value` <- round(tableit_uni$Pr...z..,4)
tableit_uni$`OR(95%CI)`<-paste0(round(tableit_uni$OR,2),'(',round(tableit_uni$LCL,2),'-',round(tableit_uni$UCL,2),')')
tableit_uni
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常'),data=pepsinogen2,family = 'binomial'))



###基础疾病史
table1(~糖尿病+高血压+高血脂+冠心病| PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x=c('糖尿病','高血压','高血脂','冠心病'),y='PG_pos',data=pepsinogen2)
logit(x=c('糖尿病','高血压','高血脂','冠心病'),y='PG_pos',data=pepsinogen2)
#多因素矫正
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','糖尿病'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','高血压'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','高血脂'),y='PG_pos',data=pepsinogen2)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','冠心病'),y='PG_pos',data=pepsinogen2)
###运动相关因素
#
table1(~运动+跑步+快走+太极+广场舞+瑜伽+游泳+球类+器械 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
chisq(x=variables8,y='PG_pos',data=pepsinogen2)
logit(x=variables8,y='PG_pos',data=pepsinogen2)
#
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','运动'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','跑步'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','快走'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','太极'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','广场舞'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','瑜伽'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','游泳'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','球类'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','器械'),y='PG_pos',data=pepsinogen2)



###生活习惯因素
#
table1(~静态时间+手机使用时间+每天早餐+准点吃饭+吃饭速度+外出吃饭 | PG_pos,data=pepsinogen2,render.categorical=my.render.cat)
#
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','静态时间'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','手机使用时间'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','每天早餐'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','准点吃饭'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','吃饭速度'),y='PG_pos',data=pepsinogen2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','外出吃饭'),y='PG_pos',data=pepsinogen2)













