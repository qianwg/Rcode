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
pepsinogen2_1<-pepsinogen2%>%filter(PG_pos4!='重度萎缩')
pepsinogen2_2<-pepsinogen2%>%filter(PG_pos4!='一般萎缩')
source('~/Rcode/statistics/OR.R')
####根据PG1与PGR将人群分为正常、一般萎缩、重度萎缩进行分析
#性别和年龄
table1(~性别+年龄分组 | PG_pos4,data=pepsinogen2_1,render.categorical=my.render.cat)
logit(x=c('性别','年龄分组'),y='PG_pos4',data=pepsinogen2_1)
#
table1(~性别+年龄分组 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
logit(x=c('性别','年龄分组'),y='PG_pos4',data=pepsinogen2_2)

##胃癌家族史
table1(~胃癌家族史 | PG_pos4,data=pepsinogen2_1,render.categorical=my.render.cat)
#chisq(x='胃癌家族史',y='PG_pos',data=pepsinogen)
logit(x=c('胃癌家族史','BMI_group','性别','年龄分组','吸烟1',
          '就业状况','饮酒','糖尿病','咖啡','油炸','偏酸'),y='PG_pos4',data=pepsinogen2_1)
#
table1(~胃癌家族史 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
#chisq(x='胃癌家族史',y='PG_pos',data=pepsinogen)
logit(x=c('胃癌家族史','BMI_group','性别','年龄分组','吸烟1',
          '就业状况','饮酒','糖尿病','咖啡','油炸','偏酸'),y='PG_pos4',data=pepsinogen2_2)

#家庭收入、教育水平、婚姻、就业状况、血型
table1(~家庭收入+教育+婚姻+就业状况+血型 | PG_pos4,data=pepsinogen2_1,render.categorical=my.render.cat)
#多因素矫正
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','家庭收入'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','教育'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','婚姻'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','就业状况'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','血型'),y='PG_pos4',data=pepsinogen2_1)

##
table1(~家庭收入+教育+婚姻+就业状况+血型 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
#多因素矫正
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','家庭收入'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','教育'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','婚姻'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','就业状况'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','吸烟1','饮酒','糖尿病','血型'),y='PG_pos4',data=pepsinogen2_2)

##吸烟
table1(~吸烟1+吸烟2+吸烟3+被动吸烟1+被动吸烟2 | PG_pos4,data=pepsinogen2_1,render.categorical=my.render.cat)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟1'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟2'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟3'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','被动吸烟1'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','被动吸烟2'),y='PG_pos4',data=pepsinogen2_1)
#
table1(~吸烟1+吸烟2+吸烟3+被动吸烟1+被动吸烟2 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟1'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟2'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','吸烟3'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','被动吸烟1'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','BMI_group','就业状况','饮酒','糖尿病','油炸','被动吸烟2'),y='PG_pos4',data=pepsinogen2_2)


###性别分层

table1(~吸烟2+被动吸烟1 | PG_pos4,data=subset(pepsinogen22,性别=='Female'),render.categorical=my.render.cat)
chisq(x=c('吸烟2','被动吸烟1'),y='PG_pos4',data=subset(pepsinogen22,性别=='Female'))
table1(~吸烟2+被动吸烟1 | PG_pos4,data=subset(pepsinogen22,性别=='Male'),render.categorical=my.render.cat)
chisq(x=c('吸烟2','被动吸烟1'),y='PG_pos4',data=subset(pepsinogen22,性别=='Male'))
##血型
with(subset(pepsinogen22,血型1!='不详'),table(血型1,PG_pos4))
with(subset(pepsinogen22,血型1!='不详'),round(prop.table(table(血型1,PG_pos4),margin = 2),4)*100)
with(subset(pepsinogen22,血型1!='不详'),chisq.test(血型1,PG_pos4))
#
with(subset(pepsinogen22,血型2!='不详'),table(血型2,PG_pos4))
with(subset(pepsinogen22,血型2!='不详'),round(prop.table(table(血型2,PG_pos4),margin = 2),4)*100)
with(subset(pepsinogen22,血型2!='不详'),chisq.test(血型2,PG_pos4))

###饮食、饮茶、饮酒、饮食偏好
table1(~饮酒+喝茶+鲜奶+酸奶+咖啡+碳酸饮料+果味饮料+茶味饮料+蔬菜+水果+谷类+肉类+鸡蛋+
         水产品+薯类+杂粮+豆类+
         坚果+大蒜+菌类+油炸+烧烤+熏制+腌制+酱制+晒制+偏咸+偏辣+偏烫+偏酸+偏甜+偏硬 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)

##
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','喝茶'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','鲜奶'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','酸奶'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','碳酸饮料'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','果味饮料'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','茶味饮料'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','蔬菜'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','水果'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','谷类'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','肉类'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','水产品'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','鸡蛋'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','杂粮'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','薯类'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','豆类'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','坚果'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','大蒜'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','菌类'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','油炸'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','烧烤'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','熏制'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','酱制'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','晒制'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','腌制'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','熏制'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','晒制'),y='PG_pos4',data=pepsinogen2_1)

logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏咸'),y='PG_pos',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏辣'),y='PG_pos',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏烫'),y='PG_pos',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏甜'),y='PG_pos',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','偏硬'),y='PG_pos',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸'),y='PG_pos',data=pepsinogen2_2)


###运动相关因素
table1(~运动+跑步+快走+太极+广场舞+瑜伽+游泳+球类+器械 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)

#
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','运动'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','跑步'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','快走'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','太极'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','广场舞'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','瑜伽'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','游泳'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','球类'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','器械'),y='PG_pos4',data=pepsinogen2_2)

###BMI
pepsinogen22%>%group_by(PG_pos4)%>%summarise(n=n(),mean=mean(BMI),sd=sd(BMI))
kruskal.test(BMI~PG_pos4,data=pepsinogen22)
table1(~BMI_group+BMI_group2 | PG_pos4,data=pepsinogen22,render.categorical=my.render.cat)
chisq(x=c('BMI_group','BMI_group2'),y='PG_pos4',data=pepsinogen22)
####疾病史
table1(~糖尿病+高血压+高血脂+冠心病| PG_pos4,data=pepsinogen2_1,render.categorical=my.render.cat)
chisq(x=c('糖尿病','高血压','高血脂','冠心病'),y='PG_pos4',data=pepsinogen2_1)
#多因素矫正
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','糖尿病'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','高血压'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','高血脂'),y='PG_pos4',data=pepsinogen2_1)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','冠心病'),y='PG_pos4',data=pepsinogen2_1)
##
table1(~糖尿病+高血压+高血脂+冠心病| PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
#多因素矫正
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','糖尿病'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','高血压'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','高血脂'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('性别','年龄分组','就业状况','饮酒','吸烟1','咖啡','油炸','偏酸','BMI_group','冠心病'),y='PG_pos4',data=pepsinogen2_2)

###生活习惯因素
#
table1(~静态时间+手机使用时间+每天早餐+准点吃饭+吃饭速度+外出吃饭 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
#
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','静态时间'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','手机使用时间'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','每天早餐'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','准点吃饭'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','吃饭速度'),y='PG_pos4',data=pepsinogen2_2)
logit(x=c('饮酒','性别','年龄分组','就业状况','吸烟1','糖尿病','咖啡','油炸','偏酸','外出吃饭'),y='PG_pos4',data=pepsinogen2_2)

###BMI

table1(~BMI_group+BMI_group2 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
chisq(x=c('BMI_group','BMI_group2'),y='PG_pos4',data=pepsinogen2_1)
##多因素矫正
logit(x=c('性别','年龄分组','就业状况','饮酒','糖尿病','吸烟1','咖啡','油炸','偏酸','BMI_group'),y='PG_pos4',data=pepsinogen2_1)
forest_model(glm(PG_pos4~性别+年龄分组+就业状况+饮酒+糖尿病+吸烟1+咖啡+油炸+偏酸+relevel(BMI_group2,ref='正常'),data=pepsinogen2_1,family = 'binomial'))

#forest_model(glm(PG_pos4~relevel(BMI_group2,ref='正常'),data=pepsinogen2_1,family = 'binomial'))
#
table1(~BMI_group+BMI_group2 | PG_pos4,data=pepsinogen2_2,render.categorical=my.render.cat)
chisq(x=c('BMI_group','BMI_group2'),y='PG_pos4',data=pepsinogen2_2)
##多因素矫正
logit(x=c('性别','年龄分组','就业状况','饮酒','糖尿病','吸烟1','咖啡','油炸','偏酸','BMI_group'),y='PG_pos4',data=pepsinogen2_2)
forest_model(glm(PG_pos4~性别+年龄分组+就业状况+饮酒+糖尿病+吸烟1+咖啡+油炸+偏酸+relevel(BMI_group2,ref='正常'),data=pepsinogen2_2,family = 'binomial'))






##胃萎缩与胃癌前病变/胃癌的关系
gastric<-import('~/data/胃癌及癌前病变.xlsx')
gastric2<-inner_join(gastric,pepsinogen22,by='ID')
gastric2%>%filter(!is.na(type))%>%transmute(type=factor(type,levels=c('胃癌','异型增生','肠上皮化生','萎缩性胃炎')),PG_pos4)%>%group_by(type,PG_pos4)%>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = type, y = perc, fill =PG_pos4)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_stack(vjust = 0.5))+
  labs(x='',y=' ',fill='PG_pos')+scale_x_discrete(labels=c('cancer','dysplasia','intestinal metaplasia','Atrophic gastritis'))+
  scale_fill_discrete(labels=c('Healthy','Mild-moderate','Severe'))+coord_flip()



