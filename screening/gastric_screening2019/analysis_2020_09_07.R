rm(list=ls())
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
library(rms)
library(htmlTable)
library(nnet)
library(effects)
library(VGAM)
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
variables<-c("胃癌家族史","性别",'年龄分组5',"就业状况2","家庭收入2","教育年数",        
             "血型1", "运动","BMI_group2" ,  "吸烟1",  
             "被动吸烟1","饮酒",   
             "喝茶",    "鲜奶",    "酸奶",    "咖啡","碳酸饮料",        
             "果味饮料","蔬菜",    "水果",    "谷类",   
             "鸡蛋",    "杂粮",    "豆类",     "坚果",   
             "大蒜",    "菌类",    "油炸",   
             "烧烤",    "熏制",    "酱制",    "偏咸",    "腌制",   
             "偏辣",    "偏烫",    "偏酸",    "偏甜",   
             "偏硬",'每天早餐','准点吃饭'  , "吃饭速度","外出吃饭","睡眠时间" ,"睡眠质量","夜班" ,
             "静态时间", "重度精神问题",
             "胃食管反流性疾病","胃息肉","萎缩性胃炎","胃肠上皮化生","胃粘膜异性增生",'消化性溃疡',
             "糖尿病",  "高血压",  "高血脂", 
             "冠心病","重度精神问题")
###数据：去除自身癌和胃部手术切除术
pepsinogen2019<-pepsinogen2019%>%
  filter(ID!=31030159,ID!=31060461,自身癌!='是',残胃!='是')
#PG_pos13的基本分布

with(pepsinogen2019,prop.table(table(PG_pos13)))
###比较吸烟者和不吸烟者中发生萎缩性胃炎的危险因素
#目前或现在吸烟
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,吸烟1=="目前吸烟" |吸烟1=="以前吸烟" ))
logit(x=c('油炸','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,吸烟1=="目前吸烟" |吸烟1=="以前吸烟" ))

#从不吸烟
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,吸烟1=="从不吸烟"))
logit(x=c('喝茶','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,吸烟1=="从不吸烟"))
logit(x=c('碳酸饮料','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,吸烟1=="从不吸烟"))
logit(x=c('大蒜','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,吸烟1=="从不吸烟"))
logit(x=c('油炸','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,吸烟1=="从不吸烟"))
##代谢综合征
with(pepsinogen2019,table(代谢综合征))
with(pepsinogen2019,table(代谢紊乱相关因素))



##
table(pepsinogen2019$阳性判定结果)
table(pepsinogen2019$CA199_pos)
table(pepsinogen2019$PG_pos)
pos<-pepsinogen2019%>%filter(阳性判定结果=="胃部高危" | CA199_pos=='阳性' | PG_pos=='阳性')

###多元线性回归
summary(lm(PG1~性别+年龄分组+就业状况2+教育年数+家庭收入2+血型1+BMI_group+吸烟1+饮酒+碳酸饮料+
             果味饮料+大蒜+油炸+咖啡+喝茶,data=pepsinogen2019))

summary(lm(PG1~性别+年龄分组+饮酒+吸烟1,data=pepsinogen2019))
summary(lm(PG1~性别+饮酒,data=subset(pepsinogen2019,吸烟1=="从不吸烟")))
summary(lm(PG1~性别+饮酒,data=pepsinogen2019,吸烟1=="目前吸烟"))
pepsinogen2019%>%filter(吸烟1=="目前吸烟",性别=="Female")%>%group_by(饮酒)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
pepsinogen2019%>%filter(吸烟1=="从不吸烟",性别=="Female")%>%group_by(饮酒)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
pepsinogen2019%>%filter(吸烟1=="过去吸烟",性别=="Female")%>%group_by(饮酒)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))

pepsinogen2019%>%filter(吸烟1=="目前吸烟",性别=="Male")%>%group_by(饮酒)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
pepsinogen2019%>%filter(吸烟1=="从不吸烟",性别=="Male")%>%group_by(饮酒)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
pepsinogen2019%>%filter(吸烟1=="过去吸烟",性别=="Male")%>%group_by(饮酒)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))

wilcox.test(PG1~饮酒,data=subset(pepsinogen2019,吸烟1=="目前吸烟" & 性别=="Female"))
wilcox.test(PG1~饮酒,data=subset(pepsinogen2019,吸烟1=="从不吸烟" & 性别=="Female"))
wilcox.test(PG1~饮酒,data=subset(pepsinogen2019,吸烟1=="过去吸烟" & 性别=="Female"))
wilcox.test(PG1~饮酒,data=subset(pepsinogen2019,吸烟1=="目前吸烟" & 性别=="Male"))
wilcox.test(PG1~饮酒,data=subset(pepsinogen2019,吸烟1=="从不吸烟" & 性别=="Male"))
wilcox.test(PG1~饮酒,data=subset(pepsinogen2019,吸烟1=="过去吸烟" & 性别=="Male"))

###
pepsinogen2019$aaa[pepsinogen2019$性别=="Female" & pepsinogen2019$吸烟1=="从不吸烟"]<-1
pepsinogen2019$aaa[pepsinogen2019$性别=="Female" & pepsinogen2019$吸烟1=="目前吸烟"]<-2
pepsinogen2019$aaa[pepsinogen2019$性别=="Female" & pepsinogen2019$吸烟1=="过去吸烟"]<-3
pepsinogen2019$aaa[pepsinogen2019$性别=="Male" & pepsinogen2019$吸烟1=="从不吸烟"]<-4
pepsinogen2019$aaa[pepsinogen2019$性别=="Male" & pepsinogen2019$吸烟1=="目前吸烟"]<-5
pepsinogen2019$aaa[pepsinogen2019$性别=="Male" & pepsinogen2019$吸烟1=="过去吸烟"]<-6
table(pepsinogen2019$aaa)
#######2020年数据
source('~/Rcode/screening/gastric_screening2020/data2020.R')


wilcox.test(PG1~饮酒,data=subset(pepsinogen2020,Hp_pos=='阴性'))
wilcox.test(PG1~饮酒,data=subset(pepsinogen2020,Hp_pos=='阳性'))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阳性'))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阳性' & 性别=="Female"))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阳性' & 性别=="Male"))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阴性' & 性别=="Female"))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阴性' & 性别=="Male"))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阴性'))
summary(lm(PG1~性别+年龄分组+吸烟1+饮酒,data=pepsinogen2020))



