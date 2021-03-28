rm(list=ls())
library(rio)
library(tidyverse)
library(ggpubr)
library(blandr)
library(htmlTable)
library(patchwork)
#读取数据
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
source('~/Rcode/screening/gastric_screening2020/data2020.R')
pepsinogen2019_2020<-inner_join(pepsinogen2019,pepsinogen2020,by='persoID')
pepsinogen2019_2020_2<-pepsinogen2019_2020%>%mutate(PG_change=factor(case_when(
  PG_pos.x=='阳性' & PG_pos.y=='阳性' ~ 1,
  PG_pos.x=="阳性" & PG_pos.y=='阴性' ~ 2,
  PG_pos.x=="阴性" & PG_pos.y=='阴性' ~ 3,
  PG_pos.x=="阴性" & PG_pos.y=='阳性' ~ 4,
),levels=c(1,2,3,4),labels=c('Positive_Positive','Positive_Negative','Negative_Negative','Negative_Positve')),
PG1.change1=round((PG1.y-PG1.x)/PG1.x,4)*100,
PG2.change1=round((PG2.y-PG2.x)/PG2.x,4)*100,
PGR.change1=round((PGR.y-PGR.x)/PGR.x,4)*100,
PG1.change2=round(PG1.y/PG1.x,2),
PG2.change2=round(PG2.y/PG2.x,2),
PGR.change2=round(PGR.y/PGR.x,2),
#定义变化：
#1: 下降50%以上
#1：下降20-50%
#3：20%以内
#5：上升20-50%
#6：上升50%以上
PG1.change1.range=factor(case_when(
  PG1.change1<(-50) ~ 1,
  PG1.change1>=(-50) &  PG1.change1<(-20)~ 2,
  PG1.change1>=(-20) &  PG1.change1<20~ 3,
  PG1.change1>=20 &  PG1.change1<=50~ 4,
  PG1.change1>50 ~ 5
  
),levels=c(1,2,3,4,5),labels=c('Decreased ≥50%','Decreased 20-50%','Within 20%','Increased 20-50%','Increased ≥50%')),
PG2.change1.range=factor(case_when(
  PG2.change1<(-50) ~ 1,
  PG2.change1>=(-50) &  PG2.change1<(-20)~ 2,
  PG2.change1>=(-20) &  PG2.change1<20~ 3,
  PG2.change1>=20 &  PG2.change1<=50~ 4,
  PG2.change1>50 ~ 5
  
),levels=c(1,2,3,4,5),labels=c('Decreased ≥50%','Decreased 20-50%','Within 20%','Increased 20-50%','Increased ≥50%')),
PGR.change1.range=factor(case_when(
  PGR.change1<(-50) ~ 1,
  PGR.change1>=(-50) &  PGR.change1<(-20)~ 2,
  PGR.change1>=(-20) &  PGR.change1<20 ~ 3,
  PGR.change1>=20 &  PGR.change1<=50~ 4,
  PGR.change1>50 ~ 5
  
),levels=c(1,2,3,4,5),labels=c('Decreased ≥50%','Decreased 20-50%','Within 20%','Increased 20-50%','Increased ≥50%'))



)
summary(pepsinogen2019_2020_2[,c('PG_change','PG1.change1','PG2.change1','PGR.change1',
                                 'PG1.change2','PG2.change2','PGR.change2','PG1.change1.range','PG2.change1.range','PGR.change1.range')])
#PG变化分布情况
PG1.normal<-pepsinogen2019_2020_2%>%ggplot(aes(x=PG1.change1))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019_2020_2$PG1.change1),sd=sd(pepsinogen2019_2020_2$PG1.change1)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(-100,-50,0,50,100,200,300),expand = c(0,0))+scale_y_continuous(expand = c(0,0))
PG2.normal<-pepsinogen2019_2020_2%>%ggplot(aes(x=PG2.change1))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019_2020_2$PG2.change1),sd=sd(pepsinogen2019_2020_2$PG2.change1)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(-100,0,100,250),expand = c(0,0))+scale_y_continuous(expand = c(0,0))
PGR.normal<-pepsinogen2019_2020_2%>%ggplot(aes(x=PGR.change1))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019_2020_2$PGR.change1),sd=sd(pepsinogen2019_2020_2$PGR.change1)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(-100,-50,0,50,100,200,400),expand = c(0,0))+scale_y_continuous(expand = c(0,0))
PG1.normal | PG2.normal | PGR.normal
variables=c("癌症家族史.x","胃癌家族史.x","性别.x","年龄分组.x","婚姻.x",              
            "就业状况2.x",          
            "家庭收入2.x","教育年数.x", "血型2.x",             
            "运动.x","BMI_group.x","吸烟1.x","每天早餐.x","准点吃饭.x","吃饭速度.x",          
            "外出吃饭.x","静态时间.x","手机使用时间.x",      
            "睡眠时间.x","睡眠质量.x","夜班.x",              
            "饮酒.x","喝茶.x","鲜奶.x",              
            "碳酸饮料.x","果味饮料.x","酸奶.x",              
            "咖啡.x","蔬菜.x","水果.x",              
            "谷类.x","鸡蛋.x","杂粮.x" ,             
            "豆类.x","坚果.x","大蒜.x" ,             
            "菌类.x","油炸.x","烧烤.x",              
            "熏制.x","酱制.x","偏咸.x",              
            "腌制.x","偏辣.x","偏烫.x",              
            "偏酸.x","偏甜.x","偏硬.x" ,             
            "十二指肠溃疡.x","胃溃疡.x",            
            "胃息肉.x","幽门螺杆菌感染史.x",             
            "消化性溃疡.x","糖尿病.x","高血压.x",            
            "高血脂.x","冠心病.x")
#PG进展情况分布
with(pepsinogen2019_2020_2,table(PG1_range3.x,PG1_range3.y))
with(pepsinogen2019_2020_2,round(prop.table(table(PG1_range3.x,PG1_range3.y),1)*100,2))

with(pepsinogen2019_2020_2,table(PG2_range5.x,PG2_range5.y))
with(pepsinogen2019_2020_2,round(prop.table(table(PG2_range5.x,PG2_range5.y),1)*100,2))

with(pepsinogen2019_2020_2,table(PGR_range2.x,PGR_range2.y))
with(pepsinogen2019_2020_2,table(PG_pos3.x,PG_pos3.y))
#总体分布
make.table(dat=pepsinogen2019_2020_2,
           strat        = "PG_change",
           cat.varlist  = variables,
           cat.ptype    = c("chisq"),
           cat.rmstat   = list(c("row")),
           output       = "html")
#单因素OR
#阳转阳 VS 阳转阴
a<-logit(x=variables,y='PG_change',data=subset(pepsinogen2019_2020_2,PG_change=="Positive_Positive" | PG_change=="Positive_Negative"))
export(a,'~/a.xlsx')
#阴转阴 VS 阴转阳
b<-logit(x=variables,y='PG_change',data=subset(pepsinogen2019_2020_2,PG_change=="Negative_Negative" | PG_change=="Negative_Positve"))
export(b,'~/b.xlsx')
#基线时做过胃镜的人群
gastroscopy<-import('~/data/示范区胃镜结果--2020-8-17.xlsx')%>%mutate(ID.x=ID)
match<-inner_join(gastroscopy,pepsinogen2019_2020_2,by='ID.x')
match$type<-factor(match$type,levels=1:9,labels=c('正常','消化性溃疡','胃息肉','胃切除术','慢性胃炎','萎缩性胃炎','肠上皮化生','不典型增生','胃癌'))
table(match$type)
match%>%filter(!is.na(type))%>%ggplot(aes(x=type,fill=PG1.change1.range))+geom_bar(position = 'fill')
with(match,table(type,PG_change))





