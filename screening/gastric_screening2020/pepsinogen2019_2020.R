rm(list=ls())
library(rio)
library(tidyverse)
library(ggpubr)
library(blandr)
library(htmlTable)
#读取数据
source('~/Rcode/statistics/data_summary.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
source('~/Rcode/screening/gastric_screening2020/data2020.R')
names(pepsinogen2019)[-which(names(pepsinogen2019) %in% names(pepsinogen2020))]
#pepsinogen2019_2020<-inner_join(pepsinogen2019,pepsinogen2020,by='persoID')
match_id<-unlist(inner_join(pepsinogen2019,pepsinogen2020,by='persoID')%>%transmute(ID.y))%>%as.vector()
pepsinogen2020_2<-pepsinogen2020[-which(pepsinogen2020$ID %in% match_id),names(pepsinogen2019)]
pepsinogen2019_2020<-rbind(pepsinogen2019,pepsinogen2020_2)
rm(pepsinogen2020_2)
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(family="serif",size=14,face="bold"),
               axis.text=element_text(family="serif",size=14,face="bold"),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black'),
               legend.title = element_text(family='serif',size=15,face='bold'),
               legend.text = element_text(family = 'serif',size=15,face='bold'),
               legend.position = 'top'
)
summary(pepsinogen2019_2020)

with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PG1.2019,PG1.2020,type='p'))
with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PG2.2019,PG2.2020,type='p'))
with(subset(pepsinogen2019_2020,PG1.2019!=200),plot(PGR.2019,PGR.2020,type='p'))
with(pepsinogen2019_2020,table(C14_pos))
####
#定义变化：
#1：下降50%
#3：50%以内
#5：上升50%
###2019-2020做过两次胃镜的：14人
id<-c('120105195608253928','120103195610082125','120103195203214864','120103194810193829',
'120101196312230224','120105195603173320','120104196311250441','120101195706263526',
'120105195605281819','120102195904090722','120102195708112162','640103195008121840',
'120101195310264020','120103194609132610')
pepsinogen2019_2020%>%filter(persoID %in% id)%>%select(name.x,PG1.x,PG2.x,PGR.x,PG1.y,PG2.y,PGR.y)

###所有人一年内PG的变化情况
pepsinogen2019_2020<-pepsinogen2019_2020%>%filter(PG1.x!='200')%>%mutate(PG1.change=round((PG1.y-PG1.x)/PG1.x,4)*100,
                                                  PG2.change=round((PG2.y-PG2.x)/PG2.x,4)*100,
                                                  PGR.change=round((PGR.y-PGR.x)/PGR.x,4)*100)
summary(pepsinogen2019_2020[,c('PG1.change','PG2.change','PGR.change')])
##变化分布
PG1.normal<-pepsinogen2019_2020%>%ggplot(aes(x=PG1.change))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
 stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019_2020$PG1.change),sd=sd(pepsinogen2019_2020$PG1.change)),col='red',size=1)+mytheme+
scale_x_continuous(breaks=c(-100,-50,0,50,100,200,300),expand = c(0,0))+scale_y_continuous(expand = c(0,0))

PG2.normal<-pepsinogen2019_2020%>%ggplot(aes(x=PG2.change))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019_2020$PG2.change),sd=sd(pepsinogen2019_2020$PG2.change)),col='red',size=1)+mytheme+
scale_x_continuous(breaks=c(-100,0,100,250,500),expand = c(0,0))+scale_y_continuous(expand = c(0,0))

PGR.normal<-pepsinogen2019_2020%>%ggplot(aes(x=PGR.change))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
 stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2019_2020$PGR.change),sd=sd(pepsinogen2019_2020$PGR.change)),col='red',size=1)+mytheme+
scale_x_continuous(breaks=c(-100,-50,0,50,100,200,400),expand = c(0,0))+scale_y_continuous(expand = c(0,0))
  PG1.normal | PG2.normal | PGR.normal

  
###PG变化的因素分析


#####合并2019和2020年的数据，分析影响PG阳性的因素####
variables<-c("胃癌家族史","性别",'年龄分组',"就业状况2","家庭收入2","教育年数",        
               "血型1", "运动",'BMI_group', "吸烟1",  
               "被动吸烟1","饮酒",   
               "喝茶",    "鲜奶",    "酸奶",    "咖啡","碳酸饮料",        
               "果味饮料","蔬菜",    "水果",    "谷类",   
               "鸡蛋",    "杂粮",    "豆类",     "坚果",   
               "大蒜",    "菌类",    "油炸",   
               "烧烤",    "熏制",    "酱制",    "腌制",   "偏咸",   
               "偏辣",    "偏烫",    "偏酸",    "偏甜",   
               "偏硬",'每天早餐','准点吃饭'  , "吃饭速度","外出吃饭","睡眠时间" ,"睡眠质量","夜班" ,
               "静态时间", "重度精神问题",
               "胃食管反流性疾病","胃息肉",'胃溃疡','十二指肠溃疡','消化性溃疡',
               "糖尿病",  "高血压",  "高血脂", 
               "冠心病","重度精神问题")
#PG阳性(PGI≤70 & PGI/II ratio ≤3)
make.table(dat=pepsinogen2019_2020,
             strat        = "PG_pos",
             cat.varlist  = variables,
             cat.ptype    = c("chisq"),
             cat.rmstat   = list(c("row")),
             output       = "html")
#PG阳性的单因素logistic回归
OR_uni<-logit(x=variables,y='PG_pos',data=pepsinogen2019_2020)
export(OR_uni,'~/OR_uni.xlsx')

###多因素
#####人口学特征####
#只矫正性别和年龄
logit(x=c('性别','年龄分组'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','就业状况2'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','家庭收入2'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','教育年数'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','血型1'),y='PG_pos',data=pepsinogen2019_2020)
#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group
logit(x=c('性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('家庭收入2','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('教育年数','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('血型1','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group、糖尿病、消化性溃疡
logit(x=c('性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('家庭收入2','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('教育年数','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('血型1','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
#####胃癌危险因素####
#只矫正性别和年龄
logit(x=c('胃癌家族史','性别','年龄分组'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','吸烟1'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','被动吸烟1'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','饮酒'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','油炸'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','烧烤'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','腌制'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group
logit(x=c('胃癌家族史','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('BMI_group','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('吸烟1','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('被动吸烟1','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('饮酒','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('油炸','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('烧烤','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('腌制','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group、糖尿病、消化性溃疡
logit(x=c('胃癌家族史','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('BMI_group','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('吸烟1','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('被动吸烟1','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('饮酒','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('油炸','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('烧烤','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('腌制','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)

#####疾病史因素####

#只矫正性别和年龄
logit(x=c('十二指肠溃疡','性别','年龄分组'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','胃溃疡'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','胃息肉'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','消化性溃疡'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','糖尿病'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','高血压'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','高血脂'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('性别','年龄分组','冠心病'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group
logit(x=c('十二指肠溃疡','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('胃溃疡','BMI_group','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('胃息肉','BMI_group','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('消化性溃疡','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('糖尿病','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('高血压','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('高血脂','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('冠心病','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group、糖尿病、消化性溃疡
logit(x=c('十二指肠溃疡','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('胃溃疡','BMI_group','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('胃息肉','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('消化性溃疡','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('糖尿病','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('高血压','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('高血脂','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('冠心病','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)


#####生活习惯因素####

#只矫正性别和年龄
logit(x=c('运动','性别','年龄分组'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('每天早餐','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('准点吃饭','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('吃饭速度','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('外出吃饭','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('静态时间','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('睡眠时间','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('睡眠质量','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('夜班','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group
logit(x=c('运动','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('每天早餐','BMI_group','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('准点吃饭','BMI_group','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('吃饭速度','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('外出吃饭','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('静态时间','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('睡眠时间','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('睡眠质量','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('夜班','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group、糖尿病、消化性溃疡
logit(x=c('运动','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('每天早餐','BMI_group','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('准点吃饭','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('吃饭速度','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('外出吃饭','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('静态时间','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('睡眠时间','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('睡眠质量','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('夜班','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)

#####饮食相关因素####

#只矫正性别和年龄
logit(x=c('喝茶','性别','年龄分组'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('鲜奶','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('碳酸饮料','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('果味饮料','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('酸奶','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('咖啡','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('蔬菜','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('水果','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('谷类','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('鸡蛋','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('杂粮','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('豆类','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('坚果','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('大蒜','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('菌类','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏咸','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏辣','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏烫','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏酸','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏甜','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏硬','年龄分组','性别'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group
logit(x=c('喝茶','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('鲜奶','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('碳酸饮料','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('果味饮料','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('酸奶','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('咖啡','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('蔬菜','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('水果','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('谷类','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('鸡蛋','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('杂粮','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('豆类','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('坚果','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('大蒜','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('菌类','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏咸','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏辣','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏烫','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏酸','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏甜','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏硬','性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)

#矫正性别、年龄、就业状况、吸烟、饮酒、BMI_group、糖尿病、消化性溃疡
logit(x=c('喝茶','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('鲜奶','BMI_group','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('碳酸饮料','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('果味饮料','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('酸奶','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('咖啡','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('蔬菜','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('水果','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('谷类','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('鸡蛋','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('杂粮','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('豆类','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('坚果','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('大蒜','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('菌类','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏咸','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏辣','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏烫','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏酸','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏甜','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)
logit(x=c('偏硬','性别','年龄分组','就业状况2','消化性溃疡','糖尿病','吸烟1','饮酒','BMI_group'),y='PG_pos',data=pepsinogen2019_2020)


##2019年与2020年比较
make.table(dat=pepsinogen2019,
           strat        = "PG_pos",
           cat.varlist  = variables,
           cat.ptype    = c("chisq"),
           cat.rmstat   = list(c("row")),
           output       = "html")

make.table(dat=pepsinogen2020,
           strat        = "PG_pos",
           cat.varlist  = c(variables,'Hp_pos2'),
           cat.ptype    = c("chisq"),
           cat.rmstat   = list(c("row")),
           output       = "html")
logit(x=c(variables,'Hp_pos'),y='PG_pos',data=pepsinogen2020)
logit2<-function(x,var,y,data){
  dataframe<-logit(x=c(x,var),y=y,data=data)
  dataframe2<-dataframe%>%filter(str_starts(variable,x))
  return(dataframe2)
}
##变量
baseline<-c('性别','年龄分组','就业状况2','家庭收入2','教育年数','血型1')
gastric_risk<-c('胃癌家族史','BMI_group','吸烟1','被动吸烟1','饮酒','油炸','烧烤','腌制')
disease<-c('十二指肠溃疡','胃溃疡','胃息肉','消化性溃疡','糖尿病','高血压','高血脂','冠心病')
habit<-c('运动','每天早餐','准点吃饭','吃饭速度','外出吃饭','静态时间','睡眠时间','睡眠质量','夜班')
diet<-c('喝茶','鲜奶','碳酸饮料','果味饮料','酸奶','咖啡','蔬菜','水果','谷类','鸡蛋','杂粮','豆类','坚果','大蒜','菌类','偏辣','偏烫','偏酸','偏甜','偏硬')
Model1<-c('性别','年龄分组')
Model2<-c('性别','年龄分组','就业状况2','吸烟1','饮酒','BMI_group')
Model3<-c(Model2,'消化性溃疡','糖尿病')
Model4<-c(Model3,'Hp_pos2')

##2020
# basleine uni and Model1
frame1<-list()
for(i in baseline){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# baseline and uni and Model2
frame1<-list()
for(i in baseline){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# baseline and uni and Model3
frame1<-list()
for(i in baseline){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# baseline and uni and Model4
frame1<-list()
for(i in baseline){
  frame1[[i]]<-logit2(x=i,var=Model4,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)


# gastric_risk and uni and Model1
frame1<-list()
for(i in gastric_risk){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# gastric_risk and uni and Model2
frame1<-list()
for(i in gastric_risk){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# gastric_risk and uni and Model3
frame1<-list()
for(i in gastric_risk){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# gastric_risk and uni and Model4
frame1<-list()
for(i in gastric_risk){
  frame1[[i]]<-logit2(x=i,var=Model4,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# disease and uni and Model1
frame1<-list()
for(i in disease){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# disease and uni and Model2
frame1<-list()
for(i in disease){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# disease and uni and Model3
frame1<-list()
for(i in disease){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# disease and uni and Model4
frame1<-list()
for(i in disease){
  frame1[[i]]<-logit2(x=i,var=Model4,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# habit and uni and Model1
frame1<-list()
for(i in habit){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# habit and uni and Model2
frame1<-list()
for(i in habit){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# habit and uni and Model3
frame1<-list()
for(i in habit){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# habit and uni and Model4
frame1<-list()
for(i in habit){
  frame1[[i]]<-logit2(x=i,var=Model4,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# diet and uni and Model1
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# diet and uni and Model2
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# diet and uni and Model3
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# diet and uni and Model4
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model4,y='PG_pos',data=pepsinogen2020)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

#####2019年分析结果####

# basleine uni and Model1
frame1<-list()
for(i in baseline){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# baseline and uni and Model2
frame1<-list()
for(i in baseline){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# baseline and uni and Model3
frame1<-list()
for(i in baseline){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)


# gastric_risk and uni and Model1
frame1<-list()
for(i in gastric_risk){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# gastric_risk and uni and Model2
frame1<-list()
for(i in gastric_risk){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# gastric_risk and uni and Model3
frame1<-list()
for(i in gastric_risk){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# disease and uni and Model1
frame1<-list()
for(i in disease){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# disease and uni and Model2
frame1<-list()
for(i in disease){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# disease and uni and Model3
frame1<-list()
for(i in disease){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# habit and uni and Model1
frame1<-list()
for(i in habit){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# habit and uni and Model2
frame1<-list()
for(i in habit){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# habit and uni and Model3
frame1<-list()
for(i in habit){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)


# diet and uni and Model1
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model1,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)
# diet and uni and Model2
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model2,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# diet and uni and Model3
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model3,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)

# diet and uni and Model4
frame1<-list()
for(i in diet){
  frame1[[i]]<-logit2(x=i,var=Model4,y='PG_pos',data=pepsinogen2019)
}
do.call(rbind,frame1)%>%rownames_to_column()%>%select(-rowname)


###PG阳性随年龄的变化
pepsinogen2019_2020%>%group_by(年龄分组3,PG_pos)%>%summarise(n=n())%>%filter(!is.na(年龄分组3))%>%
  group_by(年龄分组3)%>%mutate(percent=round(n/sum(n),2)*100)%>%filter(PG_pos=='阳性')%>%
  ggplot(aes(x=年龄分组3,y=percent))+geom_point()+geom_line(aes(group=1))+scale_y_continuous(limits = c(0,70))+mytheme+
  labs(x='年龄',y='Percent(%)')

pepsinogen2019_2020%>%group_by(性别,年龄分组3,PG_pos)%>%summarise(n=n())%>%filter(!is.na(年龄分组3))%>%
  group_by(年龄分组3)%>%mutate(percent=round(n/sum(n),2)*100)%>%filter(PG_pos=='阳性')%>%
  ggplot(aes(x=年龄分组3,y=percent,color=性别))+geom_point()+geom_line(aes(group=性别))+scale_y_continuous(limits = c(0,70))+mytheme+
  labs(x='年龄',y='Percent(%)')


