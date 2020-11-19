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
source('~/Rcode/biomarker/stomach_data.R')
#基本特征分布
#summary(pepsinogen)
table1(~ 胃癌家族史+年龄+性别+家庭收入+教育+婚姻+BMI+吸烟+
         手机使用时间+饮酒+喝茶+酸奶+咖啡+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+大蒜+菌类+油炸+烧烤+
         熏制+酱制+偏咸+腌制+
         十二指肠溃疡+胃溃疡+胃息肉+ 幽门螺杆菌感染史+癌前病变+
         残胃+糖尿病+高血压+高血脂+冠心病 | PG_pos, data=pepsinogen,render.categorical=my.render.cat)
###<<<<<<<<<PG1
#
PG1.plot<-pepsinogen%>%group_by(PG1_range)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
ggbarplot(x='PG1_range',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI',label = 'percent2')+scale_y_continuous(limits=c(0,45))
#PGR
PGR.plot<-pepsinogen%>%filter(!is.na(PGR))%>%group_by(PGR_range)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PGR_range',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI/II ratio',label = 'percent2')+scale_y_continuous(limits = c(0,100))
#PG1 and PGR
PG1R.plot<-pepsinogen%>%mutate(m='v')%>%filter(!is.na(PGR))%>%group_by(m,PG1_range,PGR_range)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range',y='percent',fill='PGR_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label='percent2')+labs(fill='PGI/II ratio')+scale_y_continuous(limits=c(0,35))
(PG1.plot |PGR.plot)/PG1R.plot +plot_annotation(tag_level = 'A')

#基线(家庭收入、教育、婚姻、就业状况、血型)
pepsinogen%>%pivot_longer(cols=variables1,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables1){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#吸烟、被动吸烟、年龄、BMI
pepsinogen%>%pivot_longer(cols=c('性别','年龄分组','吸烟','被动吸烟','BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('性别','年龄分组','吸烟','被动吸烟','BMI_group')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#与年龄2的关系
pepsinogen%>%filter(!is.na(年龄分组2))%>%group_by(年龄分组2)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(年龄分组2,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
kruskal.test(PG1~年龄分组2,data=pepsinogen)
#饮食、饮酒、饮茶
pepsinogen%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()

p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#身体活动和重度精神问题
pepsinogen%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()

p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#慢性疾病史
pepsinogen%>%pivot_longer(cols=variables7,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()

p<-list()
for(i in variables7){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#胃病疾病史
pepsinogen%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
##饮食习惯
pepsinogen%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
##<<<<<<<<<<<<<<<<<<<PG2
#
summary(pepsinogen$PG2)
#基线(家庭收入、教育、婚姻、就业状况、血型)
pepsinogen%>%pivot_longer(cols=variables1,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables1){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#吸烟、被动吸烟、年龄、BMI
pepsinogen%>%pivot_longer(cols=c('性别','年龄分组','吸烟','被动吸烟','BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('性别','年龄分组','吸烟','被动吸烟','BMI_group')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#饮食、饮酒、饮茶
pepsinogen%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()

p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#身体活动和重度精神问题
pepsinogen%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()

p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#基础疾病史
pepsinogen%>%pivot_longer(cols=variables7,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables7){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#饮食偏好
pepsinogen%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#胃病疾病史
pepsinogen%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)

###<<<<<<<<<<<<<<PGR
#
summary(pepsinogen$PGR)
#基线(家庭收入、教育、婚姻、就业状况、血型)
pepsinogen%>%filter(!is.na(PGR))%>%pivot_longer(cols=variables1,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables1){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#吸烟、被动吸烟、年龄、BMI
pepsinogen%>%filter(!is.na(PGR))%>%pivot_longer(cols=c('性别','年龄分组','吸烟','被动吸烟','BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('性别','年龄分组','吸烟','被动吸烟','BMI_group')){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#饮食、饮酒、饮茶
pepsinogen%>%filter(!is.na(PGR))%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()

p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#身体活动和重度精神问题
pepsinogen%>%filter(!is.na(PGR))%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()

p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#基础疾病史
pepsinogen%>%filter(!is.na(PGR))%>%pivot_longer(cols=variables7,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables7){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#胃病疾病史
pepsinogen%>%filter(!is.na(PGR))%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#饮食偏好
pepsinogen%>%filter(!is.na(PGR))%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)

##<<<<<PG1分层层
PG_split(a='性别',b='PG1',c='年龄分组')
PG_split(a='性别',b='PG1',c='吸烟')
PG_split(a='性别',b='PG1',c='BMI_group')
PG_split(a='性别',b='PG1',c='饮酒')
PG_split(a='性别',b='PG1',c='酸奶')
PG_split(a='性别',b='PG1',c='水果')

PG_split(a='年龄分组',b='PG1',c='性别')
PG_split(a='年龄分组',b='PG1',c='吸烟')
PG_split(a='年龄分组',b='PG1',c='BMI_group')
PG_split(a='年龄分组',b='PG1',c='饮酒')
PG_split(a='年龄分组',b='PG1',c='酸奶')
PG_split(a='十二指肠溃疡',b='PG1',c='吸烟')
PG_split(a='胃溃疡',b='PG1',c='吸烟')
##
PG_split()

##<<<<<PG2分层
PG_split(a='性别',b='PG2',c='年龄分组')
PG_split(a='性别',b='PG2',c='吸烟')
PG_split(a='性别',b='PG2',c='BMI_group')
PG_split(a='性别',b='PG2',c='饮酒')
PG_split(a='性别',b='PG2',c='酸奶')
PG_split(a='性别',b='PG2',c='水果')
PG_split(a='性别',b='PG2',c='糖尿病')
PG_split(a='性别',b='PG2',c='冠心病')
PG_split(a='性别',b='PG2',c='高血压')
PG_split(a='性别',b='PG2',c='高血脂')

PG_split(a='年龄分组',b='PG2',c='性别')
PG_split(a='年龄分组',b='PG2',c='吸烟')
PG_split(a='年龄分组',b='PG2',c='BMI_group')
PG_split(a='年龄分组',b='PG2',c='饮酒')
PG_split(a='年龄分组',b='PG2',c='酸奶')
PG_split(a='年龄分组',b='PG2',c='水果')
PG_split(a='年龄分组',b='PG2',c='糖尿病')
PG_split(a='年龄分组',b='PG2',c='冠心病')
PG_split(a='年龄分组',b='PG2',c='高血压')
PG_split(a='年龄分组',b='PG2',c='高血脂')




##<<<<<<PGR分层
PG_split(a='性别',b='PGR',c='年龄分组')
PG_split(a='性别',b='PGR',c='吸烟')
PG_split(a='性别',b='PGR',c='BMI_group')
PG_split(a='性别',b='PGR',c='喝茶')
PG_split(a='性别',b='PGR',c='坚果')
PG_split(a='性别',b='PGR',c='菌类')
PG_split(a='性别',b='PGR',c='糖尿病')
PG_split(a='性别',b='PGR',c='冠心病')
PG_split(a='性别',b='PGR',c='高血压')
PG_split(a='性别',b='PGR',c='高血脂')


PG_split(a='年龄分组',b='PGR',c='性别')
PG_split(a='年龄分组',b='PGR',c='吸烟')
PG_split(a='年龄分组',b='PGR',c='BMI_group')
PG_split(a='年龄分组',b='PGR',c='喝茶')
PG_split(a='年龄分组',b='PGR',c='坚果')
PG_split(a='年龄分组',b='PGR',c='菌类')
PG_split(a='年龄分组',b='PGR',c='糖尿病')
PG_split(a='年龄分组',b='PGR',c='冠心病')
PG_split(a='年龄分组',b='PGR',c='高血压')
PG_split(a='年龄分组',b='PGR',c='高血脂')






##画图
#PG与性别
median2.sex<-pepsinogen%>%group_by(性别)%>%summarise(median=median(PG1))
pg1.sex<-pepsinogen%>%ggviolin(x="性别", y="PG1", fill = "性别", 
                               palette = c("#00AFBB", "#E7B800"), 
                               add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(label.y = 260,label.x=1)+theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median2.sex,aes(x=性别,y=median),color='red')+geom_line(data=median2.sex,aes(x=as.numeric(性别),y=median),color='red',size=1)

median3.sex<-pepsinogen%>%filter(PG2<100)%>%group_by(性别)%>%summarise(median=median(PG2))
pg2.sex<-pepsinogen%>%filter(PG2<100)%>%ggviolin(x="性别", y="PG2", fill = "性别", 
                                                 palette = c("#00AFBB", "#E7B800"), 
                                                 add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(label.y = 100,label.x=1)+theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median3.sex,aes(x=性别,y=median),color='red')+geom_line(data=median3.sex,aes(x=as.numeric(性别),y=median),color='red',size=1)

median1.sex<-pepsinogen%>%filter(!is.na(PGR))%>%group_by(性别)%>%summarise(median=median(PGR))
pgr.sex<-pepsinogen%>%filter(PGR<30,!is.na(PGR))%>%ggviolin(x="性别", y="PGR", fill = "性别", 
                                                            palette = c("#00AFBB", "#E7B800"), 
                                                            add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(label.y = 30,label.x=1)+theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median1.sex,aes(x=性别,y=median),color='red')+geom_line(data=median1.sex,aes(x=as.numeric(性别),y=median),color='red',size=1)

ggarrange(pg1.sex,pg2.sex,pgr.sex,ncol=3)
(pg1.sex | PG1.plot3) / (pg2.sex | pgr.sex)+plot_annotation(tag_levels = 'A')
###性别和百分比
pepsinogen%>%group_by(性别,PG1_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = 性别, y = perc, fill =PG1_range)) +
  geom_bar(stat="identity", width = 0.7,position = position_dodge()) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_dodge(width=0.7))+
  labs(x='性别',y='百分比',fill='PG1分组')
pepsinogen%>%filter(!is.na(PGR))%>%group_by(性别,PGR_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = 性别, y = perc, fill =PGR_range)) +
  geom_bar(stat="identity", width = 0.7,position = position_dodge()) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_dodge(width=0.7))+
  labs(x='性别',y='百分比',fill='PGR分组')
pepsinogen%>%group_by(性别,PG1_range) %>% 
  summarise(count=n()) %>% group_by(PG1_range)%>%
  mutate(perc=round((count/sum(count))*100,2))
#PG1
PG1.plot3<-pepsinogen%>%group_by(PG1_range,性别) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = PG1_range, y = perc, fill =性别)) +
  geom_bar(stat="identity", width = 0.7,position = position_dodge()) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_dodge(width=0.7))+
  labs(x='PGI',y='Percent',fill='Sex')
#PGR
pepsinogen%>%filter(!is.na(PGR))%>%group_by(PGR_range,性别) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = PGR_range, y = perc, fill =性别)) +
  geom_bar(stat="identity", width = 0.7,position = position_dodge()) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_dodge(width=0.7))+
  labs(x='PGI/II ratio',y='Percent',fill='Sex')

##PG与年龄
pepsinogen%>%filter(年龄>=40,年龄<=74)%>%group_by(年龄)%>%
  summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75),median.pg2=median(PG2),Q1.pg2=quantile(PG2,0.25),Q3.pg2=quantile(PG2,0.75))%>%
  ggplot()+geom_ribbon(aes(x=年龄,ymin=Q1,ymax=Q3), fill="#6699CC", alpha=.4)+
  mytheme+geom_line(aes(x=年龄,y=median),colour='#003366',size=1)+
  geom_ribbon(aes(x=年龄,ymin=Q1.pg2,ymax=Q3.pg2),fill="#FFCC00",alpha=0.4)+
  geom_line(aes(x=年龄,y=median.pg2),colour='#FFCC00',size=1)+
  labs(title = "年龄与PG1、PG2的相关性")+scale_x_continuous(breaks=seq(42,74,5))+
  scale_y_log10()
pepsinogen%>%filter(年龄>=40,年龄<=74,!is.na(PGR))%>%group_by(年龄)%>%
  summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_ribbon(aes(x=年龄,ymin=Q1,ymax=Q3), fill="#FF9933", alpha=.6)+
  mytheme+geom_line(aes(x=年龄,y=median),colour='#FF9933',size=1)+
  labs(title = "年龄与PGR的相关性")+scale_x_continuous(breaks=seq(42,74,5))
#PG与年龄(性别分组)
pepsinogen%>%filter(年龄>=40,年龄<=74)%>%group_by(性别,年龄)%>%
  summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75),median.pg2=median(PG2),Q1.pg2=quantile(PG2,0.25),Q3.pg2=quantile(PG2,0.75))%>%
  ggplot()+geom_ribbon(aes(x=年龄,ymin=Q1,ymax=Q3), fill="#6699CC", alpha=.4)+
  mytheme+geom_line(aes(x=年龄,y=median),colour='#003366',size=1)+
  geom_ribbon(aes(x=年龄,ymin=Q1.pg2,ymax=Q3.pg2),fill="#FFCC00",alpha=0.4)+
  geom_line(aes(x=年龄,y=median.pg2),colour='#FFCC00',size=1)+
  labs(title = "")+scale_x_continuous(breaks=seq(42,74,5))+
  scale_y_log10()+facet_grid(.~性别,scales = 'free')

#PG与吸烟
my_comparisons <- list(c("从不吸烟", "目前仍在吸烟"), c("从不吸烟", "以前吸烟"), c("目前仍在吸烟", "以前吸烟"))
median2<-pepsinogen%>%group_by(吸烟)%>%summarise(median=median(PG1))
pg1.smoking<-pepsinogen%>%ggviolin(x="吸烟", y="PG1", fill = "吸烟", 
         palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
         add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+#label这里表示选择显著性标记（星号） 
  stat_compare_means(label.y = 260,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median2,aes(x=吸烟,y=median),color='red')+geom_line(data=median2,aes(x=as.numeric(吸烟),y=median),color='red',size=1)
median3<-pepsinogen%>%group_by(吸烟)%>%summarise(median=median(PG2))
pg2.smoking<-pepsinogen%>%filter(PG2<100)%>%ggviolin(x="吸烟", y="PG2", fill = "吸烟", 
                      palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                      add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+#label这里表示选择显著性标记（星号） 
  stat_compare_means(label.y = 100,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median3,aes(x=吸烟,y=median),color='red')+geom_line(data=median3,aes(x=as.numeric(吸烟),y=median),color='red',size=1)
median1<-pepsinogen%>%filter(PGR<30)%>%group_by(吸烟)%>%summarise(median=median(PGR))
pgr.smoking<-ggviolin(pepsinogen[which(pepsinogen$PGR<30),],x="吸烟", y="PGR", fill = "吸烟", 
                      palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                      add = "boxplot", add.params = list(fill="white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+#label这里表示选择显著性标记（星号） 
  stat_compare_means(label.y = 30,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+geom_point(data=median1,aes(x=吸烟,y=median),color='red')+geom_line(data=median1,aes(x=as.numeric(吸烟),y=median),color='red',size=1)
ggarrange(pg1.smoking,pg2.smoking,pgr.smoking,ncol=3)
#PG与吸烟量的关联
comparision3<-list(c('<10','-20'),c('<10','-30'),c('<10','-40'),c('<10','-50'),c('<10','>50'),
                   c('-20','-30'),c('-20','-40'),c('-20','-50'),c('-20','>50'),c('-30','-40'),c('30','-50'),c('-30','>50'),
                   c('-40','-50'),c('-40','>50'),c('-50','>50'))
pepsinogen%>%filter(吸烟=='目前仍在吸烟')%>%ggplot(aes(包年,PG1))+geom_point()+geom_smooth(method='lm')+mytheme+stat_cor(method='spearman')
pepsinogen%>%filter(吸烟=='目前仍在吸烟')%>%ggviolin(x='包年分组',y='PG1',fill='包年分组',palette = 'jco',add = "boxplot", add.params = list(fill="white"))+
  theme(legend.position = 'none')+stat_compare_means(comparisons = comparision3, label = "p.signif")+#label这里表示选择显著性标记（星号） 
  stat_compare_means(label.y = 210,label.x=0.7)
#PG与吸烟年数的关系
pepsinogen%>%filter(吸烟=='目前仍在吸烟')%>%ggplot(aes(smkyrs,PG1))+geom_point()+geom_smooth(method='lm')+mytheme+stat_cor(method='spearman')
pepsinogen$smkyrs_range<-case_when(
  pepsinogen$smkyrs<20 ~ 1,
  between(pepsinogen$smkyrs,20,30) ~ 2,
  between(pepsinogen$smkyrs,30.01,40) ~ 3,
  pepsinogen$smkyrs>40 ~ 4
  )
comparision4<-list(c('<20年','-30年'),c('<20年','-40年'),c('<20年','>40年'),c('-30年','-40年'),c('-30年','>40年'),c('-40年','>40年'))
pepsinogen$smkyrs_range<-factor(pepsinogen$smkyrs_range,levels = c(1,2,3,4),labels = c('<20年','-30年','-40年','>40年'))
median.smkyrs<-pepsinogen%>%filter(吸烟=='目前仍在吸烟')%>%group_by(smkyrs_range)%>%summarise(median=median(PG1))
pepsinogen%>%filter(吸烟=='目前仍在吸烟')%>%ggviolin(x='smkyrs_range',y='PG1',fill='smkyrs_range',palette = 'jco',add = "boxplot", add.params = list(fill="white"))+
  theme(legend.position = 'none')+geom_point(data=median.smkyrs,aes(x=smkyrs_range,y=median),color='red')+geom_line(data=median.smkyrs,aes(x=as.numeric(smkyrs_range),y=median),color='red',size=1)+
  stat_compare_means(label.y = 210,label.x=0.7)+labs(x='吸烟年数')#+stat_compare_means(comparisons = comparision4, label = "p.signif")
#
median.smkyrs2<-pepsinogen%>%filter(吸烟=='目前仍在吸烟')%>%group_by(smkyrs_range)%>%summarise(median=median(PG2))
pepsinogen%>%filter(PG2!=100)%>%filter(吸烟=='目前仍在吸烟')%>%ggviolin(x='smkyrs_range',y='PG2',fill='smkyrs_range',palette = 'jco',add = "boxplot", add.params = list(fill="white"))+
  theme(legend.position = 'none')+geom_point(data=median.smkyrs2,aes(x=smkyrs_range,y=median),color='red')+geom_line(data=median.smkyrs2,aes(x=as.numeric(smkyrs_range),y=median),color='red',size=1)+
  stat_compare_means(label.y = 75,label.x=0.7)+labs(x='吸烟年数')#+stat_compare_means(comparisons = comparision4, label = "p.signif")
#PG1分层后比例关系
pepsinogen%>%filter(!is.na(吸烟))%>%group_by(吸烟,PG1_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = 吸烟, y = perc, fill =PG1_range)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_stack(vjust = 0.5))+
  labs(x='吸烟',y='百分比',fill='PG1分组')
pepsinogen%>%filter(!is.na(吸烟))%>%group_by(吸烟,PG2_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = 吸烟, y = perc, fill =PG2_range)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_stack(vjust = 0.5))+
  labs(x='吸烟',y='百分比',fill='PG1分组')




##PG与BMI
comparisions1<-list(c('正常','超重'),c('正常','肥胖'),c('超重','肥胖'))
forest_model(lm(log(PG1)~性别+年龄+吸烟,data=pepsinogen))#,limits = c(-0.3,0.3)
forest_model(lm(log(PG2)~性别+年龄+吸烟,data=pepsinogen))#,limits = c(-0.3,0.3)
forest_model(lm(log(PGR)~性别+年龄+吸烟,data=pepsinogen))#,limits = c(-0.3,0.3)
forest_model(lm(log(PG1)~性别+年龄+BMI_group+吸烟,data=pepsinogen),limits = c(-0.3,0.21))
median2.bmi<-pepsinogen%>%group_by(BMI_group)%>%summarise(median=median(PG1))
pg1.bmi<-pepsinogen%>%filter(PG1!=200)%>%ggviolin(x="BMI_group", y="PG1", fill = "BMI_group", 
                                   palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                   add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(comparisons = comparisions1, label = "p.signif")+#label这里表示选择显著性标记（星号） 
  stat_compare_means(label.y = 260,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median2.bmi,aes(x=BMI_group,y=median),color='red')+geom_line(data=median2.bmi,aes(x=as.numeric(BMI_group),y=median),color='red',size=1)
##性别、年龄分层下，BMI与PG1的相关性
pepsinogen%>%group_by(年龄分组,性别,BMI_group)%>%summarise(n=n(),median=median(PG1,na.rm=TRUE),Q1=quantile(PG1,0.25,na.rm=TRUE),Q3=quantile(PG1,0.75,na.rm=TRUE))%>%
            mutate(Median1=paste0(median,"(",Q1,"-",Q3,")"))%>%select(年龄分组,性别,BMI_group,n,Median1)%>%datatable()
kruskal.test(PG1~BMI_group,data=subset(pepsinogen,年龄分组=='<50' & 性别=="男"))
kruskal.test(PG1~BMI_group,data=subset(pepsinogen,年龄分组=='<50' & 性别=="女"))
kruskal.test(PG1~BMI_group,data=subset(pepsinogen,年龄分组=='50-60' & 性别=="男"))
kruskal.test(PG1~BMI_group,data=subset(pepsinogen,年龄分组=='50-60' & 性别=="女"))
kruskal.test(PG1~BMI_group,data=subset(pepsinogen,年龄分组=='>60' & 性别=="男"))
kruskal.test(PG1~BMI_group,data=subset(pepsinogen,年龄分组=='>60' & 性别=="女"))

facet(ggboxplot(pepsinogen,x='BMI_group',y='PG1')+stat_compare_means(label.y = 200,label.x=0.7)+stat_compare_means(comparisons = comparisions1, label = "p.signif"),facet.by = c('性别','年龄分组'),scales='free')

#PG2
median3.bmi<-pepsinogen%>%group_by(BMI_group)%>%summarise(median=median(PG2))
pg2.bmi<-pepsinogen%>%filter(PG1!=200)%>%ggviolin(x="BMI_group", y="PG2", fill = "BMI_group", 
                                                  palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                                  add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(comparisons = comparisions1, label = "p.signif")+#label这里表示选择显著性标记（星号） 
  stat_compare_means(label.y = 100,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median3.bmi,aes(x=BMI_group,y=median),color='red')+geom_line(data=median3.bmi,aes(x=as.numeric(BMI_group),y=median),color='red',size=1)
forest_model(lm(PG2~性别+年龄分组+BMI_group,data=pepsinogen))#,limits = c(-0.3,0.3)
pepsinogen%>%group_by(年龄分组,性别,BMI_group)%>%summarise(n=n(),median=median(PG2,na.rm=TRUE),Q1=quantile(PG2,0.25,na.rm=TRUE),Q3=quantile(PG2,0.75,na.rm=TRUE))%>%
  mutate(Median1=paste0(median,"(",Q1,"-",Q3,")"))%>%select(年龄分组,性别,BMI_group,n,Median1)%>%datatable()
kruskal.test(PG2~BMI_group,data=subset(pepsinogen,年龄分组=='<50' & 性别=="男"))
kruskal.test(PG2~BMI_group,data=subset(pepsinogen,年龄分组=='<50' & 性别=="女"))
kruskal.test(PG2~BMI_group,data=subset(pepsinogen,年龄分组=='50-60' & 性别=="男"))
kruskal.test(PG2~BMI_group,data=subset(pepsinogen,年龄分组=='50-60' & 性别=="女"))
kruskal.test(PG2~BMI_group,data=subset(pepsinogen,年龄分组=='>60' & 性别=="男"))
kruskal.test(PG2~BMI_group,data=subset(pepsinogen,年龄分组=='>60' & 性别=="女"))

#PGR
forest_model(lm(PGR~性别+年龄分组+BMI_group,data=pepsinogen))#,limits = c(-0.3,0.3)

###饮酒、饮食、饮茶的关联分析

median2.alcohol<-pepsinogen%>%group_by(饮酒)%>%summarise(median=median(PG1))
pg1.alcohol<-pepsinogen%>%ggviolin(x="饮酒", y="PG1", fill = "饮酒", 
                                   palette = c("#00AFBB", "#E7B800"), 
                                   add = "boxplot", add.params = list(fill="white"))+ 
                                    theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median2.alcohol,aes(x=饮酒,y=median),color='red')+geom_line(data=median2.alcohol,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
  stat_compare_means(label.y = 260,label.x=0.7)
median3.alcohol<-pepsinogen%>%group_by(饮酒)%>%summarise(median=median(PG2))
pg2.alcohol<-pepsinogen%>%filter(PG2<100)%>%ggviolin(x="饮酒", y="PG2", fill = "饮酒", 
                                                     palette = c("#00AFBB", "#E7B800"), 
                                                     add = "boxplot", add.params = list(fill="white"))+ 
                                                     theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median3.alcohol,aes(x=饮酒,y=median),color='red')+geom_line(data=median3.alcohol,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
  stat_compare_means(label.y = 100,label.x=0.7)
median1.alcohol<-pepsinogen%>%filter(PGR<30)%>%group_by(饮酒)%>%summarise(median=median(PGR))
pgr.alcohol<-ggviolin(pepsinogen[which(pepsinogen$PGR<30),],x="饮酒", y="PGR", fill = "饮酒", 
                      palette = c("#00AFBB", "#E7B800"), 
                      add = "boxplot", add.params = list(fill="white"))+
                     theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median1.alcohol,aes(x=饮酒,y=median),color='red')+geom_line(data=median1.alcohol,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
  stat_compare_means(label.y = 25,label.x=0.7)
ggarrange(pg1.alcohol,pg2.alcohol,pgr.alcohol,ncol=3)
#矫正性别
median2.alcohol.sex<-pepsinogen%>%group_by(性别,饮酒)%>%summarise(median=median(PG1))
pg1.alcohol<-pepsinogen%>%ggviolin(x="饮酒", y="PG1", fill = "饮酒",facet.by = '性别',
                                   palette = c("#00AFBB", "#E7B800"), 
                                   add = "boxplot", add.params = list(fill="white"))+ 
  theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median2.alcohol.sex,aes(x=饮酒,y=median),color='red')+geom_line(data=median2.alcohol.sex,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
  stat_compare_means(label.y = 260,label.x=0.7)
median3.alcohol.sex<-pepsinogen%>%group_by(性别,饮酒)%>%summarise(median=median(PG2))
pg2.alcohol<-pepsinogen%>%filter(PG2<100)%>%ggviolin(x="饮酒", y="PG2", fill = "饮酒", facet.by = '性别',
                                                     palette = c("#00AFBB", "#E7B800"), 
                                                     add = "boxplot", add.params = list(fill="white"))+ 
  theme(legend.position = 'none')+labs(x='')+
  geom_point(data=median3.alcohol,aes(x=饮酒,y=median),color='red')+geom_line(data=median3.alcohol,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
  stat_compare_means(label.y = 100,label.x=0.7)
ggarrange(pg1.alcohol,pg2.alcohol,nrow=2)



###PG1随年龄分布画图
pepsinogen%>%filter(!is.na(年龄分组2))%>%group_by(年龄分组2,PG1_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
ggplot(aes(x = 年龄分组2, y = perc, fill =PG1_range)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_stack(vjust = 0.5))+
  labs(x='年龄',y='百分比',fill='PG1分组')
#性别分层
pepsinogen%>%filter(!is.na(年龄分组2))%>%group_by(性别,年龄分组2,PG1_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%filter(PG1_range=='<20' | PG1_range==">=200")%>%datatable()

pepsinogen%>%filter(!is.na(年龄分组2))%>%group_by(性别,年龄分组2,PG1_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%filter(PG1_range=='<20' | PG1_range==">=200")%>%
  ggplot(aes(x = 年龄分组2, y = perc, fill =性别)) +
  geom_bar(stat="identity", width = 0.7,position = 'dodge') +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_dodge(width = 0.5))+
  labs(x='年龄',y='百分比',fill='性别')+facet_grid(.~PG1_range)+scale_fill_manual(values=c('#6699CC','#FF6600'))


###PGR随年龄分布画图



PGR.age1<-pepsinogen%>%filter(!is.na(年龄分组2),!is.na(PGR))%>%group_by(年龄分组2,PGR_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = 年龄分组2, y = perc, fill =PGR_range)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_stack(vjust = 0.5))+
  labs(x='年龄',y='百分比',fill='PGR分组')

##血型
median2.blood<-pepsinogen%>%filter(!is.na(血型))%>%group_by(血型)%>%summarise(median=median(PG1))
pg1.blood<-pepsinogen%>%filter(!is.na(血型))%>%ggviolin(x="血型", y="PG1", fill = "血型", 
                                   palette = 'jco', 
                                   add = "boxplot", add.params = list(fill="white"))+ 
  theme(legend.position = 'none')+labs(x='',title='PGI')+
  geom_point(data=median2.blood,aes(x=血型,y=median),color='red')+geom_line(data=median2.blood,aes(x=as.numeric(血型),y=median),color='red',size=1)+
  stat_compare_means(label.y = 210,label.x=0.5)#+stat_compare_means(comparisons =comparision2, label = "p.signif")#label这里表示选择显著性标记（星号） 
median3.blood<-pepsinogen%>%filter(!is.na(血型))%>%group_by(血型)%>%summarise(median=median(PG2))
pg2.blood<-pepsinogen%>%filter(!is.na(血型))%>%filter(PG2<100)%>%ggviolin(x="血型", y="PG2", fill = "血型", 
                                                     palette = 'jco', 
                                                     add = "boxplot", add.params = list(fill="white"))+ 
  theme(legend.position = 'none')+labs(x='',title='PGII')+
  geom_point(data=median3.blood,aes(x=血型,y=median),color='red')+geom_line(data=median3.blood,aes(x=as.numeric(血型),y=median),color='red',size=1)+
  stat_compare_means(label.y = 100,label.x=0.5)#+stat_compare_means(comparisons = comparision2, label = "p.signif")#label这里表示选择显著性标记（星号） 
pg1.blood | pg2.blood
##PG1分组下各因素的分布
table1(~性别+年龄+吸烟+BMI+饮酒+酸奶+糖尿病+高血压+PG2+PGR+幽门螺杆菌感染史 | PG1_range,data=pepsinogen,render.categorical=my.render.cat)
#

##PGR分组下各因素的分布
round(with(pepsinogen,prop.table(table(PG2_range)))*100,2)
    table1(~性别+年龄+吸烟+BMI+饮酒+酸奶+糖尿病+高血压+PGR+PG1+幽门螺杆菌感染史 | PG2_range,data=pepsinogen,render.categorical=my.render.cat)












###鲜奶
forest_model(glm(log(PG1)~性别+血型,data=pepsinogen))
forest_model(glm(log(PG2)~性别+血型,data=pepsinogen))
 ####
#The association between milk and yogurt consumption and gastric atrophy
#Gastric atrophy (defined as a serum level of pepsinogen I <55 μg/L) 
pepsinogen2<-pepsinogen%>%transmute(性别,年龄,年龄分组,BMI_group,吸烟,鲜奶,酸奶,家庭收入,就业状况,血型,糖尿病,高血压,高血脂,冠心病,教育,婚姻,饮酒,油炸,残胃,腌制,胃病,幽门螺杆菌感染史,萎缩性胃炎,胃萎缩=factor(ifelse(PG1<55,1,0),levels=c(0,1),labels=c('否','是')))
with(pepsinogen2,table(鲜奶,胃萎缩))
with(pepsinogen2,table(酸奶,胃萎缩))
with(pepsinogen2,prop.table(table(鲜奶,胃萎缩),margin = 1))
with(pepsinogen2,prop.table(table(酸奶,胃萎缩),margin = 1))
with(pepsinogen2,chisq.test(table(鲜奶,胃萎缩)))
with(pepsinogen2,chisq.test(table(酸奶,胃萎缩)))

forest_model(glm(胃萎缩~鲜奶,family = 'binomial',data=pepsinogen2))
forest_model(glm(胃萎缩~酸奶,family = 'binomial',data=pepsinogen2))
forest_model(glm(胃萎缩~酸奶+性别+年龄分组+BMI_group+吸烟+教育+婚姻+饮酒+血型+糖尿病+高血压,family = 'binomial',data=pepsinogen2))

table(pepsinogen2$胃萎缩)
with(pepsinogen,prop.table(table(酸奶,幽门螺杆菌感染史),margin = 1))
with(pepsinogen,prop.table(table(鲜奶,残胃),margin = 1))
with(pepsinogen,prop.table(table(酸奶,胃溃疡),margin = 1))
with(pepsinogen,prop.table(table(酸奶,十二指肠溃疡),margin = 1))
with(pepsinogen,prop.table(table(酸奶,胃病),margin = 1))


##胃癌及癌前病变
gastric<-import('~/data/胃癌及癌前病变.xlsx')
gastric2<-inner_join(gastric,pepsinogen,by='ID')
gastric%>%group_by(type)%>%summarise(n=n(),median=median(PGⅠ),Q1=quantile(PGⅠ,0.25),Q3=quantile(PGⅠ,0.75))
summary(gastric$PGⅠ) 
gastric2%>%group_by(性别.y,type)%>%summarise(n=n(),median=median(PGⅠ),Q1=quantile(PGⅠ,0.25),Q3=quantile(PGⅠ,0.75))

gastric%>%group_by(type)%>%summarise(n=n(),median=median(PGⅡ),Q1=quantile(PGⅡ,0.25),Q3=quantile(PGⅡ,0.75))
summary(gastric$PGⅡ) 
gastric%>%group_by(type)%>%summarise(n=n(),median=median(`PGⅠ/PGⅡ`),Q1=quantile(`PGⅠ/PGⅡ`,0.25),Q3=quantile(`PGⅠ/PGⅡ`,0.75))
summary(gastric$`PGⅠ/PGⅡ`) 
gastric%>%group_by(type)%>%summarise(n=n(),mean=mean(年龄),sd=sd(年龄))
summary(gastric$年龄);sd(gastric$年龄)
gastric%>%group_by(type)%>%summarise(n=n(),median=median(年龄),Q1=quantile(年龄,0.25),Q3=quantile(年龄,0.75))
gastric%>%group_by(type,性别)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,4))
summary(gastric$年龄);sd(gastric$年龄)
prop.table(table(gastric$性别))
gastric%>%group_by(type,年龄分组)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,4))
prop.table(table(gastric$年龄分组))
#吸烟
gastric2%>%group_by(type,BMI_group)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,4))

gastric$PG1_range=case_when(
  gastric$PGⅠ<=30 ~ 1,
  between(gastric$PGⅠ,30.1,50) ~ 2,
  between(gastric$PGⅠ,50.1,70) ~ 3,
  gastric$PGⅠ>70 ~ 4
)
gastric$PG1_range=factor(gastric$PG1_range,levels = c(1,2,3,4),labels=c('<=30','30.1-50','50.1-70','>70'))
gastric$PG2_range=case_when(
  gastric$PGⅡ<=6.50 ~ 1,
  between(gastric$PGⅡ,6.51,9.80) ~ 2,
  between(gastric$PGⅡ,9.81,15.30) ~ 3,
  gastric$PGⅡ>15.30 ~ 4
)
gastric$PG2_range=factor(gastric$PG2_range,levels = c(1,2,3,4),labels=c('<6.5','6.51-9.80','9.81-15.30','>15.30'))
gastric$PGR_range=case_when(
  gastric$`PGⅠ/PGⅡ`<=2 ~ 1,
  between(gastric$`PGⅠ/PGⅡ`,2.1,3) ~ 2,
  gastric$`PGⅠ/PGⅡ`>3 ~ 3
)
gastric$PGR_range=factor(gastric$PGR_range,levels=c(1,2,3),labels=c('<=2','2.01-3','>3'))
#
gastric2%>%group_by(type,PG1_range)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,4))
PG1.plot2<-gastric%>%group_by(type,PG1_range)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,2),percent2=paste0(round(n/sum(n)*100,2),'%'))%>%
  ggbarplot(x='type',y='percent',fill='PG1_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='',lab.size = 3.5,label='percent2',x.text.angle= 50)+labs(fill='PGI')+
  scale_x_discrete(labels=c('cancer','dysplasia','intestinal metaplasia','Atrophic gastritis'))

gastric%>%group_by(type,PGR_range)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,4))

PGR.plot2<-gastric%>%group_by(type,PGR_range)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,2),percent2=paste0(round(n/sum(n)*100,2),'%'))%>%
  ggbarplot(x='type',y='percent',fill='PGR_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',lab.size =3.5,xlab='',label='percent2',x.text.angle=50)+labs(fill='PGI/II ratio')+
  scale_x_discrete(labels=c('cancer','dysplasia','intestinal metaplasia','Atrophic gastritis'))
PG1.plot2 | PGR.plot2 

##
gastric2%>%transmute(年龄=factor(ifelse(年龄.y>60,1,0)),type)%>%group_by(type,年龄)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=n/sum(n))
gastric2%>%transmute(年龄=factor(ifelse(年龄.y>60,1,0)),type,PG1)%>%group_by(年龄,type)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
gastric2%>%group_by(性别.y)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
gastric2%>%transmute(年龄=factor(ifelse(年龄.y>60,1,0)),type,PG1)%>%group_by(年龄)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
##PGR
gastric2%>%transmute(年龄=factor(ifelse(年龄.y>60,1,0)),type,PGR)%>%group_by(年龄,type)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))
gastric2%>%group_by(性别.y)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))
gastric2%>%transmute(年龄=factor(ifelse(年龄.y>60,1,0)),type,PGR)%>%group_by(年龄)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))
gastric2%>%group_by(性别.y,type)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))





