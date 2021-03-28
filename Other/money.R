rm(list=ls())
##血脂生化指标与癌前状态的关联
library(patchwork)
library(compareGroups)
data<-import('~/data/money/统计表.xls')%>%select(-日期)%>%
  mutate(HP=factor(ifelse(HP=="（-）",0,1),levels=c(0,1),labels=c('Negative','Positive')),
         group1=factor(case_when(
           gruop1=="浅表性胃炎" ~ 1,
           gruop1=="萎缩性胃炎" ~ 2
         ),levels=c(1,2),labels = c('浅表性胃炎','萎缩性胃炎')),
         age_group=factor(ifelse(年龄<60,0,1),levels=c(0,1),labels = c('<60','>=60')))

#几个联系指标分布
variables<-c('年龄','血糖','胆固醇','LDL','HDL','TG')
P<-list()
for (i in variables){
  P[[i]]<-ggplot(data=data)+geom_histogram(aes_string(x=i),color="black",fill="lightblue")+labs(x=i)
}
wrap_plots(P,nrow=2,guide='collect')
#结局的查看
with(data,table(gruop1))
with(data,table(gruop2))
#萎缩与非萎缩
#1、基线查看
descrTable(group1~.-胃镜结果-gruop2-gruop1,data=data,method=c(血糖=2,胆固醇=2,LDL=2,HDL=2,TG=2))
#1、分性别
descrTable(group1~.-age_group-胃镜结果-gruop2-gruop1,data=data,method=c(血糖=2,胆固醇=2,LDL=2,HDL=2,TG=2))%>%strataTable("age_group")
#2、分年龄