rm(list=ls())
library(rio)
library(ggpubr)
library(table1)
library(tidyverse)
library(corrplot)
library(pROC)
library(epiR)
library(verification)
library(ggstatsplot)
#函数 ####
CA125_split<-function(x,y){
  x1<-CA125[[x]]
  CA125_split<-split(CA125,x1)
  formula_uni<-as.formula(paste('CA125','~', y)) 
  a<-lapply(CA125_split, function(x)x%>%group_by(x[[y]])%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75)))
  b<-lapply(CA125_split,function(x)kruskal.test(formula_uni,x))
  out<-list(分层变量=a,分层变量2=b)
  return(out)
}
CA153_split<-function(x,y){
  x1<-CA153[[x]]
  CA153_split<-split(CA153,x1)
  formula_uni<-as.formula(paste('CA153','~', y)) 
  a<-lapply(CA153_split, function(x)x%>%group_by(x[[y]])%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
  b<-lapply(CA153_split,function(x)kruskal.test(formula_uni,x))
  out<-list(分层变量=a,分层变量2=b)
  return(out)
}
CEA_split<-function(x,y){
  x1<-CEA[[x]]
  CEA_split<-split(CEA,x1)
  formula_uni<-as.formula(paste('CEA','~', y)) 
  a<-lapply(CEA_split, function(x)x%>%group_by(x[[y]])%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
  b<-lapply(CEA_split,function(x)kruskal.test(formula_uni,x))
  out<-list(分层变量=a,分层变量2=b)
  return(out)
}

#读取数据 ####
source('~/Rcode/biomarker/data_biomarker.R')
source("~/Rcode/screening/breastBUS.R")
baseline2<-left_join(baseline,breastBUS,by='ID_BLAST')
source('~/Rcode/statistics/data_summary.R')
source('~/Rcode/statistics/OR.R')
CA125<-baseline2%>%filter(性别=="女性",!is.na(CA125),!is.na(吸烟))
CEA<-baseline2%>%filter(性别=="女性",!is.na(CEA),!is.na(吸烟))
CA153<-baseline2%>%filter(性别=="女性",!is.na(CA153),!is.na(吸烟),!is.na(年龄),!is.na(就业状况))
baseline3<-baseline2%>%filter(性别=="女性",!is.na(吸烟),!is.na(年龄),!is.na(就业状况))
#一、肿瘤标志物在检测年份、地区、基础性疾病、绝经、肿瘤家族史的基本分布情况 ####
summary(baseline3[,c('CEA','CA125','CA153')])
#画图 ####
baseline3%>%transmute( CA125=ifelse(CA125<quantile(CA125,0.99,na.rm=T),CA125,NA),
             CEA=ifelse(CEA<quantile(CEA,0.99,na.rm=T),CEA,NA),
             CA153=ifelse(CA153<quantile(CA153,0.99,na.rm=T),CA153,NA),
             )%>%pivot_longer(cols=c('CA125','CEA','CA153'),names_to='marker',values_to='value')%>%ggplot(aes(x=value,y=..density..))+geom_histogram(bins=30,color='black',fill='white')+facet_wrap(marker~.,scales = 'free',nrow=1)+mytheme+labs(x='')+
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# CA125 ####
CA125%>%pivot_longer(cols=c('基础疾病史','绝经','癌症家族史'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
#p值
p<-list()
for(i in c('基础疾病史','绝经','癌症家族史')){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
# CA153 ####
CA153%>%pivot_longer(cols=c('基础疾病史','绝经','癌症家族史'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
p<-list()
for(i in c('基础疾病史','绝经','癌症家族史')){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
# CEA ####
CEA%>%pivot_longer(cols=c('基础疾病史','绝经','癌症家族史'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))

#p值
p<-list()
for(i in c('基础疾病史','绝经','癌症家族史')){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)







#二、CA125
#2.1 CA125共性危险因素
#2.1.1年龄、吸烟、被动吸烟、BMI ####
CA125%>%pivot_longer(cols=c('年龄','吸烟2','被动吸烟','BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
#p值
p<-list()
for(i in c('年龄','吸烟2','被动吸烟','BMI_group')){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)















#2.1.1.1年龄、吸烟、被动吸烟、BMI(基础疾病分层) ####
CA125_split<-split(CA125,CA125$基础疾病史)
#
lapply(CA125_split, function(x)x%>%group_by(BMI_group)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75)))
lapply(CA125_split,function(x)kruskal.test(CA125~BMI_group,x))

#2.1.1.2年龄、吸烟、被动吸烟、BMI(绝经状态分层) ####
CA125_split<-split(CA125,CA125$绝经)
#
lapply(CA125_split, function(x)x%>%group_by(BMI_group)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75)))
lapply(CA125_split,function(x)kruskal.test(CA125~BMI_group,x))

#2.1.1.3年龄、吸烟、被动吸烟、BMI(绝经状态分层) ####
CA125_split<-split(CA125,CA125$癌症家族史)
#
lapply(CA125_split, function(x)x%>%group_by(BMI_group)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75)))
lapply(CA125_split,function(x)kruskal.test(CA125~BMI_group,x))







#2.1.2饮食相关因素
#2.1.2.1基础疾病分层 ####
CA125_split(x='基础疾病史',y='饮酒')
CA125_split(x='基础疾病史',y='喝茶')
CA125_split(x='基础疾病史',y='酸奶')
CA125_split(x='基础疾病史',y='蔬菜')
CA125_split(x='基础疾病史',y='水果')
CA125_split(x='基础疾病史',y='谷类')
CA125_split(x='基础疾病史',y='鸡蛋')
CA125_split(x='基础疾病史',y='杂粮')
CA125_split(x='基础疾病史',y='豆类')
CA125_split(x='基础疾病史',y='坚果')
CA125_split(x='基础疾病史',y='菌类')














#2.1.2.2绝经状态分层 ####
CA125_split(x='绝经',y='饮酒')
CA125_split(x='绝经',y='喝茶')
CA125_split(x='绝经',y='酸奶')
CA125_split(x='绝经',y='蔬菜')
CA125_split(x='绝经',y='水果')
CA125_split(x='绝经',y='谷类')
CA125_split(x='绝经',y='鸡蛋')
CA125_split(x='绝经',y='杂粮')
CA125_split(x='绝经',y='豆类')
CA125_split(x='绝经',y='坚果')
CA125_split(x='绝经',y='菌类')








#2.1.3身体活动及精神问题 ####
#2.1.3.1基础疾病分层 ####
CA125_split(x='基础疾病史',y='运动')
CA125_split(x='基础疾病史',y='静态时间')
CA125_split(x='基础疾病史',y='重大精神创伤')













#2.1.3.1绝经状态分层 ####
CA125_split(x='绝经',y='运动')
CA125_split(x='绝经',y='静态时间')
CA125_split(x='绝经',y='重大精神创伤')














#2.2 CA125特异危险因素
#2.2.1基础疾病分层 ####
CA125_split(x='基础疾病史',y='女性良性病变史')
CA125_split(x='基础疾病史',y='初潮年龄分组')
CA125_split(x='基础疾病史',y='绝经年龄分组')
CA125_split(x='基础疾病史',y='首次生育年龄分组')
CA125_split(x='基础疾病史',y='哺乳时间分组')
CA125_split(x='基础疾病史',y='口服避孕药')
CA125_split(x='基础疾病史',y='雌激素代替治疗')
CA125_split(x='基础疾病史',y='人工流产次数分组')









#2.2.2绝经状态分层 ####
CA125_split(x='绝经',y='女性良性病变史')
CA125_split(x='绝经',y='初潮年龄分组')
CA125_split(x='绝经',y='首次生育年龄分组')
CA125_split(x='绝经',y='哺乳时间分组')
CA125_split(x='绝经',y='口服避孕药')
CA125_split(x='绝经',y='雌激素代替治疗')
CA125_split(x='绝经',y='人工流产次数分组')



#2.2.3 年龄分组

#2.3 CA125特征影像
#2.2.1基础疾病分层 ####
CA125_split(x='基础疾病史',y='乳腺超声描述')
CA125_split(x='基础疾病史',y='肿块')
CA125_split(x='基础疾病史',y='钙化')
CA125_split(x='基础疾病史',y='周围组织异常')
CA125_split(x='基础疾病史',y='特殊情况')
CA125_split(x='基础疾病史',y='淋巴结肿大')

#2.2.2绝经状态分层 ####
CA125_split(x='绝经',y='乳腺超声描述')
CA125_split(x='绝经',y='肿块')
CA125_split(x='绝经',y='钙化')
CA125_split(x='绝经',y='周围组织异常')
CA125_split(x='绝经',y='特殊情况')
CA125_split(x='绝经',y='淋巴结肿大')









CA125_split(x='年龄',y='雌激素代替治疗')



#2.4 CA125与腺体密度
#2.2.1基础疾病分层 ####
CA125_split(x='基础疾病史',y='乳腺组织构成')

#2.2.2绝经状态分层 ####
CA125_split(x='绝经',y='乳腺组织构成')















#2.5 CA125与乳腺超声初筛结局 
CA125%>%group_by(BI_rads)%>%summarise(n=n(),median=median(CA125))
wilcox.test(CA125~BI_rads,data=CA125)
#2.2.1基础疾病分层 ####
CA125_split(x='基础疾病史',y='BI_rads')

#2.2.2绝经状态分层 ####
CA125_split(x='绝经',y='BI_rads')






























#三、CA153
#2.1 CA153共性危险因素
#2.1.1年龄、吸烟、被动吸烟、BMI ####
CA153%>%pivot_longer(cols=c('年龄','吸烟2','被动吸烟','BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
#p值
p<-list()
for(i in c('年龄','吸烟2','被动吸烟','BMI_group')){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)















#2.1.1.1年龄、吸烟、被动吸烟、BMI(基础疾病分层) ####
CA153_split(x='基础疾病史',y='年龄')
CA153_split(x='基础疾病史',y='吸烟2')
CA153_split(x='基础疾病史',y='被动吸烟')
CA153_split(x='基础疾病史',y='BMI_group')
#2.1.1.2年龄、吸烟、被动吸烟、BMI(绝经状态分层) ####
CA153_split(x='绝经',y='年龄')
CA153_split(x='绝经',y='吸烟2')
CA153_split(x='绝经',y='被动吸烟')
CA153_split(x='绝经',y='BMI_group')
#2.1.2饮食相关因素
#2.1.1.2年龄、吸烟、被动吸烟、BMI(癌症家族史分层) ####
CA153_split(x='癌症家族史',y='年龄')
CA153_split(x='癌症家族史',y='吸烟2')
CA153_split(x='癌症家族史',y='被动吸烟')
CA153_split(x='癌症家族史',y='BMI_group')
#2.1.2饮食相关因素 ####

#2.1.2.1基础疾病分层 ####
CA153_split(x='基础疾病史',y='饮酒')
CA153_split(x='基础疾病史',y='喝茶')
CA153_split(x='基础疾病史',y='酸奶')
CA153_split(x='基础疾病史',y='蔬菜')
CA153_split(x='基础疾病史',y='水果')
CA153_split(x='基础疾病史',y='谷类')
CA153_split(x='基础疾病史',y='鸡蛋')
CA153_split(x='基础疾病史',y='杂粮')
CA153_split(x='基础疾病史',y='豆类')
CA153_split(x='基础疾病史',y='坚果')
CA153_split(x='基础疾病史',y='菌类')
#2.1.2.2绝经状态分层 ####
CA153_split(x='绝经',y='饮酒')
CA153_split(x='绝经',y='喝茶')
CA153_split(x='绝经',y='酸奶')
CA153_split(x='绝经',y='蔬菜')
CA153_split(x='绝经',y='水果')
CA153_split(x='绝经',y='谷类')
CA153_split(x='绝经',y='鸡蛋')
CA153_split(x='绝经',y='杂粮')
CA153_split(x='绝经',y='豆类')
CA153_split(x='绝经',y='坚果')
CA153_split(x='绝经',y='菌类')

#2.1.2.3检测年份分层
CA153_split(x='year_group',y='饮酒')
CA153_split(x='year_group',y='喝茶')
CA153_split(x='year_group',y='酸奶')
CA153_split(x='year_group',y='蔬菜')
CA153_split(x='year_group',y='水果')
CA153_split(x='year_group',y='谷类')
CA153_split(x='year_group',y='鸡蛋')
CA153_split(x='year_group',y='杂粮')
CA153_split(x='year_group',y='豆类')
CA153_split(x='year_group',y='坚果')
CA153_split(x='year_group',y='菌类')

#2.1.3身体活动及精神问题 ####
#2.1.3.1基础疾病分层 ####
CA153_split(x='基础疾病史',y='运动')
CA153_split(x='基础疾病史',y='静态时间')
CA153_split(x='基础疾病史',y='重大精神创伤')

#2.1.3.1绝经状态分层 ####
CA153_split(x='绝经',y='运动')
CA153_split(x='绝经',y='静态时间')
CA153_split(x='绝经',y='重大精神创伤')


#2.2 CA153特异危险因素
#2.2ca153与特异危险因素
#2.2.1基础疾病分层 ####
CA153_split(x='基础疾病史',y='女性良性病变史')
CA153_split(x='基础疾病史',y='初潮年龄分组')
CA153_split(x='基础疾病史',y='绝经年龄分组')
CA153_split(x='基础疾病史',y='首次生育年龄分组')
CA153_split(x='基础疾病史',y='哺乳时间分组')
CA153_split(x='基础疾病史',y='口服避孕药')
CA153_split(x='基础疾病史',y='雌激素代替治疗')
CA153_split(x='基础疾病史',y='人工流产次数分组')

#2.2.2绝经状态分层 ####
CA153_split(x='绝经',y='女性良性病变史')
CA153_split(x='绝经',y='初潮年龄分组')
CA153_split(x='绝经',y='首次生育年龄分组')
CA153_split(x='绝经',y='哺乳时间分组')
CA153_split(x='绝经',y='口服避孕药')
CA153_split(x='绝经',y='雌激素代替治疗')
CA153_split(x='绝经',y='人工流产次数分组')




#2.3 CA153特征影像 ####
#2.2.1基础疾病分层 ####
CA153_split(x='基础疾病史',y='乳腺超声描述')
CA153_split(x='基础疾病史',y='肿块')
CA153_split(x='基础疾病史',y='钙化')
CA153_split(x='基础疾病史',y='周围组织异常')
CA153_split(x='基础疾病史',y='特殊情况')
CA153_split(x='基础疾病史',y='淋巴结肿大')

#2.2.2绝经状态分层 ####
CA153_split(x='绝经',y='乳腺超声描述')
CA153_split(x='绝经',y='肿块')
CA153_split(x='绝经',y='钙化')
CA153_split(x='绝经',y='周围组织异常')
CA153_split(x='绝经',y='特殊情况')
CA153_split(x='绝经',y='淋巴结肿大')
#2.4 CA153与腺体密度
#2.2.1基础疾病分层 ####
CA153_split(x='基础疾病史',y='乳腺组织构成')
#2.2.2绝经状态分层 ####
CA153_split(x='绝经',y='乳腺组织构成')
#2.5 CA153与乳腺超声初筛结局 ####
CA153%>%group_by(BI_rads)%>%summarise(n=n(),median=median(CA153))
wilcox.test(CA153~BI_rads,data=CA153)
#2.2.1基础疾病分层 ####
CA153_split(x='基础疾病史',y='BI_rads')

#2.2.2绝经状态分层 ####
CA153_split(x='绝经',y='BI_rads')





















































#2.2.3检测年份分层 ####
CA153_split(x='year_group',y='女性良性病变史')
CA153_split(x='year_group',y='初潮年龄分组')
CA153_split(x='year_group',y='首次生育年龄分组')
CA153_split(x='year_group',y='哺乳时间分组')
CA153_split(x='year_group',y='口服避孕药')
CA153_split(x='year_group',y='雌激素代替治疗')
CA153_split(x='year_group',y='人工流产次数分组')


#四、CEA ####
#2.1 CEA共性危险因素
#2.1.1年龄、吸烟、被动吸烟、BMI ####
CEA%>%pivot_longer(cols=c('年龄','吸烟2','被动吸烟','BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
#p值
p<-list()
for(i in c('年龄','吸烟2','被动吸烟','BMI_group')){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)

#2.1.1.1年龄、吸烟、被动吸烟、BMI(基础疾病分层) ####
CEA_split(x='基础疾病史',y='年龄')
CEA_split(x='基础疾病史',y='吸烟2')
CEA_split(x='基础疾病史',y='被动吸烟')
CEA_split(x='基础疾病史',y='BMI_group')
#2.1.1.2年龄、吸烟、被动吸烟、BMI(绝经状态分层) ####
CEA_split(x='绝经',y='年龄')
CEA_split(x='绝经',y='吸烟2')
CEA_split(x='绝经',y='被动吸烟')
CEA_split(x='绝经',y='BMI_group')
#2.1.1.3年龄、吸烟、被动吸烟、BMI(癌症家族史分层) ####
CEA_split(x='癌症家族史',y='年龄')
CEA_split(x='癌症家族史',y='吸烟2')
CEA_split(x='癌症家族史',y='被动吸烟')
CEA_split(x='癌症家族史',y='BMI_group')
#2.1.2饮食相关因素 ####
#2.1.2.1基础疾病分层 ####
CEA_split(x='基础疾病史',y='饮酒')
CEA_split(x='基础疾病史',y='喝茶')
CEA_split(x='基础疾病史',y='酸奶')
CEA_split(x='基础疾病史',y='蔬菜')
CEA_split(x='基础疾病史',y='水果')
CEA_split(x='基础疾病史',y='谷类')
CEA_split(x='基础疾病史',y='鸡蛋')
CEA_split(x='基础疾病史',y='杂粮')
CEA_split(x='基础疾病史',y='豆类')
CEA_split(x='基础疾病史',y='坚果')
CEA_split(x='基础疾病史',y='菌类')
#2.1.2.2绝经状态分层 ####
CEA_split(x='绝经',y='饮酒')
CEA_split(x='绝经',y='喝茶')
CEA_split(x='绝经',y='酸奶')
CEA_split(x='绝经',y='蔬菜')
CEA_split(x='绝经',y='水果')
CEA_split(x='绝经',y='谷类')
CEA_split(x='绝经',y='鸡蛋')
CEA_split(x='绝经',y='杂粮')
CEA_split(x='绝经',y='豆类')
CEA_split(x='绝经',y='坚果')
CEA_split(x='绝经',y='菌类')

#2.1.3身体活动及精神问题 ####
#2.1.3.1基础疾病分层 ####
CEA_split(x='基础疾病史',y='运动')
CEA_split(x='基础疾病史',y='静态时间')
CEA_split(x='基础疾病史',y='重大精神创伤')

#2.1.3.1绝经状态分层 ####
CEA_split(x='绝经',y='运动')
CEA_split(x='绝经',y='静态时间')
CEA_split(x='绝经',y='重大精神创伤')


#2.2 CEA特异危险因素 #####
#2.2.1基础疾病分层 ####
CEA_split(x='基础疾病史',y='女性良性病变史')
CEA_split(x='基础疾病史',y='初潮年龄分组')
CEA_split(x='基础疾病史',y='绝经年龄分组')
CEA_split(x='基础疾病史',y='首次生育年龄分组')
CEA_split(x='基础疾病史',y='哺乳时间分组')
CEA_split(x='基础疾病史',y='口服避孕药')
CEA_split(x='基础疾病史',y='雌激素代替治疗')
CEA_split(x='基础疾病史',y='人工流产次数分组')

#2.2.2绝经状态分层 ####
CEA_split(x='绝经',y='女性良性病变史')
CEA_split(x='绝经',y='初潮年龄分组')
CEA_split(x='绝经',y='首次生育年龄分组')
CEA_split(x='绝经',y='哺乳时间分组')
CEA_split(x='绝经',y='口服避孕药')
CEA_split(x='绝经',y='雌激素代替治疗')
CEA_split(x='绝经',y='人工流产次数分组')




#2.3 CEA特征影像 ####
#2.2.1基础疾病分层 ####
CEA_split(x='基础疾病史',y='乳腺超声描述')
CEA_split(x='基础疾病史',y='肿块')
CEA_split(x='基础疾病史',y='钙化')
CEA_split(x='基础疾病史',y='周围组织异常')
CEA_split(x='基础疾病史',y='特殊情况')
CEA_split(x='基础疾病史',y='淋巴结肿大')

#2.2.2绝经状态分层 ####
CEA_split(x='绝经',y='乳腺超声描述')
CEA_split(x='绝经',y='肿块')
CEA_split(x='绝经',y='钙化')
CEA_split(x='绝经',y='周围组织异常')
CEA_split(x='绝经',y='特殊情况')
CEA_split(x='绝经',y='淋巴结肿大')
#2.2.3癌症家族史分层 ####
CEA_split(x='癌症家族史',y='乳腺超声描述')
CEA_split(x='癌症家族史',y='肿块')
CEA_split(x='癌症家族史',y='钙化')
CEA_split(x='癌症家族史',y='周围组织异常')
CEA_split(x='癌症家族史',y='特殊情况')
CEA_split(x='癌症家族史',y='淋巴结肿大')
#2.4 CEA与腺体密度 ####
#2.2.1基础疾病分层 ####
CEA_split(x='基础疾病史',y='乳腺组织构成')
#2.2.2绝经状态分层 ####
CEA_split(x='绝经',y='乳腺组织构成')
#2.5 CEA与乳腺超声初筛结局 ####
CEA%>%group_by(BI_rads)%>%summarise(n=n(),median=median(CEA))
wilcox.test(CEA~BI_rads,data=CEA)
#2.2.1基础疾病分层 ####
CEA_split(x='基础疾病史',y='BI_rads')

#2.2.2绝经状态分层 ####
CEA_split(x='绝经',y='BI_rads')


#乳腺超声复查结局 ####
summary(baseline3$乳腺超声描述)
baseline3$乳腺超声异常<-0
baseline3$乳腺超声异常[baseline3$肿块=="有"]<-1
baseline3$乳腺超声异常[baseline3$钙化=="有"]<-1
baseline3$乳腺超声异常[baseline3$周围组织异常=="有"]<-1
baseline3$乳腺超声异常[baseline3$特殊情况=="有"]<-1
baseline3$乳腺超声异常[baseline3$淋巴结肿大=="有"]<-1

table(baseline3$乳腺超声异常)
summary(baseline3$乳腺组织构成)
##

CA125_split(x='子宫摘除术',y='雌激素代替治疗')

