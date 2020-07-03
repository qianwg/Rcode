rm(list=ls())
library(rio)
library(tidyverse)
library(glmnet)
library(ggpubr)
library(table1)
library(knitr)
library(kableExtra)
library(car)
library(grpreg)
source('~/Rcode/statistics/OR.R')
source('~/Rcode/statistics/ifdif.R')
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(face="bold",size=10),
               axis.text=element_text(face="bold",size=9),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black')
)
variables1<-c('性别','年龄','教育','婚姻','收入','就业状况','血型')
variables2<-c("饮酒","喝茶","酸奶","蔬菜","水果","谷类","鸡蛋","杂粮","豆类","坚果","菌类")
variables3<-c("运动","快走","太极","广场舞","瑜伽","游泳","跑步","球类","器械","静态时间")
variables4<-c("糖尿病","高血压","高血脂","冠心病","中风")
variables5<-c("胆囊息肉","胆结石","脂肪肝","肝硬化","慢性乙型肝炎","慢性丙型肝炎")
variables6<-c('镉','石棉','镍','砷','氡','氯乙烯','X射线')
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}

#肝部肿瘤标志物数据读取(AFP,CA199,HBsAg)
source('~/Rcode/biomarker/data_liver.R')
AFP<-biomarker%>%filter(!is.na(AFP) & !is.na(吸烟))#21970
CA199<-biomarker%>%filter(!is.na(CA199) & !is.na(吸烟),!is.na(AFP))#21970
HBsAg<-biomarker%>%filter(!is.na(HBsAg_pos))
ggplot(data=AFP,aes(x=AFP,y=..density..))+geom_histogram(bins=30,color='black',fill='blue')+mytheme+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,30))
#人口学特征及临床资料
table1(~肝癌家族史+性别+年龄+收入+教育+婚姻+就业状况+职业+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+
         静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线,data=AFP2,render.categorical=my.render.cat)
#基本人群特征分布比较
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=variables1,names_to='variable',values_to = 'level')%>%
group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in variables1){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=variables1,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in variables1){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)
##矫正
forest_model(lm(log(AFP)~教育+婚姻+收入+就业状况+血型+年龄+性别,data=AFP),limits=c(-0.1,0.2))
forest_model(lm(log(CA199)~教育+婚姻+收入+就业状况+血型+年龄+性别,data=CA199),limits=c(-0.1,0.3))


#HBsA阳性在基本人群特征中的分布比较

table1::table1(~性别+年龄+教育+婚姻+收入+就业状况+血型 | HBsAg_pos,data=HBsAg,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables1){
  y<-HBsAg[[i]]
  p1[[i]]<-with(HBsAg,as.data.frame(round(prop.table(table(y,HBsAg_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
p<-list()
for(i in variables1){
  y<-HBsAg[[i]]
  p[[i]]<-round(chisq.test(table(y,HBsAg$HBsAg_pos))$p.value,3)
}
do.call(rbind,p)

##吸烟、被动吸烟情况
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=c('吸烟','被动吸烟'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in c('吸烟','被动吸烟')){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=c('吸烟','被动吸烟'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in c('吸烟','被动吸烟')){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)

#多因素矫正
forest_model(lm(log(AFP)~被动吸烟+吸烟+年龄+性别,data=AFP),limits=c(-0.05,0.15))
forest_model(lm(log(CA199)~被动吸烟+吸烟+年龄+性别,data=CA199),limits=c(-0.1,0.2))


##与肥胖之间的关联分析
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=c('BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
kruskal.test(AFP~BMI_group,data=AFP)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=c('BMI_group'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
kruskal.test(AFP~BMI_group,data=CA199)
#多因素矫正
forest_model(lm(log(AFP)~BMI_group+年龄+性别,data=AFP),limits = c(-0.1,0.15))
forest_model(lm(log(CA199)~BMI_group+年龄+性别,data=CA199),limits=c(-0.05,0.2))
##画图
ggplot







##饮酒、饮茶、饮食的关联分析
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=variables2,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in variables2){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=variables2,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in variables2){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)
##多因素矫正
forest_model(lm(log(AFP)~饮酒+喝茶+年龄+性别,data=AFP),limits=c(0,0.1))
forest_model(lm(log(CA199)~饮酒+蔬菜+年龄+性别,data=CA199),limits=c(-0.15,0.20))


##身体活动的关联分析
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=variables3,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in variables3){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=variables3,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in variables3){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)

##基础性疾病史

#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)
###多因素矫正
forest_model(lm(log(AFP)~高血脂+糖尿病+高血压+年龄+性别,data=AFP),limits=c(-0.15,0.10))
forest_model(lm(log(CA199)~高血脂+糖尿病+高血压+冠心病+中风+年龄+性别,data=CA199),limits=c(-0.15,0.20))


##特殊疾病史
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)
###特殊职业暴露
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)
##<<<<<<<<<<<<<<<<<<<<特殊癌症家族史>>>>>>>>>>>>>>>>>>>>>>
#AFP/CA199连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=c('肝癌家族史','胃癌家族史'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
p<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP)$p.value,4)
  }
}
do.call(rbind,p)
#CA199
means_CA199<-CA199%>%pivot_longer(cols=c('肝癌家族史','胃癌家族史'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=126)
p<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)
##分类变量
#AFP
table1(~肝癌家族史+胃癌家族史 | AFP_pos,data=AFP,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  y<-AFP[[i]]
  p1[[i]]<-with(AFP,as.data.frame(round(prop.table(table(y,AFP_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  y<-AFP[[i]]
  p2[[i]]<-round(chisq.test(table(y,AFP$AFP_pos))$p.value,3)
}
do.call(rbind,p2)
#CA199
table1(~肝癌家族史+胃癌家族史 | CA199_pos,data=CA199,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  y<-AFP[[i]]
  p1[[i]]<-with(AFP,as.data.frame(round(prop.table(table(y,AFP_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  y<-CA199[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA199$CA199_pos))$p.value,3)
}
do.call(rbind,p2)
#HBsAg
table1(~肝癌家族史+胃癌家族史 | HBsAg_pos,data=HBsAg,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  y<-HBsAg[[i]]
  p1[[i]]<-with(HBsAg,as.data.frame(round(prop.table(table(y,HBsAg_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('肝癌家族史','胃癌家族史')){
  y<-HBsAg[[i]]
  p2[[i]]<-round(chisq.test(table(y,HBsAg$HBsAg_pos))$p.value,3)
}
do.call(rbind,p2)





#<<<<<<<<<<<<<<<<<<<<<<<<对于肝癌的诊断效果(诊断效果很差的)>>>>>>>>>>>>>>>>>>>>>>>>
AFP%>%group_by(CA_type)%>%summarise(n=n(),median=median(AFP))
roc(AFP$CA_liver, AFP$AFP,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(AFP$CA_liver, AFP$AFP,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
#对于肝癌的诊断效果(诊断效果很差的)
CA199%>%group_by(CA_type)%>%summarise(n=n(),median=median(CA199))
roc(CA199$CA_liver, CA199$CA199,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(CA199$CA_liver, CA199$CA199,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)





##HBsAg

HBsAg<-biomarker%>%filter(!is.na(HBsAg))
HBsAg$HBsAg_pos[HBsAg$HBsAg_pos==1]<-0
HBsAg$HBsAg_pos[HBsAg$HBsAg_pos==2]<-1
HBsAg2<-HBsAg
HBsAg2[sapply(HBsAg2, is.numeric)] <- lapply(HBsAg2[sapply(HBsAg2, is.numeric)], as.factor)
#HBsAg作为二分类变量
#频率分布+单因素分析
table1(~肝癌家族史+性别+年龄+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+
       静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线 | HBsAg_pos,data=HBsAg2,render.categorical=my.render.cat)
p2<-list()
for(i in variables){
  y<-HBsAg[[i]]
  p2[[i]]<-round(chisq.test(table(y,HBsAg$HBsAg_pos))$p.value,3)
}
do.call(rbind,p2)
#多因素logistic回归
#多重共线性检验
vif_data<-vif(glm(HBsAg_pos~.,family='binomial',data=HBsAg[,c('HBsAg_pos',variables)]))
vif_data
sort(vif_data,decreasing = TRUE)
#logistic回归
summary(glm(HBsAg_pos~.,data=HBsAg[,c('HBsAg_pos',variables)],family='binomial'))
logit(y='HBsAg_pos',x=c("肝癌家族史",'性别',"年龄","就业状况","BMI",              
                        "偏咸","腌制","饮酒","喝茶",             
                        "酸奶","吸烟" ,"被动吸烟", "婚姻",             
                        "教育", "血型","蔬菜","水果",             
                        "谷类","鸡蛋","杂粮","豆类",              
                        "坚果", "菌类","油炸","烧烤",              
                        "熏制","运动","快走","太极","广场舞","瑜伽",          
                        "游泳","跑步","球类","器械","静态时间","手机使用时间",  
                        "胆囊息肉","胆结石","脂肪肝","肝硬化","慢性乙型肝炎","慢性丙型肝炎",  
                        "血吸虫病感染史","糖尿病","高血压","高血脂","冠心病","中风",          
                        "镉","石棉","镍","砷","氡","氯乙烯","X射线"),data=HBsAg)

#lasso-logistic回归
HBsAg_2<-na.omit(AFP[,c('HBsAg_pos',variables)])
x<-model.matrix(HBsAg_pos~.,HBsAg_2,contrasts.arg = lapply(HBsAg_2[ ,sapply(HBsAg_2, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y<-HBsAg_2[,c('HBsAg_pos')]
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
coef(cv_output,s='lambda.min')#系数
get_coe(cv_output,cv_output$lambda.min)
#group-lasso logistic
group<-c()
fit <- grpreg(x, y, group, penalty="grLasso", family="binomial")
plot(fit)
cvfit<- cv.grpreg(x, y, group, penalty="grLasso",family='binomial')
plot(cvfit)
as.matrix(coef(cvfit,lambda=cvfit$lambda.min))


#####AFP与特征影像检查的关联性分析
liverBUS2<-import('~/data/word3.xlsx')
liverBUS2$AFP_pos[liverBUS2$AFP_pos=='阴性']<-0
liverBUS2$AFP_pos[liverBUS2$AFP_pos=='阳性']<-1
liverBUS2$AFP_pos<-factor(liverBUS2$AFP_pos)
with(liverBUS2,table(nan,is.na(占位1) & is.na(占位2) & is.na(占位3) & is.na(占位4)))
with(liverBUS2,table(nan,占位1=='未见明显异常' | 占位2=='未见明显异常' | 占位3=='未见明显异常' | 占位4=='未见明显异常'))
liverBUS2[]
liverBUS2%>%pivot_longer(cols=c('占位1','占位2','占位3','占位4'),names_to = '占位m',values_to = '占位类型')%>%group_by(占位类型)%>%
  summarise(n=n())%>%arrange(desc(n))%>%filter(n>30,!is.na(占位类型))%>%ggplot(aes(reorder(占位类型,n),n)) +
  geom_col(show.legend = FALSE)+mytheme+labs(x='占位类型')+
  coord_flip()

#1、AFP与占位的相关性分析
#占位类型：01.肝硬化, 02.门脉高压, 
#03.胆囊炎, 04.脾大, 05.腹水, 06.脂肪肝, 07.肝囊肿, 08.肝脓肿, 09.肝血管瘤, 10.肝占位性病变,
#11.肝癌, 12.门静脉栓塞, 13.肝胆管扩张, 14.未见异常,15.其他
#liverAFP2%>%group_by(占位1类型)%>%summarise(n=n())%>%filter(!is.na(占位1类型))
#liverAFP2%>%group_by(占位2类型)%>%summarise(n=n())%>%filter(!is.na(占位2类型))
#liverAFP2%>%group_by(占位3类型)%>%summarise(n=n())%>%filter(!is.na(占位3类型))
#ggplot(data=liverAFP2,aes(x=占位1类型))+geom_bar()+mytheme+scale_x_continuous(breaks=seq(0,15,1))
#ggplot(data=liverAFP2,aes(x=占位2类型))+geom_bar()+mytheme+scale_x_continuous(breaks=seq(0,15,1))
#ggplot(data=liverAFP2,aes(x=占位3类型))+geom_bar()+mytheme+scale_x_continuous(breaks=seq(0,15,1))
#2、AFP与肝硬化/脂肪肝/肝囊肿/肝癌等占位类型的相关性分析
#2.1 肝硬化(占位类型=01)
#liverBUS2[which(liverBUS2$占位1=='肝硬化' | liverBUS2$占位2=='肝硬化' | liverBUS2$占位3=='肝硬化' | 
#                  liverBUS2$占位4=='肝硬化' | liverBUS2$占位1类型==1 | liverBUS2$占位2类型==1 | liverBUS2$占位3类型==1),
#          c('占位','占位1类型','占位2类型','占位3类型',"livultrdia","占位1", "占位2","占位3","占位4")]
liverBUS2$脂肪肝<-0
liverBUS2$肝硬化<-0
liverBUS2$肝囊肿<-0
liverBUS2$肝癌<-0
liverBUS2$脂肪肝[liverBUS2$占位1类型==6 |  liverBUS2$占位2类型==6 | liverBUS2$占位3类型 ==6 
              | liverBUS2$占位1=='脂肪肝' | liverBUS2$占位2=='脂肪肝' | liverBUS2$占位3=='脂肪肝' | liverBUS2$占位4=='脂肪肝' ]<-1
liverBUS2$肝硬化[liverBUS2$占位1类型==1 |  liverBUS2$占位2类型==1 | liverBUS2$占位3类型 ==1 | liverBUS2$占位1=='肝硬化' | liverBUS2$占位2=='肝硬化' |
                liverBUS2$占位3=='肝硬化' | liverBUS2$占位4=='肝硬化' ]<-1
liverBUS2$肝囊肿[liverBUS2$占位1类型==7 |  liverBUS2$占位2类型==7 | liverBUS2$占位3类型 ==7 | liverBUS2$占位1=='肝囊肿' | liverBUS2$占位2=='肝囊肿' |
                liverBUS2$占位3=='肝囊肿' | liverBUS2$占位4=='肝囊肿' ]<-1
liverBUS2$肝癌[liverBUS2$占位1类型==11 |  liverBUS2$占位2类型==11 | liverBUS2$占位3类型 ==11 | liverBUS2$占位1=='肝癌' | 
               liverBUS2$占位2=='肝癌' | liverBUS2$占位3=='肝癌' | liverBUS2$占位4=='肝癌' ]<-1
liverBUS2$新占位<-ifelse(liverBUS2$脂肪肝==1 | liverBUS2$肝硬化==1 | liverBUS2$肝囊肿==1 | liverBUS2$肝癌,1,0 )
with(liverBUS2,table(Year,脂肪肝));with(liverBUS2,prop.table(table(Year,脂肪肝),margin = 1))
with(liverBUS2,table(Year,肝硬化));with(liverBUS2,prop.table(table(Year,肝硬化),margin = 1))
with(liverBUS2,table(Year,肝囊肿));with(liverBUS2,prop.table(table(Year,肝囊肿),margin = 1))
with(liverBUS2,table(Year,肝癌));with(liverBUS2,prop.table(table(Year,肝癌),margin = 1))
#四种类型占位的合计人数(12764)
with(liverBUS2,table(Year,新占位));with(liverBUS2,prop.table(table(Year,新占位),margin = 1))
#脂肪肝
liverBUS2%>%group_by(年龄,脂肪肝)%>%summarise(n=n())%>%group_by(年龄)%>%mutate(n,n/sum(n))
liverBUS2%>%group_by(脂肪肝)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~脂肪肝,data=liverBUS2)
with(liverBUS2,table(脂肪肝,AFP_pos))
with(liverBUS2,prop.table(table(脂肪肝,AFP_pos),margin = 2))
with(liverBUS2,chisq.test(table(脂肪肝,AFP_pos)))
logit(y='AFP_pos',x='脂肪肝',data=liverBUS2)
liverBUS2%>%group_by(性别,脂肪肝)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~脂肪肝,data=subset(liverBUS2,性别==0))
wilcox.test(AFP~脂肪肝,data=subset(liverBUS2,性别==1))
with(subset(liverBUS2,性别==0),prop.table(table(脂肪肝,AFP_pos),margin = 2))
with(subset(liverBUS2,性别==0),chisq.test(table(脂肪肝,AFP_pos)))
with(subset(liverBUS2,性别==1),prop.table(table(脂肪肝,AFP_pos),margin = 2))
with(subset(liverBUS2,性别==1),chisq.test(table(脂肪肝,AFP_pos)))
logit(y='AFP_pos',x='脂肪肝',data=liverBUS2)
liverBUS2_0<-liverBUS2[which(liverBUS2$性别==0),]
liverBUS2_1<-liverBUS2[which(liverBUS2$性别==1),]
logit(y='AFP_pos',x='脂肪肝',data=liverBUS2)
logit(y='AFP_pos',x='脂肪肝',data=liverBUS2_0)
logit(y='AFP_pos',x='脂肪肝',data=liverBUS2_1)

#肝硬化
liverBUS2%>%group_by(肝硬化)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~肝硬化,data=liverBUS2)
with(liverBUS2,table(肝硬化,AFP_pos))
with(liverBUS2,prop.table(table(肝硬化,AFP_pos),margin = 2))
with(liverBUS2,chisq.test(table(肝硬化,AFP_pos)))
logit(y='AFP_pos',x='肝硬化',data=liverBUS2)
liverBUS2%>%group_by(性别,肝硬化)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
  wilcox.test(AFP~肝硬化,data=subset(liverBUS2,性别==0))
  wilcox.test(AFP~肝硬化,data=subset(liverBUS2,性别==1))
  with(subset(liverBUS2,性别==0),prop.table(table(肝硬化,AFP_pos),margin = 2))
  with(subset(liverBUS2,性别==0),chisq.test(table(肝硬化,AFP_pos)))
  with(subset(liverBUS2,性别==1),prop.table(table(肝硬化,AFP_pos),margin = 2))
  with(subset(liverBUS2,性别==1),chisq.test(table(肝硬化,AFP_pos)))
  logit(y='AFP_pos',x='肝硬化',data=subset(liverBUS2,性别==0))
  logit(y='AFP_pos',x='肝硬化',data=subset(liverBUS2,性别==1))
  #肝囊肿
liverBUS2%>%group_by(肝囊肿)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~肝囊肿,data=liverBUS2)
with(liverBUS2,table(肝囊肿,AFP_pos))
with(liverBUS2,prop.table(table(肝囊肿,AFP_pos),margin = 2))
with(liverBUS2,chisq.test(table(肝囊肿,AFP_pos)))
logit(y='AFP_pos',x='肝囊肿',data=liverBUS2)
liverBUS2%>%group_by(性别,肝囊肿)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~肝囊肿,data=subset(liverBUS2,性别==0))
wilcox.test(AFP~肝囊肿,data=subset(liverBUS2,性别==1))
with(subset(liverBUS2,性别==0),prop.table(table(肝囊肿,AFP_pos),margin = 2))
with(subset(liverBUS2,性别==0),chisq.test(table(肝囊肿,AFP_pos)))
with(subset(liverBUS2,性别==1),prop.table(table(肝囊肿,AFP_pos),margin = 2))
with(subset(liverBUS2,性别==1),chisq.test(table(肝囊肿,AFP_pos)))
logit(y='AFP_pos',x='肝囊肿',data=subset(liverBUS2,性别==0))
logit(y='AFP_pos',x='肝囊肿',data=subset(liverBUS2,性别==1))
#疑似肝癌
liverBUS2%>%group_by(肝癌)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~肝癌,data=liverBUS2)
#合计
liverBUS2%>%group_by(新占位)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~新占位,data=liverBUS2)
with(liverBUS2,table(新占位,AFP_pos))
with(liverBUS2,prop.table(table(新占位,AFP_pos),margin = 2))
with(liverBUS2,chisq.test(table(新占位,AFP_pos)))
logit(y='AFP_pos',x='新占位',data=liverBUS2)
liverBUS2%>%group_by(性别,新占位)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~新占位,data=subset(liverBUS2,性别==0))
wilcox.test(AFP~新占位,data=subset(liverBUS2,性别==1))
with(subset(liverBUS2,性别==0),prop.table(table(新占位,AFP_pos),margin = 2))
with(subset(liverBUS2,性别==0),chisq.test(table(新占位,AFP_pos)))
with(subset(liverBUS2,性别==1),prop.table(table(新占位,AFP_pos),margin = 2))
with(subset(liverBUS2,性别==1),chisq.test(table(新占位,AFP_pos)))
logit(y='AFP_pos',x='新占位',data=subset(liverBUS2,性别==0))
logit(y='AFP_pos',x='新占位',data=subset(liverBUS2,性别==1))
#多因素矫正肝脏超声影像特征与AFP的关联性
liverBUS3<-inner_join(liverBUS2,biomarker,by='ID_BLAST')
#矫正性别和年龄model1
logit(y='AFP_pos.x',x=c('脂肪肝.x','性别.x','年龄.x'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('肝硬化.x','性别.x','年龄.x'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('肝囊肿','性别.x','年龄.x'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('新占位','性别.x','年龄.x'),data=liverBUS3)

#矫正model1+血型、被动吸烟 model2
logit(y='AFP_pos.x',x=c('脂肪肝.x','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('肝硬化.x','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('肝囊肿','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('新占位','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=liverBUS3)

#矫正model2+肝硬化、糖尿病、高血压+高血脂、冠心病+中风
logit(y='AFP_pos.x',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','脂肪肝.x'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','肝硬化.x'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','肝囊肿'),data=liverBUS3)
logit(y='AFP_pos.x',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','新占位'),data=liverBUS3)




#肝脏超声特征影像与CA199阴阳性的关联性分析
CA199_liverBUS<-inner_join(CA199_2,liverBUS2,by='ID_BLAST')
#脂肪肝
with(CA199_liverBUS,table(脂肪肝.y,CA199_pos))
with(CA199_liverBUS,prop.table(table(脂肪肝.y,CA199_pos),margin = 2))
with(CA199_liverBUS,chisq.test(table(脂肪肝.y,CA199_pos)))
logit(y='CA199_pos',x='脂肪肝.y',data=CA199_liverBUS)
#肝硬化
with(CA199_liverBUS,table(肝硬化.y,CA199_pos))
with(CA199_liverBUS,prop.table(table(肝硬化.y,CA199_pos),margin = 2))
with(CA199_liverBUS,chisq.test(table(肝硬化.y,CA199_pos)))
logit(y='CA199_pos',x='肝硬化.y',data=CA199_liverBUS)
#肝囊肿
with(CA199_liverBUS,table(肝囊肿,CA199_pos))
with(CA199_liverBUS,prop.table(table(肝囊肿,CA199_pos),margin = 2))
with(CA199_liverBUS,chisq.test(table(肝囊肿,CA199_pos)))
logit(y='CA199_pos',x='肝囊肿',data=CA199_liverBUS)
#新占位
with(CA199_liverBUS,table(新占位,CA199_pos))
with(CA199_liverBUS,prop.table(table(新占位,CA199_pos),margin = 2))
with(CA199_liverBUS,chisq.test(table(新占位,CA199_pos)))
logit(y='CA199_pos',x='新占位',data=CA199_liverBUS)
#多因素矫正
#矫正性别和年龄model1
logit(y='CA199_pos',x=c('脂肪肝.y','性别.x','年龄.x'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('肝硬化.y','性别.x','年龄.x'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('肝囊肿','性别.x','年龄.x'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('新占位','性别.x','年龄.x'),data=CA199_liverBUS)

#矫正model1+血型、被动吸烟 model2
logit(y='CA199_pos',x=c('脂肪肝.y','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('肝硬化.y','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('肝囊肿','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('新占位','BMI.x','性别.x','年龄.x','血型','被动吸烟'),data=CA199_liverBUS)

#矫正model2+肝硬化、糖尿病、高血压+高血脂、冠心病+中风
logit(y='CA199_pos',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','脂肪肝.y'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','肝硬化.y'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','肝囊肿'),data=CA199_liverBUS)
logit(y='CA199_pos',x=c('BMI.x','性别.x','年龄.x','血型','被动吸烟','糖尿病','高血压','高血脂','冠心病','中风','新占位'),data=CA199_liverBUS)





###乙肝携带者与AFP的关系性研究
HBsAG<-biomarker%>%filter(!is.na(HBsAg_pos))
HBsAG$HBsAg_pos[HBsAG$HBsAg_pos==1]<-0
HBsAG$HBsAg_pos[HBsAG$HBsAg_pos==2]<-1

HBsAG%>%group_by(HBsAg_pos)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
wilcox.test(AFP~HBsAg_pos,data=HBsAG)
#二分类
with(HBsAG,table(HBsAg_pos,AFP_pos))
with(HBsAG,prop.table(table(HBsAg_pos,AFP_pos),margin=2))
with(HBsAG,chisq.test(AFP_pos,HBsAg_pos))
##
##肝脏超声影像特征与HBsAg阴阳性的关联性分析
HBsAG_liverBUS<-inner_join(HBsAG,liverBUS2,by='ID_BLAST')
#脂肪肝
with(HBsAG_liverBUS,table(脂肪肝.y,HBsAg_pos))
with(HBsAG_liverBUS,prop.table(table(脂肪肝.y,HBsAg_pos),margin = 2))
with(HBsAG_liverBUS,chisq.test(table(脂肪肝.y,HBsAg_pos)))
logit(y='HBsAg_pos',x='脂肪肝.y',data=HBsAG_liverBUS)
#肝硬化
with(HBsAG_liverBUS,table(肝硬化.y,HBsAg_pos))
with(HBsAG_liverBUS,prop.table(table(肝硬化.y,HBsAg_pos),margin = 2))
with(HBsAG_liverBUS,chisq.test(table(肝硬化.y,HBsAg_pos)))
logit(y='HBsAg_pos',x='肝硬化.y',data=HBsAG_liverBUS)
#肝囊肿
with(HBsAG_liverBUS,table(肝囊肿,HBsAg_pos))
with(HBsAG_liverBUS,prop.table(table(肝囊肿,HBsAg_pos),margin = 2))
with(HBsAG_liverBUS,chisq.test(table(肝囊肿,HBsAg_pos)))
logit(y='HBsAg_pos',x='肝囊肿',data=HBsAG_liverBUS)
#新占位
with(HBsAG_liverBUS,table(新占位,HBsAg_pos))
with(HBsAG_liverBUS,prop.table(table(新占位,HBsAg_pos),margin = 2))
with(HBsAG_liverBUS,chisq.test(table(新占位,HBsAg_pos)))
logit(y='HBsAg_pos',x='新占位',data=HBsAG_liverBUS)








