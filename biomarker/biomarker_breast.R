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
#读取数据
source('~/Rcode/biomarker/data_biomarker.R')
source("~/Rcode/screening/breastBUS.R")
baseline2<-left_join(baseline,breastBUS,by='ID_BLAST')
source('~/Rcode/statistics/data_summary.R')
source('~/Rcode/statistics/OR.R')
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}

source('~/Rcode/statistics/two_y_axis.R')
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(face="bold",size=10),
               axis.text=element_text(face="bold",size=9),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black'),
               legend.position = 'none'
)
#variables<-c()
variables2<-c('年龄','家庭收入','教育','婚姻','就业状况','职业类型','血型')
variables3<-c('初潮年龄分组','生育','首次生育年龄分组','生育次数分组','哺乳','哺乳时间分组','流产','人工流产','绝经',"绝经年龄分组")
variables4<-c("饮酒","喝茶","酸奶","蔬菜","水果","谷类","鸡蛋","杂粮","豆类","坚果","菌类")
variables5<-c("运动","快走","太极","广场舞","瑜伽","游泳","跑步","球类","器械","静态时间")
variables6<-c("糖尿病","高血压","高血脂","冠心病","中风")
variables7<-c('镉','石棉','镍','砷','氡','氯乙烯','X射线')
variables8<-c("左乳超声描述","左乳肿块","左乳钙化","左乳周围组织异常","左乳淋巴结肿大",
              "右乳超声描述","右乳肿块","右乳钙化","右乳周围组织异常","右乳淋巴结肿大",
              "乳腺超声描述","肿块","钙化","周围组织异常","淋巴结肿大","乳腺组织构成","BIRADS")
#summary(baseline2)
CA125<-baseline2%>%filter(性别=="女性",!is.na(CA125),!is.na(吸烟))
CEA<-baseline2%>%filter(性别=="女性",!is.na(CEA),!is.na(吸烟))
CA153<-baseline2%>%filter(性别=="女性",!is.na(CA153),!is.na(吸烟),!is.na(年龄),!is.na(就业状况))
baseline3<-baseline2%>%filter(性别=="女性",!is.na(吸烟),!is.na(年龄),!is.na(就业状况))

##基线描述
table1(~年龄连续+年龄+家庭收入+教育+婚姻+就业状况+血型+初潮年龄+初潮年龄分组+生育+生育次数+生育次数分组+初次生育年龄+首次生育年龄分组+哺乳+哺乳月份+人工流产+
         BMI+BMI_group+吸烟+被动吸烟+绝经+绝经年龄+绝经年龄分组+哺乳时间分组 ,data=CEA,render.categorical=my.render.cat)
table1(~年龄连续+年龄+家庭收入+教育+婚姻+就业状况+血型+初潮年龄+初潮年龄分组+生育+生育次数+生育次数分组+初次生育年龄+首次生育年龄分组+哺乳+哺乳月份+人工流产+
         BMI+BMI_group+吸烟+被动吸烟+绝经+绝经年龄+绝经年龄分组+哺乳时间分组 ,data=CA125,render.categorical=my.render.cat)
table1(~年龄连续+年龄+家庭收入+教育+婚姻+就业状况+血型+初潮年龄+初潮年龄分组+生育+生育次数+生育次数分组+初次生育年龄+首次生育年龄分组+哺乳+哺乳月份+人工流产+
         BMI+BMI_group+吸烟+被动吸烟+绝经+绝经年龄+绝经年龄分组+哺乳时间分组 ,data=CA153,render.categorical=my.render.cat)
table1(~年龄连续+年龄+家庭收入+教育+婚姻+就业状况+血型+初潮年龄+初潮年龄分组+生育+生育次数+生育次数分组+初次生育年龄+首次生育年龄分组+哺乳+哺乳月份+人工流产+
         BMI+BMI_group+吸烟+被动吸烟+绝经+绝经年龄+绝经年龄分组+哺乳时间分组 ,data=baseline3,render.categorical=my.render.cat)

with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
with(CA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
#plot1
table<-baseline3%>%
  pivot_longer(cols=c('婚姻','教育','就业状况','家庭收入','血型',"BMI_group","吸烟",'被动吸烟'),names_to='variables',values_to='levels')%>%
  filter(!is.na(levels))%>%group_by(variables,levels)%>%summarise(n=n())%>%group_by(variables)%>%mutate(percent=round(n/sum(n),2))

facet(ggbarplot(data=table,x='levels',y='percent',label = 'percent',lab.pos = 'out',fill='levels',xlab = '',ylab='百分比',x.text.angle=50),
             facet.by = 'variables',scales='free',panel.labs=list(variables=c('BMI','吸烟','婚姻','家庭收入','就业状况',"教育","血型",'被动吸烟')),
             nrow=2)+theme(legend.position = 'none')+scale_y_continuous(limits=c(0,1))

#plot2
table<-baseline3%>%
  pivot_longer(cols=c('初潮年龄分组','生育','生育次数分组','首次生育年龄分组','哺乳','人工流产','绝经',"绝经年龄分组"),names_to='variables',values_to='levels')%>%
  filter(!is.na(levels))%>%group_by(variables,levels)%>%summarise(n=n())%>%group_by(variables)%>%mutate(percent=round(n/sum(n),2))

facet(ggbarplot(data=table,x='levels',y='percent',label = 'percent',lab.pos = 'out',fill='levels',xlab = '',ylab='百分比',x.text.angle=50),
      facet.by = 'variables',scales='free', nrow=2)+theme(legend.position = 'none')+scale_y_continuous(limits=c(0,1))


##CEA
#基线情况
means_CEA<-CEA%>%pivot_longer(cols=variables2,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables2){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~年龄+家庭收入+教育+婚姻+就业状况+职业类型+血型 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables2){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables2){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)



#女性生理生育因素
means_CEA<-CEA%>%pivot_longer(cols=variables3,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables3){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~初潮年龄分组+生育+首次生育年龄分组+生育次数分组+哺乳时间分组+流产+人工流产+绝经+绝经年龄分组 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables3){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables3){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)
#矫正
logit(x=c('初潮年龄分组',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('生育',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('首次生育年龄分组',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('生育次数分组',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('哺乳时间分组',"年龄"),y='CEA_pos',data=CEA)
forest_model(glm(CEA_pos~哺乳时间分组+年龄,family='binomial',data=CEA))
logit(x=c('流产',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('人工流产',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('绝经',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('绝经年龄分组',"年龄"),y='CEA_pos',data=CEA)

##矫正2
logit(x=c('初潮年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('首次生育年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育次数分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('哺乳时间分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('人工流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('绝经',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('绝经年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)


####吸烟情况
#CEA
CEA%>%group_by(吸烟)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
CEA%>%group_by(被动吸烟)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
kruskal.test(CEA~吸烟,data=CEA)
kruskal.test(CEA~被动吸烟,data=CEA)
table1(~吸烟+被动吸烟 | CEA_pos,data=CEA,render.categorical=my.render.cat)
p1<-list()
for(i in c('吸烟','被动吸烟')){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,round(prop.table(table(y,CEA_pos),margin = 1)*100,2))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('吸烟','被动吸烟')){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)
#plot
CEA%>%transmute(CEA=log(CEA),吸烟=吸烟)%>%ggboxplot(x='吸烟',y='CEA',color='吸烟')+theme(legend.position = 'none')+labs(y='log(CEA)')+
  stat_compare_means(comparisons = list(c(1,2),c(1,3),c(2,3)),label='p.signif')+stat_compare_means(label.y=10)



#CA125
CA125%>%group_by(吸烟)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
CA125%>%group_by(被动吸烟)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
kruskal.test(CA125~吸烟,data=CA125)
kruskal.test(CA125~被动吸烟,data=CA125)
table1(~吸烟+被动吸烟 | CA125_pos,data=CA125,render.categorical=my.render.cat)
p1<-list()
for(i in c('吸烟','被动吸烟')){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,round(prop.table(table(y,CA125_pos),margin = 1)*100,2))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('吸烟','被动吸烟')){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)



#CA153
CA153%>%group_by(吸烟)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
CA153%>%group_by(被动吸烟)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
kruskal.test(CA153~吸烟,data=CA153)
kruskal.test(CA153~被动吸烟,data=CA153)
table1(~吸烟+被动吸烟 | CA153_pos,data=CA153,render.categorical=my.render.cat)
p1<-list()
for(i in c('吸烟','被动吸烟')){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,round(prop.table(table(y,CA153_pos),margin = 1)*100,2))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('吸烟','被动吸烟')){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)

###########CA125

#女性生理生育因素分布特征比较

means_CA125<-CA125%>%pivot_longer(cols=variables3,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables3){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~初潮年龄分组+生育+首次生育年龄分组+生育次数分组+哺乳时间分组+流产+人工流产+绝经+绝经年龄分组 | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
with(CA125,round(prop.table(table(人工流产,CA125_pos),margin = 1)*100,2))
p1<-list()
for(i in variables3){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables3){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)
#矫正
logit(x=c('初潮年龄分组',"年龄"),y='CA125_pos',data=CA125)
logit(x=c('生育',"年龄"),y='CA125_pos',data=CA125)
logit(x=c('首次生育年龄分组',"年龄"),y='CA125_pos',data=CA125)
logit(x=c('生育次数分组',"年龄"),y='CA125_pos',data=CA125)
logit(x=c('哺乳时间分组',"年龄"),y='CA125_pos',data=CA125)
#forest_model(glm(CA125_pos~哺乳时间分组+年龄,family='binomial',data=CA125))
logit(x=c('流产',"年龄"),y='CA125_pos',data=CA125)
logit(x=c('人工流产',"年龄"),y='CA125_pos',data=CA125)
logit(x=c('绝经',"年龄"),y='CA125_pos',data=CA125)
logit(x=c('绝经年龄分组',"年龄"),y='CA125_pos',data=CA125)
##矫正2
logit(x=c('初潮年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('首次生育年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育次数分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('哺乳时间分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('人工流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('绝经年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)


##CA153

#女性生理生育因素分布特征比较

means_CA153<-CA153%>%pivot_longer(cols=variables3,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables3){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~初潮年龄分组+生育+首次生育年龄分组+生育次数分组+哺乳时间分组+流产+人工流产+绝经+绝经年龄分组 | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
with(CA153,round(prop.table(table(人工流产,CA153_pos),margin = 1)*100,2))
p1<-list()
for(i in variables3){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables3){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)

#矫正
logit(x=c('初潮年龄分组',"年龄"),y='CA153_pos',data=CA153)
logit(x=c('生育',"年龄"),y='CA153_pos',data=CA153)
logit(x=c('首次生育年龄分组',"年龄"),y='CA153_pos',data=CA153)
logit(x=c('生育次数分组',"年龄"),y='CA153_pos',data=CA153)
logit(x=c('哺乳时间分组',"年龄"),y='CA153_pos',data=CA153)
#forest_model(glm(CA125_pos~哺乳时间分组+年龄,family='binomial',data=CA125))
logit(x=c('流产',"年龄"),y='CA153_pos',data=CA153)
logit(x=c('人工流产',"年龄"),y='CA153_pos',data=CA153)
logit(x=c('绝经',"年龄"),y='CA153_pos',data=CA153)
logit(x=c('绝经年龄分组',"年龄"),y='CA153_pos',data=CA153)
##矫正2
logit(x=c('初潮年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('首次生育年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育次数分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('哺乳时间分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('人工流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('绝经',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('绝经年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)


##绝经状态分层下，女性人群特征的分布
table1(~初潮年龄分组+生育+首次生育年龄分组+生育次数分组+哺乳时间分组+流产+人工流产 | CA153_pos, data=subset(CA153,绝经=="否"),render.categorical=my.render.cat)
CA153_1<-CA153%>%filter(绝经=="否")
CA153_2<-CA153%>%filter(绝经=="是")
CA125_1<-CA125%>%filter(绝经=="否")
CA125_2<-CA125%>%filter(绝经=="是")
p1<-list()
for(i in variables3){
  y<-CA125_2[[i]]
  p1[[i]]<-with(CA125_2,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables3){
  y<-CA125_2[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125_2$CA125_pos))$p.value,3)
}
do.call(rbind,p2)



##BMI
#CEA
CEA%>%group_by(BMI_group)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
kruskal.test(CEA~BMI_group,data=CEA)
table1(~BMI_group | CEA_pos,data=CEA,render.categorical=my.render.cat)
with(CEA,round(prop.table(table(BMI_group,CEA_pos),margin = 1)*100,2))
round(chisq.test(table(CEA$BMI_group,CEA$CEA_pos))$p.value,3)
##CA125
CA125%>%group_by(BMI_group)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
kruskal.test(CA125~BMI_group,data=CA125)
table1(~BMI_group | CA125_pos,data=CA125,render.categorical=my.render.cat)
with(CA125,round(prop.table(table(BMI_group,CA125_pos),margin = 1)*100,2))
round(chisq.test(table(CA125$BMI_group,CA125$CA125_pos))$p.value,3)
#CA153
CA153%>%group_by(BMI_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
kruskal.test(CA153~BMI_group,data=CA153)
table1(~BMI_group | CA153_pos,data=CA153,render.categorical=my.render.cat)
with(CA153,round(prop.table(table(BMI_group,CA153_pos),margin = 1)*100,2))
round(chisq.test(table(CA153$BMI_group,CA153$CA153_pos))$p.value,3)
#plot
CEA%>%transmute(CEA=log(CEA),BMI=BMI_group)%>%ggboxplot(x='BMI',y='CEA',color='BMI')+theme(legend.position = 'none')+labs(y='log(CEA)')+
  stat_compare_means(comparisons = list(c(1,2),c(1,3),c(2,3)),label='p.signif')+stat_compare_means(label.y=10)
#CEA男女分层
CEA%>%group_by(性别,BMI_group)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
kruskal.test(CEA~BMI_group,data=CEA)



###饮食/饮酒/喝茶等因素

#CEA
means_CEA<-CEA%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~饮酒+喝茶+酸奶+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+菌类 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
with(CEA,round(prop.table(table(人工流产,CEA_pos),margin = 1)*100,2))
p1<-list()
for(i in variables4){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables4){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)


##CA125

means_CA125<-CA125%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~饮酒+喝茶+酸奶+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+菌类 | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
with(CA125,round(prop.table(table(人工流产,CA125_pos),margin = 1)*100,2))
p1<-list()
for(i in variables4){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables4){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)

##CA153

means_CA153<-CA153%>%pivot_longer(cols=variables4,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables4){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~饮酒+喝茶+酸奶+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+菌类 | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
with(CA153,round(prop.table(table(人工流产,CA153_pos),margin = 1)*100,2))
p1<-list()
for(i in variables4){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables4){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)


##身体活动的关联性分析

##CEA

means_CEA<-CEA%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+静态时间+手机使用时间 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables5){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables5){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)

##CA125

means_CA125<-CA125%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+静态时间+手机使用时间 | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables5){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables5){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)


###CA153  
means_CA153<-CA153%>%pivot_longer(cols=variables5,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables5){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+静态时间+手机使用时间 | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables5){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables5){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)



##负性生活事件


##CEA

means_CEA<-CEA%>%pivot_longer(cols=c('重大精神创伤','精神压抑'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in c('重大精神创伤','精神压抑')){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~重大精神创伤+精神压抑 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('重大精神创伤','精神压抑')){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('重大精神创伤','精神压抑')){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)


##CA125

means_CA125<-CA125%>%pivot_longer(cols=c('重大精神创伤','精神压抑'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in c('重大精神创伤','精神压抑')){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~重大精神创伤+精神压抑 | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('重大精神创伤','精神压抑')){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('重大精神创伤','精神压抑')){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)


###CA153  
means_CA153<-CA153%>%pivot_longer(cols=c('重大精神创伤','精神压抑'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in c('重大精神创伤','精神压抑')){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~重大精神创伤+精神压抑 | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('重大精神创伤','精神压抑')){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('重大精神创伤','精神压抑')){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)


###基础疾病史

##CEA

means_CEA<-CEA%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
wilcox.test(CEA~糖尿病,data=CEA)
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~糖尿病+高血压+高血脂+冠心病+中风 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables6){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables6){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)


##CA125

means_CA125<-CA125%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~糖尿病+高血压+高血脂+冠心病+中风 | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables6){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables6){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)


###CA153  
means_CA153<-CA153%>%pivot_longer(cols=variables6,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables6){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~糖尿病+高血压+高血脂+冠心病+中风 | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables6){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables6){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)


###妇科疾病史

##CEA

means_CEA<-CEA%>%pivot_longer(cols=c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗"),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~绝育手术+子宫摘除术+卵巢摘除术+口服避孕药+雌激素代替治疗 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#绝经状态分层
CEA%>%group_by(绝经,口服避孕药)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
CEA%>%group_by(绝经,雌激素代替治疗)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
wilcox.test(CEA~口服避孕药,data=subset(CEA,绝经=="否"))
#P值
p2<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)
#
means_CEA<-CEA%>%pivot_longer(cols=c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗"),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)



##CA125

means_CA125<-CA125%>%pivot_longer(cols=c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗"),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#绝经状态分层
#绝经状态分层
CA125%>%group_by(绝经,口服避孕药)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
CA125%>%group_by(绝经,雌激素代替治疗)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
wilcox.test(CA125~口服避孕药,data=subset(CA125,绝经=="否"))

#p值
p<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~绝育手术+子宫摘除术+卵巢摘除术+口服避孕药+雌激素代替治疗 | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)


###CA153  
means_CA153<-CA153%>%pivot_longer(cols=c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗"),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#绝经状态分层
CA153%>%group_by(绝经,口服避孕药)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
CA153%>%group_by(绝经,雌激素代替治疗)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
wilcox.test(CA153~口服避孕药,data=subset(CA153,绝经=="否"))

#p值
p<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~绝育手术+子宫摘除术+卵巢摘除术+口服避孕药+雌激素代替治疗 | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in c('绝育手术','子宫摘除术','卵巢摘除术',"口服避孕药","雌激素代替治疗")){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)


##乳腺特殊疾病史

means_CEA<-CEA%>%pivot_longer(cols=c('女性良性病变史','乳腺不典型增生'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
CEA%>%filter(Year<=2018)%>%group_by(乳腺纤维瘤)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
CEA%>%filter(Year==2019)%>%group_by(乳腺小叶原位癌)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))

#p值
p<-list()
for(i in c('女性良性病变史','乳腺不典型增生')){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
with(subset(CEA,Year<=2018),wilcox.test(CEA~乳腺纤维瘤))
with(subset(CEA,Year==2019),wilcox.test(CEA~乳腺小叶原位癌))



##CA125

means_CA125<-CA125%>%pivot_longer(cols=c('女性良性病变史','乳腺不典型增生'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
CA125%>%filter(Year<=2018)%>%group_by(乳腺纤维瘤)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
CA125%>%filter(Year==2019)%>%group_by(乳腺小叶原位癌)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))

#p值
p<-list()
for(i in c('女性良性病变史','乳腺不典型增生')){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
with(subset(CA125,Year<=2018),wilcox.test(CA125~乳腺纤维瘤))
with(subset(CA125,Year==2019),wilcox.test(CA125~乳腺小叶原位癌))



###CA153  
means_CA153<-CA153%>%pivot_longer(cols=c('女性良性病变史','乳腺不典型增生'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
CA153%>%filter(Year<=2018)%>%group_by(乳腺纤维瘤)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
CA153%>%filter(Year==2019)%>%group_by(乳腺小叶原位癌)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))

#p值
p<-list()
for(i in c('女性良性病变史','乳腺不典型增生')){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
with(subset(CA153,Year<=2018),wilcox.test(CA125~乳腺纤维瘤))
with(subset(CA153,Year==2019),wilcox.test(CA125~乳腺小叶原位癌))



####<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<特殊职业暴露>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

means_CEA<-CEA%>%pivot_longer(cols=variables7,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
CEA%>%filter(Year==2019)%>%group_by(苯)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))

#p值
p<-list()
for(i in variables7){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
with(subset(CEA,Year==2019),wilcox.test(CEA~苯))
#分类变量
table1(~镉+石棉+镍+砷+氡+氯乙烯+X射线 | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables7){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables7){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)


##CA125

means_CA125<-CA125%>%pivot_longer(cols=variables7,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
CA125%>%filter(Year==2019)%>%group_by(苯)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))

#p值
p<-list()
for(i in variables7){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
with(subset(CA125,Year==2019),wilcox.test(CA125~苯))
#分类变量
table1(~镉+石棉+镍+砷+氡+氯乙烯+X射线 | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables7){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables7){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)


###CA153  
means_CA153<-CA153%>%pivot_longer(cols=variables7,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
CA153%>%filter(Year==2019)%>%group_by(苯)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))

#p值
p<-list()
for(i in variables7){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
with(subset(CA153,Year==2019),wilcox.test(CA153~苯))
#分类变量
table1(~镉+石棉+镍+砷+氡+氯乙烯+X射线 | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables7){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables7){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)



####<<<<<<<<<<<<<<<<乳腺超声影像学特征>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
table1(~乳腺超声描述+肿块+钙化+周围组织异常+淋巴结肿大+乳腺组织构成+BIRADS, data=baseline3,render.categorical=my.render.cat)


##CEA

means_CEA<-CEA%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~左乳超声描述+左乳肿块+左乳钙化+左乳周围组织异常+左乳淋巴结肿大+
       右乳超声描述+右乳肿块+右乳钙化+右乳周围组织异常+右乳淋巴结肿大+
       乳腺超声描述+肿块+钙化+周围组织异常+淋巴结肿大+乳腺组织构成+BIRADS | CEA_pos, data=CEA,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables8){
  y<-CEA[[i]]
  p1[[i]]<-with(CEA,as.data.frame(round(prop.table(table(y,CEA_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables8){
  y<-CEA[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)
#绝经状态分层下
means_CEA<-CEA%>%filter(绝经=="是")%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[which(CEA$绝经=="是"),i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(CEA,绝经=="是"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(CEA,绝经=="是"))$p.value,4)
  }
}
do.call(rbind,p)

##CA125

means_CA125<-CA125%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~左乳超声描述+左乳肿块+左乳钙化+左乳周围组织异常+左乳淋巴结肿大+
         右乳超声描述+右乳肿块+右乳钙化+右乳周围组织异常+右乳淋巴结肿大+
         乳腺超声描述+肿块+钙化+周围组织异常+淋巴结肿大+乳腺组织构成+BIRADS | CA125_pos, data=CA125,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables8){
  y<-CA125[[i]]
  p1[[i]]<-with(CA125,as.data.frame(round(prop.table(table(y,CA125_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables8){
  y<-CA125[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA125$CA125_pos))$p.value,3)
}
do.call(rbind,p2)
##绝经状态分层
means_CA125<-CA125%>%filter(绝经=="否")%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[which(CA125$绝经=="否"),i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(CA125,绝经=="否"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(CA125,绝经=="否"))$p.value,4)
  }
}
do.call(rbind,p)

###CA153  
means_CA153<-CA153%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#分类变量
table1(~左乳超声描述+左乳肿块+左乳钙化+左乳周围组织异常+左乳淋巴结肿大+
         右乳超声描述+右乳肿块+右乳钙化+右乳周围组织异常+右乳淋巴结肿大+
         乳腺超声描述+肿块+钙化+周围组织异常+淋巴结肿大+乳腺组织构成+BIRADS | CA153_pos, data=CA153,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables8){
  y<-CA153[[i]]
  p1[[i]]<-with(CA153,as.data.frame(round(prop.table(table(y,CA153_pos),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables8){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)
#绝经状态分层
means_CA153<-CA153%>%filter(绝经=="否")%>%pivot_longer(cols=variables8,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables8){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[which(CA153$绝经=="否"),i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(CA153,绝经=="否"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(CA153,绝经=="否"))$p.value,4)
  }
}
do.call(rbind,p)
##<<<<<<<<<<<<<<<<女性生理生育因素与乳腺超声影像特征的关联性分析>>>>>>>>>>>>>>>>>>>
#肿块
table1(~初潮年龄+初潮年龄分组+生育+初次生育年龄+首次生育年龄分组+生育次数+生育次数分组+哺乳月份+哺乳时间分组+流产+人工流产+绝经+绝经年龄+绝经年龄分组 | 肿块, data=baseline2,render.categorical=my.render.cat)
#频数
p1<-list()
for(i in variables3){
  y<-baseline2[[i]]
  p1[[i]]<-with(baseline2,as.data.frame(round(prop.table(table(y,肿块),margin = 1)*100,2)))
}
do.call(rbind,p1)
#P值
p2<-list()
for(i in variables3){
  y<-baseline2[[i]]
  p2[[i]]<-round(chisq.test(table(y,baseline2$肿块))$p.value,3)
}
do.call(rbind,p2)
#
p2<-list()
for(i in variables2){
  y<-baseline2[[i]]
  p2[[i]]<-round(chisq.test(table(y,baseline2$肿块))$p.value,3)
}
do.call(rbind,p2)
#
chisq.test(table(baseline$BMI_group,baseline2$肿块))
chisq.test(table(baseline$吸烟,baseline2$肿块))
chisq.test(table(baseline$被动吸烟,baseline2$肿块))
###矫正
#矫正
logit(x=c('初潮年龄分组',"年龄"),y='肿块',data=baseline2)
logit(x=c('生育',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('首次生育年龄分组',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('生育次数分组',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('哺乳时间分组',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('流产',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('人工流产',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('绝经',"年龄"),y='CEA_pos',data=CEA)
logit(x=c('绝经年龄分组',"年龄"),y='CEA_pos',data=CEA)

##矫正2
logit(x=c('初潮年龄分组',"年龄",'教育','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('首次生育年龄分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('生育次数分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('哺乳时间分组',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('人工流产',"年龄",'教育','就业状况','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('绝经',"年龄",'教育','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)
logit(x=c('绝经年龄分组',"年龄",'教育','血型','吸烟','被动吸烟','BMI'),y='CEA_pos',data=CEA)

##初潮年龄与肿块率
baseline3%>%transmute(初潮年龄=case_when(
  初潮年龄<=12 ~ 1,
  初潮年龄==13 ~ 2,
  初潮年龄==14 ~ 3,
  初潮年龄==15 ~ 4,
  初潮年龄==16 ~ 5,
  初潮年龄==17 ~ 6,
  初潮年龄>=18 ~ 7,
  
),初潮年龄=factor(初潮年龄,levels=c(1,2,3,4,5,6,7),labels = c('<=12','13','14','15','16','17','>=18')),肿块
)%>%group_by(初潮年龄,肿块)%>%summarise(n=n())%>%filter(!is.na(肿块),!is.na(初潮年龄))%>%group_by(初潮年龄)%>%
  mutate(percent=round(n/sum(n),4)*100)%>%filter(肿块=="有")%>%ggbarplot(x='初潮年龄',y='percent',label="percent",lab.pos = "out")+labs(y="肿块率(%)")
#

####<<<<<<<<<<<<不同肿瘤标志物用于肿瘤初筛的效果评价>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
#
baseline3%>%filter(绝经=="否")%>%group_by(CA_type_breast)%>%summarise(n=n(),median=median(CA125,na.rm=TRUE))
baseline3%>%filter(绝经=="否")%>%group_by(CA_type_breast)%>%summarise(n=n(),median=median(CEA,na.rm=TRUE))
baseline3%>%filter(绝经=="否")%>%group_by(CA_type_breast)%>%summarise(n=n(),median=median(CA153,na.rm=TRUE))

##
with(subset(baseline,CA_type_breast==0),summary(CEA));with(subset(baseline,CA_type_breast==1),summary(CEA))
with(subset(baseline,CA_type_breast==0),summary(CA125));with(subset(baseline,CA_type_breast==1),summary(CA125))
with(subset(baseline,CA_type_breast==0),summary(CA153));with(subset(baseline,CA_type_breast==1),summary(CA153))
wilcox.test(CEA~CA_type_breast,data=baseline)
wilcox.test(CA125~CA_type_breast,data=baseline)
wilcox.test(CA153~CA_type_breast,data=baseline)

#plot
CEA.roc<-roc(baseline3$CA_type_breast, baseline3$CEA,direction='<',legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3$CA_type_breast, baseline3$CA125,direction="<",col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3$CA_type_breast, baseline3$CA153,direction="<",col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
#
plot.roc(baseline3$CA_type_breast, baseline3$CEA,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(baseline3$CA_type_breast, baseline3$CA125,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(baseline3$CA_type_breast, baseline3$CA153,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)


plot.roc(baseline3$CA_type_breast, baseline3$CEA,direction='<',percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="2")
#lines.roc(match2$type2, match2$PG2,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="3")
lines.roc(baseline3$CA_type_breast, baseline3$CA125,direction="<",percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="4")
lines.roc(baseline3$CA_type_breast, baseline3$CA153,direction="<",percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="5")
legend("bottomright",legend=c("CEA, AUC: 0.48(0.44,0.52)", 
                              "CA125, AUC: 0.46(0.40,0.53)",
                              "CA153, AUC: 0.51(0.44,0.56)"
),
col=c("2",'4',"5"),lwd=3,cex=0.6)
#阳性预测值与阴性预测值计算
table<-baseline3%>%transmute(CEA=ifelse(CEA>4.395,1,0),
                           CA125=ifelse(CA125>17.325,1,0),
                           CA153=ifelse(CA153>17.415,1,0),
                          type=CA_type_breast)
table1<-baseline3%>%filter(绝经=="是")%>%transmute(CEA=ifelse(CEA>4.49,1,0),
                             CA125=ifelse(CA125>3.745,1,0),
                             CA153=ifelse(CA153>13.255,1,0),
                             type=CA_type_breast)
table2<-baseline3%>%filter(绝经=="否")%>%transmute(CEA=ifelse(CEA>0.575,1,0),
                              CA125=ifelse(CA125>18.395,1,0),
                              CA153=ifelse(CA153>16.955,1,0),
                              type=CA_type_breast)


#CEA
table(table$CEA,table$type)
epi.tests(as.table(matrix(c(17,447,201,7975), nrow = 2, byrow = TRUE)))
#绝经后
table(table1$CEA,table1$type)
epi.tests(as.table(matrix(c(15,385,156,6239), nrow = 2, byrow = TRUE)))
#绝经前
table(table2$CEA,table2$type)
epi.tests(as.table(matrix(c(46,1673,1,122), nrow = 2, byrow = TRUE)))

#CA125
table(table$CA125,table$type)
epi.tests(as.table(matrix(c(9,507,64,4447), nrow = 2, byrow = TRUE)))
#绝经后
table(table1$CA125,table1$type)
epi.tests(as.table(matrix(c(60,3942,0,53), nrow = 2, byrow = TRUE)))
#绝经前
table(table2$CA125,table2$type)
epi.tests(as.table(matrix(c(6,240,7,718), nrow = 2, byrow = TRUE)))
#CA153
table(table$CA153,table$type)
epi.tests(as.table(matrix(c(30,1600,88,7914), nrow = 2, byrow = TRUE)))
#绝经后
table(table1$CA153,table1$type)
epi.tests(as.table(matrix(c(38,2506,56,5355), nrow = 2, byrow = TRUE)))
#绝经前
table(table2$CA153,table2$type)
epi.tests(as.table(matrix(c(7,221,17,1429), nrow = 2, byrow = TRUE)))
##AUC的P值
#plot(绝经分层)
par(mfrow = c(3,3))
plot.roc(baseline3$CA_type_breast, baseline3$CA125,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="全部女性")
#CA125
roc(baseline3[which(baseline3$绝经=="是"),"CA_type_breast"], baseline3[which(baseline3$绝经=="是"),"CA125"],direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3[which(baseline3$绝经=="否"),"CA_type_breast"], baseline3[which(baseline3$绝经=="否"),"CA125"],direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)

plot.roc(baseline3[which(baseline3$绝经=="是"),"CA_type_breast"], baseline3[which(baseline3$绝经=="是"),"CA125"],direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="绝经后女性")
plot.roc(baseline3[which(baseline3$绝经=="否"),"CA_type_breast"], baseline3[which(baseline3$绝经=="否"),"CA125"],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="绝经前女性")
##
par(mfrow = c(1,3))
plot.roc(baseline3$CA_type_breast, baseline3$CA125,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="全部女性")
plot.roc(baseline3[which(baseline3$绝经=="是"),'CA_type_breast'],baseline3[which(baseline3$绝经=="是"),'CA125'],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="已绝经女性")
plot.roc(baseline3[which(baseline3$绝经=="否"),'CA_type_breast'],baseline3[which(baseline3$绝经=="否"),'CA125'],direction="<",add=F,legacy.axes=T,las=1,col="3", print.auc=T,print.thres=T,main='未绝经女性')


#CEA
roc(baseline3[which(baseline3$绝经=="是"),"CA_type_breast"], baseline3[which(baseline3$绝经=="是"),"CEA"],direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3[which(baseline3$绝经=="否"),"CA_type_breast"], baseline3[which(baseline3$绝经=="否"),"CEA"],direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)

plot.roc(baseline3$CA_type_breast, baseline3$CEA,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="全部女性")
plot.roc(baseline3[which(baseline3$绝经=="是"),"CA_type_breast"], baseline3[which(baseline3$绝经=="是"),"CEA"],direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="绝经后女性")
plot.roc(baseline3[which(baseline3$绝经=="否"),"CA_type_breast"], baseline3[which(baseline3$绝经=="否"),"CEA"],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="绝经前女性")
#CA153
roc(baseline3[which(baseline3$绝经=="是"),"CA_type_breast"], baseline3[which(baseline3$绝经=="是"),"CA153"],direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3[which(baseline3$绝经=="否"),"CA_type_breast"], baseline3[which(baseline3$绝经=="否"),"CA153"],direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(baseline3$CA_type_breast, baseline3$CA153,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="全部女性")
plot.roc(baseline3[which(baseline3$绝经=="是"),"CA_type_breast"], baseline3[which(baseline3$绝经=="是"),"CA153"],direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="绝经后女性")
plot.roc(baseline3[which(baseline3$绝经=="否"),"CA_type_breast"], baseline3[which(baseline3$绝经=="否"),"CA153"],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main="绝经前女性")





#联合初筛
roc(baseline3$CA_type_breast, baseline3$CA125+baseline3$CA153,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3$CA_type_breast, baseline3$CEA+baseline3$CA153,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3$CA_type_breast, baseline3$CEA+baseline3$CA125,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(baseline3$CA_type_breast,baseline3$CA125+baseline3$CA153+baseline3$CEA,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
par(mfrow = c(2,2))
plot.roc(baseline3$CA_type_breast,baseline3$CA125+baseline3$CA153,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3$CA_type_breast,baseline3$CEA+baseline3$CA153,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CEA联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3$CA_type_breast,baseline3$CEA+baseline3$CA125,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA153+CEA联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3$CA_type_breast,baseline3$CA125+baseline3$CA153+baseline3$CEA,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153+CEA联合诊断BI-RADS及乳腺癌')

##CA125+CA153
CA125_CA153<-glm(CA_type_breast~CA125+CA153,data=baseline3,family="binomial")
baseline3$CA125_CA153_prob<-predict(CA125_CA153,baseline3,type="response")
roc(baseline3$CA_type_breast,baseline3$CA125_CA153_prob,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(baseline3$CA_type_breast,baseline3$CA125_CA153_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153联合诊断BI-RADS及乳腺癌')
#CA153+CEA
CEA_CA153<-glm(CA_type_breast~CA153+CEA,data=baseline3,family="binomial")
baseline3$CEA_CA153_prob<-predict(CEA_CA153,baseline3,type="response")
roc(baseline3$CA_type_breast,baseline3$CEA_CA153_prob,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(baseline3$CA_type_breast,baseline3$CEA_CA153_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153联合诊断BI-RADS及乳腺癌')
#CA125+CEA
CA125_CEA<-glm(CA_type_breast~CA125+CEA,data=baseline3,family="binomial")
baseline3$CA125_CEA_prob<-predict(CA125_CEA,baseline3,type="response")
roc(baseline3$CA_type_breast,baseline3$CA125_CEA_prob,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(baseline3$CA_type_breast,baseline3$CA125_CEA_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153联合诊断BI-RADS及乳腺癌')
#CA125+CA153+CEA
CA125_CA153_CEA<-glm(CA_type_breast~CA125+CEA+CA153,data=baseline3,family="binomial")
baseline3$CA125_CA153_CEA_prob<-predict(CA125_CA153_CEA,baseline3,type="response")
roc(baseline3$CA_type_breast,baseline3$CA125_CA153_CEA_prob,direction='<',col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(baseline3$CA_type_breast,baseline3$CA125_CA153_CEA_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153+CEA联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3[which(baseline3$绝经=="是"),'CA_type_breast'],baseline3[which(baseline3$绝经=="是"),'CA125_CA153_CEA_prob'],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153+CEA联合诊断BI-RADS及乳腺癌(绝经后)')
plot.roc(baseline3[which(baseline3$绝经=="否"),'CA_type_breast'],baseline3[which(baseline3$绝经=="否"),'CA125_CA153_CEA_prob'],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153+CEA联合诊断BI-RADS及乳腺癌(绝经前)')



##
par(mfrow = c(2,2))
plot.roc(baseline3$CA_type_breast,baseline3$CA125_CA153_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3$CA_type_breast,baseline3$CEA_CA153_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CEA+CA153联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3$CA_type_breast,baseline3$CA125_CEA_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CEA+CA125联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3$CA_type_breast,baseline3$CA125_CA153_CEA_prob,direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153+CEA联合诊断BI-RADS及乳腺癌')
#绝经状态分层
par(mfrow = c(4,2))
plot.roc(baseline3[which(baseline3$绝经=="是"),'CA_type_breast'],baseline3[which(baseline3$绝经=="是"),'CA125_CEA_prob'],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CEA联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3[which(baseline3$绝经=="否"),'CA_type_breast'],baseline3[which(baseline3$绝经=="否"),'CA125_CEA_prob'],direction="<",add=F,legacy.axes=T,las=1,col="3", print.auc=T,print.thres=T,main='CA125+CEA联合诊断BI-RADS及乳腺癌')
#
plot.roc(baseline3[which(baseline3$绝经=="是"),'CA_type_breast'],baseline3[which(baseline3$绝经=="是"),'CEA_CA153_prob'],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA153+CEA联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3[which(baseline3$绝经=="否"),'CA_type_breast'],baseline3[which(baseline3$绝经=="否"),'CEA_CA153_prob'],direction="<",add=F,legacy.axes=T,las=1,col="3", print.auc=T,print.thres=T,main='CA153+CEA联合诊断BI-RADS及乳腺癌')
#
plot.roc(baseline3[which(baseline3$绝经=="是"),'CA_type_breast'],baseline3[which(baseline3$绝经=="是"),'CA125_CA153_prob'],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3[which(baseline3$绝经=="否"),'CA_type_breast'],baseline3[which(baseline3$绝经=="否"),'CA125_CA153_prob'],direction="<",add=F,legacy.axes=T,las=1,col="3", print.auc=T,print.thres=T,main='CA125+CA153联合诊断BI-RADS及乳腺癌')
#
plot.roc(baseline3[which(baseline3$绝经=="是"),'CA_type_breast'],baseline3[which(baseline3$绝经=="是"),'CA125_CA153_CEA_prob'],direction="<",add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T,main='CA125+CA153+CEA联合诊断BI-RADS及乳腺癌')
plot.roc(baseline3[which(baseline3$绝经=="否"),'CA_type_breast'],baseline3[which(baseline3$绝经=="否"),'CA125_CA153_CEA_prob'],direction="<",add=F,legacy.axes=T,las=1,col="3", print.auc=T,print.thres=T,main='CA125+CA153+CEA联合诊断BI-RADS及乳腺癌')
#
plot.roc(baseline3$CA_type_breast,baseline3$CA125_CA153_prob,direction="<",percent=F,reuse.auc=TRUE,axes=TRUE, legacy.axes=T,col="2",main='女性肿瘤标志物联合诊断BI-RADS及乳腺癌')
lines.roc(baseline3$CA_type_breast,baseline3$CEA_CA153_prob,direction="<",reuse.auc=TRUE,axes=TRUE, legacy.axes=T,col="3")
lines.roc(baseline3$CA_type_breast,baseline3$CA125_CEA_prob,direction="<",reuse.auc=TRUE,axes=TRUE, legacy.axes=T,col="4",)
lines.roc(baseline3$CA_type_breast,baseline3$CA125_CA153_CEA_prob,direction="<",reuse.auc=TRUE,axes=TRUE, legacy.axes=T,col="5")
legend("bottomright",legend=c("CA125+CA153, AUC: 0.53(0.46,0.61)", 
                              "CEA+CA153, AUC: 0.53(0.46,0.60)",
                              "CEA+CA125, AUC: 0.53(0.47,0.60)",
                              "CEA+CA125+CEA, AUC: 0.53(0.46,0.61)"),col=c("2",'3','4',"5"),lwd=3,cex=0.6)

###














##<<<<<<<<<<<<<<<女性肿瘤标志物与生理生育因素的关联研究>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CA125与初潮年龄
CA125%>%transmute(初潮年龄,CA125=log(CA125))%>%filter(!is.na(初潮年龄))%>%
ggboxplot(x='初潮年龄',y="CA125",add='jitter')
##
CA125%>%transmute(初次生育年龄,CA125=log(CA125))%>%filter(!is.na(初次生育年龄))%>%
  ggboxplot(x='初次生育年龄',y="CA125",add='jitter')
#

CA125%>%transmute(绝经年龄,CA125=log(CA125))%>%filter(!is.na(绝经年龄))%>%
  ggboxplot(x='绝经年龄',y="CA125",add='jitter')

#######分层分析
summary(glm(CA125~初潮年龄,data=subset(CA125,肿块=="有")))
summary(glm(CA125~初潮年龄,data=subset(CA125,肿块=="无")))



#########<<<<<<女性生理生育因素时序性与肿瘤标志物的关联性分析>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


##初潮年龄+CEA+BMI+
corrplot(method='number',type='upper',
  cor(baseline2[,c('CEA','CA125','CA153','初潮年龄','BMI',"绝经年龄","哺乳月份","初次生育年龄")],
      use='na.or.complete',method="spearman"))

lm(BMI)

##########<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<基线特征描述作图>>>>>>>>>>>>>>>>>>>>..
#血型
ggplot(data=baseline3,aes(x=血型,y=log(CA125),fill=血型))+geom_boxplot()
baseline3%>%transmute(CEA=log(CEA),CA125=log(CA125),CA153=log(CA153),血型=血型)%>%pivot_longer(
  cols=c('CEA','CA125','CA153'),names_to = 'marker',values_to = 'value'
)%>%filter(!is.na(血型))%>%ggplot(aes(x=血型,y=value,fill=血型))+geom_boxplot()+facet_grid(.~marker,scales = 'free_y')+mytheme+border()
#年龄
age_marker<-baseline3%>%transmute(CEA=log(CEA),CA125=log(CA125),CA153=log(CA153),年龄)%>%
  pivot_longer(cols=c('CEA','CA125','CA153'),names_to="肿瘤标志物",values_to = "value")
 
facet(ggboxplot(data=age_marker,x='年龄',y='value',add="jitter",color='年龄')+stat_compare_means(),
      facet.by = '肿瘤标志物',scales='free',nrow=1)+theme(legend.position = 'none')
#吸烟、被动吸烟、BMI与CEA
ggboxplot(data=CEA,x='吸烟',y='CEA')+stat_compare_means()+stat_compare_means(label='p.signif')
##女性生理生育因素
median2<-CA125%>%group_by(首次生育年龄分组)%>%summarise(median=median(log(CA125)))
median3<-CA125%>%group_by(绝经)%>%summarise(median=median(log(CA125)))

a1<-plot_ly(
  subset(CA125,!is.na(生育)),
  x = ~ 生育,
  y = ~ log(CA125),
  type = "box",
  showlegend = FALSE
) %>% add_lines(data = median1, x = ~ 生育, y = ~median ) %>%
  layout(yaxis = list(title = 'log(CA125)'))
a2<-plot_ly(
  subset(CA125,!is.na(首次生育年龄分组)),
  x = ~ 首次生育年龄分组,
  y = ~ log(CA125),
  type = "box",
  showlegend = FALSE
) %>% add_lines(data = median2, x = ~ 首次生育年龄分组, y = ~median ) %>%
  layout(yaxis = list(title = 'log(CA125)'))

a3<-plot_ly(
  subset(CA125,!is.na(绝经)),
  x = ~ 绝经,
  y = ~ log(CA125),
  type = "box",
  showlegend = FALSE
) %>% add_lines(data = median3, x = ~ 绝经, y = ~median ) %>%
  layout(yaxis = list(title = 'log(CA125)'))

### 
surgery<-baseline3%>%transmute(绝育手术,子宫摘除术,卵巢摘除术,CEA=log(CEA),CA125=log(CA125),CA153=log(CA153))%>%
  pivot_longer(cols=c('CEA','CA125','CA153'),names_to='marker',values_to = 'value')
surgery_median1<-surgery%>%group_by(绝育手术,marker)%>%summarise(median=median(value,na.rm=TRUE))
plot_ly(surgery, x = ~marker, y = ~value, color = ~绝育手术, type = "box") %>% layout(boxmode = "group")
plot_ly(surgery, x = ~marker, y = ~value, color = ~子宫摘除术, type = "box") %>% layout(boxmode = "group")
plot_ly(surgery, x = ~marker, y = ~value, color = ~卵巢摘除术, type = "box") %>% layout(boxmode = "group")
###特殊药物的使用
CA125_median<-CA125%>%transmute(CA125=log(CA125),口服避孕药,雌激素代替治疗,绝经)%>%pivot_longer(cols=c('口服避孕药','雌激素代替治疗'),names_to = '药物',values_to = 'levels')%>%filter(!is.na(药物) & !is.na(levels))%>%
  group_by(绝经,药物,levels)%>%summarise(median=median(CA125))
CA125%>%transmute(CA125=log(CA125),口服避孕药,雌激素代替治疗,绝经)%>%pivot_longer(cols=c('口服避孕药','雌激素代替治疗'),names_to = '药物',values_to = 'levels')%>%filter(!is.na(药物) & !is.na(levels))%>%
  ggboxplot(x='levels',y='CA125',color='levels')+facet_grid(药物~绝经)+border()+stat_compare_means(label.x=0.7)+theme(legend.position = 'none')+labs(x='药物使用情况',y='log(CA125)')
CA153%>%transmute(CA153=log(CA153),口服避孕药,绝经)%>%filter(!is.na(口服避孕药) & !is.na(绝经))%>%
  ggboxplot(x='口服避孕药',y='CA153',color='口服避孕药')+facet_grid(.~绝经)+border()+stat_compare_means(label.x=0.7)+theme(legend.position = 'none')+labs(x='口服避孕药',y='log(CA153)')
  

##肿瘤标志物与初潮年龄，初次生育年龄、哺乳月份、生育次数的趋势性分析
#
with(baseline3,table(出生年份))#1945-1979
with(baseline3,table(出生年份,is.na(CA125)))#1945-1978,CA125
with(baseline3,table(出生年份,is.na(CA153)))#1945-1978,CA125
with(baseline3,table(出生年份,is.na(CEA)))#1945-1979,CA125

#初潮年龄、肿瘤标志物与出生年份
par(mfrow = c(1,1))
menarch_mean<-baseline3%>%group_by(出生年份)%>%summarise(初潮年龄=mean(初潮年龄,na.rm=TRUE),CA125=mean(log(CA125),na.rm=TRUE))%>%filter(出生年份<=1975,出生年份>=1945,!is.na(CA125),!is.na(初潮年龄))
menarch_mean1<-baseline3%>%group_by(出生年份)%>%summarise(初潮年龄=mean(初潮年龄,na.rm=TRUE),CEA=mean(log(CEA),na.rm=TRUE))%>%filter(出生年份<=1975,出生年份>=1945,!is.na(CEA),!is.na(初潮年龄))
menarch_mean2<-baseline3%>%filter(CA153!=0)%>%group_by(出生年份)%>%summarise(初潮年龄=mean(初潮年龄,na.rm=TRUE),CA153=mean(log(CA153),na.rm=TRUE))%>%filter(出生年份<=1970,出生年份>=1945,!is.na(CA153),!is.na(初潮年龄))

library(plotrix)
par(mfrow = c(1,3))
twoord.plot(lx=menarch_mean$出生年份, ly=menarch_mean$初潮年龄,rx=menarch_mean$出生年份,ry=menarch_mean$CA125,
            main='CA125、初潮年龄与出生年份的趋势分析',xlab='出生年份', ylab='初潮年龄(mean)', rylab='log(CA125)(mean)', type=c('line','line'))
twoord.plot(lx=menarch_mean1$出生年份, ly=menarch_mean1$初潮年龄,rx=menarch_mean1$出生年份,ry=menarch_mean1$CEA,
            main='CEA、初潮年龄与出生年份的趋势分析',xlab='出生年份', ylab='初潮年龄(mean)', rylab='log(CEA)(mean)', type=c('line','line'))
twoord.plot(lx=menarch_mean2$出生年份, ly=menarch_mean2$初潮年龄,rx=menarch_mean2$出生年份,ry=menarch_mean2$CA153,
            main='CA153、初潮年龄与出生年份的趋势分析',xlab='出生年份', ylab='初潮年龄(mean)', rylab='log(CA153)(mean)', type=c('line','line'))
###ggplot2作图




#初次生育年龄、肿瘤标志物、生育年份
with(baseline3,table(生育年份))#1970-2000
with(baseline3,table(生育年份,is.na(CA125)))#1974-1998,CA125
with(baseline3,table(生育年份,is.na(CA153)))#1971-2000,CA125
with(baseline3,table(生育年份,is.na(CEA)))#1971-2004,CA125

age2_mean<-baseline3%>%group_by(生育年份)%>%summarise(初次生育年龄=mean(初次生育年龄,na.rm=TRUE),CA125=mean(log(CA125),na.rm=TRUE))%>%filter(生育年份<=1998,生育年份>=1974,!is.na(CA125),!is.na(初次生育年龄))
age2_mean1<-baseline3%>%group_by(生育年份)%>%summarise(初次生育年龄=mean(初次生育年龄,na.rm=TRUE),CEA=mean(log(CEA),na.rm=TRUE))%>%filter(生育年份<=1998,生育年份>=1974,!is.na(CEA),!is.na(初次生育年龄))
age2_mean2<-baseline3%>%filter(CA153!=0)%>%group_by(生育年份)%>%summarise(初次生育年龄=mean(初次生育年龄,na.rm=TRUE),CA153=mean(log(CA153),na.rm=TRUE))%>%filter(生育年份<=1998,生育年份>=1974,!is.na(CA153),!is.na(初次生育年龄))
twoord.plot(lx=age2_mean$生育年份, ly=age2_mean$初次生育年龄,rx=age2_mean$生育年份,ry=age2_mean$CA125,
            main='CA125、初次生育年龄与出生年份的趋势分析',xlab='生育年份', ylab='初次生育年龄(mean)', rylab='log(CA125)(mean)', type=c('line','line'))
twoord.plot(lx=age2_mean1$生育年份, ly=age2_mean1$初次生育年龄,rx=age2_mean1$生育年份,ry=age2_mean1$CEA,
            main='CEA、初次生育年龄与出生年份的趋势分析',xlab='生育年份', ylab='初次生育年龄(mean)', rylab='log(CEA)(mean)', type=c('line','line'))
twoord.plot(lx=age2_mean2$生育年份, ly=age2_mean2$初次生育年龄,rx=age2_mean2$生育年份,ry=age2_mean2$CA153,
            main='CA153、初次生育年龄与出生年份的趋势分析',xlab='生育年份', ylab='初次生育年龄(mean)', rylab='log(CA153)(mean)', type=c('line','line'))

##哺乳月份、肿瘤标志物、生育年份
with(baseline3,table(生育年份))#1970-2000
with(baseline3,table(生育年份,is.na(哺乳月份)))#1970-2000

with(subset(baseline3,!is.na(哺乳月份)),table(生育年份,is.na(CA125)))#1974-1998,CA125
with(subset(baseline3,!is.na(哺乳月份)),table(生育年份,is.na(CA153)))#1971-2000,CA125
with(subset(baseline3,!is.na(哺乳月份)),table(生育年份,is.na(CEA)))#1971-2004,CA125

breed_mean<-baseline3%>%group_by(生育年份)%>%summarise(哺乳月份=mean(哺乳月份,na.rm=TRUE),CA125=mean(log(CA125),na.rm=TRUE))%>%filter(生育年份<=1998,生育年份>=1974,!is.na(CA125),!is.na(哺乳月份))
breed_mean1<-baseline3%>%group_by(生育年份)%>%summarise(哺乳月份=mean(哺乳月份,na.rm=TRUE),CEA=mean(log(CEA),na.rm=TRUE))%>%filter(生育年份<=1998,生育年份>=1974,!is.na(CEA),!is.na(哺乳月份))
breed_mean2<-baseline3%>%filter(CA153!=0)%>%group_by(生育年份)%>%summarise(哺乳月份=mean(哺乳月份,na.rm=TRUE),CA153=mean(log(CA153),na.rm=TRUE))%>%filter(生育年份<=1998,生育年份>=1974,!is.na(CA153),!is.na(哺乳月份))
twoord.plot(lx=breed_mean$生育年份, ly=breed_mean$哺乳月份,rx=breed_mean$生育年份,ry=breed_mean$CA125,
            main='CA125、哺乳月份与出生年份的趋势分析',xlab='生育年份', ylab='哺乳月份(mean)', rylab='log(CA125)(mean)', type=c('line','line'))
twoord.plot(lx=breed_mean1$生育年份, ly=breed_mean1$哺乳月份,rx=breed_mean1$生育年份,ry=breed_mean1$CEA,
            main='CEA、哺乳月份与出生年份的趋势分析',xlab='生育年份', ylab='哺乳月份(mean)', rylab='log(CEA)(mean)', type=c('line','line'))
twoord.plot(lx=breed_mean2$生育年份, ly=breed_mean2$哺乳月份,rx=breed_mean2$生育年份,ry=breed_mean2$CA153,
            main='CA153、哺乳月份与出生年份的趋势分析',xlab='生育年份', ylab='哺乳月份(mean)', rylab='log(CA153)(mean)', type=c('line','line'))


######<<<<<<<<<<<<<补充结果分析》》》》》》》》》》》
#1、肿瘤标志物在检测年份和检测地区的分布比较
#2017年女性只做了CA153,2018-2019女性做了CA125、CA153、CEA
#蓟州区只有2019年做了CEA
#年份分组
CEA%>%group_by(year_group)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
CA125%>%group_by(year_group)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
CA153%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
wilcox.test(CA153~year_group,data=CA153)
#地区分组
CEA%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
CA125%>%group_by(地区)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
CA153%>%group_by(地区)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
wilcox.test(CEA~地区,data=CEA)

#2、CA125在筛查人群共性危险因素的分布结果显示
means_CA125<-CA125%>%pivot_longer(cols=c('年龄','吸烟2','被动吸烟'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in c('年龄','吸烟2','被动吸烟')){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
##出行方式
CA125%>%group_by(出行方式)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
kruskal.test(CA125~出行方式,data=CA125)

###CA153
means_CA153<-CA153%>%pivot_longer(cols=c('年龄','吸烟2','被动吸烟'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in c('年龄','吸烟2','被动吸烟')){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
##分年份 & 年龄
means_CA153<-CA153%>%pivot_longer(cols=c('年龄','吸烟2','被动吸烟'),names_to='variable',values_to = 'level')%>%
  group_by(year_group,variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
CA153_split<-split(CA153,CA153$年龄)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))
##分年份 & 吸烟
CA153_split<-split(CA153,CA153$吸烟2)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))
#分年份 & 被动吸烟
CA153_split<-split(CA153,CA153$被动吸烟)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))

#分地区
#1、分地区 & 年龄
CA153_split<-split(CA153,CA153$年龄)
lapply(CA153_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~地区,x))

###饮酒/饮茶/饮食
means_CA153<-CA153%>%pivot_longer(cols=c('BMI_group',variables4),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in c('BMI_group',variables4)){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#BMI分层 & 检测年份
CA153_split<-split(CA153,CA153$BMI_group)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))

#饮食饮茶分层 & 检测年份

CA153_split<-split(CA153,CA153$菌类)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))
####运动
means_CA153<-CA153%>%pivot_longer(cols=c('运动','出行方式','静态时间','重大精神创伤'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in c('运动','出行方式','静态时间','重大精神创伤')){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
##分年份 & 运动
CA153_split<-split(CA153,CA153$静态时间)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))

###<<<<<<<CEA

means_CEA<-CEA%>%pivot_longer(cols=c('年龄','吸烟2','被动吸烟'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in c('年龄','吸烟2','被动吸烟')){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)

###分地区

##分地区 & 年龄
CEA_split<-split(CEA,CEA$年龄)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))
##分地区 & 吸烟
CEA_split<-split(CEA,CEA$吸烟2)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split[-2],function(x) wilcox.test(CEA~地区,x))
#分地区 & 被动吸烟
CEA_split<-split(CEA,CEA$被动吸烟)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))

####在BMI、饮食等因素的分布比较

means_CEA<-CEA%>%pivot_longer(cols=c('BMI_group',variables4),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in c('BMI_group',variables4)){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)

##分层，比较地区差异
CEA_split<-split(CEA,CEA$菌类)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))


#####锻炼方式、出行方式、静态时间、精神问题

means_CEA<-CEA%>%pivot_longer(cols=c('运动','出行方式','静态时间','重大精神创伤'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
p<-list()
for(i in c('运动','出行方式','静态时间','重大精神创伤')){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)

####分地区


##分地区 & 运动
CEA_split<-split(CEA,CEA$运动)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))
##分地区 & 出行方式
CEA_split<-split(CEA,CEA$出行方式)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split[-2],function(x) wilcox.test(CEA~地区,x))
#分地区 & 静态时间
CEA_split<-split(CEA,CEA$静态时间)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))
#分地区 & 精神问题 
CEA_split<-split(CEA,CEA$重大精神创伤)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))

###CA125与乳腺特异性危险因素(女性生理生育因素)的关联分析
variables9<-c('女性良性病变史','初潮年龄分组','绝经','绝经年龄分组','首次生育年龄分组','哺乳时间分组','口服避孕药','雌激素代替治疗','人工流产次数分组')
means_CA125<-CA125%>%pivot_longer(cols=variables9,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables9){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
##
###CA153
means_CA153<-CA153%>%pivot_longer(cols=variables9,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables9){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
##分年份 & 女性良性病变史  & 初潮年龄分组& 绝经& 绝经年龄分组 & 首次生育年龄分组
CA153_split<-split(CA153,CA153$女性良性病变史)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))

###CEA
means_CEA<-CEA%>%pivot_longer(cols=variables9,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables9){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)

##分地区 & 运动
CEA_split<-split(CEA,CEA$初潮年龄分组)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))

######<<<<<<<<<<<<<<<<<<<<<<<<<补充：女性肿瘤标志物与乳腺超声初筛结局的关联分析>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CA125
variables10<-c("乳腺超声描述",'肿块','钙化','周围组织异常','特殊情况','淋巴结肿大')
means_CA125<-CA125%>%pivot_longer(cols=variables10,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
print(means_CA125,n=126)
#p值
p<-list()
for(i in variables10){
  formula_uni<-as.formula(paste('CA125','~', i))
  if(length(table(CA125[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA125)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA125)$p.value,4)
  }
}
do.call(rbind,p)
##CA153
means_CA153<-CA153%>%pivot_longer(cols=variables10,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=126)
#p值
p<-list()
for(i in variables10){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)

#按地区分层 & 初筛结局

CA153_split<-split(CA153,CA153$乳腺超声描述)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))




##CEA
means_CEA<-CEA%>%pivot_longer(cols=variables10,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
#p值
p<-list()
for(i in variables10){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)

##分地区 & 初筛影像结局
CEA_split<-split(CEA,CEA$乳腺超声描述)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))

######乳腺密度分级
CA125%>%group_by(乳腺组织构成)%>%summarise(n=n(),median=median(CA125),Q1=quantile(CA125,0.25),Q3=quantile(CA125,0.75))
kruskal.test(CA125~乳腺组织构成,data=CA125)
CA153%>%group_by(乳腺组织构成)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
kruskal.test(CA153~乳腺组织构成,data=CA153)
CEA%>%group_by(乳腺组织构成)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
kruskal.test(CEA~乳腺组织构成,data=CEA)
##分地区 & 乳腺密度分级
CEA_split<-split(CEA,CEA$乳腺组织构成)
lapply(CEA_split, function(x)x%>%group_by(地区)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75)))
lapply(CEA_split,function(x)wilcox.test(CEA~地区,x))

#分年份 & 乳腺密度分级
CA153_split<-split(CA153,CA153$乳腺组织构成)
lapply(CA153_split, function(x)x%>%group_by(year_group)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75)))
lapply(CA153_split,function(x)wilcox.test(CA153~year_group,x))

##h画图
ggplot(data=subset(CEA,!is.na(乳腺组织构成)),aes(乳腺组织构成,log(CEA)))+geom_boxplot()+border()+mytheme+
  stat_compare_means(comparisons = list(c(1,2),c(1,3),c(2,3)))+stat_compare_means()




