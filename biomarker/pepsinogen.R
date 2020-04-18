rm(list=ls())
library(rio)
library(openxlsx)
library(glmnet)
library(tidyverse)
library(randomForest)
library(table1)
library(grpreg)
library(knitr)
library(kableExtra)
library(ggstatsplot)
library(survival)
library(dplyr)
library(coin)
library(MatchIt)
###2020/4/2
#人群特征及其影响因素
source('~/Rcode/statistics/OR.R')
source('~/Rcode/statistics/data_summary.R')
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
#读取数据
source('~/Rcode/biomarker/biomarker_data.R')
#PG1,PG2,PGR的基本分布
ggplot(data=pepsinogen,aes(x=log(PG1),y=..density..))+geom_histogram(bins=30,color='black',fill='green')
ggplot(data=pepsinogen,aes(x=log(PG2),y=..density..))+geom_histogram(bins=30,color='black',fill='green')
ggplot(data=pepsinogen,aes(x=log(PGR),y=..density..))+geom_histogram(bins=30,color='black',fill='green')
ks.test(pepsinogen$PG1,'pnorm')
ks.test(pepsinogen$PG2,'pnorm')
ks.test(pepsinogen$PGR,'pnorm')
#基本描述
pepsinogen2<-pepsinogen
pepsinogen2[,-1:-8]<-data.frame(apply(pepsinogen2[,-1:-8],2,as.factor))
str(pepsinogen2)
table1(~ 胃癌家族史+年龄+性别+家庭收入+教育+婚姻+BMI+吸烟+
         手机使用时间+饮酒+喝茶+酸奶+咖啡+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+大蒜+菌类+油炸+烧烤+
         熏制+酱制+偏咸+腌制+
         十二指肠溃疡+胃溃疡+胃息肉+ 幽门螺杆菌感染史+癌前病变+
         残胃+糖尿病+高血压+高血脂+冠心病 | PG_pos, data=pepsinogen2,render.categorical=my.render.cat)
a<-as.data.frame(do.call(rbind,apply(pepsinogen[which(pepsinogen$PG1!=200),c('PG1','PG2','PGR')],2,data_summary2)))
export(a,'~/PG_summary.xlsx')
variable<-c("胃癌家族史" ,"年龄","性别","家庭收入", "教育", "婚姻",
            "BMI","吸烟" ,"手机使用时间" ,"饮酒" ,"喝茶" ,
            "十二指肠溃疡","胃溃疡","胃息肉","幽门螺杆菌感染史","癌前病变",
            "残胃" ,"糖尿病", "高血压" ,"高血脂" ,"冠心病")
variable2<-c("胃癌家族史" ,"年龄","家庭收入", "教育", "婚姻",
             "BMI","吸烟" ,"手机使用时间" ,"饮酒" ,"喝茶" ,'酸奶','咖啡','蔬菜','水果',
             '谷类','鸡蛋','杂粮','豆类','坚果','大蒜','菌类','油炸','烧烤','偏咸',
             "十二指肠溃疡","胃溃疡","胃息肉","幽门螺杆菌感染史","癌前病变",
             "残胃" ,"糖尿病", "高血压" ,"高血脂" ,"冠心病")
##对于PG(连续性,把PG1>200的剔除)
#单因素分析
#PG1
p<-list()
for(i in variable){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen2[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2)$p.value,4)
  }
}
do.call(rbind,p)
#PG2
p2<-list()
for(i in variable){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen2[,i]))==2){
    p2[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2)$p.value,4)
  }
  else{
    p2[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2)$p.value,4)
  }
}
do.call(rbind,p2)
#PGR
pr<-list()
for(i in variable){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen2[,i]))==2){
    pr[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2)$p.value,4)
  }
  else{
    pr[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2)$p.value,4)
  }
}
do.call(rbind,pr)
#均值比较
#PG1
means_PG1<-pepsinogen2%>%pivot_longer(cols=variable,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
print(means_PG1,n=85)
#PG2
means_PG2<-pepsinogen2%>%pivot_longer(cols=variable,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))
print(means_PG2,n=85)
export(means_PG2,'~/means_PG2.xlsx')
#PGR
means_PGR<-pepsinogen2%>%pivot_longer(cols=variable,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))
print(means_PGR,n=85)
export(means_PGR,'~/means_PGR.xlsx')
##多水平的两两比较
library(PMCMR)
pepsinogen2%>%
  ggbetweenstats(
    x = BMI,
    y =PG1,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  )  
posthoc.kruskal.nemenyi.test(PG1 ~ BMI, data = pepsinogen2, dist="Tukey")
#回归
summary(glm(log(PG1)~.,data=pepsinogen[,c('PG1',variable)]))
summary(glm(log(PG2)~.,data=pepsinogen[,c('PG2',variable)]))
summary(glm(log(PGR)~.,data=pepsinogen[,c('PGR',variable)]))
#lasso
x1 <- model.matrix(PG1~.,pepsinogen, contrasts.arg = lapply(pepsinogen[ ,sapply(pepsinogen, is.factor)], contrasts, contrasts = FALSE ))
x1<-x1[,-1]
y1 <- pepsinogen[, c('PG1')]
#lasso regression
model<-glmnet(x1,y1,alpha = 1,family = 'gaussian')
plot(model,xvar='lambda',label=TRUE)
model2<-cv.glmnet(x1,y1,alpha=1,family='gaussian',type.measure = 'deviance')
plot(model2)
coef(model2,model2$lambda.min)
coef(model2,model2$lambda.1se)
#group-lasso
fit1 <- grpreg(x1, y1, group, penalty="grLasso", family="gaussian")
plot(fit1)
cvfit1<- cv.grpreg(x1, y1, group, penalty="grLasso",family='gaussian')
plot(cvfit1)
as.matrix(coef(cvfit1,lambda=cvfit1$lambda.min))

##二分类：PG1<=70 & PGR<=3 定义为阳性；
#multiple logistic regression
summary(glm(PG_pos~.,data=pepsinogen[,c('PG_pos',variable2)],family = 'binomial'))
summary(glm(PG_pos~.,data=pepsinogen[which(pepsinogen$性别==1),c('PG_pos',variable2)],family = 'binomial'))
#单因素
p2<-list()
for(i in variable2){
  y<-pepsinogen[[i]]
  p2[[i]]<-round(chisq.test(table(y,pepsinogen2$PG_pos))$p.value,3)
}
do.call(rbind,p2)
#多因素
logit(y='PG_pos',x=c("胃癌家族史" ,"年龄","性别","家庭收入", "教育", "婚姻",
                     "BMI","吸烟" ,"手机使用时间" ,"饮酒" ,"喝茶" ,'酸奶','咖啡','蔬菜','水果',
                     '谷类','鸡蛋','杂粮','豆类','坚果','大蒜','菌类','油炸','烧烤','偏咸',
                     "十二指肠溃疡","胃溃疡","胃息肉","幽门螺杆菌感染史","癌前病变",
                     "残胃" ,"糖尿病", "高血压" ,"高血脂" ,"冠心病"),data=pepsinogen2)
logit(y='PG_pos',x=c("胃癌家族史" ,"年龄","家庭收入", "教育", "婚姻",
                     "BMI","吸烟" ,"手机使用时间" ,"饮酒" ,"喝茶" ,'酸奶','咖啡','蔬菜','水果',
                     '谷类','鸡蛋','杂粮','豆类','坚果','大蒜','菌类','油炸','烧烤','偏咸',
                     "十二指肠溃疡","胃溃疡","胃息肉","幽门螺杆菌感染史","癌前病变",
                     "残胃" ,"糖尿病", "高血压" ,"高血脂" ,"冠心病"),data=subset(pepsinogen,性别==1))
#向后逐步回归
step(glm(PG_pos~.,data=pepsinogen[,c('PG_pos',variable)],family = 'binomial'),direction = 'backward')
##lasso-logistic
x <- model.matrix(PG_pos~.,pepsinogen, contrasts.arg = lapply(pepsinogen[ ,sapply(pepsinogen, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y <- pepsinogen[, c('PG_pos')]
#conditional logistic regression
#根据性别1：5匹配
seed(12345)
m.out<-matchit(PG_pos~性别,data=pepsinogen,exact=c('性别'),ratio=5)
m.data<-match.data(m.out)
logit(y='PG_pos',x=c("胃癌家族史","家庭收入", "教育", "婚姻",
                     "BMI","吸烟" ,"手机使用时间" ,"饮酒" ,"喝茶" ,
                     "十二指肠溃疡","胃溃疡","胃息肉","幽门螺杆菌感染史","癌前病变",
                     "残胃" ,"糖尿病", "高血压" ,"高血脂" ,"冠心病"),data=m.data2)
#根据性别年龄1：5匹配
set.seed(12345)
m.out2<-matchit(PG_pos~性别+年龄,data=pepsinogen,method='nearest',exact=c('性别','年龄'),ratio=5)
m.data2<-match.data(m.out2)
m.matrix<-m.out2$match.matrix
m.matrix1<-data.frame(id=row.names(m.matrix),m.matrix[,1:5],row.names = NULL)
m.matrix2<-m.matrix1%>%pivot_longer(cols = c('X1','X2','X3','X4','X5'),names_to='match_id',values_to = "n")
m.matrix2$n<-as.character(m.matrix2$n)
m.matrix2$id<-as.character(m.matrix2$id)
m.data3<-left_join(m.data2,m.matrix2,by='n')
m.data3$id<-ifelse(is.na(m.data3$id),m.data3$n,m.data3$id)
arrange(m.data5[,c('PG_pos','性别','年龄','n','id','m')],id)
m.data3$id<-as.numeric(m.data3$id)
m.data3$PG_pos<-ifelse(m.data3$PG_pos==1,2,1)
#单因素
m.data4<-m.data3
m.data4[,-1:-3]<-data.frame(apply(m.data4[,-1:-3],2,as.factor))
table1(~ 胃癌家族史+年龄+性别+家庭收入+教育+婚姻+BMI+吸烟+
         手机使用时间+饮酒+喝茶+酸奶+咖啡+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+大蒜+菌类+油炸+烧烤+
         熏制+酱制+偏咸+腌制+
         十二指肠溃疡+胃溃疡+胃息肉+ 幽门螺杆菌感染史+癌前病变+
         残胃+糖尿病+高血压+高血脂+冠心病 | PG_pos, data=m.data4,render.categorical=my.render.cat)
summary(clogit(PG_pos~残胃+strata(id),m.data3))
#logstic
summary(clogit(PG_pos ~胃癌家族史+家庭收入+教育+婚姻+BMI+吸烟+
                 手机使用时间+饮酒+喝茶+酸奶+咖啡+蔬菜+水果+谷类+鸡蛋+杂粮+
                 豆类+坚果+大蒜+菌类+油炸+烧烤+偏咸+
                 十二指肠溃疡+胃溃疡+胃息肉+ 幽门螺杆菌感染史+癌前病变+
                 残胃+糖尿病+高血压+高血脂+冠心病+ strata(id),m.data3))


##分层
summary(glm(PG_pos~糖尿病,family = 'binomial',data=subset(pepsinogen,性别='女')))
summary(glm(PG_pos~糖尿病,family = 'binomial',data=subset(pepsinogen,性别='男')))
#lasso回归
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
print(lasso)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
best_lam<-cv_output$lambda.min#选择lambda值
coef(cv_output,s='lambda.min')#系数
##group-lasso logistic
group<-c("癌症家族史", "胃癌家族史","年龄", "年龄", "年龄", "性别","婚姻", "婚姻", "婚姻","教育","教育" ,"教育",  
         "家庭收入" ,"家庭收入" ,"家庭收入" ,
         "家庭收入" , "BMI", "BMI", "BMI",      
         "饮酒","喝茶","酸奶", "吸烟","吸烟","吸烟","被动吸烟",  "被动吸烟",
         "蔬菜","水果", "谷类", "鸡蛋", '偏咸','腌制',"杂粮","豆类","坚果",    
          "静态时间", "静态时间",  "静态时间",  "静态时间", 
         "手机使用时间","手机使用时间" ,"手机使用时间"  , "手机使用时间" , "十二指肠溃疡",
         "胃溃疡",     "胃息肉",     "幽门螺杆菌感染史" ,"胃粘膜异性增生" , "胃肠上皮化生" ,"残胃","糖尿病",
          "高血压","高血脂",    
         "冠心病", "中风")
fit <- grpreg(x, y, group, penalty="grLasso", family="binomial")
plot(fit)
cvfit<- cv.grpreg(x, y, group, penalty="grLasso",family='binomial')
plot(cvfit)
as.matrix(coef(cvfit,lambda=cvfit$lambda.min))
#restricted cubic spline
library(rms)
ddist<-datadist(pepsinogen)
options(datadist='ddist')
model_rcs<-lrm(PG_pos~rcs(年龄2,4)+糖尿病+吸烟+饮酒,data=pepsinogen)
plot(Predict(model_rcs,年龄2))
#女
pepsinogen_female<-subset(pepsinogen,性别='女')
ddist1<-datadist(pepsinogen_female)
options(datadist='ddist1')
model_rcs2<-lrm(PG_pos~rcs(年龄2,4),data=pepsinogen_female)
plot(Predict(model_rcs2,年龄2))
##性别和年龄
tabla<-with(data=subset(pepsinogen,性别==1),table(年龄,PG_pos))
prop.table(with(data=subset(pepsinogen,性别==1),table(年龄,PG_pos)),margin=2)
spineplot(tabla)
lxl<-lbl_test(tabla)
statistic(lxl)^2
#2020-4-13--PG test  and 胃镜检查结果
gastroscopy<-import('~/data/示范区+做过胃镜的(2020-4-13).xlsx')
match<-left_join(gastroscopy,pepsinogen,by=c('ID','name'))
match%>%filter(!is.na(type))%>%
  ggbetweenstats(
    x = type,
    y =PGR,
    nboot = 10,type='np',
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  )  
#区分萎缩性病变和正常或发炎患者
match2<-left_join(pepsinogen,gastroscopy,by='ID')
match2$type2<-ifelse(is.na(match2$type),0,1)
roc_PG1<-roc(match2$type2, match2$PG1,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc_PG2<-roc(match2$type2, match2$PG2,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc_PGR<-roc(match2$type2, match2$PGR,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc_PG1R<-roc(match2$type2, match2$PG1+match2$PGR,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
#曲线下面积的比较
roc.test(roc_PG1,roc_PG2)
roc.test(roc_PG1,roc_PGR,method = "bootstrap",boot.n=10000)#p=0.06
roc.test(roc_PG2,roc_PGR)
roc.test(roc_PGR,roc_PG1R,method = "bootstrap",boot.n=10000)#p=0.12
roc.test(roc_PG1,roc_PG1R,method = "bootstrap",boot.n=10000)#p<0.01
#plot
plot.roc(match2$type2, match2$PG1,direction='>',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(match2$type2, match2$PG2,add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(match2$type2, match2$PGR,add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
#plot
plot.roc(match2$type2, match2$PG1,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="2")
#lines.roc(match2$type2, match2$PG2,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="3")
lines.roc(match2$type2, match2$PGR,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="4")
#lines.roc(match2$type2, match2$PG1+match2$PGR,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="5")
legend("bottomright",legend=c("PG1, AUC: 0.76(0.66,0.86)", 
                              "PGR, AUC: 0.84(0.76,0.94)"
                               ),
                              col=c("2","3","4","5"),lwd=3,cex=0.6)
#不同诊断标准的灵敏度和特异度
library(epiR)
table<-match2%>%transmute(PG_pos2=ifelse(PG1<=30 & PGR<=3,1,0),
                          PG_pos3=ifelse(PG1<=40 & PGR<=3,1,0),
                          PG_pos4=ifelse(PG1<=50 & PGR<=3,1,0),
                          PG_pos5=ifelse(PG1<=60 & PGR<=3,1,0),
                          PG_pos6=ifelse(PG1<=70 & PGR<=3,1,0),
                          PG_pos11=ifelse(PG1<=80 & PGR<=3,1,0),
                          PG_pos7=ifelse(PG1<=30 | PGR<=3,1,0),
                          PG_pos8=ifelse(PG1<=40 | PGR<=3,1,0),
                          PG_pos9=ifelse(PG1<=50 | PGR<=3,1,0),
                          PG_pos10=ifelse(PG1<=70 & PGR<=6,1,0),
                          type=type2)
#PG1<=30 & PGR<=3
table(table$PG_pos2,table$type)
epi.tests(as.table(matrix(c(12,167,18,5733), nrow = 2, byrow = TRUE)))

#PG1<=40 & PGR<=3
table(table$PG_pos3,table$type)
epi.tests(as.table(matrix(c(18,218,12,5682), nrow = 2, byrow = TRUE)))
#PG1<=50 & PGR<=3
table(table$PG_pos4,table$type)
epi.tests(as.table(matrix(c(20,262,10,5638), nrow = 2, byrow = TRUE)))
#PG1<=60 & PGR<=3
table(table$PG_pos5,table$type)
epi.tests(as.table(matrix(c(22,298,8,5602), nrow = 2, byrow = TRUE)))
#PG1<=70 & PGR<=3
table(table$PG_pos6,table$type)
epi.tests(as.table(matrix(c(24,330,6,5570), nrow = 2, byrow = TRUE)))

#PG1<=30 | PGR<=3
table(table$PG_pos7,table$type)
epi.tests(as.table(matrix(c(24,620,6,5280), nrow = 2, byrow = TRUE)))
#PG1<=40 | PGR<=3
table(table$PG_pos8,table$type)
epi.tests(as.table(matrix(c(25,1339,5,4561), nrow = 2, byrow = TRUE)))
#PG1<=50 | PGR<=3
table(table$PG_pos9,table$type)
epi.tests(as.table(matrix(c(26,2392,4,3508), nrow = 2, byrow = TRUE)))
#PG1<=70 | PGR<=3
table(table$PG_pos10,table$type)
epi.tests(as.table(matrix(c(24,1952,6,3948), nrow = 2, byrow = TRUE)))
#不同诊断标准的检出率情况
match2%>%transmute(type2=type2,a=ifelse(PG1<=30,1,ifelse(PG1<=40,2,ifelse(PG1<=50,3,ifelse(PG1<=60,4,ifelse(PG1<=70,5,6))))))%>%
  group_by(type2,a)%>%summarise(n=n())
match2%>%transmute(type2=type2,a=ifelse(PGR<=2,1,ifelse(PGR<=3,2,ifelse(PGR<=4,3,ifelse(PGR<=5,4,ifelse(PGR<=6,5,6))))))%>%
  group_by(type2,a)%>%summarise(n=n())


#2020-4-13 CA199与PG的相关性分析
#1连续性变量
CA199<-biomarker%>%transmute(
  PG_pos=factor(ifelse(PGI<=70 & PG_ratio<=3,1,0)),
  PG1=PGI,PG2=PGII,PGR=PG_ratio,
)
ggscatter(data=pepsinogen,x='PG1',y='CA199',add='reg.line',add.params = list(color='red'))+stat_cor()
ggscatter(data=pepsinogen,x='PG2',y='CA199',add='reg.line',add.params = list(color='red'))+stat_cor()
ggscatter(data=pepsinogen,x='PGR',y='CA199',add='reg.line',add.params = list(color='red'))+stat_cor()
#分类变量
prop.table(with(pepsinogen,table(CA199_pos,PG_pos)),margin = 2)
chisq_test(with(pepsinogen,table(CA199_pos,PG_pos)))







