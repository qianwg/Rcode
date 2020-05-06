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
#肝部肿瘤标志物分析(AFP,CA199,HBsAg)
source('~/Rcode/biomarker/data_liver.R')
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
              axis.title=element_text(face="bold",size=10),
              axis.text=element_text(face="bold",size=9),
              panel.grid.major = element_line(colour=NA),
              panel.grid.minor = element_blank(),
              panel.background=element_rect(fill=NA),
              axis.line = element_line(color='black')
              )
variables<-c("肝癌家族史","性别",  
             "年龄","就业状况","BMI",              
             "偏咸","腌制","饮酒","喝茶",             
             "酸奶","吸烟" ,"被动吸烟", "婚姻",             
             "教育", "血型","蔬菜","水果",             
             "谷类","鸡蛋","杂粮","豆类",              
             "坚果", "菌类","油炸","烧烤",              
             "熏制","运动","快走","太极","广场舞","瑜伽",          
             "游泳","跑步","球类","器械","静态时间","手机使用时间",  
              "胆囊息肉","胆结石","脂肪肝","肝硬化","慢性乙型肝炎","慢性丙型肝炎",  
              "血吸虫病感染史","糖尿病","高血压","高血脂","冠心病","中风",          
             "镉","石棉","镍","砷","氡","氯乙烯","X射线"  )
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
#基本情况
ggplot(data=AFP,aes(x=AFP))+geom_histogram()+coord_cartesian(ylim=c(0,5))+scale_x_continuous(breaks = seq(0,1200,50))
#AFP
AFP<-biomarker%>%filter(!is.na(AFP) & !is.na(吸烟) & AFP<30)
AFP2<-biomarker%>%filter(!is.na(AFP) & !is.na(吸烟))
AFP3<-AFP2
AFP3[sapply(AFP3, is.numeric)] <- lapply(AFP3[sapply(AFP3, is.numeric)], as.factor)
ggplot(data=AFP,aes(x=AFP,y=..density..))+geom_histogram(bins=30,color='black',fill='blue')+mytheme+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,30))
#连续型变量(单因素分析)
means_AFP<-AFP%>%pivot_longer(cols=variables,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP,n=126)
means_AFP2<-AFP2%>%pivot_longer(cols=variables,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(AFP),Q1=quantile(AFP,0.25),Q3=quantile(AFP,0.75))
print(means_AFP2,n=128)
export(means_AFP2,'~/means_AFP2.xlsx')
p<-list()
for(i in variables){
  formula_uni<-as.formula(paste('AFP','~', i))
  if(length(table(AFP2[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=AFP2)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=AFP2)$p.value,4)
  }
}
do.call(rbind,p)
#多因素回归分析
summary(glm(log(AFP)~.,data=AFP[,c('AFP',variables)]))
summary(glm(log(AFP)~.,data=AFP[,c('AFP',"年龄","性别","血型","BMI","吸烟","被动吸烟","饮酒",
                                   "喝茶","跑步","肝硬化","慢性乙型肝炎","糖尿病","高血压","高血脂","中风",
                                   "石棉")]))
#AFP作为二分类变量(AFP>7)
#频率分布+单因素分析
table1(~肝癌家族史+性别+年龄+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+
         静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线 | AFP_pos, data=AFP2,render.categorical=my.render.cat)
p2<-list()
for(i in variables){
  y<-AFP2[[i]]
  p2[[i]]<-round(chisq.test(table(y,AFP2$AFP_pos))$p.value,3)
}
do.call(rbind,p2)
#多因素logistic回归
#多重共线性检验
vif_data<-vif(glm(AFP_pos~.,family='binomial',data=AFP2[,c('AFP_pos',variables)]))
vif_data
sort(vif_data,decreasing = TRUE)
#logistic回归
summary(glm(AFP_pos~.,data=AFP2[,c('AFP_pos',variables)],family='binomial'))
logit(y='AFP_pos',x=c("肝癌家族史","性别","年龄","就业状况","BMI",              
                        "偏咸","腌制","饮酒","喝茶",             
                        "酸奶","吸烟" ,"被动吸烟", "婚姻",             
                        "教育", "血型","蔬菜","水果",             
                        "谷类","鸡蛋","杂粮","豆类",              
                        "坚果", "菌类","油炸","烧烤",              
                        "熏制","运动","快走","太极","广场舞","瑜伽",          
                        "游泳","跑步","球类","器械","静态时间","手机使用时间",  
                        "胆囊息肉","胆结石","脂肪肝","肝硬化","慢性乙型肝炎","慢性丙型肝炎",  
                        "血吸虫病感染史","糖尿病","高血压","高血脂","冠心病","中风",          
                        "镉","石棉","镍","砷","氡","氯乙烯","X射线"),data=AFP2)

#lasso-logistic回归
AFP_2<-na.omit(AFP2[,c('AFP_pos',variables)])
x<-model.matrix(AFP_pos~.,AFP_2,contrasts.arg = lapply(AFP_2[ ,sapply(AFP_2, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y<-AFP_2[,c('AFP_pos')]
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
coef(cv_output,s='lambda.min')#系数
get_coe(cv_output,cv_output$lambda.min)
#group-lasso logistic
group<-c(
  "肝癌家族史否","肝癌家族史是","性别",                  
  "年龄<49","年龄50-60","年龄>60",                 
  "就业状况在业","就业状况离退休","就业状况失业/下岗/待业",  
   "就业状况家务/无业","BMI正常","BMI偏瘦",                 
  "BMI超重","BMI肥胖","偏咸",                    
  "腌制","饮酒","喝茶",                    
  "酸奶","吸烟从不吸烟","吸烟目前仍在吸烟",        
   "吸烟以前吸烟","被动吸烟0","被动吸烟1",               
  "被动吸烟2","婚姻已婚","婚姻未婚",                
   "婚姻离婚或丧偶","教育初中及以下","教育高中/中专/技校",      
   "教育大学及以上","血型A","血型B",                   
  "血型O","血型AB","血型不详",                
  "蔬菜","水果","谷类",                    
  "鸡蛋","杂粮","豆类",                    
  "坚果","菌类","油炸",                    
  "烧烤","熏制","运动",                    
  "快走","太极","广场舞",                  
  "瑜伽","游泳","跑步",                    
  "球类","器械","静态时间1",               
  "静态时间2","静态时间3","静态时间4",               
  "手机使用时间少于3小时","手机使用时间3-6小时","手机使用时间7-12小时",    
  "手机使用时间13小时及以上" ,"胆囊息肉","胆结石",                  
  "脂肪肝","肝硬化","慢性乙型肝炎",            
  "慢性丙型肝炎","血吸虫病感染史","糖尿病",                  
  "高血压","高血脂","冠心病",                  
  "中风","镉","石棉",                    
  "镍","砷","氡",                      
  "氯乙烯","X射线"   
)
fit <- grpreg(x, y, group, penalty="grLasso", family="binomial")
plot(fit)
cvfit<- cv.grpreg(x, y, group, penalty="grLasso",family='binomial')
plot(cvfit)
as.matrix(coef(cvfit,lambda=cvfit$lambda.min))
#
modelit_uni<-summary(glm(AFP_pos~.,data=AFP2[,c('AFP_pos','年龄','性别','BMI','婚姻','教育','静态时间','脂肪肝','糖尿病','高血压','高血脂','氯乙烯')],family='binomial'))
tableit_uni<-data.frame(modelit_uni$coef)
tableit_uni$OR<-exp(tableit_uni$Estimate)
tableit_uni$LCL <- exp(tableit_uni$Estimate - tableit_uni$Std..Error * 1.96 )
tableit_uni$UCL <- exp(tableit_uni$Estimate + tableit_uni$Std..Error * 1.96 )
tableit_uni$`OR(95%CI)`<-paste0(round(tableit_uni$OR,2),'(',round(tableit_uni$LCL,2),'-',round(tableit_uni$UCL,2),')')
as.matrix(tableit_uni[,c('OR(95%CI)','Pr...z..')])
#性别分层
AFP3_male<-AFP3%>%filter(性别=='0')
AFP3_female<-AFP3%>%filter(性别=="1")
table1(~肝癌家族史+年龄+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+
         静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线 | 性别, data=AFP3,render.categorical=my.render.cat)

p3<-list()
for(i in variables){
  y<-AFP3[[i]]
  p3[[i]]<-round(chisq.test(table(y,AFP3$性别))$p.value,3)
}
do.call(rbind,p3)
#男
table1(~肝癌家族史+年龄+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械+
         静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线 | AFP_pos, data=subset(AFP3,性别=='1'),render.categorical=my.render.cat)


variables2<-c("肝癌家族史", "年龄","就业状况","BMI",              
             "偏咸","腌制","饮酒","喝茶",             
             "酸奶","吸烟" ,"被动吸烟", "婚姻",             
             "教育", "血型","蔬菜","水果",             
             "谷类","鸡蛋","杂粮","豆类",              
             "坚果", "菌类","油炸","烧烤",              
             "熏制","运动","快走","太极","广场舞","瑜伽",          
             "游泳","跑步","球类","器械","静态时间","手机使用时间",  
             "胆囊息肉","胆结石","脂肪肝","肝硬化","慢性乙型肝炎","慢性丙型肝炎",  
             "血吸虫病感染史","糖尿病","高血压","高血脂","冠心病","中风",          
             "镉","石棉","镍","砷","氡","氯乙烯","X射线"  )
#性别分层下饮食习惯于AFP阴阳性
p4<-list()
for(i in variables2){
  y<-AFP3_female[[i]]
  p4[[i]]<-round(chisq.test(table(y,AFP3_female$AFP_pos))$p.value,3)
}
do.call(rbind,p4)

logit(y='AFP_pos',x=variables2,data=subset(AFP3,性别=='0'))#男
logit(y='AFP_pos',x=variables2,data=subset(AFP3,性别=='1'))#女
##多个模型矫正
logit(y='AFP_pos',x=c('饮酒','年龄'),data=subset(AFP3,性别=='1'))
logit(y='AFP_pos',x=c('饮酒','年龄','BMI','吸烟'),data=subset(AFP3,性别=='1'))
logit(y='AFP_pos',x=c('饮酒','年龄','BMI','吸烟','教育','婚姻','血型'),data=subset(AFP3,性别=='1'))
#性别分层下特殊职业暴露于AFP阴阳性的关系分析
logit(y='AFP_pos',x=c('镉','年龄'),data=subset(AFP3,性别=='0'))
logit(y='AFP_pos',x=c('镉','年龄','BMI','吸烟'),data=subset(AFP3,性别=='0'))
logit(y='AFP_pos',x=c('石棉','年龄','BMI','吸烟','教育','婚姻','血型'),data=subset(AFP3,性别=='0'))



  #对于肝癌的诊断效果(诊断效果很差的)
roc(AFP$CA_liver, AFP$AFP,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(AFP$CA_liver, AFP$AFP,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)


###CA199

CA199<-biomarker%>%filter(!is.na(CA199) & !is.na(吸烟))
#
ggplot(data=CA199,aes(x=CA199))+geom_histogram(color='black',fill='blue')+scale_x_continuous(limits = c(0,60))+
  mytheme+stat_overlay_normal_density(color = "red", linetype = "dashed")

#连续型变量(单因素分析)
means_CA199<-CA199%>%pivot_longer(cols=variables,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA199),Q1=quantile(CA199,0.25),Q3=quantile(CA199,0.75))
print(means_CA199,n=128)
export(means_CA199,'~/means_CA199.xlsx')
p<-list()
for(i in variables){
  formula_uni<-as.formula(paste('CA199','~', i))
  if(length(table(CA199[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA199)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA199)$p.value,4)
  }
}
do.call(rbind,p)
#多因素回归分析
summary(glm(log(CA199)~.,data=CA199[,c('CA199',variables)]))

#CA199作为二分类变量(CA199>27)
#频率分布+单因素分析
table1(~肝癌家族史+年龄+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         +菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械
       静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+血吸虫感染史+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线 | CA199_pos, data=CA199,render.categorical=my.render.cat)
p2<-list()
for(i in variables){
  y<-CA199[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA199$CA199_pos))$p.value,3)
}
do.call(rbind,p2)
#多因素logistic回归
#多重共线性检验
vif_data<-vif(glm(CA199_pos~.,family='binomial',data=CA199[,c('CA199_pos',variables)]))
vif_data
sort(vif_data,decreasing = TRUE)
#logistic回归
summary(glm(CA199_pos~.,data=CA199[,c('CA199_pos',variables)],family='binomial'))
logit(y='CA199_pos',x=c("肝癌家族史","年龄","就业状况","BMI",              
                      "偏咸","腌制","饮酒","喝茶",             
                      "酸奶","吸烟" ,"被动吸烟", "婚姻",             
                      "教育", "血型","蔬菜","水果",             
                      "谷类","鸡蛋","杂粮","豆类",              
                      "坚果", "菌类","油炸","烧烤",              
                      "熏制","运动","快走","太极","广场舞","瑜伽",          
                      "游泳","跑步","球类","器械","静态时间","手机使用时间",  
                      "胆囊息肉","胆结石","脂肪肝","肝硬化","慢性乙型肝炎","慢性丙型肝炎",  
                      "血吸虫病感染史","糖尿病","高血压","高血脂","冠心病","中风",          
                      "镉","石棉","镍","砷","氡","氯乙烯","X射线"),data=CA199)

#lasso-logistic回归
CA199_2<-na.omit(AFP[,c('CA199_pos',variables)])
x<-model.matrix(CA199_pos~.,CA199_2,contrasts.arg = lapply(CA199_2[ ,sapply(CA199_2, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y<-CA199_2[,c('CA199_pos')]
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
#



#对于肝癌的诊断效果(诊断效果很差的)
roc(CA199$CA_liver, CA199$CA199,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(CA199$CA_liver, CA199$CA199,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)


##HBsAg

HBsAg<-biomarker%>%filter(!is.na(HBsAg))
#HBsAg作为二分类变量
#频率分布+单因素分析
table1(~肝癌家族史+年龄+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         +菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械
       静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+血吸虫感染史+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线 | HBsAg_pos, data=HBsAg,render.categorical=my.render.cat)
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
logit(y='HBsAg_pos',x=c("肝癌家族史","年龄","就业状况","BMI",              
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
liverBUS2[which(liverBUS2$占位1=='肝硬化' | liverBUS2$占位2=='肝硬化' | liverBUS2$占位3=='肝硬化' | 
                  liverBUS2$占位4=='肝硬化' | liverBUS2$占位1类型==1 | liverBUS2$占位2类型==1 | liverBUS2$占位3类型==1),
          c('占位','占位1类型','占位2类型','占位3类型',"livultrdia","占位1", "占位2","占位3","占位4")]



















