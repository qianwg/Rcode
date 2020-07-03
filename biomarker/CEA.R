rm(list=ls())
#读取数据
source('~/Rcode/biomarker/data_CEA.R')
variables<-c("癌症家族史" ,"肺癌家族史","乳腺癌家族史","肝癌家族史",        
            "胃癌家族史","年龄","性别","腌制","饮酒","喝茶",              
             "酸奶","吸烟","被动吸烟", "婚姻","教育","血型",              
              "蔬菜","水果","谷类",  "鸡蛋","杂粮","豆类",              
          "坚果","菌类","油炸","烧烤","熏制","运动",              
             "快走","太极","广场舞","瑜伽","游泳","跑步",              
             "球类","器械","静态时间",  "手机使用时间","弥漫性肺间质纤维化" ,"肺结核",            
              "慢性支气管炎","肺气肿","哮喘支气管扩张", "脂肪肝","肝硬化","慢性乙型肝炎",      
             "慢性丙型肝炎","十二指肠溃疡","萎缩性胃炎","胃溃疡","胃息肉","幽门螺杆菌感染史",  
             "胃粘膜异性增生","胃肠上皮化生","残胃", "糖尿病","高血压","高血脂",            
              "冠心病","中风","镉","石棉","镍","砷", "氡","氯乙烯","X射线", "苯" )
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,sprintf("%d (%0.2f %%)", FREQ, PCT))))
}

#连续型变量(单因素分析)
means_CEA<-CEA%>%pivot_longer(cols=variables,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CEA),Q1=quantile(CEA,0.25),Q3=quantile(CEA,0.75))
print(means_CEA,n=126)
p<-list()
for(i in variables){
  formula_uni<-as.formula(paste('CEA','~', i))
  if(length(table(CEA[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CEA)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CEA)$p.value,4)
  }
}
do.call(rbind,p)
#多因素回归分析
summary(glm(log(CEA)~.,data=CEA[,c('CEA',variables)]))

#CEA作为二分类变量(CEA>5)
#频率分布+单因素分析
table1(~肝癌家族史+年龄+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         +菌类+油炸+烧烤+熏制+运动+快走+太极+广场舞+瑜伽+游泳+跑步+球类+器械
       静态时间+手机使用时间+胆囊息肉+胆结石+脂肪肝+肝硬化+慢性乙型肝炎+慢性丙型肝炎+血吸虫感染史+
         糖尿病+高血压+高血脂+冠心病+中风+镉+石棉+镍+砷+氡+氯乙烯+X射线 | CEA_pos, data=CEA,render.categorical=my.render.cat)
p2<-list()
for(i in variables){
  y<-AFP[[i]]
  p2[[i]]<-round(chisq.test(table(y,CEA$CEA_pos))$p.value,3)
}
do.call(rbind,p2)
#多因素logistic回归
#多重共线性检验
vif_data<-vif(glm(CEA_pos~.,family='binomial',data=CEA[,c('CEA_pos',variables)]))
vif_data
sort(vif_data,decreasing = TRUE)
#logistic回归
summary(glm(CEA_pos~.,data=CEA[,c('CEA_pos',variables)],family='binomial'))
logit(y='CEA_pos',x=c("癌症家族史" ,"肺癌家族史","乳腺癌家族史","肝癌家族史",        
                      "胃癌家族史","年龄","性别","腌制","饮酒","喝茶",              
                      "酸奶","吸烟","被动吸烟", "婚姻","教育","血型",              
                      "蔬菜","水果","谷类",  "鸡蛋","杂粮","豆类",              
                      "坚果","菌类","油炸","烧烤","熏制","运动",              
                      "快走","太极","广场舞","瑜伽","游泳","跑步",              
                      "球类","器械","静态时间",  "手机使用时间","弥漫性肺间质纤维化" ,"肺结核",            
                      "慢性支气管炎","肺气肿","哮喘支气管扩张", "脂肪肝","肝硬化","慢性乙型肝炎",      
                      "慢性丙型肝炎","十二指肠溃疡","萎缩性胃炎","胃溃疡","胃息肉","幽门螺杆菌感染史",  
                      "胃粘膜异性增生","胃肠上皮化生","残胃", "糖尿病","高血压","高血脂",            
                      "冠心病","中风","镉","石棉","镍","砷", "氡","氯乙烯","X射线", "苯" ),data=CEA)

#lasso-logistic回归
AFP_2<-na.omit(CEA[,c('CEA_pos',variables)])
x<-model.matrix(CEA_pos~.,CEA_2,contrasts.arg = lapply(CEA_2[ ,sapply(CEA_2, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y<-CEA_2[,c('CEA_pos')]
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
#对于肝癌的诊断效果(诊断效果很差的)
roc(CEA$CA, CEA$CEA,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(CEA$CA_lung, CEA$CEA,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(CEA$CA_liver, CEA$CEA,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(CEA$CA_gastric, CEA$CEA,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
#只针对于女性
roc(CEA$CA_type_female, CEA$CEA,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc(CEA$CA_breast, CEA$CEA,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
#plot
plot.roc(CEA$CA, CEA$CEA,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(CEA$CA_liver, CEA$CEA,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(CEA$CA_gastric, CEA$CEA,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(CEA$CA_lung, CEA$CEA,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(CEA$CA_type_female, CEA$CEA,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(CEA$CA_breast, CEA$CEA,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)


