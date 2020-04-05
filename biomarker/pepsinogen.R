rm(list=ls())
library(rio)
library(openxlsx)
library(glmnet)
library(tidyverse)
library(randomForest)
library(grpreg)
library(knitr)
library(kableExtra)
#全部做过PG的人群
biomarker<-read.xlsx('~/data/biomarker/Biomarker+baseline(2017+18+19).xlsx',detectDates = TRUE)
biomarker<-biomarker%>%filter(!is.na(PGI))
#做过胃镜有病理结果的
gastroscopy<-read.xlsx('~/data/biomarker/PG_gastric.xlsx',sheet=3)
gastroscopy%>%group_by(病理结果2)%>%summarise(mean=mean(PG1))
###2020/4/2
#人群特征及其影响因素
source('~/Rcode/statistics/OR.R')
source('~/Rcode/statistics/data_summary.R')
#读取数据
biomarker<-import('~/data/biomark2017-2019(剔除自身癌).sav')
pepsinogen<-biomarker%>%filter(!is.na(PGI))%>%transmute(
  PG_pos=factor(ifelse(PGI<=70 &  PG_ratio<=3,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG1=PGI,PG2=PGII,PGR=PG_ratio,
  FATH_sim=ifelse(cancerFH==2 | !is.na(CATPFath) |!is.na(CATPMoth) | !is.na(CATPBrot) |
                    !is.na(Catpbrot1) |!is.na(Catpbrot2)| !is.na(Catpsist1) | !is.na(CATPSist) |
                    !is.na(Catpsist2) |!is.na(CATPChil) | !is.na(Catpchil1) | !is.na(Catpchil2),1,0),
  FATH_sim=ifelse(is.na(FATH_sim),0,FATH_sim),
  lung_sim=ifelse(CATPFath==34 | CATPMoth==34 | CATPBrot==34 | Catpbrot1==34 |Catpbrot2==34 |
                    CATPSist==34 | Catpsist1==34 | Catpsist2==34 | CATPChil==34 | Catpchil1==34 | Catpchil2==34,1,0),
  lung_sim=ifelse(is.na(lung_sim),0,lung_sim),
  breast_sim=ifelse(CATPMoth==47 |  CATPSist==47 | Catpsist1==47 | Catpsist2==47 | CATPChil==47 |                            Catpchil2==47,1,0),
  breast_sim=factor(ifelse(is.na(breast_sim),0,breast_sim),levels=c(0,1),labels=c('否','是')),
  liver_sim=ifelse(CATPFath==24 | CATPMoth==24 | CATPBrot==24 | Catpbrot1==24 |Catpbrot2==24 |
                     CATPSist==24 | Catpsist1==24 | Catpsist2==24 | CATPChil==24 |                               Catpchil1==24 | Catpchil2==24,1,0),
  liver_sim=factor(ifelse(is.na(liver_sim),0,liver_sim),levels=c(0,1),labels=c('否','是')),
  gastric_sim=ifelse(CATPFath==16 | CATPMoth==16 | CATPBrot==16 | Catpbrot1==16 |Catpbrot2==16 |
                       CATPSist==16 | Catpsist1==16 | Catpsist2==16 | CATPChil==16 |                               Catpchil1==16 | Catpchil2==16,1,0),
  gastric_sim=ifelse(is.na(gastric_sim),0,gastric_sim),
  age_risk=factor(ifelse(age<=49,0,ifelse(age>=60,2,1))),
  sex_risk=ifelse(sex_check==1,0,1),
  marriage_risk=case_when(
    marriag==1 ~ 1,
    marriag==2 ~ 2,
    marriag==3  | marriag==4 ~ 3
  ),
  marriage_risk=factor(marriage_risk,levels = c(1,2,3),labels=c('已婚','未婚','离婚或丧偶')),
  education_risk=case_when(
    educati==1 | educati==2 | educati==3 ~ 1,
    educati==4 ~ 2,
    educati==5 | educati==6 ~ 3,
  ),
  education_risk=factor(education_risk,levels=c(1,2,3),labels=c('初中及以下','高中/中专/技校','大学及以上')),
  income_risk=factor(income,levels = c(1,2,3,4),labels=c('<3000','3000-4999','5000-9999','>10000')),
  employm_risk=factor(employm,levels=c(1,2,3,4),labels=c('在业','离退休','失业/下岗/待业','家务/无业')),
  blood_risk=factor(ifelse(is.na(bloodtp),5,bloodtp),levels = c(1,2,3,4,5),labels=c('A','B','O','AB','不详')),
  smk_risk=case_when(
    smoking==1 ~ 0,
    smoking==3 ~ 1,
    smoking==2 ~ 2,
  
  ),
  smk_risk=ifelse(is.na(smk_risk),0,smk_risk),
  #psmk_risk=case_when(
  #  passivesmk==1 ~ 0,
  #  passivesmk==2 & psmkyrs<10 ~ 1,
  #  passivesmk==2 & psmkyrs>=10 ~ 2,
  #),
  psmk_risk=passivesmk,
  psmk_risk=ifelse(is.na(psmk_risk),0,psmk_risk),
  BMI_risk=factor(ifelse(10000*weight/(height*height)<24,0,ifelse(10000*weight/(height*height)<28,1,2))),
  #饮食
  alcohol_risk=ifelse(alcohol==2 & !is.na(alcohol),1,0),
  tea_risk=ifelse(tea==2 & !is.na(tea),1,0),
  yogurt_risk=ifelse(yogurt==2 & !is.na(yogurt),1,0),
  veget_risk=ifelse(veget==2 & !is.na(veget),1,0),
  fruit_risk=ifelse(fruit==2 & !is.na(fruit),1,0),
  grain_risk=ifelse(grain==2 & !is.na(grain),1,0),
  egg_risk=ifelse(egg==2 & !is.na(egg),1,0),
  cereal_risk=ifelse(cereal==2 & !is.na(cereal),1,0),
  beans_risk=ifelse(beans==2 & !is.na(beans),1,0),
  nuts_risk=ifelse(nuts==2 & !is.na(nuts),1,0),
  fungus_risk=ifelse(fungus==2 & !is.na(fungus),1,0),
  #饮食偏好
  salty_risk=ifelse(salty==2 & !is.na(salty),1,0),#偏咸
  #饮食喜好
  salted_risk=ifelse(salted==2 & !is.na(salted),1,0),#喜好腌制食品
  #体育锻炼
  exercise_risk=ifelse(exercise==2 & !is.na(exercise),1,0),
  jog_risk=ifelse(jog==2 & !is.na(jog),1,0),#快走
  taichi_risk=ifelse(taichi==2 & !is.na(taichi),1,0),#太极
  fitdance_risk=ifelse(fitdance==2 & !is.na(fitdance),1,0),#广场舞
  yoga_risk=ifelse(yoga==2 & !is.na(yoga),1,0),#瑜伽
  swim_risk=ifelse(swim==2 & !is.na(swim),1,0),#游泳
  run_risk=ifelse(run==2 & !is.na(run),1,0),#跑步
  ball_risk=ifelse(ball==2 & !is.na(ball),1,0),#球类
  apparatus_risk=ifelse(apparatus==2 & !is.na(apparatus),1,0),#器械
  #静态时间
  sedentaryh_risk=ifelse(is.na(sedentaryh),1,sedentaryh),
  #手机使用时间
  cellphoneh_risk=factor(cellphoneh,levels=c(1,2,3,4),labels=c('少于3小时','3-6小时','7-12小时','13小时及以上')),
  #基础性疾病
  disea1_risk=ifelse(Disea1==2 & !is.na(Disea1),1,0),#弥漫性肺间质纤维化
  disea2_risk=ifelse(Disea2==2 & !is.na(Disea2),1,0),#肺结核
  disea3_risk=ifelse(Disea3==2 & !is.na(Disea3),1,0),#慢性支气管炎
  disea4_risk=ifelse(Disea4==2 & !is.na(Disea4),1,0),#肺气肿
  disea5_risk=ifelse(Disea5==2 & !is.na(Disea5),1,0),#哮喘支气管扩张
  disea6_risk=ifelse(Disea6==2 & !is.na(Disea6),1,0),#矽肺或尘肺
  disea7_risk=ifelse(Disea7==2 & !is.na(Disea7),1,0),#胆囊息肉
  disea8_risk=ifelse(Disea8==2 & !is.na(Disea8),1,0),#胆结石
  disea9_risk=ifelse(Disea9==2 & !is.na(Disea9),1,0),#脂肪肝
  disea10_risk=ifelse(Disea10==2 & !is.na(Disea10),1,0),#肝硬化
  disea11_risk=ifelse(Disea11==2 & !is.na(Disea11),1,0),#慢性乙型肝炎
  disea12_risk=ifelse(Disea12==2 & !is.na(Disea12),1,0),#慢性丙型肝炎
  disea13_risk=ifelse(Disea13==2 & !is.na(Disea13),1,0),#血吸虫病感染史
  disea14_risk=ifelse(Disea14==2 & !is.na(Disea14),1,0),#食管或胃上皮内瘤变
  disea15_risk=ifelse(Disea15==2 & !is.na(Disea15),1,0),#十二指肠溃疡
  disea16_risk=ifelse(Disea16==2 & !is.na(Disea16),1,0),#Barrett食管
  disea17_risk=ifelse(Disea17==2 & !is.na(Disea17),1,0),#萎缩性胃炎
  disea18_risk=ifelse(Disea18==2 & !is.na(Disea18),1,0),#胃溃疡
  disea19_risk=ifelse(Disea19==2 & !is.na(Disea19),1,0),#胃息肉
  disea20_risk=ifelse(Disea20==2 & !is.na(Disea20),1,0),#幽门螺杆菌感染史
  disea21_risk=ifelse(Disea21==2 & !is.na(Disea21),1,0),#EB病毒感染史
  disea22_risk=ifelse(Disea22==2 & !is.na(Disea22),1,0),#胃粘膜异性增生
  disea23_risk=ifelse(Disea23==2 & !is.na(Disea23),1,0),#胃肠上皮化生
  disea24_risk=ifelse(Disea24==2 & !is.na(Disea24),1,0),#残胃
  disea25_risk=ifelse(Disea25==2 & !is.na(Disea25),1,0),#乳腺小叶不典型增生
  disea26_risk=ifelse(Disea26==2 & !is.na(Disea26),1,0),#乳腺导管不典型增生
  disea27_risk=ifelse(Disea27==2 & !is.na(Disea27),1,0),#乳腺小叶原位癌
  disea28_risk=ifelse(Disea28==2 & !is.na(Disea28),1,0),#糖尿病
  disea29_risk=ifelse(Disea29==2 & !is.na(Disea29),1,0),#高血压
  disea30_risk=ifelse(Disea30==2 & !is.na(Disea30),1,0),#高血脂
  disea31_risk=ifelse(Disea31==2 & !is.na(Disea31),1,0),#冠心病
  disea32_risk=ifelse(Disea32==2 & !is.na(Disea32),1,0),#中风
  #职业暴露
  cadmium_risk=ifelse(cadmium==2,1,0),#镉
  asbestos_risk=ifelse(asbestos==2,1,0),#石棉
  nickel_risk=ifelse(nickel==2,1,0),#镍
  arsenic_risk=ifelse(arsenic==2,1,0),#砷
  radon_risk=ifelse(radon==2,1,0),#氡
  chloroethy_risk=ifelse(chloroethy==2,1,0),#氯乙烯
  Xray_risk=ifelse(Xray==2,1,0),#X射线
  benzene_risk=ifelse(benzene==2,1,0),#苯
)%>%transmute(PG1=PG1,
              癌症家族史=FATH_sim,胃癌家族史=gastric_sim,
              年龄=age_risk,性别=sex_risk,婚姻=marriage_risk,教育=education_risk,
              家庭收入=income_risk,BMI=BMI_risk,饮酒=alcohol_risk,喝茶=tea_risk,酸奶=yogurt_risk,
              吸烟=factor(smk_risk),被动吸烟=factor(psmk_risk),
              蔬菜=veget_risk,水果=fruit_risk,谷类=grain_risk,鸡蛋=egg_risk,偏咸=salty_risk,腌制=salted_risk,
              杂粮=cereal_risk,豆类=beans_risk,坚果=nuts_risk,
              静态时间=factor(sedentaryh_risk),手机使用时间=cellphoneh_risk,
              十二指肠溃疡=disea15_risk,
              胃溃疡=disea18_risk,胃息肉=disea19_risk,幽门螺杆菌感染史=disea20_risk,
              胃粘膜异性增生=disea22_risk,胃肠上皮化生=disea23_risk,残胃=disea24_risk,糖尿病=disea28_risk,高血压=disea29_risk,
              高血脂=disea30_risk,冠心病=disea31_risk,中风=disea32_risk
)%>%filter(PG1!=200)
#基本描述
as.data.frame(do.call(rbind,apply(pepsinogen[which(pepsinogen$PG1!=200),c('PG1','PG2','PGR')],2,data_summary)))
#
variable<-c("癌症家族史","胃癌家族史" ,"年龄","性别","家庭收入", "教育", "婚姻",
            "BMI","吸烟" ,"被动吸烟","静态时间","手机使用时间" ,"饮酒" ,"喝茶" , "酸奶", "蔬菜","水果"  ,          
            "谷类" ,"鸡蛋","杂粮","豆类","坚果" , "偏咸","腌制" ,
            "十二指肠溃疡","胃溃疡","胃息肉","幽门螺杆菌感染史", "胃粘膜异性增生","胃肠上皮化生",
            "残胃" ,"糖尿病", "高血压" ,"高血脂" ,"冠心病" ,"中风" )
logit(y='PG_pos',x=c("癌症家族史","胃癌家族史" ,"年龄","性别","家庭收入", "教育", "婚姻",
                      "BMI","吸烟" ,"被动吸烟","静态时间","手机使用时间" ,"饮酒" ,"喝茶" , "酸奶", "蔬菜","水果"  ,          
                     "谷类" ,"鸡蛋","杂粮","豆类","坚果","偏咸","腌制" ,
                   "十二指肠溃疡","胃溃疡","胃息肉","幽门螺杆菌感染史", "胃粘膜异性增生","胃肠上皮化生",
                     "残胃" ,"糖尿病", "高血压" ,"高血脂" ,"冠心病" ,"中风"),data=pepsinogen)
summary(glm(PG_pos~.,data=pepsinogen[,c('PG_pos',variable)],family = 'binomial'))
#向后逐步回归
step(glm(PG_pos~.,data=pepsinogen[,c('PG_pos',variable)],family = 'binomial'),direction = 'backward')
##lasso-logistic
x <- model.matrix(PG_pos~.,pepsinogen, contrasts.arg = lapply(pepsinogen[ ,sapply(pepsinogen, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y <- pepsinogen[, c('PG_pos')]
####lasso回归
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
print(lasso)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
best_lam<-cv_output$lambda.min#选择lambda值
var_coef<-coef(cv_output,s='lambda.min')#系数
var_coef
#######group-lasso logistic
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
##随机森林
set.seed(20200420)
ind <- sample(2,nrow(pepsinogen),replace = T,prob=c(0.7,0.3))
traindata <- pepsinogen[ind==1,]
testdata <- pepsinogen[ind==2,]
for (i in 1:(ncol(pepsinogen)-1)) {
  m.mtry <- randomForest(PG_pos~.,data = traindata,mtry = i)
  err <- mean(m.mtry$err.rate)
  print(err)
}
set.seed(20190809)
tuned.mtry <- tuneRF(traindata[,-1],traindata[,1],stepFactor = 1.5)
print(tuned.mtry)
randmodel <- randomForest(PG_pos~.,data = traindata)
plot(randmodel)
randmodel1 <- randomForest(PG_pos~.,data = traindata,ntree=200,importance=T)
randmodel1
importance(x=randmodel1)
varImpPlot(randmodel1,,sort=T,pch=19,main="Feature importance",n.var = 10)
##对于PG1(连续性)
summary(glm(PG1~.,data=pepsinogen[,c('PG1',variable)]))
x1 <- model.matrix(PG1~.,pepsinogen, contrasts.arg = lapply(pepsinogen[ ,sapply(pepsinogen, is.factor)], contrasts, contrasts = FALSE ))
x1<-x1[,-1]
y1 <- pepsinogen[, c('PG1')]
fit1 <- grpreg(x1, y1, group, penalty="grLasso", family="gaussian")
plot(fit1)
cvfit1<- cv.grpreg(x1, y1, group, penalty="grLasso",family='gaussian')
plot(cvfit1)
as.matrix(coef(cvfit1,lambda=cvfit1$lambda.min))
select(fit1,'AIC')







