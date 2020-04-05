library(glmnet) #allows ridge regression, LASSO and elastic net
library(randomForest)
baseline_compare<-biomarker%>%transmute(
  AFP_pos=factor(ifelse(AFP>7,2,1),levels = c(1,2),labels=c('阴性','阳性')),
  CEA_pos=factor(ifelse(CEA>5,2,1),levels = c(1,2),labels=c('阴性','阳性')),
  CA199_pos=factor(ifelse(CA199>27,2,1),levels=c(1,2),labels=c('阴性','阳性')),
  CA125_pos=factor(ifelse(CA125>35,2,1),levels=c(1,2),labels=c('阴性','阳性')),
  CA153_pos=factor(ifelse(CA153>25,2,1),levels=c(1,2),labels=c('阴性','阳性')),
  PG_pos=factor(ifelse(PGI<=70 &  PG_ratio<=3,2,1),levels=c(1,2),labels=c('阴性','阳性')),
  HBsAg_pos=factor(ifelse(HBsAg>0,2,1),levels=c(1,2),labels=c('阴性','阳性')),
  FATH_sim=ifelse(cancerFH==2 | !is.na(CATPFath) |!is.na(CATPMoth) | !is.na(CATPBrot) |
                    !is.na(Catpbrot1) |!is.na(Catpbrot2)| !is.na(Catpsist1) | !is.na(CATPSist) |
                    !is.na(Catpsist2) |!is.na(CATPChil) | !is.na(Catpchil1) | !is.na(Catpchil2),1,0),
  FATH_sim=factor(ifelse(is.na(FATH_sim),0,FATH_sim),levels=c(0,1),labels = c('否','是')),
  lung_sim=ifelse(CATPFath==34 | CATPMoth==34 | CATPBrot==34 | Catpbrot1==34 |Catpbrot2==34 |
                    CATPSist==34 | Catpsist1==34 | Catpsist2==34 | CATPChil==34 | Catpchil1==34 | Catpchil2==34,1,0),
  lung_sim=factor(ifelse(is.na(lung_sim),0,lung_sim),levels=c(0,1),labels=c('否','是')),
  breast_sim=ifelse(CATPMoth==47 |  CATPSist==47 | Catpsist1==47 | Catpsist2==47 | CATPChil==47 |                            Catpchil2==47,1,0),
  breast_sim=factor(ifelse(is.na(breast_sim),0,breast_sim),levels=c(0,1),labels=c('否','是')),
  liver_sim=ifelse(CATPFath==24 | CATPMoth==24 | CATPBrot==24 | Catpbrot1==24 |Catpbrot2==24 |
                     CATPSist==24 | Catpsist1==24 | Catpsist2==24 | CATPChil==24 |                               Catpchil1==24 | Catpchil2==24,1,0),
  liver_sim=factor(ifelse(is.na(liver_sim),0,liver_sim),levels=c(0,1),labels=c('否','是')),
  gastric_sim=ifelse(CATPFath==16 | CATPMoth==16 | CATPBrot==16 | Catpbrot1==16 |Catpbrot2==16 |
                       CATPSist==16 | Catpsist1==16 | Catpsist2==16 | CATPChil==16 |                               Catpchil1==16 | Catpchil2==16,1,0),
  gastric_sim=factor(ifelse(is.na(gastric_sim),0,gastric_sim),levels = c(0,1),labels=c('否','是')),
  age_risk=ifelse(age<=49,'0',ifelse(age>=60,'2','1')),
  age_risk=ifelse(is.na(age_risk),0,age_risk),
  sex_risk=ifelse(sex_check==1,0,1),
  marriage_risk=case_when(
    marriag==1 ~ 1,
    marriag==2 ~ 2,
    marriag==3  | marriag==4 ~ 3
  ),
  
  income_risk=income,
  marriage_risk=factor(marriage_risk,levels = c(1,2,3),labels=c('已婚','未婚','离婚或丧偶')),
  education_risk=case_when(
    educati==1 | educati==2 | educati==3 ~ 1,
    educati==4 ~ 2,
    educati==5 | educati==6 ~ 3,
  ),
  employm_risk=factor(employm,levels=c(1,2,3,4),labels=c('在业','离退休','失业/下岗/待业','家务/无业')),
  blood_risk=factor(bloodtp,levels = c(1,2,3,4,5),labels=c('A','B','O','AB','不详')),
  smk_risk=case_when(
    smoking==1 ~ 0,
    smoking==3 & quitsmkyrs>=15 & cpd*smkyrs<400 ~ 1,
    smoking==3 & quitsmkyrs<15 & cpd*smkyrs<400 ~ 2,
    smoking==2 & cpd*smkyrs<400 ~ 3,
    smoking==3 & quitsmkyrs>=15 & cpd*smkyrs>=400 ~ 4,
    smoking==3 & quitsmkyrs<15 & cpd*smkyrs>=400 ~ 5,
    smoking==2 & cpd*smkyrs>=400 ~ 6
  ),
  smk_risk=ifelse(is.na(smk_risk),0,smk_risk),
  smk_risk2=factor(smoking,levels=c(1,2,3),labels=c('从不吸烟','目前仍在吸烟','以前吸烟')),
  psmk_risk=case_when(
    passivesmk==1 ~ 0,
    passivesmk==2 & psmkyrs<10 ~ 1,
    passivesmk==2 & psmkyrs>=10 ~ 2,
  ),
  psmk_risk=ifelse(is.na(psmk_risk),0,psmk_risk),
  BMI_risk=ifelse(10000*weight/(height*height)<24,0,ifelse(10000*weight/(height*height)<28,1,2)),
  BMI_risk=ifelse(is.na(BMI_risk),0,BMI_risk),
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
  #体育锻炼
  exercise_risk=ifelse(exercise==2 & !is.na(exercise),1,0),
  jog_risk=factor(ifelse(jog==2,1,0),levels=c(0,1),labels=c('否','是')),#快走
  taichi_risk=factor(ifelse(taichi==2,1,0),levels=c(0,1),labels=c('否','是')),#太极
  fitdance_risk=factor(ifelse(fitdance==2,1,0),levels=c(0,1),labels=c('否','是')),#广场舞
  yoga_risk=factor(ifelse(yoga==2,1,0),levels=c(0,1),labels=c('否','是')),#瑜伽
  swim_risk=factor(ifelse(swim==2,1,0),levels=c(0,1),labels=c('否','是')),#游泳
  run_risk=factor(ifelse(run==2,1,0),levels=c(0,1),labels=c('否','是')),#跑步
  ball_risk=factor(ifelse(ball==2,1,0),levels=c(0,1),labels=c('否','是')),#球类
  apparatus_risk=factor(ifelse(apparatus==2,1,0),levels=c(0,1),labels=c('否','是')),#器械
  #静态时间
  sedentaryh_risk=ifelse(is.na(sedentaryh),1,sedentaryh),
  #手机使用时间
  cellphoneh_risk=factor(cellphoneh,levels=c(1,2,3,4),labels=c('少于3小时','3-6小时','7-12小时','13小时及以上')),
  #基础性疾病
  disea1_risk=factor(ifelse(Disea1==2 & !is.na(Disea1),1,0),levels=c(0,1),labels=c('否','是')),#弥漫性肺间质纤维化
  disea2_risk=factor(ifelse(Disea2==2 & !is.na(Disea2),1,0),levels=c(0,1),labels=c('否','是')),#肺结核
  disea3_risk=factor(ifelse(Disea3==2 & !is.na(Disea3),1,0),levels=c(0,1),labels=c('否','是')),#慢性支气管炎
  disea4_risk=factor(ifelse(Disea4==2 & !is.na(Disea4),1,0),levels=c(0,1),labels=c('否','是')),#肺气肿
  disea5_risk=factor(ifelse(Disea5==2 & !is.na(Disea5),1,0),levels=c(0,1),labels=c('否','是')),#哮喘支气管扩张
  disea6_risk=factor(ifelse(Disea6==2 & !is.na(Disea6),1,0),levels=c(0,1),labels=c('否','是')),#矽肺或尘肺
  disea7_risk=factor(ifelse(Disea7==2 & !is.na(Disea7),1,0),levels=c(0,1),labels=c('否','是')),#胆囊息肉
  disea8_risk=factor(ifelse(Disea8==2 & !is.na(Disea8),1,0),levels=c(0,1),labels=c('否','是')),#胆结石
  disea9_risk=factor(ifelse(Disea9==2 & !is.na(Disea9),1,0),levels=c(0,1),labels=c('否','是')),#脂肪肝
  disea10_risk=factor(ifelse(Disea10==2 & !is.na(Disea10),1,0),levels=c(0,1),labels=c('否','是')),#肝硬化
  disea11_risk=factor(ifelse(Disea11==2 & !is.na(Disea11),1,0),levels=c(0,1),labels=c('否','是')),#慢性乙型肝炎
  disea12_risk=factor(ifelse(Disea12==2 & !is.na(Disea12),1,0),levels=c(0,1),labels=c('否','是')),#慢性丙型肝炎
  disea13_risk=factor(ifelse(Disea13==2 & !is.na(Disea13),1,0),levels=c(0,1),labels=c('否','是')),#血吸虫病感染史
  disea14_risk=factor(ifelse(Disea14==2 & !is.na(Disea14),1,0),levels=c(0,1),labels=c('否','是')),#食管或胃上皮内瘤变
  disea15_risk=factor(ifelse(Disea15==2 & !is.na(Disea15),1,0),levels=c(0,1),labels=c('否','是')),#十二指肠溃疡
  disea16_risk=factor(ifelse(Disea16==2 & !is.na(Disea16),1,0),levels=c(0,1),labels=c('否','是')),#Barrett食管
  disea17_risk=factor(ifelse(Disea17==2 & !is.na(Disea17),1,0),levels=c(0,1),labels=c('否','是')),#萎缩性胃炎
  disea18_risk=factor(ifelse(Disea18==2 & !is.na(Disea18),1,0),levels=c(0,1),labels=c('否','是')),#胃溃疡
  disea19_risk=factor(ifelse(Disea19==2 & !is.na(Disea19),1,0),levels=c(0,1),labels=c('否','是')),#胃息肉
  disea20_risk=factor(ifelse(Disea20==2 & !is.na(Disea20),1,0),levels=c(0,1),labels=c('否','是')),#幽门螺杆菌感染史
  disea21_risk=factor(ifelse(Disea21==2 & !is.na(Disea21),1,0),levels=c(0,1),labels=c('否','是')),#EB病毒感染史
  disea22_risk=factor(ifelse(Disea22==2 & !is.na(Disea22),1,0),levels=c(0,1),labels=c('否','是')),#胃粘膜异性增生
  disea23_risk=factor(ifelse(Disea23==2 & !is.na(Disea23),1,0),levels=c(0,1),labels=c('否','是')),#胃肠上皮化生
  disea24_risk=factor(ifelse(Disea24==2 & !is.na(Disea24),1,0),levels=c(0,1),labels=c('否','是')),#残胃
  disea25_risk=factor(ifelse(Disea25==2 & !is.na(Disea25),1,0),levels=c(0,1),labels=c('否','是')),#乳腺小叶不典型增生
  disea26_risk=factor(ifelse(Disea26==2 & !is.na(Disea26),1,0),levels=c(0,1),labels=c('否','是')),#乳腺导管不典型增生
  disea27_risk=factor(ifelse(Disea27==2 & !is.na(Disea27),1,0),levels=c(0,1),labels=c('否','是')),#乳腺小叶原位癌
  disea28_risk=factor(ifelse(Disea28==2 & !is.na(Disea28),1,0),levels=c(0,1),labels=c('否','是')),#糖尿病
  disea29_risk=factor(ifelse(Disea29==2 & !is.na(Disea29),1,0),levels=c(0,1),labels=c('否','是')),#高血压
  disea30_risk=factor(ifelse(Disea30==2 & !is.na(Disea30),1,0),levels=c(0,1),labels=c('否','是')),#高血脂
  disea31_risk=factor(ifelse(Disea31==2 & !is.na(Disea31),1,0),levels=c(0,1),labels=c('否','是')),#冠心病
  disea32_risk=factor(ifelse(Disea32==2 & !is.na(Disea32),1,0),levels=c(0,1),labels=c('否','是')),#中风
  #职业暴露
  cadmium_risk=factor(ifelse(cadmium==2,1,0),levels=c(0,1),labels=c('否','是')),#镉
  asbestos_risk=factor(ifelse(asbestos==2,1,0),levels=c(0,1),labels=c('否','是')),#石棉
  nickel_risk=factor(ifelse(nickel==2,1,0),levels=c(0,1),labels=c('否','是')),#镍
  arsenic_risk=factor(ifelse(arsenic==2,1,0),levels=c(0,1),labels=c('否','是')),#砷
  radon_risk=factor(ifelse(radon==2,1,0),levels=c(0,1),labels=c('否','是')),#氡
  chloroethy_risk=factor(ifelse(chloroethy==2,1,0),levels=c(0,1),labels=c('否','是')),#氯乙烯
  Xray_risk=factor(ifelse(Xray==2,1,0),levels=c(0,1),labels=c('否','是')),#X射线
  benzene_risk=factor(ifelse(benzene==2,1,0),levels=c(0,1),labels=c('否','是')),#苯
  #结局
  CA=factor(ifelse(CA==1,1,0),levels = c(0,1),labels=c('Negative','Positive')),CA_type=CA_type,
  
)%>%transmute(
              CEA_pos=CEA_pos,AFP_pos=AFP_pos,CA199_pos=CA199_pos,CA153_pos=CA153_pos,CA125_pos=CA125_pos,PG_pos=PG_pos,HBsAg_pos=HBsAg_pos,
              癌症家族史=FATH_sim,肺癌家族史=lung_sim,乳腺癌家族史=breast_sim,肝癌家族史=liver_sim,胃癌家族史=gastric_sim,
              年龄=as.numeric(age_risk),性别=factor(sex_risk,levels=c(0,1),
            labels=c('男性','女性')),家庭收入=income_risk,就业状况=employm_risk,
            BMI=BMI_risk,饮酒=factor(alcohol_risk),喝茶=factor(tea_risk),
            酸奶=factor(yogurt_risk),吸烟=factor(smk_risk),吸烟2=smk_risk2,被动吸烟=factor(psmk_risk),婚姻=marriage_risk,教育=education_risk,血型=blood_risk,
            蔬菜=factor(veget_risk),水果=factor(fruit_risk),谷类=factor(grain_risk),鸡蛋=factor(egg_risk),杂粮=factor(cereal_risk),豆类=factor(beans_risk),
            坚果=factor(nuts_risk),菌类=factor(fungus_risk),运动=factor(exercise_risk),快走=jog_risk,太极=taichi_risk,广场舞=fitdance_risk,瑜伽=yoga_risk,
            游泳=swim_risk,跑步=run_risk,球类=ball_risk,器械=apparatus_risk,静态时间=factor(sedentaryh_risk),手机使用时间=cellphoneh_risk,
            弥漫性肺间质纤维化=disea1_risk,肺结核=disea2_risk,慢性支气管炎=disea3_risk,肺气肿=disea4_risk,哮喘支气管扩张=disea5_risk,矽肺或尘肺=disea6_risk,
            胆囊息肉=disea7_risk,胆结石=disea8_risk,脂肪肝=disea9_risk,肝硬化=disea10_risk,慢性乙型肝炎=disea11_risk,慢性丙型肝炎=disea12_risk,
            血吸虫病感染史=disea13_risk,食管或胃上皮内瘤变=disea14_risk,十二指肠溃疡=disea15_risk,Barrett食管=disea16_risk,萎缩性胃炎=disea17_risk,
            胃溃疡=disea18_risk,胃息肉=disea19_risk,幽门螺杆菌感染史=disea20_risk,EB病毒感染史=disea21_risk,胃粘膜异性增生=disea22_risk,
            胃肠上皮化生=disea23_risk,残胃=disea24_risk,乳腺小叶不典型增生=disea25_risk,乳腺导管不典型增生=disea26_risk,乳腺小叶原位癌=disea27_risk,
            糖尿病=disea28_risk,高血压=disea29_risk,高血脂=disea30_risk,冠心病=disea31_risk,中风=disea32_risk,镉=cadmium_risk,石棉=asbestos_risk,
            镍=nickel_risk,砷=arsenic_risk,氡=radon_risk,氯乙烯=chloroethy_risk,X射线=Xray_risk,苯=benzene_risk,CA=CA,CA_type=CA_type,
              CA_type_female=ifelse(CA_type=='宫颈癌' | CA_type=='乳腺癌' | CA_type=='输卵管癌' | CA_type=='子宫内膜癌',1,0)
)
baseline_compare_HBsAg<-baseline_compare%>%select(-(CEA_pos:PG_pos),-吸烟2,-(CA:CA_type_female),-(蔬菜:手机使用时间),-(镉:苯),
                              -弥漫性肺间质纤维化,-血吸虫病感染史,-(食管或胃上皮内瘤变:Barrett食管),-(乳腺小叶不典型增生:乳腺小叶原位癌))
##logistic回归
summary(glm(HBsAg_pos~.,data=baseline_compare_HBsAg,family = 'binomial'))
####岭回归
baseline_compare_CEA<-baseline_compare_CEA%>%filter(!is.na(CEA_pos))
baseline_compare_CEA<-na.omit(baseline_compare_CEA)
x <- model.matrix(CEA_pos~.,baseline_compare_CEA)[,-1]
y <- baseline_compare_CEA[, c('CEA_pos')]
ridge <- glmnet(x, y, family = "binomial", alpha = 0)
print(ridge)
cv_ridge<-cv.glmnet(x,y,alpha=0,family='binomial')#lambda
plot(cv_ridge)
coef(cv_ridge,s='lambda.min')#系数

####lasso回归
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
print(lasso)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
best_lam<-cv_output$lambda.min#选择lambda值
var_coef<-coef(cv_output,s='lambda.min')#系数
var_coef
####随机森林
set.seed(20200420)
ind <- sample(2,nrow(baseline_compare_CEA),replace = T,prob=c(0.7,0.3))
traindata <- baseline_compare_CEA[ind==1,]
testdata <- baseline_compare_CEA[ind==2,]
for (i in 1:(ncol(baseline_compare_CEA)-1)) {
  m.mtry <- randomForest(CEA_pos~.,data = traindata,mtry = i)
  err <- mean(m.mtry$err.rate)
  print(err)
}
set.seed(20190809)
tuned.mtry <- tuneRF(traindata[,-1],traindata[,1],stepFactor = 1.5)
print(tuned.mtry)
randmodel <- randomForest(CEA_pos~.,data = traindata)
plot(randmodel)
randmodel1 <- randomForest(CEA_pos~.,data = traindata,ntree=200,importance=T)
randmodel1
importance(x=randmodel1)
varImpPlot(randmodel1,,sort=T,pch=19,main="Feature importance",n.var = 10)
##
testdata$pre.response <- predict(randmodel1,testdata)
install.packages("caret")
library(caret)
confusionMatrix(data=testdata$pre.response,  
                reference=testdata$Smoking_status,positive = "1")
install.packages("pROC")
library(pROC)
testdata$prob <-  predict(model,newdata = testdata,type="response")
g <- roc(Smoking_status~prob, data = testdata,smooth=F)
plot(g, print.auc=TRUE, print.thres=TRUE,main = "ROC For Random Forest (RED) vs Logistic Regression (BLUE)",
     col= "blue",print.thres.col="blue",identity.col="blue",
     identity.lty=1,identity.lwd=1)
r <- roc(Smoking_status~testdata$probran[,1], data = testdata,smooth=F)
roc.test(g,r)
lines(r, col = "red")
legend("bottomright", legend = c("logit","random forest") , pch = 15, bty = 'n', col = c("blue","red"))