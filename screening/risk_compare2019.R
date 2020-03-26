rm(list=ls())
library(table1)
library(rio)
library(tidyverse)
baseline<-import('~/data/Baseline2017-2019.sav')
names(baseline)<-tolower(names(baseline))
baseline$year2[baseline$year==2017]<-1
baseline$year2[baseline$year==2018]<-2
baseline$year2[baseline$year==2019]<-3
#table(baseline$year2)#2019,48216名纳入分析
#---------------去除患有自身癌的
#2017-2019,设计到的变量有caself、catpself
#1、caself==2;
#2、caself==1 but catpself 不为空
#3、caself为空 but catpself 不为空
baseline$cancer_self[baseline$caself==2]<-1
baseline$cancer_self[baseline$caself==1 & !is.na(baseline$catpself)]<-2
baseline$cancer_self[is.na(baseline$caself) & !is.na(baseline$catpself)]<-3
baseline$cancer_self<-ifelse(is.na(baseline$cancer_self),0,baseline$cancer_self)
#table(baseline$cancer_self)
baseline2<-baseline%>%filter(cancer_self==0)
#table(baseline2$year2)
rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
          sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
#baseline risk factors
baseline_risk<-baseline2%>%transmute(year=factor(year2,levels=c(1,2,3,4),labels=c('2017','2018','2019','P value')),
                     age_risk=ifelse(age<=49,'0',ifelse(age>=60,'2','1')),
                     age_risk=ifelse(is.na(age_risk),0,age_risk),
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
                     psmk_risk=case_when(
                       passivesmk==1 ~ 0,
                       passivesmk==2 & psmkyrs<10 ~ 1,
                       passivesmk==2 & psmkyrs>=10 ~ 2,
                     ),
                     psmk_risk=ifelse(is.na(psmk_risk),0,psmk_risk),
                     BMI_risk=ifelse(10000*weight/(height*height)<24,0,ifelse(10000*weight/(height*height)<28,1,2)),
                     BMI_risk=ifelse(is.na(BMI_risk),0,BMI_risk),
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
                     exercise_risk=ifelse(exercise==2 & !is.na(exercise),1,0),
                     sedentaryh_risk=ifelse(is.na(sedentaryh),1,sedentaryh)
)%>%transmute(year=year,年龄=factor(age_risk),
              BMI=factor(BMI_risk),饮酒=factor(alcohol_risk),喝茶=factor(tea_risk),酸奶=factor(yogurt_risk),吸烟=factor(smk_risk),被动吸烟=factor(psmk_risk),
              蔬菜=factor(veget_risk),水果=factor(fruit_risk),谷类=factor(grain_risk),鸡蛋=factor(egg_risk),
              杂粮=factor(cereal_risk),豆类=factor(beans_risk),坚果=factor(nuts_risk),菌类=factor(fungus_risk),
              运动=factor(exercise_risk),静态时间=factor(sedentaryh_risk)
  
)

rndr_baseline<-function(x, name, ...){
  if (length(x) == 0) {
    y <- baseline_risk[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- kruskal.test(y ~ baseline_risk$year)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(baseline_risk$year)))$p.value
      #p <- chisq.test(table(y, droplevels(lung$year)))$statistic
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name,...)
  }
}
table1(~年龄+吸烟+被动吸烟+BMI+饮酒+喝茶+酸奶+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+菌类+运动+静态时间 | year,render.categorical=my.render.cat,
       data=baseline_risk,overall=F,render = rndr_baseline,render.strat=rndr.strat,droplevels = F) 

#Lung cancer and risk factors
#--------------------------------------------risk factors
#一级亲属肺癌家族史
lung<-baseline2%>%transmute(year=factor(year2,levels=c(1,2,3,4),labels=c('2017','2018','2019','P value')),
  lung_risk1=ifelse(catpfath==34 | catpmoth==34 | catpbrot==34 | catpbrot1==34 |catpbrot2==34 |
                      catpsist==34 | catpsist1==34 | catpsist2==34 | catpchil==34 | catpchil1==34 | catpchil2==34,1,0),
  lung_risk1=ifelse(is.na(lung_risk1),0,lung_risk1),
  lung_risk2=ifelse(age>=55 & !is.na(age),1,0),
  lung_risk3=ifelse(disea1==2 & !is.na(disea1),1,0),
  lung_risk4=ifelse(disea2==2 & !is.na(disea2),1,0),
  lung_risk5=ifelse(disea3==2 & !is.na(disea3),1,0),
  lung_risk6=ifelse(disea4==2 & !is.na(disea4),1,0),
  lung_risk7=ifelse(disea5==2 & !is.na(disea5),1,0),
  lung_risk8=ifelse(disea6==2 & !is.na(disea6),1,0),
  lung_risk9=ifelse(passivesmk==2 & psmkyrs>=10 & !is.na(passivesmk) & !is.na(psmkyrs),1,0),
  lung_risk10=ifelse(stress==2 & !is.na(stress),1,0),
  lung_risk11=ifelse(smoking>1 & !is.na(smoking),1,0),
  lung_risk12=ifelse(asbestos==2 | cadmium==2 | nickel==2 | arsenic==2 | radon==2 | xray==2 ,1,0),
  lung_risk12=ifelse(is.na(lung_risk12),0,lung_risk12),
  risk=lung_risk1+lung_risk2+lung_risk3+lung_risk4+lung_risk5+lung_risk6+lung_risk7+lung_risk8+
    lung_risk9+lung_risk10+lung_risk11+lung_risk12
  )%>%transmute(year=year,肺癌家族史=factor(lung_risk1),年龄=factor(lung_risk2),弥漫性肺间质纤维化=factor(lung_risk3),
                肺结核=factor(lung_risk4),慢性支气管炎=factor(lung_risk5),肺气肿=factor(lung_risk6),
                哮喘支气管扩张=factor(lung_risk7),矽肺或尘肺=factor(lung_risk8),被动吸烟=factor(lung_risk9),
                精神问题=factor(lung_risk10),曾经或现在吸烟=factor(lung_risk11),职业暴露=factor(lung_risk12),
  至少2个因素合计=ifelse(risk>=2,'1','0'),
  至少3个因素合计=ifelse(risk>=3,'1','0'),
  至少4个因素合计=ifelse(risk>=4,'1','0'))
#------------------------------------------------------------------------------------
rndr_lung<- function(x, name, ...){
  if (length(x) == 0) {
    y <- lung[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- kruskal.test(y ~ lung$year)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(lung$year)))$p.value
     #p <- chisq.test(table(y, droplevels(lung$year)))$statistic
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name,...)
  }
}

table1(~肺癌家族史+年龄+肺结核+慢性支气管炎+肺气肿+哮喘支气管扩张+精神问题+弥漫性肺间质纤维化+
         矽肺或尘肺+职业暴露+被动吸烟+曾经或现在吸烟+至少2个因素合计+至少3个因素合计+至少4个因素合计 | year,render.categorical=my.render.cat,
       data=lung,overall=F,render = rndr_lung,render.strat=rndr.strat,droplevels = F) 
##breast cancer and its risk factors
#------------------------------------factors---
breast<-baseline2%>%filter(sex==2)%>%transmute(year=factor(year2,levels=c(1,2,3,4),labels=c('2017','2018','2019','P value')),
                              breast_risk1=ifelse(catpfath==47 | catpmoth==47 | catpbrot==47 | catpbrot1==47 |catpbrot2==47 |
                                                  catpsist==47 | catpsist1==47 | catpsist2==47 | catpchil==47 | catpchil1==47 | catpchil2==47,1,0),
                             breast_risk1=ifelse(is.na(breast_risk1),0,breast_risk1),
                               breast_risk2=ifelse(disea25==2 | disea19_2==2,1,0),
                             breast_risk2=ifelse(is.na(breast_risk2),0,breast_risk2),
                              breast_risk3=ifelse(disea26==2 & !is.na(disea26),1,0),
                              breast_risk4=ifelse(disea27==2 & !is.na(disea27),1,0),
                              breast_risk5=ifelse(age>=50 & !is.na(age),1,0),
                              breast_risk6=ifelse(agemenarch<13 & !is.na(agemenarch),1,0),
                              breast_risk7=ifelse(agemenopau>=55 & agemenopau<99 & !is.na(agemenopau),1,0),
                              breast_risk8_1=ifelse(agefirdeli>30 & deliver==2 & !is.na(agefirdeli) & !is.na(deliver),1,0),
                              breast_risk8_2=ifelse(deliver==1 & !is.na(deliver),1,0),
                              breast_risk8=ifelse(breast_risk8_1==1 | breast_risk8_2==1,1,0),
                              breast_risk9_1=ifelse(breastfeed==2 & brstfedmth<6 &!is.na(breastfeed) &!is.na(brstfedmth),1,0),
                              breast_risk9_2=ifelse(breastfeed==1 & !is.na(breastfeed),1,0),
                              breast_risk9=ifelse(breast_risk9_1==1 | breast_risk9_2==1,1,0),
                              breast_risk10=ifelse(oralcontra==2 & !is.na(oralcontra),1,0),
                              breast_risk11=ifelse(hrt==2 & !is.na(hrt),1,0),
                              breast_risk12=ifelse(weight/((height/100)^2)>=28 & !is.na(weight) & !is.na(height),1,0),
                              breast_risk13=ifelse(induabort>=2 & !is.na(induabort),1,0),
                              breast_risk14=ifelse(stress==2 &  !is.na(stress),1,0),
                               risk=breast_risk1+breast_risk2+breast_risk3+breast_risk4+breast_risk5+breast_risk6+
                               breast_risk7+breast_risk8+breast_risk9+breast_risk10+breast_risk11+breast_risk12+
                               breast_risk13+breast_risk14
                              )%>%
  transmute(year=year,乳腺癌家族史=factor(ifelse(is.na(breast_risk1),1,breast_risk1)),乳腺小叶不典型增生=factor(breast_risk2),
  乳腺导管不典型增生=factor(breast_risk3),乳腺小叶原位癌=factor(breast_risk4),年龄=factor(breast_risk5),
  初潮年龄=factor(breast_risk6),绝经年龄=factor(breast_risk7),未生育或首次生育年龄=factor(breast_risk8),
  未哺乳或哺乳时间=factor(breast_risk9),口服避孕药=factor(breast_risk10),激素替代治疗=factor(breast_risk11),
  BMI=factor(breast_risk12),人工流产次数=factor(breast_risk13),精神问题=factor(breast_risk14),
  至少2个因素合计=ifelse(risk>=2,'1','0'),
  至少3个因素合计=ifelse(risk>=3,'1','0'),
  至少4个因素合计=ifelse(risk>=4,'1','0')
  )
#--------------------------------------------------------------------------------------------------------------
rndr_breast<- function(x, name, ...){
  if (length(x) == 0) {
    y <- breast[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- kruskal.test(y ~ breast$year)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(breast$year)))$p.value
      #p <- chisq.test(table(y, droplevels(breast$year)))$statistic
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

table1(~乳腺癌家族史+年龄+初潮年龄+绝经年龄+未生育或首次生育年龄+未哺乳或哺乳时间+口服避孕药+精神问题+乳腺小叶不典型增生+
         乳腺导管不典型增生+乳腺小叶原位癌+
         激素替代治疗+BMI+人工流产次数+至少2个因素合计+至少3个因素合计+至少4个因素合计 | year,data=breast,overall=F,render.categorical=my.render.cat,
       render = rndr_breast,render.strat=rndr.strat,droplevels = F) 

  ##liver cancer and its risk factors
#risk factors-----------------------------------------------------------------------------------------
liver<-baseline2%>%transmute(year=factor(year2,levels=c(1,2,3,4),labels=c('2017','2018','2019','P value')),
                             liver_risk1=ifelse(catpfath==24 | catpmoth==24 | catpbrot==24 | catpbrot1==24 |catpbrot2==24 |
                                                catpsist==24 | catpsist1==24 | catpsist2==24 | catpchil==24 | catpchil1==24 | catpchil2==24,1,0),
                             liver_risk1=ifelse(is.na(liver_risk1),0,liver_risk1),
                             liver_risk2=ifelse(disea10==2 & !is.na(disea10),1,0),#肝硬化
                             liver_risk3=ifelse(disea11==2 & !is.na(disea11),1,0),#慢性乙型肝炎
                             liver_risk4=ifelse(disea12==2 & !is.na(disea12),1,0),#慢性丙型肝炎
                             liver_risk5=ifelse(alcohol==2 & !is.na(alcohol),1,0),#饮酒
                             liver_risk6=ifelse(chloroethy==2 & !is.na(chloroethy),1,0),#氯乙烯
                             liver_risk7=ifelse(disea9==2 & !is.na(disea9),1,0),#脂肪肝
                             liver_risk8=ifelse(disea13==2 & !is.na(disea13),1,0),#血吸虫感染史
                             liver_risk9=ifelse(disea7==2 & !is.na(disea7),1,0),#胆囊息肉
                             liver_risk10=ifelse(disea8==2 & !is.na(disea8),1,0),#胆结石
                             liver_risk11=ifelse(weight/((height/100)^2)>=28 & !is.na(weight) & !is.na(height),1,0),#BMI
                             liver_risk12=ifelse(stress==2 &  !is.na(stress),1,0),
                             liver_risk13=ifelse(fried==2 | barbecued==2 | smked==2 | salted==2 | sauced==2,1,0),
                             liver_risk13=ifelse(is.na(liver_risk13),0,liver_risk13),
                             liver_risk14=ifelse(sex==1 & age>=40,1,ifelse(sex==2 & age>=50,1,0)),
                             liver_risk14=ifelse(is.na(liver_risk14),0,liver_risk14),
                             liver_risk15=ifelse(liver_risk7==1 | liver_risk8==1 | liver_risk9==1 | liver_risk10==1,1,0),
                             liver_risk15=ifelse(is.na(liver_risk15),0,liver_risk15),
                             risk=liver_risk1+liver_risk2+liver_risk3+liver_risk4+liver_risk5+liver_risk6+liver_risk11+
                               liver_risk12+liver_risk13+liver_risk14+liver_risk15)%>%
transmute(year=year,肝癌家族史=factor(ifelse(is.na(liver_risk1),1,liver_risk1)),肝硬化=factor(liver_risk2),
 慢性乙型肝炎=factor(liver_risk3),慢性丙型肝炎=factor(liver_risk4),饮酒=factor(liver_risk5),
 氯乙烯暴露=factor(liver_risk6),脂肪肝=factor(liver_risk7),血吸虫感染史=factor(liver_risk8),
 胆囊息肉=factor(liver_risk9),胆结石=factor(liver_risk10),BMI=factor(liver_risk11),精神问题=factor(liver_risk12),偏好食品=factor(liver_risk13),
 年龄=factor(liver_risk14),肝部其他疾病=factor(liver_risk15),
 至少2个因素合计=ifelse(risk>=2,'1','0'),
 至少3个因素合计=ifelse(risk>=3,'1','0'),
 至少4个因素合计=ifelse(risk>=4,'1','0'))
#--------------------------------------------------------------------------------------------------
rndr_liver<- function(x, name, ...){
  if (length(x) == 0) {
    y <- liver[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- kruskal.test(y ~ liver$year)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(liver$year)))$p.value
      #p <- chisq.test(table(y, droplevels(liver$year)))$statistic
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

table1(~肝癌家族史+肝硬化+慢性乙型肝炎+慢性丙型肝炎+饮酒+氯乙烯暴露+年龄+肝部其他疾病+精神问题+
       BMI+偏好食品+至少2个因素合计+至少3个因素合计+至少4个因素合计 | year,data=liver,overall=F,render.categorical=my.render.cat,
       render = rndr_liver,render.strat=rndr.strat,droplevels = F) 
      #gastric cancer and its risk factors 
#-------------------------------------------------risk factors
gastric<-baseline2%>%transmute(year=factor(year2,levels=c(1,2,3,4),labels=c('2017','2018','2019','P value')),
                               gastric_risk1=ifelse(catpfath==16 | catpmoth==16 | catpbrot==16 | catpbrot1==16 |catpbrot2==16 |
                                                  catpsist==16 | catpsist1==16 | catpsist2==16 | catpchil==16 | catpchil1==16 | catpchil2==16,1,0),
                               gastric_risk1=ifelse(is.na(gastric_risk1),0, gastric_risk1),
                               gastric_risk2=ifelse(disea18==2 & !is.na(disea18),1,0),#胃溃疡
                               gastric_risk3=ifelse(disea20==2 & !is.na(disea20),1,0),#幽门螺旋杆菌感染史
                               gastric_risk4=ifelse(disea22==2 & !is.na(disea22),1,0),#胃粘膜异型或不典型增生
                               gastric_risk5=ifelse(disea24==2 & !is.na(disea24),1,0),#残胃
                               gastric_risk6=ifelse(disea23==2 & !is.na(disea23),1,0),#胃肠上皮化生
                               gastric_risk7=ifelse((cpd*smkyrs)/20>=20 & !is.na(cpd) & !is.na(smkyrs),1,0),#吸烟≥20包年
                               gastric_risk8=ifelse(disea17==2 & !is.na(disea17),1,0),#萎缩性胃炎
                               gastric_risk9=ifelse(disea19==2 & !is.na(disea19),1,0),#胃息肉
                               gastric_risk10=ifelse(disea21==2 & !is.na(disea21),1,0),#EB病毒感染史
                               gastric_risk11=ifelse(weight/((height/100)^2)>=28 & !is.na(weight) & !is.na(height),1,0),#肥胖（BMI≥28）
                               gastric_risk12=ifelse(fried==2 | barbecued==2 | smked==2 | salted==2 | sauced==2,1,0),
                               gastric_risk12=ifelse(is.na(gastric_risk12),0, gastric_risk12),
                               gastric_risk13=ifelse(age>=55 & !is.na(age),1,0),
                               gastric_risk14=ifelse(disea14==2 & !is.na(disea14),1,0),
                               gastric_risk15=ifelse(disea15==2 & !is.na(disea15),1,0),
                               gastric_risk16=ifelse(disea16==2 & !is.na(disea16),1,0),
                               gastric_risk17=ifelse(stress==2  & !is.na(stress),1,0),
                               risk=gastric_risk1+gastric_risk2+gastric_risk3+gastric_risk4+gastric_risk5+gastric_risk6+
                                 gastric_risk7+gastric_risk8+gastric_risk9+gastric_risk10+gastric_risk11+gastric_risk12+gastric_risk13+
                                 gastric_risk14+gastric_risk15+gastric_risk16+gastric_risk17
                               )%>%
  transmute(year=year,胃癌家族史=factor(ifelse(is.na(gastric_risk1),1,gastric_risk1)),胃溃疡=factor(gastric_risk2),
  幽门螺旋杆菌感染史=factor(gastric_risk3),胃粘膜异型或不典型增生=factor(gastric_risk4),残胃=factor(gastric_risk5),胃肠上皮化生=factor(gastric_risk6),
  吸烟20包年=factor(gastric_risk7),萎缩性胃炎=factor(gastric_risk8),胃息肉=factor(gastric_risk9),食管或胃上皮内瘤变=factor(gastric_risk14),
  十二指肠溃疡=factor(gastric_risk15),Barrett食管=factor(gastric_risk16),精神问题=factor(gastric_risk17),
  EB病毒感染史=factor(gastric_risk10),肥胖=factor(gastric_risk11),偏好食品=factor(gastric_risk12),年龄=factor(gastric_risk13),
  至少2个因素合计=ifelse(risk>=2,'1','0'),
  至少3个因素合计=ifelse(risk>=3,'1','0'),
  至少4个因素合计=ifelse(risk>=4,'1','0'))
#--------------------------------------------------------------------------------------------------------------
rndr_gastric<- function(x, name, ...){
  if (length(x) == 0) {
    y <- gastric[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- kruskal.test(y ~ gastric$year)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(gastric$year)))$p.value
      #p <- chisq.test(table(y, droplevels(gastric$year)))$statistic
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

table1(~胃癌家族史+胃溃疡+幽门螺旋杆菌感染史+胃粘膜异型或不典型增生+残胃+胃肠上皮化生+吸烟20包年+年龄+食管或胃上皮内瘤变+
         十二指肠溃疡+Barrett食管+精神问题+萎缩性胃炎+胃息肉+EB病毒感染史+肥胖+偏好食品+
         至少2个因素合计+至少3个因素合计+至少4个因素合计 | year,data=gastric,overall=F,render.categorical=my.render.cat,
       render = rndr_gastric,render.strat=rndr.strat,droplevels = F) 

