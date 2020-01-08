rm(list=ls())
library(table1)
library(rio)
library(tidyverse)
baseline<-import('~/data/Baseline2017-2019.sav')
names(baseline)<-tolower(names(baseline))
baseline$year2[baseline$year==2017]<-1
baseline$year2[baseline$year==2018]<-1
baseline$year2[baseline$year==2019]<-2
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
#Lung cancer and risk factors
#--------------------------------------------risk factors
#一级亲属肺癌家族史
lung<-baseline2%>%transmute(year=factor(year2,levels=c(1,2,3),labels=c('2017-2018','2019','P value')),
  lung_risk1=ifelse(catpfath==34 | catpmoth==34 | catpbrot==34 | catpbrot1==34 |catpbrot2==34 |
                      catpsist==34 | catpsist1==34 | catpsist2==34 | catpchil==34 | catpchil1==34 | catpchil2==34,2,1),
  lung_risk2=ifelse(age>=55,2,1),
  lung_risk4=ifelse(disea2==2,2,1),
  lung_risk5=ifelse(disea3==2,2,1),
  lung_risk6=ifelse(disea4==2,2,1),
  lung_risk7=ifelse(disea5==2,2,1),
  lung_risk8=ifelse(disea6==2,2,1),
  lung_risk9=ifelse(passivesmk==2 & psmkyrs>=10,2,1),
  lung_risk10=ifelse(stress==2,2,1),
  lung_risk11=ifelse(smoking>1,2,1),
  lung_risk12=ifelse(asbestos==2 | cadmium==2 | nickel==2 | arsenic==2 | radon==2 | xray==2 ,1,0)
  )%>%transmute(year=year,肺癌家族史=factor(ifelse(is.na(lung_risk1),1,lung_risk1)),年龄=factor(lung_risk2),
                肺结核=factor(lung_risk4),慢性支气管炎=factor(lung_risk5),肺气肿=factor(lung_risk6),
                哮喘支气管扩张=factor(lung_risk7),矽肺或尘肺=factor(lung_risk8),被动吸烟=factor(lung_risk9),
                精神问题=factor(lung_risk10),曾经或现在吸烟=factor(lung_risk11),职业暴露=factor(lung_risk12))
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
     p <- chisq.test(table(y, droplevels(lung$year)))$statistic
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

table1(~肺癌家族史+肺结核+慢性支气管炎+肺气肿+哮喘支气管扩张+
         矽肺或尘肺+职业暴露+被动吸烟+曾经或现在吸烟 | year,render.continuous=c(.="Median [Q1,Q3]"),
       data=lung,overall=F,render = rndr_lung,render.strat=rndr.strat,droplevels = F) 
##breast cancer and its risk factors
#------------------------------------factors---
breast<-baseline2%>%filter(sex==2)%>%transmute(year=factor(year2,levels=c(1,2,3),labels=c('2017-2018','2019','P value')),
                              breast_risk1=ifelse(catpfath==47 | catpmoth==47 | catpbrot==47 | catpbrot1==47 |catpbrot2==47 |
                                                  catpsist==47 | catpsist1==47 | catpsist2==47 | catpchil==47 | catpchil1==47 | catpchil2==47,2,1),
                              breast_risk2=ifelse(disea25==2,2,1),
                              breast_risk3=ifelse(disea26==2,2,1),
                              breast_risk4=ifelse(disea27==2,2,1),
                              breast_risk5=ifelse(age>=50,2,1),
                              breast_risk6=ifelse(agemenarch<13,2,1),
                              breast_risk7=ifelse(agemenopau>=55 & agemenopau<99,2,1),
                              breast_risk8_1=ifelse(agefirdeli>30 & deliver==2,2,1),
                              breast_risk8_2=ifelse(deliver==1,2,1),
                              breast_risk8=ifelse(breast_risk8_1==2 | breast_risk8_2==2,2,1),
                              breast_risk9_1=ifelse(breastfeed==2 & brstfedmth<6,2,1),
                              breast_risk9_2=ifelse(breastfeed==1,2,1),
                              breast_risk9=ifelse(breast_risk9_1==2 | breast_risk9_2==2,2,1),
                              breast_risk10=ifelse(oralcontra==2,2,1),
                              breast_risk11=ifelse(hrt==2,2,1),
                              breast_risk12=ifelse(weight/((height/100)^2)>=28,2,1),
                              breast_risk13=ifelse(induabort>=2,2,1),
                              breast_risk14=ifelse(stress==2,2,1)
                              )%>%
  transmute(year=year,乳腺癌家族史=factor(ifelse(is.na(breast_risk1),1,breast_risk1)),乳腺小叶不典型增生=factor(breast_risk2),
  乳腺导管不典型增生=factor(breast_risk3),乳腺小叶原位癌=factor(breast_risk4),年龄=factor(breast_risk5),
  初潮年龄=factor(breast_risk6),绝经年龄=factor(breast_risk7),未生育或首次生育年龄=factor(breast_risk8),
  未哺乳或哺乳时间=factor(breast_risk9),口服避孕药=factor(breast_risk10),激素替代治疗=factor(breast_risk11),
  BMI=factor(breast_risk12),人工流产次数=factor(breast_risk13),精神问题=factor(breast_risk14))
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

table1(~乳腺癌家族史+初潮年龄+绝经年龄+未生育或首次生育年龄+未哺乳或哺乳时间+口服避孕药+
         激素替代治疗+BMI+人工流产次数 | year,data=breast,overall=F,
       render = rndr_breast,render.strat=rndr.strat,droplevels = F) 

##liver cancer and its risk factors
#risk factors-----------------------------------------------------------------------------------------
liver<-baseline2%>%transmute(year=factor(year2,levels=c(1,2,3),labels=c('2017-2018','2019','P value')),
                             liver_risk1=ifelse(catpfath==24 | catpmoth==24 | catpbrot==24 | catpbrot1==24 |catpbrot2==24 |
                                                catpsist==24 | catpsist1==24 | catpsist2==24 | catpchil==24 | catpchil1==24 | catpchil2==24,2,1),
                             liver_risk2=ifelse(disea10==2,2,1),#肝硬化
                             liver_risk3=ifelse(disea11==2,2,1),#慢性乙型肝炎
                             liver_risk4=ifelse(disea12==2,2,1),#慢性丙型肝炎
                             liver_risk5=ifelse(alcohol==2,2,1),#饮酒
                             liver_risk6=ifelse(chloroethy==2,2,1),#氯乙烯
                             liver_risk7=ifelse(disea9==2,2,1),#脂肪肝
                             liver_risk8=ifelse(disea13==2,2,1),#血吸虫感染史
                             liver_risk9=ifelse(disea7==2,2,1),#胆囊息肉
                             liver_risk10=ifelse(disea8==2,2,1),#胆结石
                             liver_risk11=ifelse(weight/((height/100)^2)>=28,2,1),#BMI
                             liver_risk12=ifelse(stress==2,2,1),
                             liver_risk13=ifelse(fried==2 | barbecued==2 | smked==2 | salted==2 | sauced==2,2,1))%>%
  transmute(year=year,肝癌家族史=factor(ifelse(is.na(liver_risk1),1,liver_risk1)),肝硬化=factor(liver_risk2),
 慢性乙型肝炎=factor(liver_risk3),慢性丙型肝炎=factor(liver_risk4),饮酒=factor(liver_risk5),
 氯乙烯暴露=factor(liver_risk6),脂肪肝=factor(liver_risk7),血吸虫感染史=factor(liver_risk8),
 胆囊息肉=factor(liver_risk9),胆结石=factor(liver_risk10),BMI=factor(liver_risk11),精神问题=factor(liver_risk12),偏好食品=factor(liver_risk13))
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

table1(~肝癌家族史+肝硬化+慢性乙型肝炎+慢性丙型肝炎+饮酒+氯乙烯暴露+
        脂肪肝+血吸虫感染史+BMI+偏好食品 | year,data=liver,overall=F,
       render = rndr_liver,render.strat=rndr.strat,droplevels = F) 
#gastric cancer and its risk factors 
#-------------------------------------------------risk factors
gastric<-baseline2%>%transmute(year=factor(year2,levels=c(1,2,3),labels=c('2017-2018','2019','P value')),
                               gastric_risk1=ifelse(catpfath==16 | catpmoth==16 | catpbrot==16 | catpbrot1==16 |catpbrot2==16 |
                                                  catpsist==16 | catpsist1==16 | catpsist2==16 | catpchil==16 | catpchil1==16 | catpchil2==16,2,1),
                               gastric_risk2=ifelse(disea18==2,2,1),#胃溃疡
                               gastric_risk3=ifelse(disea20==2,2,1),#幽门螺旋杆菌感染史
                               gastric_risk4=ifelse(disea22==2,2,1),#胃粘膜异型或不典型增生
                               gastric_risk5=ifelse(disea24==2,2,1),#残胃
                               gastric_risk6=ifelse(disea23==2,2,1),#胃肠上皮化生
                               gastric_risk7=ifelse((cpd*smkyrs)/20>=20,2,1),#吸烟≥20包年
                               gastric_risk8=ifelse(disea17==2,2,1),#萎缩性胃炎
                               gastric_risk9=ifelse(disea19==2,2,1),#胃息肉
                               gastric_risk10=ifelse(disea21==2,2,1),#EB病毒感染史
                               gastric_risk11=ifelse(weight/((height/100)^2)>=28,2,1),#肥胖（BMI≥28）
                               gastric_risk12=ifelse(fried==2 | barbecued==2 | smked==2 | salted==2 | sauced==2,2,1)
                               )%>%
  transmute(year=year,胃癌家族史=factor(ifelse(is.na(gastric_risk1),1,gastric_risk1)),胃溃疡=factor(gastric_risk2),
  幽门螺旋杆菌感染史=factor(gastric_risk3),胃粘膜异型或不典型增生=factor(gastric_risk4),残胃=factor(gastric_risk5),胃肠上皮化生=factor(gastric_risk6),
  吸烟20包年=factor(gastric_risk7),萎缩性胃炎=factor(gastric_risk8),胃息肉=factor(gastric_risk9),
  EB病毒感染史=factor(gastric_risk10),肥胖=factor(gastric_risk11),偏好食品=factor(gastric_risk12))
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

table1(~胃癌家族史+胃溃疡+幽门螺旋杆菌感染史+胃粘膜异型或不典型增生+残胃+胃肠上皮化生+吸烟20包年+
         萎缩性胃炎+胃息肉+EB病毒感染史+肥胖+偏好食品 | year,data=gastric,overall=F,
       render = rndr_gastric,render.strat=rndr.strat,droplevels = F) 



