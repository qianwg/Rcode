###2021-03-18:针对Charls分析结果进行敏感性分析
#1、敏感性分析1：矫正代谢综合征指标
#2、去除前三年诊断为癌症的患者
rm(list=ls())
#1、加载包及函数
library(rio)
library(tidyverse)
library(mice)
library(survival)
library(compareGroups)
library(rms)
library(survminer)
library(mediation)
library(ggpubr)
source('~/Rcode/statistics/HR.R')
source('~/Rcode/statistics/two_y_axis.R')
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(family="serif",size=14,face="bold"),
               axis.text=element_text(family="serif",size=14,face="bold"),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black'),
               legend.title = element_text(family='serif',size=15,face='bold'),
               legend.text = element_text(family = 'serif',size=15,face='bold'),
               legend.position = 'top'
)
#1、读取数据
source('~/Rcode/CHARLS/analysis20210316/data20210316.R')
#2、筛选变量
variables<-c('urban_nbs','rw1bmi','ragender','rw1educa','depressive_symptoms','rw1cesd10','Time','status','depressive_group4','depressive_group1','depressive_group2',
             'rw1mnev','Agew1','Agew1_group','rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye','rw1bmi1_group2',
             "rw1depresl_scale2","rw1effortl_scale2","rw1sleeprl_scale2","rw1whappyl_scale2","rw1flonel_scale2", 'rw1educa2',   
             "rw1goingl_scale2","rw1botherl_scale2","rw1mindtsl_scale2","rw1fhopel_scale2","rw1fearll_scale2",
             'Brain','Oral','Larynx','Pharynx','Thyroid','Lung','Breast','Oesophagus','Stomach','depressive_group3',
             'Liver','Pancreas','Kidney','Prostate','Testicle','Ovary',
             'Cervix','Endometrium','ColonRectum','Bladder','Skin','Lymphoma','Leukemia','cancer_other','cancer_Hemato','cancer_Respir','cancer_Gyneco','cancer_Gastrointest','cancer_Breast'
)
apply(Charls[which(Charls$Agew1>=45 & Charls$cancre2011==0 & !is.na(Charls$rw1cesd10)),variables],2,function(x)round(sum(is.na(x))/17708,2))#变量缺失比重
Charls2<-na.omit(Charls[which(Charls$Agew1>=45 & Charls$cancre2011==0),variables])
Charls3<-Charls2[-which(Charls2$status==1 & Charls2$Time<=2),]
summary(Charls)
summary(Charls2)
#####分析
#抑郁分布基线表
baseline2<-descrTable(depressive_symptoms~Agew1+ragender+urban_nbs+rw1educa+rw1mnev+rw1bmi+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye,
                      data=Charls2,show.all = TRUE,digits = c(urban_nbs=2,rw1bmi=2,ragender=2,rw1educa=2,rw1mnev=2,Agew1=2,rw1smoke=2,rw1drinke=2,rw1hibpe=2,rw1dyslipe=2,rw1diabe=2,rw1kidneye=2))
export2xls(baseline2,'~/baseline_Charls1_depressive.xlsx')
#抑郁与患癌的风险
##人年
(265/sum(Charls2$Time))*100000#整体
#按抑郁症状分层
with(Charls2,table(depressive_symptoms,status))
(179/sum(Charls2[which(Charls2$depressive_symptoms=="No symptoms"),'Time']))*100000#无抑郁
(86/sum(Charls2[which(Charls2$depressive_symptoms=="Symptoms"),'Time']))*100000#抑郁
#按抑郁得分(四分位数)分层
with(Charls2,table(depressive_group4,status))
(88/sum(Charls2[which(Charls2$depressive_group4=="<6"),'Time']))*100000#抑郁的得分<6
(50/sum(Charls2[which(Charls2$depressive_group4=="6-8"),'Time']))*100000#抑郁得分6-8
(48/sum(Charls2[which(Charls2$depressive_group4=="9-12"),'Time']))*100000#抑郁得分9-12
(79/sum(Charls2[which(Charls2$depressive_group4==">=13"),'Time']))*100000#抑郁得分>=13
##按抑郁得分(四分位数)分层2
with(Charls2,table(depressive_group3,status))
(102/sum(Charls2[which(Charls2$depressive_group3=="<=6"),'Time']))*100000#抑郁的得分<6
(84/sum(Charls2[which(Charls2$depressive_group3=="7-12"),'Time']))*100000#抑郁得分6-8
(79/sum(Charls2[which(Charls2$depressive_group3==">12"),'Time']))*100000#抑郁得分9-12
#Model1
with(Charls3,summary(coxph(Surv(Time,status)~ragender+rw1cesd10+Agew1+urban_nbs)))
with(Charls3,summary(coxph(Surv(Time,status)~ragender+depressive_symptoms+Agew1+urban_nbs)))
with(Charls3,summary(coxph(Surv(Time,status)~ragender+depressive_group3+Agew1+urban_nbs)))
#Model2
with(Charls3,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+rw1cesd10+
                             rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(Charls3,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_symptoms+
                             rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(Charls3,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_group3+
                             rw1mnev+Agew1+rw1smoke+rw1drinke)))
#Model3
with(Charls3,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1cesd10+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(Charls3,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(Charls3,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#3、考虑其他竞争原因，采用竞争风险模型分析
