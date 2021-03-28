##2021036:重新整理数据，验证分析
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
with(Charls2,summary(coxph(Surv(Time,status)~ragender+rw1cesd10+Agew1+urban_nbs)))
with(Charls2,summary(coxph(Surv(Time,status)~ragender+depressive_symptoms+Agew1+urban_nbs)))
with(Charls2,summary(coxph(Surv(Time,status)~ragender+depressive_group3+Agew1+urban_nbs)))
#Model2
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+rw1cesd10+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_symptoms+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_group3+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke)))
#Model3
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1cesd10+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
##CES-D score and Cancer risk(RCS)
dd<-datadist(Charls2) 
options(datadist='dd')
fit<-cph(Surv(Time,status)~ rcs(rw1cesd10,3)+urban_nbs+rw1bmi+ragender+rw1educa+
           rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe,data=Charls2) 
HR<-Predict(fit,rw1cesd10,fun=exp,ref.zero = TRUE) ####预测HR值
rcs<-ggplot(data=HR)+geom_line(aes(rw1cesd10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_line(aes(rw1cesd10,lower),color="black",linetype=2)+geom_line(aes(rw1cesd10,upper),color="black",linetype=2)+
  mytheme+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = " ", x="CES-D score", y="Adjusted HR (95%CI)")
density<-ggplot(data=Charls2,aes(x=rw1cesd10))+geom_density(fill="lightblue")+mytheme
ggplot2.two_y_axis(density,rcs)
anova(fit)
##分析每个抑郁特征与癌症的风险
confounders<-c('urban_nbs','rw1bmi','ragender','rw1educa','rw1mnev','Agew1','rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye')
items<-c("rw1depresl_scale2","rw1effortl_scale2","rw1sleeprl_scale2","rw1whappyl_scale2","rw1flonel_scale2",    
           "rw1goingl_scale2","rw1botherl_scale2","rw1mindtsl_scale2","rw1fhopel_scale2","rw1fearll_scale2")
combination<-list()
for(i in items){
  combination[[i]]<-cox(x=c(i,confounders),y='Surv(Time,status)',data=Charls2)%>%filter(str_detect(variable,i))
}
do.call(rbind,combination)
#亚组分析
#年龄
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,Agew1_group=='<65'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,Agew1_group=='>=65'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+Agew1_group*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#性别
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,ragender=='Female'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,ragender=='Male'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+Agew1+ragender*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#居住地
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,urban_nbs=='Urban'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,urban_nbs=='Rural'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1bmi+rw1educa+Agew1+ragender+urban_nbs*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#教育水平
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1educa=='<=Primary school'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1educa=='Middled school'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1educa=='high school and above'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1bmi+Agew1+ragender+urban_nbs+rw1educa*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#吸烟
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1smoke=='Never'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1smoke=='Former'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1smoke=='Current'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1bmi+Agew1+ragender+urban_nbs+rw1educa+rw1smoke*depressive_symptoms+
                                                rw1mnev+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#饮酒
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Never'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Former'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Current'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1bmi+Agew1+ragender+urban_nbs+rw1educa+rw1smoke+rw1drinke*depressive_symptoms+
                                                rw1mnev+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#BMI
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1bmi<24))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1bmi>=24))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi1_group2*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#糖尿病
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1diabe=='Yes'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1diabe=='No'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#高血压
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1hibpe=='Yes'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1hibpe=='No'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe+rw1hibpe*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1dyslipe+rw1diabe+rw1kidneye)))
#血脂异常
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1dyslipe=='Yes'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1dyslipe=='No'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe+rw1dyslipe*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1diabe+rw1kidneye)))
#慢性肾病
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1kidneye=='Yes'))%>%filter(str_detect(variable,'depressive_symptoms'))
cox(x=c('depressive_symptoms',confounders),y='Surv(Time,status)',data=subset(Charls2,rw1kidneye=='No'))%>%filter(str_detect(variable,'depressive_symptoms'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe+rw1kidneye*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1diabe+rw1dyslipe)))

###具体癌症与抑郁的关联
#1、Hematological cancer
#2、Respiratory cancer
with(subset(Charls2,cancer_Respir==2 | status==0),table(depressive_group3,status))
with(subset(Charls2,cancer_Respir==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                                      Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

with(subset(Charls2,cancer_Respir==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+as.numeric(depressive_group3)+
                                                                  Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#3、Gastrointestional cancer
with(subset(Charls2,cancer_Gastrointest==2 | status==0),table(depressive_group3,status))
with(subset(Charls2,cancer_Gastrointest==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,cancer_Gastrointest==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+as.numeric(depressive_group3)+
                                                                        rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#4、Malignant melanoma

#5、Urological cancer

#6、Breast cancer(选择女性)
with(subset(Charls2,(cancer_Breast==2 | status==0) & ragender=="Female"),table(depressive_group3,status))
with(subset(Charls2,(cancer_Breast==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+depressive_group3+
                                                                        Agew1+rw1hibpe+rw1dyslipe+rw1diabe)))
with(subset(Charls2,(cancer_Breast==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+as.numeric(depressive_group3)+
                                                                                         Agew1+rw1hibpe+rw1dyslipe+rw1diabe)))

#7、Gynecological cancer(选择女性)
with(subset(Charls2,(cancer_Gyneco==2 | status==0) & ragender=="Female"),table(depressive_group3,status))
with(subset(Charls2,(cancer_Gyneco==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+depressive_group3+
                                                                  Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,(cancer_Gyneco==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+as.numeric(depressive_group3)+
                                                                                         Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#8、Prostate cancer
#8、Other cancer
with(subset(Charls2,cancer_other==2 | status==0 | cancer_Hemato==2),table(depressive_group3,status))
with(subset(Charls2,cancer_other==2 | status==0 | cancer_Hemato==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                                                                  Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,cancer_other==2 | status==0 | cancer_Hemato==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+as.numeric(depressive_group3)+
                                                                                    Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

##激素相关癌种
with(subset(Charls2,(cancer_Gyneco==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+as.numeric(depressive_group3)+
                                                        Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))



##############多重填补分析#########################
variables_input<-c('depressive_symptoms','depressive_group3','rw1educa','Time','status','urban_nbs','rw1bmi',"cancer_other","cancer_Hemato","cancer_Respir",
                   "cancer_Gyneco","cancer_Gastrointest","cancer_Breast",
                   'ragender','rw1educa','rw1mnev','Agew1','rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye')


Charls3<-Charls[which(Charls$Agew1>=45 & Charls$cancre2011==0 & !is.na(Charls$rw1cesd10)),c(variables_input)]
#基线描述
#抑郁分布基线表
baseline2<-descrTable(depressive_symptoms~Agew1+ragender+urban_nbs+rw1educa+rw1mnev+rw1bmi+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye,
  data=Charls3,show.all = TRUE,digits = c(urban_nbs=2,rw1bmi=2,ragender=2,rw1educa=2,rw1mnev=2,Agew1=2,rw1smoke=2,rw1drinke=2,rw1hibpe=2,rw1dyslipe=2,rw1diabe=2,rw1kidneye=2))
export2xls(baseline2,'~/baseline_Charls1_depressive.xlsx')
##填补
a<-mice(Charls3,maxit=0)
Charls3_impution<-mice(Charls3,m=5,seed=666666)
#抑郁与患癌的风险
##人年
(322/sum(Charls3$Time))*100000#整体
#按抑郁症状分层
with(Charls3,table(depressive_symptoms,status))
(179/sum(Charls3[which(Charls3$depressive_symptoms=="No symptoms"),'Time']))*100000#无抑郁
(86/sum(Charls3[which(Charls3$depressive_symptoms=="Symptoms"),'Time']))*100000#抑郁
#按抑郁得分(四分位数)分层
with(Charls3,table(depressive_group4,status))
(88/sum(Charls3[which(Charls3$depressive_group4=="<6"),'Time']))*100000#抑郁的得分<6
(50/sum(Charls3[which(Charls3$depressive_group4=="6-8"),'Time']))*100000#抑郁得分6-8
(48/sum(Charls3[which(Charls3$depressive_group4=="9-12"),'Time']))*100000#抑郁得分9-12
(79/sum(Charls3[which(Charls3$depressive_group4==">=13"),'Time']))*100000#抑郁得分>=13
##按抑郁得分(四分位数)分层2
with(Charls3,table(depressive_group3,status))
(102/sum(Charls3[which(Charls3$depressive_group3=="<=6"),'Time']))*100000#抑郁的得分<6
(84/sum(Charls3[which(Charls3$depressive_group3=="7-12"),'Time']))*100000#抑郁得分6-8
(79/sum(Charls3[which(Charls3$depressive_group3==">12"),'Time']))*100000#抑郁得分9-12

#Model1
with(Charls3_impution,coxph(Surv(Time,status)~ragender+rw1cesd10+Agew1+urban_nbs))%>%pool()%>%summary()
with(Charls3_impution,coxph(Surv(Time,status)~ragender+depressive_symptoms+Agew1+urban_nbs))%>%pool()%>%summary()
with(Charls3_impution,summary(coxph(Surv(Time,status)~ragender+depressive_group3+Agew1+urban_nbs)))
#Model2
with(Charls3_impution,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+rw1cesd10+
                             rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(Charls3_impution,coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_symptoms+
                             rw1mnev+Agew1+rw1smoke+rw1drinke))%>%pool()%>%summary()
with(Charls2_impution,summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_group3+
                             rw1mnev+Agew1+rw1smoke+rw1drinke)))
#Model3
with(Charls3_impution,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1cesd10+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))%>%pool()%>%summary()
with(Charls3_impution,coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye))%>%pool()%>%summary()
with(Charls3_impution,coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye))%>%pool()%>%summary()



###具体癌症与抑郁的关联
#1、Hematological cancer
#2、Respiratory cancer
with(subset(Charls2,cancer_Respir==2 | status==0),table(depressive_group3,status))
with(subset(Charls2,cancer_Respir==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                                                                  Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

with(subset(Charls2,cancer_Respir==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+as.numeric(depressive_group3)+
                                                                  Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#3、Gastrointestional cancer
with(subset(Charls2,cancer_Gastrointest==2 | status==0),table(depressive_group3,status))
with(subset(Charls2,cancer_Gastrointest==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                                                                        rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,cancer_Gastrointest==2 | status==0),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+as.numeric(depressive_group3)+
                                                                        rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#4、Malignant melanoma

#5、Urological cancer

#6、Breast cancer(选择女性)
with(subset(Charls2,(cancer_Breast==2 | status==0) & ragender=="Female"),table(depressive_group3,status))
with(subset(Charls2,(cancer_Breast==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+depressive_group3+
                                                                                         Agew1+rw1hibpe+rw1dyslipe+rw1diabe)))
with(subset(Charls2,(cancer_Breast==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+as.numeric(depressive_group3)+
                                                                                         Agew1+rw1hibpe+rw1dyslipe+rw1diabe)))

#7、Gynecological cancer(选择女性)
with(subset(Charls2,(cancer_Gyneco==2 | status==0) & ragender=="Female"),table(depressive_group3,status))
with(subset(Charls2,(cancer_Gyneco==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+depressive_group3+
                                                                                         Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,(cancer_Gyneco==2 | status==0) & ragender=="Female"),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+as.numeric(depressive_group3)+
                                                                                         Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#8、Prostate cancer
#8、Other cancer
with(subset(Charls2,cancer_other==2 | status==0 | cancer_Hemato==2),table(depressive_group3,status))
with(subset(Charls2,cancer_other==2 | status==0 | cancer_Hemato==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                                                                                    Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,cancer_other==2 | status==0 | cancer_Hemato==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+as.numeric(depressive_group3)+
                                                                                    Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
















