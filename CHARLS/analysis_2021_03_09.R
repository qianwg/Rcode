rm(list=ls())
library(rio)
library(tidyverse)
library(mice)
library(survival)
library(compareGroups)
library(rms)
library(survminer)
library(mediation)
source('~/Rcode/statistics/HR.R')
variables2<-c('urban_nbs','rw1bmi','ragender','rw1educa','depressive_symptoms','rw1cesd10','Time','status','depressive_group4','depressive_group1','depressive_group2',
              'rw1mnev','Agew1','Agew1_group','rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye','rw1bmi1_group2',
              "rw1depresl_scale2","rw1effortl_scale2","rw1sleeprl_scale2","rw1whappyl_scale2","rw1flonel_scale2", 'rw1educa2',   
              "rw1goingl_scale2","rw1botherl_scale2","rw1mindtsl_scale2","rw1fhopel_scale2","rw1fearll_scale2",
              'Brain','Oral','Larynx','Pharynx','Thyroid','Lung','Breast','Oesophagus','Stomach','depressive_group3',
              'Liver','Pancreas','Kidney','Prostate','Testicle','Ovary',
              'Cervix','Endometrium','ColonRectum','Bladder','Skin',
              'Lymphoma','Leukemia','Other','cancer_female','Digestive','head_neck','cancer_male'
              )
variables3<-c(variables2,"WBC","Hs_CRP","FBG","TC","TG",'HDL_c',"LDL_c","cystatinc","newhba1c")
variables4<-c(variables2,"WBC","Hs_CRP")
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
Charls<-import('~/data/CHARLS/CHARLS2011_depression_followup.sav')%>%
  transmute(ID,urban_nbs=factor(urban_nbs,levels=c(0,1),labels=c('Rural','Urban')),
            ragender=factor(ragender,levels=c(1,2),labels=c('Male','Female')),
            Agew1,Agew1_group=factor(ifelse(Agew1<65,1,2),levels=c(1,2),labels=c('<65','>=65')),
            rw1mnev=factor(rw1mnev,levels=c(0,1),labels=c('Married','Never Married')),
            rw1educa=factor(rw1educa,levels=c(1,2,3),labels=c('<=MIiddled school','high school','collage and above')),
            rw1educa2=factor(ifelse(rw1educa=='<=MIiddled school',1,2),levels=c(1,2),labels=c('<=MIiddled school','>=high school')),
            rw1height,rw1weight,rw1bmi,rw1bmi1_group=factor(case_when(
              rw1bmi<18.5 ~ 2, #偏瘦
              rw1bmi<24 & rw1bmi>=18.5 ~ 1,#正常
              rw1bmi<28 & rw1bmi>=24 ~ 3,#超重
              rw1bmi>=28 ~ 4#肥胖
            ),levels=seq(4),labels=c('Normal','Thin','Overweight','Obesity')),
            rw1bmi1_group2=factor(ifelse(rw1bmi<24,0,1),levels=c(0,1),labels=c('<24','>=24')),
            rw1smoke=factor(rw1smoke,levels=c(1,2,3),labels=c('Never','Former','Current')),
            rw1drinke=factor(rw1drinke,levels=c(1,2,3),labels=c('Never','Former','Current')),
            rw1hibpe=factor(ifelse(rw1hibpe<9,rw1hibpe,NA),levels=c(0,1),labels=c('No','Yes')),
            rw1dyslipe=factor(ifelse(rw1dyslipe<9,rw1dyslipe,NA),levels=c(0,1),labels=c('No','Yes')),
            rw1diabe=factor(ifelse(rw1diabe<9,rw1diabe,NA),levels=c(0,1),labels=c('No','Yes')),
            rw1kidneye=factor(ifelse(rw1kidneye<9,rw1kidneye,NA),levels=c(0,1),labels=c('No','Yes')),
            #女性初潮年龄、绝经、绝经年龄
            Menarche,AMenopause,Menopause=factor(ifelse(da027==1 & !is.na(da027) & ragender=="Female",1,0),levels = c(0,1),labels=c('No','Yes')),
            Menarche_group=factor(ifelse(Menarche<16,1,2),levels=c(1,2),labels=c('<16','>=16')),
            scales=AMenopause-Menarche,scales_group=factor(ifelse(scales<=33,1,2),levels=c(1,2),labels=c('<=33','>33')),
            #抑郁量表
            rw1depresl_scale,rw1effortl_scale,   
            rw1sleeprl_scale,rw1whappyl_scale,rw1flonel_scale,rw1goingl_scale,    
            rw1botherl_scale,rw1mindtsl_scale,rw1fhopel_scale,rw1fearll_scale,    
            rw1cesd10,depressive_symptoms=factor(depressive_symptoms,levels=c(1,2),labels=c('No symptoms','Symptoms')),
            depressive_group1=factor(depressive_group1,levels=1:5,labels=c('<=3','4-6','7-9','10-14','>=15')),
            depressive_group2=factor(depressive_group2,levels=,labels=c('<=3','4-6','7-9','9-12','>12')), 
            depressive_group3=factor(depressive_group3,levels=,labels=c('<=6','7-12','>12')),
            depressive_group4=factor(
              case_when(
                rw1cesd10<6 ~ 1,
                rw1cesd10<9 & rw1cesd10>=6 ~ 2,
                rw1cesd10<13  & rw1cesd10>=9~ 3,
                rw1cesd10>=13 ~ 4
              ),levels=c(1,2,3,4),labels=c('<6','6-8','9-12','>=13')
            ),
            #具体抑郁特征
            rw1depresl_scale2=factor(ifelse(rw1depresl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1effortl_scale2=factor(ifelse(rw1effortl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1sleeprl_scale2=factor(ifelse(rw1sleeprl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1whappyl_scale2=factor(ifelse(rw1whappyl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1flonel_scale2=factor(ifelse(rw1flonel_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1goingl_scale2=factor(ifelse(rw1goingl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1botherl_scale2=factor(ifelse(rw1botherl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1mindtsl_scale2=factor(ifelse(rw1mindtsl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1fhopel_scale2=factor(ifelse(rw1fhopel_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1fearll_scale2=factor(ifelse(rw1fearll_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            
            #血检数据
            WBC=qc1_vb002,qc1_vb006,qc1_vb009,newbun,FBG=newglu,newcrea,TC=newcho,TG=newtg,             
            HDL_c=newhdl,LDL_c=newldl,Hs_CRP=newcrp,newhba1c,newua,qc1_vb005,qc1_vb004,cystatinc,
            #权重
            bloodweight,HH_weight,HH_weight_ad1,ind_weight,ind_weight_ad1,
            ind_weight_ad2,bio_weight1,bio_weight2,
            #结局
            status=preload,
            T2_2=T2-2011,T1_2=T1-2011,
            Time=ifelse(status==1,(T2_2+T1_2)/2,T1_2),
            #癌症类型
            Brain=factor(ifelse(is.na(Brain),1,Brain)),
            Oral=factor(ifelse(is.na(Oral),1,Oral)),Larynx=factor(ifelse(is.na(Larynx),1,Larynx)),
            Pharynx=factor(ifelse(is.na(Pharynx),1,Pharynx)),Thyroid=factor(ifelse(is.na(Thyroid),1,Thyroid)),
            Lung=factor(ifelse(is.na(Lung),1,Lung)),Breast=factor(ifelse(is.na(Breast),1,Breast)),
            Oesophagus=factor(ifelse(is.na(Oesophagus),1,Oesophagus)),Stomach=factor(ifelse(is.na(Stomach),1,Stomach)),
            Liver=factor(ifelse(is.na(Liver),1,Liver)),Pancreas=factor(ifelse(is.na(Pancreas),1,Pancreas)),
            Kidney=factor(ifelse(is.na(Kidney),1,Kidney)),Prostate=factor(ifelse(is.na(Prostate),1,Prostate)),
            Testicle=factor(ifelse(is.na(Testicle),1,Testicle)),Ovary=factor(ifelse(is.na(Ovary),1,Ovary)),
            Cervix=factor(ifelse(is.na(Cervix),1,Cervix)),Endometrium=factor(ifelse(is.na(Endometrium),1,Endometrium)),
            ColonRectum=factor(ifelse(is.na(ColonRectum),1,ColonRectum)),
            Bladder=factor(ifelse(is.na(Bladder),1,Bladder)),Skin=factor(ifelse(is.na(Skin),1,Skin)),
            Lymphoma=factor(ifelse(is.na(Lymphoma),1,Lymphoma)),Leukemia=factor(ifelse(is.na(Leukemia),1,Leukemia)),
            Other=factor(ifelse(is.na(Other),1,Other)),
            cancer_female=factor(ifelse(is.na(cancer_female),1,cancer_female)),Digestive=factor(ifelse(is.na(Digestive),1,Digestive)),
            head_neck=factor(ifelse(is.na(head_neck),1,head_neck)),cancer_male=factor(ifelse(is.na(cancer_male),1,cancer_male))
            )
Charls2=na.omit(Charls[,variables2])
Charls3=na.omit(Charls[,variables3])
Charls3=na.omit(Charls[,variables3])
Charls4<-na.omit(Charls[,variables4])%>%filter(Time>0)

##########预分析######################################################################
#无BMI
coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_symptoms+
        rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1kidneye+rw1diabe,data=Charls)
#加上BMI
coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1kidneye+
        rw1diabe,data=subset(Charls,!is.na(rw1bmi)))
summary(glm(status~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
      rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+
      rw1diabe,data=subset(Charls,!is.na(rw1bmi)),family = 'binomial'))

#加上血检
coxph(Surv(Time,status)~urban_nbs+rw1bmi1_group+ragender+rw1educa+depressive_symptoms+
        rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+newglu+cystatinc+newcrp+newglu+
        rw1diabe,data=subset(Charls,!is.na(rw1bmi)))


###缺失情况
summary(Charls[,variables])
summary(Charls[,variables2])
Charls2<-na.omit(Charls[,variables2])
Charls3<-na.omit(Charls[,variables3])

####不填补时的基线表
baseline1<-descrTable(status~.-Time,data=na.omit(Charls[,variables2]),show.all = TRUE,digits = c(urban_nbs=2,
rw1bmi=2,ragender=2,rw1educa=2,rw1mnev=2,Agew1=2,rw1smoke=2,rw1drinke=2,rw1hibpe=2,rw1dyslipe=2,rw1diabe=2,rw1kidneye=2))
export2xls(baseline1,'~/baseline_Charls1.xlsx')
baseline2<-descrTable(depressive_symptoms~.-Time,data=na.omit(Charls[,variables2]),show.all = TRUE,digits = c(urban_nbs=2,
  rw1bmi=2,ragender=2,rw1educa=2,rw1mnev=2,Agew1=2,rw1smoke=2,rw1drinke=2,rw1hibpe=2,rw1dyslipe=2,rw1diabe=2,rw1kidneye=2))
export2xls(baseline2,'~/baseline_Charls1_depressive.xlsx')

#人年
(264/sum(Charls2$Time))*100000#整体
#按抑郁症状分层
with(Charls2,table(depressive_symptoms,status))
(178/sum(Charls2[which(Charls2$depressive_symptoms=="No symptoms"),'Time']))*100000#无抑郁
(86/sum(Charls2[which(Charls2$depressive_symptoms=="Symptoms"),'Time']))*100000#抑郁
#按抑郁得分(四分位数)分层
with(Charls2,table(depressive_group4,status))
(87/sum(Charls2[which(Charls2$depressive_group4=="<6"),'Time']))*100000#抑郁的得分<6
(50/sum(Charls2[which(Charls2$depressive_group4=="6-8"),'Time']))*100000#抑郁得分6-8
(48/sum(Charls2[which(Charls2$depressive_group4=="9-12"),'Time']))*100000#抑郁得分9-12
(79/sum(Charls2[which(Charls2$depressive_group4==">=13"),'Time']))*100000#抑郁得分>=13
##按抑郁得分(四分位数)分层2
with(Charls2,table(depressive_group3,status))
(101/sum(Charls2[which(Charls2$depressive_group3=="<=6"),'Time']))*100000#抑郁的得分<6
(84/sum(Charls2[which(Charls2$depressive_group3=="7-12"),'Time']))*100000#抑郁得分6-8
(79/sum(Charls2[which(Charls2$depressive_group3==">12"),'Time']))*100000#抑郁得分9-12

#抑郁与患癌的风险
#Model1
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~ragender+rw1cesd10+Agew1+urban_nbs)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~ragender+depressive_symptoms+Agew1+urban_nbs)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~ragender+depressive_group4+Agew1+urban_nbs)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~ragender+depressive_group1+Agew1+urban_nbs)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~ragender+depressive_group3+Agew1+urban_nbs)))
#Model2
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+rw1cesd10+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_symptoms+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_group4+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_group1+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_group3+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke)))
#Model3
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1cesd10+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group4+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group1+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group3+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))


##RCS分析
dd<-datadist(Charls2) 
options(datadist='dd')
fit<-cph(Surv(Time,status)~ rcs(rw1cesd10,4)+urban_nbs+rw1bmi+ragender+rw1educa+
             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe,data=Charls2) 
HR<-Predict(fit, rw1cesd10,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=HR, aes(rw1cesd10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR, aes(rw1cesd10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  mytheme+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = " ", x="Depressive Sympptoms Scores", y="Adjusted HR (95%CI)") 
anova(fit)
#分析每个抑郁特征与癌症的风险
vairables3=c("rw1depresl_scale2","rw1effortl_scale2","rw1sleeprl_scale2","rw1whappyl_scale2","rw1flonel_scale2",    
              "rw1goingl_scale2","rw1botherl_scale2","rw1mindtsl_scale2","rw1fhopel_scale2","rw1fearll_scale2")

apply(Charls2[,vairables3],2,table)
apply(Charls2[,vairables3],2,function(x)round(prop.table(table(x))*100,2))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1depresl_scale2+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1effortl_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1sleeprl_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1whappyl_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1flonel_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1goingl_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1botherl_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1mindtsl_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1fhopel_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
with(Charls2,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1fearll_scale2+
                             rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe)))
#亚组分析
#年龄分层
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','ragender','rw1educa','rw1mnev','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,Agew1_group=='<65'))
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','ragender','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,Agew1_group=='>=65'))
with(na.omit(Charls[,variables2]),anova(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+Agew1_group*depressive_symptoms+
                                                  rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','ragender','rw1educa','rw1mnev','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,Agew1<60))
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','ragender','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,Agew1>=60))
#性别
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,ragender=='Female'))
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,ragender=='Male'))
with(na.omit(Charls[,variables2]),anova(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+Agew1+ragender*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#居住地
cox(x=c('depressive_symptoms','rw1bmi','rw1educa','Agew1','ragender',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,urban_nbs=='Urban'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,urban_nbs=='Rural'))
with(na.omit(Charls[,variables2]),anova(coxph(Surv(Time,status)~rw1bmi+rw1educa+Agew1+ragender+urban_nbs*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#教育水平
cox(x=c('depressive_symptoms','rw1bmi','Agew1','ragender','urban_nbs',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1educa2=='<=MIiddled school'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','urban_nbs','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1educa2=='>=high school'))
with(na.omit(Charls[,variables2]),anova(coxph(Surv(Time,status)~rw1bmi+Agew1+ragender+urban_nbs+rw1educa2*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#吸烟
cox(x=c('depressive_symptoms','rw1bmi','Agew1','ragender','urban_nbs','rw1educa',
        'rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1smoke=='Never'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','urban_nbs','Agew1',
        'rw1educa','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1smoke=='Former'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','urban_nbs','Agew1',
        'rw1educa','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1smoke=='Current'))
with(na.omit(Charls[,variables2]),anova(coxph(Surv(Time,status)~rw1bmi+Agew1+ragender+urban_nbs+rw1educa+rw1smoke*depressive_symptoms+
                                                rw1mnev+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#饮酒
cox(x=c('depressive_symptoms','rw1bmi','Agew1','ragender','urban_nbs','rw1educa',
        'rw1smoke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Never'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','urban_nbs','Agew1',
        'rw1educa','rw1smoke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Former'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','urban_nbs','Agew1',
        'rw1educa','rw1smoke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Current'))
with(na.omit(Charls[,variables2]),anova(coxph(Surv(Time,status)~rw1bmi+Agew1+ragender+urban_nbs+rw1educa+rw1smoke+rw1drinke*depressive_symptoms+
                                                rw1mnev+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#饮酒
cox(x=c('depressive_symptoms','rw1bmi','Agew1','ragender','urban_nbs','rw1educa',
        'rw1smoke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Never'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','urban_nbs','Agew1',
        'rw1educa','rw1smoke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Former'))
cox(x=c('depressive_symptoms','ragender','rw1bmi','urban_nbs','Agew1',
        'rw1educa','rw1smoke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1drinke=='Current'))
with(na.omit(Charls[,variables2]),anova(coxph(Surv(Time,status)~rw1bmi+Agew1+ragender+urban_nbs+rw1educa+rw1smoke+rw1drinke*depressive_symptoms+
                                                rw1mnev+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#BMI
cox(x=c('depressive_symptoms','urban_nbs','rw1educa','Agew1','ragender',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1bmi<24))
cox(x=c('depressive_symptoms','ragender','urban_nbs','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1bmi>=24))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi1_group2*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#糖尿病
cox(x=c('depressive_symptoms','urban_nbs','rw1educa','Agew1','ragender','rw1bmi',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe'),y='Surv(Time,status)',data=subset(Charls2,rw1diabe=='Yes'))
cox(x=c('depressive_symptoms','ragender','urban_nbs','rw1educa','Agew1','rw1bmi',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1diabe=='No'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#高血压
cox(x=c('depressive_symptoms','urban_nbs','rw1educa','Agew1','ragender','rw1bmi',
        'rw1smoke','rw1drinke','rw1diabe','rw1dyslipe'),y='Surv(Time,status)',data=subset(Charls2,rw1hibpe=='Yes'))
cox(x=c('depressive_symptoms','ragender','urban_nbs','rw1educa','Agew1','rw1bmi',
        'rw1smoke','rw1drinke','rw1diabe','rw1dyslipe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1hibpe=='No'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe+rw1hibpe*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1dyslipe+rw1diabe+rw1kidneye)))
#血脂异常
cox(x=c('depressive_symptoms','urban_nbs','rw1educa','Agew1','ragender','rw1bmi',
        'rw1smoke','rw1drinke','rw1diabe','rw1hibpe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1dyslipe=='Yes'))
cox(x=c('depressive_symptoms','ragender','urban_nbs','rw1educa','Agew1','rw1bmi',
        'rw1smoke','rw1drinke','rw1diabe','rw1hibpe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2,rw1dyslipe=='No'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe+rw1dyslipe*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1diabe+rw1kidneye)))
#慢性肾病
cox(x=c('depressive_symptoms','urban_nbs','rw1educa','Agew1','ragender','rw1bmi',
        'rw1smoke','rw1drinke','rw1diabe','rw1hibpe','rw1dyslipe'),y='Surv(Time,status)',data=subset(Charls2,rw1kidneye=='Yes'))
cox(x=c('depressive_symptoms','ragender','urban_nbs','rw1educa','Agew1','rw1bmi',
        'rw1smoke','rw1drinke','rw1diabe','rw1hibpe','rw1dyslipe'),y='Surv(Time,status)',data=subset(Charls2,rw1kidneye=='No'))
with(Charls2,anova(coxph(Surv(Time,status)~rw1educa+Agew1+ragender+urban_nbs+rw1bmi+rw1diabe+rw1kidneye*depressive_symptoms+
                           rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1diabe+rw1dyslipe)))

###具体癌症与抑郁的关联
variables_cancer<-c('Brain','Oral','Larynx','Pharynx','Thyroid','Lung','Breast','Oesophagus','Stomach',
                    'Liver','Pancreas','Kidney','Prostate','Testicle','Ovary',
                    'Cervix','Endometrium','ColonRectum','Bladder','Skin',
                    'Lymphoma','Leukemia','Other','cancer_female','Digestive','head_neck','cancer_male')
with(Charls2,fisher.test(table(depressive_symptoms,Brain)))
with(Charls2,table(depressive_symptoms,Oral))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Larynx))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Pharynx))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Thyroid))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Lung))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Breast))%>%chisq.test()
with(Charls2,table(depressive_symptoms,Oesophagus))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Stomach))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Liver))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Pancreas))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Kidney))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Prostate))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Testicle))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Ovary))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Cervix))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Endometrium))%>%fisher.test()
with(Charls2,table(depressive_symptoms,ColonRectum))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Bladder))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Skin))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Lymphoma))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Leukemia))%>%fisher.test()
with(Charls2,table(depressive_symptoms,Other))%>%fisher.test()
with(Charls2,table(depressive_symptoms,cancer_female))%>%chisq.test()
with(Charls2,table(depressive_symptoms,Digestive))%>%chisq.test()
with(Charls2,table(depressive_symptoms,head_neck))%>%chisq.test()

with(subset(Charls2,status==0 | Breast==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,status==0 | Ovary==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                                           rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

with(subset(Charls2,status==0 | Cervix==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                                          rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(subset(Charls2,status==0 | cancer_female==2),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                                           rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))

#女性癌症与抑郁的KM
kms<-survfit(Surv(Time,status)~depressive_symptoms,data=subset(Charls2,status==0 | cancer_female==2))
#survdiff(Surv(DFI.time,DFI)~rs1192691,data=phen_survival_T)
ggsurvplot(kms, data =subset(Charls2,status==0 | cancer_female==2), pval='Log-rank test p<0.01',
                    break.x.by = 1,legend.title="Depression",fun='cumhaz', legend.labs =
                    c("No symptoms",'symptoms'),xlab = "Time in years",pval.coord=c(1,0.02),
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),
                  tables.theme=clean_theme(),title='')


##炎症标志物WBC","Hs_CRP
#
with(na.omit(Charls[,variables2]),summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1cesd10+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
with(Charls4,summary(coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1cesd10+Hs_CRP+
                                                  rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
##中介效应分析
Y<-survreg(Surv(Time,status)~Hs_CRP+rw1cesd10+urban_nbs+rw1bmi+ragender+rw1educa+rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye,data=Charls4)
M<-lm(rw1cesd10~Hs_CRP+urban_nbs+rw1bmi+ragender+rw1educa+rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye,data=Charls4)
cont<-mediate(M,Y,sims=50,treat='rw1cesd10',mediator='Hs_CRP')
summary(cont)
plot(cont)
##女性人群
Charls2_Female<-na.omit(Charls[which(Charls$ragender=='Female'),c('ID',variables2)])
Charls2_Female2<-left_join(Charls2_Female,Charls[,c('ID','Menarche_group','Menopause','scales_group','scales','AMenopause')],by='ID')

#基线
baseline3<-descrTable(depressive_symptoms~rw1bmi+rw1educa+rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye,data=Charls2_Female,show.all = TRUE,digits = c(urban_nbs=2,
rw1bmi=2,rw1educa=2,rw1mnev=2,Agew1=2,rw1smoke=2,rw1drinke=2,rw1hibpe=2,rw1dyslipe=2,rw1diabe=2,rw1kidneye=2))
export2xls(baseline3,'~/baseline_Charls1_depressive_female.xlsx')

baseline4<-descrTable(depressive_symptoms~rw1bmi+rw1educa+rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye+Menarche_group+Menopause+scales_group+scales+AMenopause,data=Charls2_Female2,show.all = TRUE,digits = c(urban_nbs=2,
 rw1bmi=2,rw1educa=2,rw1mnev=2,Agew1=2,rw1smoke=2,rw1drinke=2,rw1hibpe=2,rw1dyslipe=2,rw1diabe=2,rw1kidneye=2,Menarche_group=2,Menopause=2,scales_group=2,scales=2,AMenopause=2))
export2xls(baseline4,'~/baseline_Charls1_depressive_female2.xlsx')

#绝经状态
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2_Female2,Menopause=='No'))

cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2_Female2,Menopause=='Yes'))
with(Charls2_Female2,anova(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+Agew1+Menopause*depressive_symptoms+
                                                rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#激素影响时间
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2_Female2,scales_group=='<=33'))
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2_Female2,scales_group=='>33'))
with(Charls2_Female2,anova(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+Agew1+scales_group*depressive_symptoms+
                                 rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))
#初潮年龄分层
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2_Female2,Menarche_group=='<16'))
cox(x=c('depressive_symptoms','urban_nbs','rw1bmi','rw1educa','Agew1',
        'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye'),y='Surv(Time,status)',data=subset(Charls2_Female2,Menarche_group=='>=16'))
with(Charls2_Female2,anova(coxph(Surv(Time,status)~urban_nbs+rw1bmi+rw1educa+Agew1+Menarche_group*depressive_symptoms+
                                 rw1mnev+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe+rw1kidneye)))



###################################################################################################
#MICE缺失值填补分析
variables2<-c('urban_nbs','rw1bmi','ragender','rw1educa','rw1mnev','Agew1','status','Time','depressive_symptoms','rw1cesd10','depressive_group4',
              
              'rw1smoke','rw1drinke','rw1hibpe','rw1dyslipe','rw1diabe','rw1kidneye')
Charls_imput<-mice(Charls[,variables2],m=5,seed=6666)
summary(Charls_imput)
Charls_imput3<-complete(Charls_imput,1)

##填补时的基线表
baseline2<-descrTable(status~.-Time,data=Charls_imput3[,variables2],show.all = TRUE,digits = c(urban_nbs=2,
                                                                                               rw1bmi=2,ragender=2,rw1educa=2,rw1mnev=2,Agew1=2,rw1smoke=2,rw1drinke=2,rw1hibpe=2,rw1dyslipe=2,rw1diabe=2,rw1kidneye=2))
export2xls(baseline1,'~/baseline_Charls2.xlsx')


#抑郁与患癌的风险
#Model1
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~ragender+rw1cesd10+Agew1+urban_nbs))
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~ragender+depressive_symptoms+Agew1+urban_nbs))
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~ragender+depressive_group4+Agew1+urban_nbs))
#Model2
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+rw1cesd10+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke))
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_symptoms+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke))
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~urban_nbs+ragender+rw1educa+depressive_group4+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke))

#Model3
imp<-with(Charls_imput,coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+rw1cesd10+
                               rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe))
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_symptoms+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe))
with(na.omit(Charls[,variables2]),coxph(Surv(Time,status)~urban_nbs+rw1bmi+ragender+rw1educa+depressive_group4+
                                          rw1mnev+Agew1+rw1smoke+rw1drinke+rw1hibpe+rw1dyslipe+rw1diabe))


