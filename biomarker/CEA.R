rm(list=ls())
library(ggplot)
library(ggpubr)
library(boot)
#读取数据
source('~/Rcode/biomarker/data.R')
biomarker2<-biomarker%>%filter(!is.na(CEA))
biomarker3<-biomarker2%>%filter(AFP<=quantile(CEA,0.75)+IQR(CEA) & CEA>=quantile(CEA,0.25)-IQR(CEA))
##频率分布----------------------------------------------------------------------------------------------
#频率计算
ggplot(data=biomarker2,aes(x=CEA))+geom_histogram(binwidth = 1)+coord_cartesian(ylim=c(0,5))+scale_x_continuous(breaks=c(0,10,20,30,50,100,200,400,600,800))
ggplot(data=biomarker2,aes(x=CEA))+geom_histogram(binwidth = 1)
p1<-biomarker2%>%filter(CEA<=30)%>%
ggplot(aes(x=CEA))+geom_histogram(aes(y=..density..),fill='grey',color='black',binwidth = 0.2)+
  stat_function(fun = dnorm, args = list(mean = mean(biomarker2$CEA),sd = sd(biomarker2$CEA)),color='red')+theme_bw()+
  geom_vline(xintercept = mean(biomarker2$CEA),colour='red',linetype=2)
p1+geom_text(aes(x=mean(biomarker2$CEA),label=round(mean(biomarker2$CEA),2),y=median(x=layer_scales(p1)$y$range$range)),nudge_x = 1)
##CEA_pos
biomarker2%>%group_by(CEA_pos)%>%summarise(n=n())%>%transmute(CEA=CEA_pos,n=n,p=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggpie(x='n',label = 'p',fill='CEA',lab.pos = 'out',color='white',palette = c("#00AFBB", "#E7B800"))+
  theme(legend.position = 'right')
#基线变量的基本分布
CEA_baseline<-biomarker2%>%
  transmute(Sex=factor(sex,label=c('Man','Woman')),
            Age=factor(Age_G1,labels = c('<=44','45-49','50-54','55-59','60-64','65-69','>=70')),
             BMI=bmi,
             smoking=factor(smoking,labels=c('Never','Current','Ago')),
             alcohol=factor(alcohol,labels=c('No','Yes')),passivesmk=factor(passivesmk,labels=c('No','Yes')),
            CEA=CEA,CEA_pos=CEA_pos
             )

table1::table1(~Sex+Age+BMI+smoking+alcohol | CEA_pos,data=CEA_baseline,render.continuous=c(.="Median [Q1,Q3]"))
#--------------------------------------------------------------------------------------------------
###
biomarker2%>%select(bmi_group3,sex,CEA)%>%transmute(
  bmi=factor(bmi_group3,labels=c('Lower','Normal','Obesity')),
  sex=factor(sex,labels=c('man','woman')),
  CEA=log(CEA))%>%filter(!is.na(bmi))%>%
  grouped_ggbetweenstats(
    x = bmi,
    y =CEA ,
    ylab='log(CEA)',
    xlab='BMI',
    k=2,
    nboot = 10,
    grouping.var = sex,
    effsize.type = "unbiased", 
    messages = FALSE,
    pairwise.comparisons = TRUE, # display results from pairwise comparisons
    pairwise.display = "significant", # display only significant pairwise comparisons
    pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  ) 
library(effects)
biomarker2$CEA.log<-log(biomarker2$CEA)
biomarker2%>%filter(!is.na(bmi_group3))%>%group_by(bmi_group3)%>%summarise(mean=mean(CEA.log,na.rm = TRUE),sd=(sd(CEA.log,na.rm = TRUE))/sqrt(length(CEA.log)))
biomarker2$bmi_group3<-factor(biomarker2$bmi_group3)
fit<-aov(log(CEA)~bmi_group3,data=biomarker2)
summary(fit)
data.frame(effect('bmi_group3',fit))
###疾病史

