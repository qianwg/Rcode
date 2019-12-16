rm(list=ls())
library(rio)
library(openxlsx)
library(ggpubr)
library(table1)
library(tidyverse)
library(fmsb)
biomarker<-read.xlsx('~/data/biomarker/Biomarker+baseline(2017+18+19).xlsx',detectDates=TRUE)
#CEA ADN afp and ca199 --1000 =   >1000
#CA153 300---=>300
#PG1 ---200 =   >200
#PG2 ---100 =   >100
#PGR ---90  =   >3
#-----------------------------------------------变量操作-----------------------------------------------
#age
biomarker$age_group[biomarker$age>=40 & biomarker$age<45]<-1
biomarker$age_group[biomarker$age>=45 & biomarker$age<50]<-2
biomarker$age_group[biomarker$age>=50 & biomarker$age<55]<-3
biomarker$age_group[biomarker$age>=55 & biomarker$age<60]<-4
biomarker$age_group[biomarker$age>=60 & biomarker$age<65]<-5
biomarker$age_group[biomarker$age>=65 & biomarker$age<70]<-6
biomarker$age_group[biomarker$age>=70]<-7
biomarker$age_group2[biomarker$age>=40 & biomarker$age<50]<-1
biomarker$age_group2[biomarker$age>=50 & biomarker$age<60]<-2
biomarker$age_group2[biomarker$age>=60 & biomarker$age<70]<-3
biomarker$age_group2[biomarker$age>=70]<-4
#baonian
biomarker$baonian<-(biomarker$cpd*biomarker$smkyrs)/20
#---BMI
biomarker$bmi<-with(biomarker,weight/((height/100)^2))
biomarker$bmi_group<-with(biomarker,case_when(
      bmi<18.5 ~ 1,
      between(bmi,18.5,23.9) ~ 2,
      between(bmi,24,27.9) ~ 3,
      bmi>=28 ~ 4
))
biomarker$bmi_group2<-with(biomarker,ifelse(bmi<25,1,2))
#BMI极端值查看
summary(biomarker$bmi)
gghistogram(biomarker,x='bmi',bins=30)+coord_cartesian(ylim=c(0,50))
biomarker%>%select(ID_BLAST,bmi,bmi_group)%>%transmute(id=ID_BLAST,bmi=bmi,bmi_group,
                      is_outlier=ifelse(is_outlier(bmi),bmi,as.numeric(NA)))%>%
  transmute(id=ifelse(is.na(is_outlier),NA,id),bmi=bmi,bmi_group=factor(bmi_group),is_outlier)%>%filter(!is.na(bmi_group))%>%
  ggboxplot(x='bmi_group',y='bmi',fill='bmi_group',palette = 'jco')+geom_text(aes(label=id),nudge_y = 0.05,na.rm = TRUE)+theme(legend.position = 'none')
biomarker%>%select(ID_BLAST,bmi,bmi_group)%>%transmute(id=ID_BLAST,bmi=bmi,bmi_group,
    is_outlier=ifelse(is_outlier(bmi),bmi,as.numeric(NA)))%>%
  transmute(id=ifelse(is.na(is_outlier),NA,id),bmi=bmi,bmi_group=factor(bmi_group),is_outlier)%>%filter(!is.na(bmi_group))%>%
group_by(bmi_group)%>%summarise(na_n=sum(!is.na(is_outlier)))

##------------------------------CEA/AFP/CA125/CA153/CA199节点值分布---------------------------------------
str(biomarker[,c('AFP','CA199','CA125','CEA','CA153')])
risk<-function(x){
  x2<-factor(x,levels = c(0,1,2,3,4),labels=c('正常','超出截值1-2倍','超出截值2-3倍','超出截值3-4倍','超出截值4倍'))
  return(x2)
}
biomarker0<-within(biomarker,{
  CEA.risk<-vector()
  AFP.risk<-vector()
  CA199.risk<-vector()
  CA153.risk<-vector()
  CA125.risk<-vector()
  #CEA
  CEA.risk[CEA<=5]<-0
  CEA.risk[CEA>5 & CEA<=10]<-1
  CEA.risk[CEA>10 & CEA<=15]<-2
  CEA.risk[CEA>15 & CEA<=20]<-3
  CEA.risk[CEA>20]<-4
  #AFP
  AFP.risk[AFP<=7]<-0
  AFP.risk[AFP>7 & AFP<=14]<-1
  AFP.risk[AFP>14 & AFP<=21]<-2
  AFP.risk[AFP>21 & AFP<=28]<-3
  AFP.risk[AFP>28]<-4
  #CA199
  CA199.risk[CA199<=27]<-0
  CA199.risk[CA199>27 & CA199<=27*2]<-1
  CA199.risk[CA199>27*2 & CA199<=27*3]<-2
  CA199.risk[CA199>27*3 & CA199<=27*4]<-3
  CA199.risk[CA199>27*4]<-4
  #CA153
  CA153.risk[CA153<=25]<-0
  CA153.risk[CA153>25 & CA153<=50]<-1
  CA153.risk[CA153>50 & CA153<=75]<-2
  CA153.risk[CA153>75 & CA153<=100]<-3
  CA153.risk[CA153>100 ]<-4
  #CA125
  CA125.risk[CA125<=35]<-0
  CA125.risk[CA125>35 & CA125<=70]<-1
  CA125.risk[CA125>70 & CA125<=105]<-2
  CA125.risk[CA125>105 & CA125<=140]<-3
  CA125.risk[CA125>140]<-4
})
biomarker1<-biomarker0%>%filter(CASelf!=2 | is.na(CASelf))
biomarker1[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')]<-apply(biomarker1[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,risk)
table_risk<-as.data.frame(apply(biomarker1[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,table))
table_risk1<-rbind(apply(table_risk,2,sum),table_risk)
table_prop<-list()
for(i in 1:5){
  table_prop[[i]]<-round(table_risk1[,i]/table_risk1[1,i],4)*100
}
table_prop1<-data.frame(t(do.call(rbind,table_prop)))
table<-list()
for(i in 1:5){
  table[[i]]<-paste(table_risk1[,i],'(',table_prop1[,i],'%)',sep='')
}
table2<-data.frame(t(do.call(rbind,table)))
table3<-cbind(c('完成人数','<1倍上限','1-2倍上限',
                '2-3倍上限','3-4倍上限','>4倍上限'),table2)
names(table3)<-c('指标','CEA','AFP','CA199','CA153(女性)','CA125(女性)')
ggtable<-ggtexttable(table3,rows=NULL,theme = ttheme('mBlue'))
ggtable
#----------------------------------基线表----------------------------------------------------
biomark_baseline<-biomarker%>%filter(CASelf!=2 | is.na(CASelf))%>%select(Age_G1,sex,BMI_G,
                   smoking,alcohol,CA199,CEA,CA125,CA153,AFP,marriag_G,educati_G)%>%
  transmute(Age=factor(Age_G1,labels = c('<=44','45-49','50-54','55-59','60-64','65-69','>=70')),
            Sex=factor(sex,label=c('Man','Woman')),
            BMI=factor(BMI_G,labels=c('18.5-23.9','<18.5','24-27.9','>=28')),
            smoking=factor(smoking,labels=c('Never','Current','Ago')),
            alcohol=factor(alcohol,labels=c('No','Yes')),
            marriag=factor(marriag_G,labels=c('unmarried','married','separation/divorced/widowed')),
            education=factor(educati_G,labels=c('Unletterd','primary','junior','senior','college')),
            CA199=CA199,CEA=CEA,CA125=CA125,CA153=CA153,AFP=AFP)%>%
  pivot_longer(cols = c('CEA','AFP','CA199','CA153','CA125'),names_to = 'biomarker',values_to = 'value')
biomark_baseline$biomarker<-ifelse(is.na(biomark_baseline$value),NA,biomark_baseline$biomarker)
strata<-c(split(biomark_baseline,biomark_baseline$biomarker),list(Overall=biomark_baseline))
labels<-list(
  variables=list(
    Age='age',
    Sex='Sex',
    BMI='BMI',
    smoking='Smoking',
    alcohol='Alcohol'),
  groups=list("tumor marker",'')
)

print(table1(strata,labels,groupspan = c(5,1),render.continuous=c(.="Median [Q1,Q3]")))
##--------------------------------------------------CEA--------------------------------------------------------------------------------------------
biomark_baseline1<-biomarker1%>%filter(CASelf!=2 | is.na(CASelf))%>%select(age,sex,BMI,
             smoking,alcohol,CA199,CEA,CA125,CA153,AFP,marriag_G,educati_G)%>%
  transmute(Age=age,
            Sex=factor(sex,label=c('Man','Woman')),
            BMI=BMI,
            smoking=factor(smoking,labels=c('Never','Current','Ago')),
            alcohol=factor(alcohol,labels=c('No','Yes')),
            marriag=factor(marriag_G,labels=c('unmarried','married','separation/divorced/widowed')),
            education=factor(educati_G,labels=c('Unletterd','primary','junior','senior','college')),
            CA199=CA199,CEA,
            CA125=CA125,CA153=CA153,AFP=AFP)
biomark_baseline1$CEA.risk[biomark_baseline1$CEA<=5]<-0
biomark_baseline1$CEA.risk[biomark_baseline1$CEA>5 & biomark_baseline1$CEA<=10]<-1
biomark_baseline1$CEA.risk[biomark_baseline1$CEA>10 & biomark_baseline1$CEA<=15]<-2
biomark_baseline1$CEA.risk[biomark_baseline1$CEA>15 & biomark_baseline1$CEA<=20]<-3
biomark_baseline1$CEA.risk[biomark_baseline1$CEA>20]<-4
biomark_baseline1$CEA.risk=factor(biomark_baseline1$CEA.risk,levels = c(0,1,2,3,4,5),labels=c('正常','超出截值1-2倍','超出截值2-3倍',
                                                           '超出截值3-4倍','超出截值4倍','P-value'))
table(biomark_baseline1$CEA.risk)
###table2(add a column of p-value)
rndr <- function(x, name, ...){
  if (length(x) == 0) {
    y <- biomark_baseline1[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- kruskal.test(y ~ biomark_baseline1$CEA.risk)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(biomark_baseline1$CEA.risk)))$p.value
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}
rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}

table1(~Age+Sex+smoking+alcohol+BMI+CA199+AFP+marriag+education | CEA.risk,render.continuous=c(.="Median [Q1,Q3]"),
       data=biomark_baseline1,overall=F,render = rndr,render.strat=rndr.strat,droplevels = F) 
#---------------------------------------------分布图-----------------------------------------------------------------------------------------------
##去除异常值

#---------------------------------------------SEX----------------------------------------------
#boxplot
biomarker%>%filter(!is.na(sex))%>%select(sex,CEA,AFP,CA199)%>%transmute(sex=factor(sex,labels=c('Man','Woman')),CEA=CEA,CA199=CA199,AFP=AFP)%>%
  pivot_longer(cols=c('CEA','CA199','AFP'),names_to = 'biomarker',values_to = 'value')%>%
  ggboxplot(x='sex',y='value',add='jitter',facet.by = 'biomarker',add.params = list(size=0.4,alpha=0.4),color='sex',palette = 'jco')+
  stat_compare_means(method = 'wilcox.test',label='p.signif',label.x=1.5,size=5)+
  yscale('log2',.format = TRUE)+theme(legend.position = 'none')
biomarker_sex<-biomarker%>%filter(!is.na(sex))%>%select(sex,CEA,AFP,CA199)%>%transmute(sex=factor(sex,levels=c(1,2,3),labels=c('Man','Woman','P-value')),CEA=CEA,CA199=CA199,AFP=AFP)
rndr_sex <- function(x, name, ...){
  if (length(x) == 0) {
    y <- biomarker_sex[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      #p <- kruskal.test(y ~ biomark_baseline1$CEA.risk)$p.value
      p <- wilcox.test(y ~ biomarker_sex$sex)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(biomarker_sex$sex)))$p.value
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}
rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}
table1(~AFP+CA199+CEA | sex ,data=biomarker_sex,render.continuous=c(.="Median [Q1,Q3]"),
overall=F,render = rndr_sex,render.strat=rndr.strat,droplevels = F)
#2性别对于阴阳性的影响
biomarker_baseline2<-biomarker%>%select(CEA,AFP,CA199,sex)%>%transmute(
  CEA=factor(ifelse(CEA<=5,1,2),labels=c('Negative','Positive')),
  AFP=factor(ifelse(AFP<=7,1,2),labels=c('Negative','Positive')),
  CA199=factor(ifelse(CA199<=27,1,2),labels=c('Negative','Positive')),
  sex=factor(sex,levels=c(1,2,3),labels = c('man','woman','P-value'))
)
head(biomarker_baseline2)
rndr_sex2 <- function(x, name, ...){
  if (length(x) == 0) {
    y <- biomarker_baseline2[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      #p <- kruskal.test(y ~ biomark_baseline1$CEA.risk)$p.value
      p <- wilcox.test(y ~ biomarker_baseline2$sex)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(biomarker_baseline2$sex)))$p.value
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name,...)
  }
}
table1(~AFP+CA199+CEA | sex ,data=biomarker_baseline2,
       overall=F,render = rndr_sex2,render.strat=rndr.strat,droplevels = F)
biomarker%>%filter(!is.na(sex) & !is.na(AFP) &!is.na(CEA) &!is.na(CA199))%>%select(CEA,AFP,CA199,sex)%>%transmute(
  CEA=factor(ifelse(CEA<=5,1,2),labels=c('Negative','Positive')),
  AFP=factor(ifelse(AFP<=7,1,2),labels=c('Negative','Positive')),
  CA199=factor(ifelse(CA199<=27,1,2),labels=c('Negative','Positive')),
  sex=factor(sex,levels=c(1,2),labels = c('man','woman'))
)%>%pivot_longer(cols=c('CEA','AFP','CA199'),names_to = 'biomarker',values_to = 'levels')%>%
  ggplot(aes(x=sex,fill=levels))+geom_bar(position = 'fill')+facet_wrap(~biomarker,nrow=1)+theme_bw()+
labs(y='density')+theme(legend.title = element_blank())
###-------------------------------------Age-----------------------------------------------

biomarker%>%select(sex,age,CEA,AFP,CA199,CA125,CA153)%>%
  transmute(sex=factor(sex,labels=c('Man','Woman')),age=age,CEA=CEA,CA199=CA199,AFP=AFP,CA153=CA153,CA125=CA125)%>%
  pivot_longer(cols=c('CEA','CA199','AFP','CA125','CA153'),names_to = 'biomarker',values_to = 'value')%>%
  ggscatter(x='age',y='value',add='reg.line',facet.by = 'biomarker',color='sex',add.params = list(color='red'))+
  stat_cor(method='spearman')+yscale('log2',.format = TRUE)
##(1)CA125
#scatter
ggscatter(data=biomarker,x='age',y='CA125',add='reg.line',add.params = list(color='red'))+
  stat_cor(method='spearman')+yscale('log2',.format = TRUE)
#boxplot
ggboxplot(data=subset(biomarker,age>=40 & age<=74),x='age',y='CA125')+yscale('log2',.format = TRUE)
##baseline
biomarker_baseline4<-biomarker%>%select(CEA,AFP,CA199,sex,CA125,CA153,age,age_group)%>%transmute(
  age=age,age_group=age_group,CA125,CA153,
  CEA=CEA,
  AFP=AFP,
  CA199=CA199,
  sex=factor(sex,levels=c(1,2,3),labels = c('man','woman','P-value'))
)
rndr_sex3 <- function(x, name, ...){
  if (length(x) == 0) {
    y <- biomarker_baseline4[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      #p <- kruskal.test(y ~ biomark_baseline1$CEA.risk)$p.value
      #p <- wilcox.test(y ~ biomarker_baseline2$sex)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
      p<-summary(lm(y~sex+age,data=biomarker_baseline4))$coefficients[3,4]
      } else {
      p <- chisq.test(table(y, droplevels(biomarker_baseline4$sex)))$p.value
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name,...)
  }
}
#矫正了年龄或性别后
table1(~AFP+CA199+CEA | sex ,data=biomarker_baseline4,
       overall=F,render = rndr_sex3,render.strat=rndr.strat,droplevels = F)
###
library(dplyr)
#cea
table.cea1<-biomarker_baseline4%>%filter(!is.na(age_group))%>%group_by(sex,age_group)%>%
  summarise(median=median(CEA,na.rm = TRUE),p25=quantile(CEA,0.25,na.rm=T),p75=quantile(CEA,0.75,na.rm=T))
table.cea2<-biomarker_baseline4%>%filter(!is.na(age_group))%>%group_by(age_group)%>%
  summarise(median=median(CEA,na.rm = TRUE),p25=quantile(CEA,0.25,na.rm=T),p75=quantile(CEA,0.75,na.rm=T))%>%
transmute(sex=factor(c(rep(1,7)),labels=('all')),
          age_group=age_group,median=median,p25=p25,p75=p75)  
table.cea<-bind_rows(table.cea1,table.cea2)  
plot.cea<-table.cea%>%pivot_longer(cols=c('median','p25','p75'),names_to = 'stat',values_to = 'value')%>%
  ggline(x='age_group',y='value',color='stat',palette = 'jco',facet.by = 'sex',ylab='CEA',xlab='')+theme(legend.position = 'none')+
scale_x_discrete(labels=c('40-44','45-49','50-54','55-59','60-64','65-69','70-74'))  
plot.cea
#afp
table.afp1<-biomarker_baseline4%>%filter(!is.na(age_group))%>%group_by(sex,age_group)%>%
  summarise(median=median(AFP,na.rm = TRUE),p25=quantile(AFP,0.25,na.rm=T),p75=quantile(AFP,0.75,na.rm=T))
table.afp2<-biomarker_baseline4%>%filter(!is.na(age_group))%>%group_by(age_group)%>%
  summarise(median=median(AFP,na.rm = TRUE),p25=quantile(AFP,0.25,na.rm=T),p75=quantile(AFP,0.75,na.rm=T))%>%
  transmute(sex=factor(c(rep(1,7)),labels=('all')),
            age_group=age_group,median=median,p25=p25,p75=p75)  
table.afp<-bind_rows(table.afp1,table.afp2)  
plot.afp<-table.afp%>%pivot_longer(cols=c('median','p25','p75'),names_to = 'stat',values_to = 'value')%>%
  ggline(x='age_group',y='value',color='stat',palette = 'jco',facet.by = 'sex',ylab='AFP',xlab='')+theme(legend.position = 'none')+
  scale_x_discrete(labels=c('40-44','45-49','50-54','55-59','60-64','65-69','70-74'))+stat_cor(method='spearman')  
 plot.afp
#ca199
table.ca1991<-biomarker_baseline4%>%filter(!is.na(age_group))%>%group_by(sex,age_group)%>%
  summarise(median=median(CA199,na.rm = TRUE),p25=quantile(CA199,0.25,na.rm=T),p75=quantile(CA199,0.75,na.rm=T))
table.ca1992<-biomarker_baseline4%>%filter(!is.na(age_group))%>%group_by(age_group)%>%
  summarise(median=median(CA199,na.rm = TRUE),p25=quantile(CA199,0.25,na.rm=T),p75=quantile(CA199,0.75,na.rm=T))%>%
  transmute(sex=factor(c(rep(1,7)),labels=('all')),
            age_group=age_group,median=median,p25=p25,p75=p75)  
table.ca199<-bind_rows(table.afp1,table.afp2)  
plot.ca199<-table.ca199%>%pivot_longer(cols=c('median','p25','p75'),names_to = 'stat',values_to = 'value')%>%
  ggline(x='age_group',y='value',color='stat',palette = 'jco',facet.by = 'sex',ylab='CA199')+theme(legend.position = 'none')+
  scale_x_discrete(labels=c('40-44','45-49','50-54','55-59','60-64','65-69','70-74'))  

ggarrange(plot.cea,plot.afp,plot.ca199,nrow=3)
#ca125
table.ca125<-biomarker_baseline4%>%filter(!is.na(age_group) , sex=='woman')%>%group_by(age_group)%>%
  summarise(median=median(CA125,na.rm = TRUE),p25=quantile(CA125,0.25,na.rm=T),p75=quantile(CA125,0.75,na.rm=T))%>%
  transmute(age_group=age_group,median=median,p25=p25,p75=p75)%>%
  pivot_longer(cols=c('median','p25','p75'),names_to = 'stat',values_to = 'value')%>%
  ggline(x='age_group',y='value',color='stat',palette = 'jco',ylab='CA125',xlab='')+theme(legend.position = 'none')+
  scale_x_discrete(labels=c('40-44','45-49','50-54','55-59','60-64','65-69','70-74'))+border()  
table.ca153<-biomarker_baseline4%>%filter(!is.na(age_group) , sex=='woman')%>%group_by(age_group)%>%
  summarise(median=median(CA153,na.rm = TRUE),p25=quantile(CA153,0.25,na.rm=T),p75=quantile(CA153,0.75,na.rm=T))%>%
  transmute(age_group=age_group,median=median,p25=p25,p75=p75)%>%
  pivot_longer(cols=c('median','p25','p75'),names_to = 'stat',values_to = 'value')%>%
  ggline(x='age_group',y='value',color='stat',palette = 'jco',ylab='CA153',xlab='age_group')+theme(legend.position = 'none')+
  scale_x_discrete(labels=c('40-44','45-49','50-54','55-59','60-64','65-69','70-74'))+border()  
ggarrange(table.ca125,table.ca153,nrow=2)
###
library(Kendall)
library(coin)
summary(Kendall(biomarker_baseline4$age_group,biomarker_baseline4$CEA))
with(data=subset(biomarker_baseline4,sex=='woman'),summary(Kendall(age_group,CEA)))
with(data=subset(biomarker_baseline4,sex=='man'),summary(Kendall(age_group,CEA)))
cor.test(biomarker_baseline4$age,biomarker_baseline4$AFP,use='complete.obs',method='spearman')
summary(Kendall(biomarker_baseline4$age_group,biomarker_baseline4$AFP))
with(data=subset(biomarker_baseline4,sex=='woman'),summary(Kendall(age_group,AFP)))
with(data=subset(biomarker_baseline4,sex=='man'),summary(Kendall(age_group,AFP)))
summary(Kendall(biomarker_baseline4$age_group,biomarker_baseline4$CA199))
with(data=subset(biomarker_baseline4,sex=='woman'),summary(Kendall(age_group,CA199)))
with(data=subset(biomarker_baseline4,sex=='man'),summary(Kendall(age_group,CA199)))

with(data=subset(biomarker_baseline4,sex=='woman'),summary(Kendall(age_group,CA125)))
with(data=subset(biomarker_baseline4,sex=='woman'),summary(Kendall(age_group,CA153)))
#----------------------------------------------------------------------------
biomarker_baseline4%>%select(CEA,AFP,CA199,CA125,CA153)%>%pivot_longer(cols=c('CEA','CA199','CA125','CA153','AFP'),names_to = 'marker',values_to = 'value')%>%
ggplot(aes(x=value))+geom_histogram(aes(y=..density..),bins=30,color='black')+facet_wrap(marker~.,nrow=2,scales = 'free')+scale_x_continuous(limits=c(-10,quantile(hist$value,0.99,na.rm=T)))+
  stat_function(fun = dnorm, args = list(mean = mean(hist$value,na.rm = T),sd = sd(hist$value,na.rm = T)),color='red')+labs(x='')
library(moments)##D’Agostino-Pearson test/正态性检验
agostino.test(log(biomarker$CA125))
agostino.test(biomarker$CA199)
agostino.test(biomarker$CA153)
agostino.test(biomarker$CEA)
agostino.test(biomarker$AFP)
#------------------------------------------肿瘤标志物在吸烟状态间的分布-------------------------------------
biomarker_baseline6<-biomarker%>%select(AFP,CA125,CA153,CEA,CA199,smoking,cpd,smkyrs)%>%
  transmute(smoking=factor(smoking,labels=c('Never','Current','Ago')),
            baonian=(cpd*smkyrs)/20,
            CEA=CEA,AFP=AFP,CA199=CA199,CA153=CA153,CA125=CA125)

biomarker_baseline6%>%pivot_longer(cols=c('CEA','AFP','CA199','CA125','CA153'),names_to = 'marker',values_to = 'value')%>%
  transmute(smoking=smoking,baonian=baonian,marker=marker,value=log(value))%>%filter(!is.na(smoking))%>%
  ggboxplot(x='smoking',y='value')+facet_wrap(marker~.,scales='free')+border()
#the CEA levels in population studied
biomarker%>%select(smoking,sex,baonian,age)%>%filter(!is.na(smoking))%>%group_by(smoking,sex)%>%
  summarise(n=n(),age=median(age,na.rm=T),age_25=quantile(age,0.25,na.rm = T),age_75=quantile(age,0.75,na.rm = T),
           pack=median(baonian,na.rm = T),pack_25=quantile(pack,0.25,na.rm = T),pack_75=quantile(pack,0.75,na.rm = T))


#(1)CEA
biomarker$CEA.group<-ifelse(biomarker$CEA<=5,1,2)
biomarker.smoking<-biomarker%>% filter(!is.na(smoking))%>%select(smoking,CEA,sex,CEA.group,age,age_group2)%>%transmute(
                                                smoking=factor(smoking,levels=c(1,2,3,4),labels=c('Never','Current','Ago','P-value')),
                                                CEA=log(CEA),sex=factor(sex,labels=c('man','woman')),
                                                CEA.group=factor(CEA.group,labels=c('negative','positive')),
                                                age.group=factor(age_group2),age=age)
 
head(biomarker.smoking)
biomarker.smoking%>%ggboxplot(x='smoking',y='CEA',add='jitter',add.params = list(size=0.1,alpha=0.4),ylab='log(CEA)')+
  stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='p.signif'
                     )+stat_compare_means(label.y=10)+border()
rndr_smoking <- function(x, name, ...){
  if (length(x) == 0) {
    y <- biomarker.smoking[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
       p <- kruskal.test(y ~ biomarker.smoking$smoking)$p.value
      #p <- wilcox.test(y ~ biomarker_baseline2$sex)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
      #p<-summary(lm(y~sex+age,data=biomarker_baseline4))$coefficients[3,4]
    } else {
      p <- chisq.test(table(y, droplevels(biomarker.smoking$smoking)))$p.value
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name,...)
  }
}
table1(~CEA+CEA.group| smoking,data=biomarker.smoking,render.continuous=c(.="Median [Q1,Q3]"),
       overall=F,render = rndr_smoking,render.strat=rndr.strat,droplevels = F)

#CEA and baonian(the relationship)
biomarker_baonian<-biomarker%>%select(CEA,cpd,smkyrs,smoking,baonian,age,age_group,age_group2)%>%filter(smoking==2)%>%transmute(
  CEA=CEA,CEA2=ifelse(CEA>quantile(CEA,na.rm=T)[4]+IQR(CEA,na.rm = T)*1.5,NA,CEA),age=age,age_group=factor(age_group),age_group2=age_group2,
  baonian=baonian,baonian2=ifelse(baonian>quantile(baonian,na.rm=T)[4]+IQR(baonian,na.rm = T)*1.5,NA,baonian),
  baonian3=case_when(
    baonian<=10 ~ 1,
    between(baonian,11,20) ~ 2,
    between(baonian,21,30) ~ 3,
    between(baonian,31,40) ~ 4,
    between(baonian,41,50) ~ 5,
    baonian>50 ~ 6
  ),
  CEA3=factor(ifelse(CEA<=5,1,2),labels = c('Negative','Positive'))
)
plot.baonian1<-biomarker_baonian%>%ggscatter(x='baonian',y='CEA',add='reg.line',
      xlab='baonian',ylab='CEA',add.params = list(color='red'))+border()+
  stat_cor(method='spearman')
plot.baonian2<-biomarker_baonian%>%ggscatter(x='baonian2',y='CEA2',add='reg.line',
    add.params = list(color='red'),xlab='baonian',ylab='CEA')+border()+
stat_cor(method='spearman',label.y=7.5)
plot.baonian3<-biomarker_baonian%>%filter(!is.na(baonian3))%>%ggviolin(x='baonian3',y='CEA2',add='boxplot',add.params = list(fill='white'),
palette ='jco',fill='baonian3',xlab='baonian',ylab = 'CEA')+border()+
  theme(legend.position = 'none')+scale_x_discrete(labels=c('<10','-20','-30','-40','-50','>50'))
plot.baonian4<-biomarker_baonian%>%filter(!is.na(CEA3) & !is.na(baonian3))%>%
  transmute(CEA3=CEA3,baonian3=factor(baonian3,labels=c('<10','-20','-30','-40','-50','>50')))%>%
  ggplot(aes(x=baonian3))+geom_bar(aes(fill=CEA3),position = 'fill')+border()+
  theme(legend.title=element_blank(),legend.position = 'top')+labs(x='baonian',y='percent')
ggarrange(plot.baonian2,plot.baonian3,plot.baonian4,nrow=1)
#吸烟状态与年龄对CEA的影响
biomarker%>%transmute(age=age,CEA=log(CEA),smoking=factor(smoking,labels=c('Never','Current','Ago')))%>%filter(!is.na(smoking) & !is.na(age))%>%
  ggscatter(x='age',y='CEA',color='smoking',palette = 'jco',add='reg.line')+theme(legend.position = 'none')+border()


biomarker%>%transmute(age=factor(age_group),CEA=CEA,CEA2=ifelse(CEA>quantile(CEA,na.rm=T)[4]+IQR(CEA,na.rm = T)*1.5,NA,CEA),
                      smoking=factor(smoking,labels=c('Never','Current','Ago')))%>%filter(!is.na(smoking) & !is.na(age))%>%
  summarise()
  

#吸烟人群中包年与年龄的关系
#1
plot_baonian1<-biomarker%>%transmute(age=age,baonian=baonian)%>%
  ggscatter(x='age',y='baonian',add='reg.line',add.params =list(color='red'))+stat_cor(method='spearman')+border()
#2
plot_baonian2<-biomarker%>%transmute(age=age_group,baonian=ifelse(baonian>quantile(baonian,na.rm=T)[4]+IQR(baonian,na.rm = T)*1.5,NA,baonian))%>%filter(!is.na(age))%>%
  ggboxplot(x='age',y='baonian',bxp.errorbar = TRUE,outlier.size=0.5,color='age',outlier.fill='age')+theme(legend.position = 'none')+border()+
  scale_x_discrete(breaks=c(1,2,3,4,5,6,7),labels=c('40-44','45-49','50-54','55-59','60-64','65-69','70-74'))
ggarrange(plot_baonian1,plot_baonian2,nrow=1)
#sex and smoking
biomarker.smoking%>%ggboxplot(x='smoking',y='CEA',facet.by = 'sex',add='jitter',add.params = list(size=0.1,alpha=0.4),ylab='log(CEA)')+
  stat_compare_means(comparisons = list(c('Never','Current'),c('Never','Ago'),c('Current','Ago')),label='p.signif'
  )+stat_compare_means(label.y=10,label.x=0.7)+border()
#矫正性别和吸烟状态后
summary(lm(CEA~sex+smoking,data=biomarker.smoking))
#戒烟的年限与体内CEA的水平关系
biomarker%>%select(sex,smoking,quitsmkyrs,CEA)%>%filter(smoking==3,sex==1)%>%
  ggscatter(x='quitsmkyrs',y='CEA',add='reg.line')+stat_cor(method='spearman')
biomarker%>%select(sex,smoking,quitsmkyrs,CEA)%>%transmute(sex=sex,smoking=smoking,
          quitsmkyrs=ifelse(quitsmkyrs>quantile(quitsmkyrs,na.rm=T)[4]+IQR(quitsmkyrs,na.rm = T)*1.5,NA,quitsmkyrs),
          CEA=ifelse(CEA>quantile(CEA,na.rm=T)[4]+IQR(CEA,na.rm = T)*1.5,NA,CEA))%>%
  filter(smoking==3,sex==1)%>%
  ggscatter(x='quitsmkyrs',y='CEA',add='reg.line')+stat_cor(method='spearman')

#age_group2 and CEA.group and sex and smoking
mosaicplot(~CEA.group+smoking+sex+age.group,data=biomarker.smoking,shade=TRUE,color=TRUE)





rndr5 <- function(x, name, ...){
  if (length(x) == 0) {
    y <- biomarker.smoking[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
       p <- kruskal.test(y ~ biomarker.smoking$smoking)$p.value
      #p <- wilcox.test(y ~ biomarker.smoking$smoking)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
    } else {
      p <- chisq.test(table(y, droplevels(biomarker.smoking$smoking)))$p.value
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name,...)
  }
}
table5<-table1(~CEA+CEA.group | smoking ,data=biomarker.smoking,render.continuous=c(.="Median [Q1,Q3]"),
       overall=F,render = rndr5,render.strat=rndr.strat,droplevels = F)

#the relationship between quitsmkrs and CEA
#the quitsmikrs-year --<=1 year,1-5nian,5-10,>=10
biomarker%>%select(smoking,quitsmkyrs,sex,CEA)%>%transmute(
  smoking=smoking,quitsmkyrs=quitsmkyrs,sex=sex,
  group=case_when(
    smoking==2 ~ 1,
    smoking==3 & quitsmkyrs<=1 ~2,
    smoking==3 & quitsmkyrs>1 & quitsmkyrs<=5 ~ 3,
    smoking==3 & quitsmkyrs>5 & quitsmkyrs<=10 ~ 3,
    smoking==3 & quitsmkyrs>10  ~ 4
  
    ),  CEA=log(CEA)
  )%>%ggboxplot(x='group',y='CEA')

#-------------------------------------------绝经年龄的分布----------------------------------------
biomarker%>%select(agemenopau,menopause)%>%transmute(agemenopau=agemenopau,menopause=factor(menopause,labels=c('绝经前','绝经后')))%>%
gghistogram(x='agemenopau',y='..density..',bins=20,fill='menopause')+theme(legend.position = 'none')

#------------------------------------------绝经前后CA125分布------------------------------------------------
biomarker%>%select(sex,menopause,CA125,CA153)%>%transmute(sex=factor(sex,labels=c('man','woman')),menopause=factor(menopause,labels=c('未绝经','绝经')),CA125=log(CA125),CA153=log(CA153))%>%
filter(sex=='woman' & !is.na(menopause) & CA153>0)%>%pivot_longer(cols=c('CA125','CA153'),names_to = 'marker',values_to = 'value')%>%
  ggviolin(x = "menopause", y = "value",fill = "menopause",facet.by = 'marker',
  palette = c("#00AFBB", "#E7B800"),add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(method='wilcox.test',label = "p.signif",label.x=1.5,size=6)+
  theme(legend.position = 'none')
biomarker.baseline5<-biomarker%>%select(menopause,CA125,CA153)%>%
  transmute(menopause=factor(menopause,levels = c(1,2,3),c('未绝经','绝经','P-value')),CA125=CA125,CA153=CA153)
rndr2 <- function(x, name, ...){
  if (length(x) == 0) {
    y <- biomarker.baseline5[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      #p <- kruskal.test(y ~ biomark_baseline1$CEA.risk)$p.value
      p <- wilcox.test(y ~ biomarker.baseline5$menopause)$p.value
      #p<-summary(lm(y~PG1_range,data=biomark_baseline,contrasts = list(PG1_range=contr.poly(4))))$coefficients[2,4]
      #p<-summary(lm(y~sex+age,data=biomarker_baseline4))$coefficients[3,4]
    } else {
      p <- chisq.test(table(y, droplevels(biomarker.baseline5$menopause)))$p.value
      #p<-pvalue(lbl_test(table(y,biomark_baseline$PG1_range)))
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name,...)
  }
}
rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}
table1(~CA125+CA153 | menopause ,data=biomarker.baseline5,render.continuous=c(.="Median [Q1,Q3]"),
       overall=F,render = rndr2,render.strat=rndr.strat,droplevels = F)
#
