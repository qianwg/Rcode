library(RMySQL)
library(tidyverse)
#-------------------------------------MySQL数数据库连接---------------------------------------------------------------
cnn<-dbConnect(MySQL(),host='49.232.130.131',user='root',password='',dbname='screening')#与mysql进行连接
#解决中文乱码问题
encoding <- if(grepl(pattern = 'utf8|utf-8',x = Sys.getlocale(),ignore.case = T)) 'utf8' else 'latin1'
dbSendQuery(cnn,paste("SET names",encoding))
#读取数据集
biomark1<-dbSendQuery(cnn,'SELECT id,name,AFP,CA199,CA153,CA125,CEA,PG1,PG2,PGR 
                    FROM biomarker WHERE year=2019 and source="示范区";')#19年示范区tumor marker数据
biomark<-dbFetch(biomark1,n=-1)
#提取针对胃部肿瘤标志物相关研究的基线资料
res<-dbSendQuery(cnn,'SELECT id,name,sex,age,disea14,disea15,disea16,disea17,disea18,disea19,disea20,disea22,disea23,disea28,disea29,disea30,disea31,
                          cancerfh,catpfath,catpmoth,catpbrot1,catpbrot2,catpsist1,catpsist2,catpchil1,catpchil2,weight,height,
                          smoking,quitsmkyrs,cpd,smkyrs,alcohol,menopause,agemenopau FROM baseline2019 WHERE source="示范区";')
baseline<-dbFetch(res,n=-1) 
dbClearResult(res)
dbClearResult(biomark1)
dbDisconnect(cnn)#断开连接
#------------------------------------------------------------------------------------------------
#---------------------------------------1.基础性操作----------------------------------------------------------------
#2.将字符串中的‘>’去除,且改为数值型
str_func<-function(x){
  b=as.numeric(str_replace_all(x,'>',''))
  return(b)
}
biomark[,c('CEA','PG1','PG2','PGR','CA199','CA125','CA153','AFP')]<-data.frame(apply(biomark[,c('CEA','PG1','PG2','PGR','CA199','CA125','CA153')],
                                                                               2,str_func))
#combine bioamrk and baseline
biomark1.1<-inner_join(biomark,baseline,by=c('id','name'))
#biomark1.1<-import('~/screening/biomarker.sav')
ggplot(data=biomark1.1,aes(x=PG1,y=PGR,size=PG2))+geom_point(alpha=0.5,shape=21,colour='black',fill='cornsilk')+
  scale_size_area()+
  scale_colour_brewer(palette = "Set1")+
  geom_hline(aes(yintercept = 3))+geom_vline(aes(xintercept=70))+geom_vline(aes(xintercept=20))+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200))+scale_y_continuous(breaks=c(0,3,5,10,15,20))
#age-->age_group
biomark1.1$age_group[biomark1.1$age>=40 & biomark1.1$age<45]<-1
biomark1.1$age_group[biomark1.1$age>=45 & biomark1.1$age<50]<-2
biomark1.1$age_group[biomark1.1$age>=50 & biomark1.1$age<55]<-3
biomark1.1$age_group[biomark1.1$age>=55 & biomark1.1$age<60]<-4
biomark1.1$age_group[biomark1.1$age>=60 & biomark1.1$age<65]<-5
biomark1.1$age_group[biomark1.1$age>=65 & biomark1.1$age<70]<-6
biomark1.1$age_group[biomark1.1$age>=70 & biomark1.1$age<75]<-7
biomark1.1$age_group<-factor(biomark1.1$age_group)
#biomark1.1$PG1_range[biomark1.1$PG1<=25 & biomark1.1$PGR<=2]<-3
##biomark1.1$PG1_range[biomark1.1$PG1>25 & biomark1.1$PG1<=70 & biomark1.1$PGR<=3]<-2
#biomark1.1$PG1_range[biomark1.1$PG1>=200]<-4
#biomark1.1$PG1_range<-ifelse(is.na(biomark1.1$PG1_range),1,biomark1.1$PG1_range)
#biomark1.1$PG1_range<-factor(biomark1.1$PG1_range,levels=c(1,2,3,4),labels=c('Normal','moderately Abnormal','strongly Abnormal','strongly abnormal2'))
biomark1.1$PG1_range2[biomark1.1$PG1<25]<-1
biomark1.1$PG1_range2[biomark1.1$PG1>=25 & biomark1.1$PG1<=70]<-2
biomark1.1$PG1_range2[biomark1.1$PG1>=200]<-4
biomark1.1$PG1_range2[biomark1.1$PG1<200 & biomark1.1$PG1>70]<-3
biomark1.1$PG1_range2<-factor(biomark1.1$PG1_range2,levels=c(1,2,3,4),labels=c('<30','30-70','70-200','>=200'),order=TRUE)
summary(biomark1.1$PG1_range2)
#3
#biomark1.1$PG1<-as.numeric(biomark1.1$PG1)
#biomark1.1$PG1_range3[biomark1.1$PG1<=30]<-1
#biomark1.1$PG1_range3[biomark1.1$PG1>30 & biomark1.1$PG1<=70]<-2
#biomark1.1$PG1_range3[biomark1.1$PG1<=200 & biomark1.1$PG1>70]<-3
#biomark1.1$PG1_range3<-factor(biomark1.1$PG1_range3)
#summary(biomark1.1$PG1_range3)
#with(biomark1.1,aggregate(age,by=list(PG1_range3),FUN='mean'))
#contrasts(biomark1.1$PG1_range3)<-contr.poly(levels(biomark1.1$PG1_range3))
#summary(lm(age~PG1_range3,data=biomark1.1))
#check the frequency
apply(biomark1.1[,c('age_group','PG1_range')],2,table)
#-------------------------------------------趋势性检验--------------------------------------------------------
#age
tabla<-with(biomark1.1,table(age_group,PG1_range))
prop.table(with(biomark1.1,table(age_group,PG1_range)),margin=2)
spineplot(tabla)
lxl<-lbl_test(tabla)
statistic(lxl)^2
#

#chisq-square test without ordered categories
#chisq_test(tabla)
#adjusted for age by using covariance mothed
#1 age(not adjusted)
with(biomark1.1,aggregate(age,by=list(PG1_range2),FUN='mean'))
with(biomark1.1,aggregate(age,by=list(sex,PG1_range2),FUN='mean'))
contrasts(biomark1.1$PG1_range2)<-contr.poly(levels(biomark1.1$PG1_range2))
summary(lm(age~PG1_range2,data=biomark1.1))
#summary(lm(age~PG1_range2,data=subset(biomark1.1,sex==1)))
#summary(lm(age~PG1_range2,data=subset(biomark1.1,sex==2)))
###
#baseline
library(table1)
biomark_baseline<-biomark1.1%>%select(age,sex,weight,height,smoking,alcohol,CA199,CEA,PG1,PG2,PGR,PG1_range2)%>%
  transmute(age=age,sex=factor(sex,label=c('man','woman')),
            BMI=weight/((height/100)^2),
            smoking=factor(smoking,labels=c('Never','Current','Ago')),
            alcohol=factor(alcohol,labels=c('No','Yes')),
            CA199=CA199,PG2=PG2,PGR=PGR,PG1_range=PG1_range2)
str(biomark_baseline)
table1(~age+sex+smoking+alcohol+CA199+PG2 | sex*PG1_range,data=biomark_baseline,overall=F)
#strata<-c(split(biomark_baseline,biomark_baseline$PG1_range),list(Overall=biomark_baseline))
#labels<-list(
#  variables=list(
#    age='age',
#    sex='sex',
#    BMI='BMI',
#    smoking='Smoking',
#    alcohol='Alcohol',
#    CA199='CA199',
#    PG2='PG2',
#    PGR='PGR'
#  ),
#  groups=list("PG1",'')
#)
#print(table1(strata,labels,groupspan = c(4,1)))






