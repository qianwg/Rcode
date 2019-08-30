library(rio)
library(reshape2)
screening<-import('C:/Users/dell/Desktop/万兴街(2019-08-30).xlsx')
#screening$sex<-factor(screening$sex,levels = c(1,2),labels=c('男','女'))
screening$age_group[screening$age<40]<-0
screening$age_group[screening$age>=40 & screening$age<45]<-1
screening$age_group[screening$age>=45 & screening$age<50]<-2
screening$age_group[screening$age>=50 & screening$age<55]<-3
screening$age_group[screening$age>=55 & screening$age<60]<-4
screening$age_group[screening$age>=60 & screening$age<65]<-5
screening$age_group[screening$age>=65 & screening$age<70]<-6
screening$age_group[screening$age>=70 & screening$age<75]<-7
screening$age_group[screening$age>=75]<-8
screening$age_group<-factor(screening$age_group,levels = c(0,1,2,3,4,5,6,7,8),
                            labels=c('<40','40-44','45-49','50-54','55-59',
                                     '60-64','65-69','70-74','>74'))
freq<-data.frame(with(screening,table(sex,age_group)))
sex.age<-dcast(data=freq,sex~age_group,value.var ='Freq')
export(sex.age,'C:/Users/dell/Desktop/万兴街(年龄+性别分布).xlsx')
