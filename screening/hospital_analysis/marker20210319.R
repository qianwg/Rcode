####################<<<<<<<<<<<<<2021-03-19分析>>>>>>>>>>>>>>>>>>>>>#####
#重新整理PG数据
rm(list=ls())
#读取数据filter(!is.na(TestResult) & !is.na(DtValidate))
##########################数据处理#################################################
PG<-import('~/data/biomarker_hospital.xlsx')%>%transmute(PtName,MRN,ExtIDPat,DtValidate=as.POSIXct(DtValidate),TestCode,TestResult)%>%
filter(!is.na(TestResult) & !is.na(DtValidate))
##########################数据处理#################################################
######查看及检查数据
#summary(PG)
#apply(PG,2,function(x)sum(is.na(x)))
#同一MRN,不同ExtIDPat###########
a<-PG%>%filter(!is.na(MRN) & !is.na(ExtIDPat))%>%group_by(MRN,ExtIDPat)%>%summarise(n=n())
#PG1.1<-PG%>%filter(MRN %in% unlist(a[which(duplicated(a$MRN)),'MRN']))
#a2<-a[which(a$MRN %in% unlist(a[which(duplicated(a$MRN)),'MRN'])),]
#同一ExtIDPat，不同MRN###########
b<-PG%>%filter(!is.na(MRN) & !is.na(ExtIDPat))%>%group_by(ExtIDPat,MRN)%>%summarise(n=n())
PG1.2<-PG%>%filter(ExtIDPat %in% unlist(b[which(duplicated(b$ExtIDPat)),'ExtIDPat']))
b2<-b[which(b$ExtIDPat %in% unlist(b[which(duplicated(b$ExtIDPat)),'ExtIDPat'])),]
b3<-b2%>%select(MRN,ExtIDPat)%>%group_by(ExtIDPat,MRN)%>%summarise(n=n())%>%group_by(ExtIDPat)%>%mutate(Order=cumsum(n))%>%pivot_wider(names_from = Order,values_from = MRN)%>%select(-n)
names(b3)[2:3]<-c('MRN1','MRN2')
#检测次数
times.1<-PG1.2%>%group_by(ExtIDPat,TestCode)%>%summarise(n=n())%>%filter(!duplicated(ExtIDPat))
PG1.2.1<-left_join(PG1.2,times.1[,c('ExtIDPat','n')],by="ExtIDPat")
#第几次
Order1<-PG1.2.1%>%group_by(ExtIDPat,DtValidate,TestCode)%>%summarise(n2=n())%>%distinct(ExtIDPat,DtValidate,.keep_all=TRUE)%>%arrange(ExtIDPat,DtValidate)%>%group_by(ExtIDPat)%>%mutate(Order=cumsum(n2))
PG1.2.2<-left_join(PG1.2.1,Order1[,c('ExtIDPat','DtValidate','Order')],by=c('ExtIDPat','DtValidate'))
##基本信息
baseline.2<-PG1.2.2%>%select(ExtIDPat,PtName,n)%>%filter(!duplicated(ExtIDPat))
##检测时间长变宽
Date1<-PG1.2.2%>%transmute(ExtIDPat,Order,DtValidate=as.character(DtValidate))%>%distinct(ExtIDPat,DtValidate,.keep_all=TRUE)%>%spread(Order,DtValidate)
Date1[,41:50]<-NA
names(Date1)[2:50]<-paste0("PGDate.",1:49)
##PG1长变宽
pg1.2<-PG1.2.2%>%filter(TestCode=="PG1")%>%transmute(ExtIDPat,Order,TestResult)%>%pivot_wider(names_from = Order,values_from =TestResult )
pg1.2[,41:50]<-NA
names(pg1.2)[2:50]<-paste0("PGI.",1:49)
##PG2长变宽
pg2.2<-PG1.2.2%>%filter(TestCode=="PG2")%>%transmute(ExtIDPat,Order,TestResult)%>%pivot_wider(names_from = Order,values_from =TestResult )
pg2.2[,41:50]<-NA
names(pg2.2)[2:50]<-paste0("PGII.",1:49)
##合并
PG3<-Reduce(function(x,y)merge(x,y,by='ExtIDPat',all=TRUE),
list(b3,baseline.2,Date1,pg1.2,pg2.2))
###针对MRN/ExtIDPat一致的##########
##其他的######
PG1.4<-PG%>%filter(!ExtIDPat %in% unlist(b[which(duplicated(b$ExtIDPat)),'ExtIDPat']))
#apply(PG1.4,2,function(x)sum(is.na(x)))
#MRN不缺失，以MRN为唯一识别
#0.PG检测次数
times<-PG1.4%>%filter(!is.na(MRN))%>%group_by(MRN,TestCode)%>%summarise(n=n())%>%filter(!duplicated(MRN))
PG1.4<-left_join(PG1.4,times[c('MRN','n')],by="MRN")
#1.每一次是第几次
Order<-PG1.4%>%group_by(MRN,DtValidate,TestCode)%>%summarise(n2=n())%>%filter(!is.na(MRN))%>%distinct(MRN,DtValidate,.keep_all=TRUE)%>%arrange(MRN,DtValidate)%>%group_by(MRN)%>%mutate(Order=cumsum(n2))
PG2<-left_join(PG1.4,Order[,c('MRN','DtValidate','Order')],by=c('MRN','DtValidate'))
#2.基线唯一
baseline.1<-PG1.4%>%filter(!duplicated(MRN)&!is.na(MRN))%>%select(PtName,MRN,ExtIDPat,n)
# 检查时间长变宽
Date<-PG2%>%filter(!is.na(MRN))%>%transmute(MRN,Order,DtValidate=as.character(DtValidate))%>%distinct(MRN,DtValidate,.keep_all=TRUE)%>%spread(Order,DtValidate)
names(Date)[2:50]<-paste0("PGDate.",1:49)
#3.PG1长变宽
pg1<-PG2%>%filter(!is.na(MRN) & TestCode=="PG1")%>%transmute(MRN,Order,TestResult)%>%pivot_wider(names_from = Order,values_from =TestResult )
names(pg1)[2:50]<-paste0('PGI.',1:49)
#4.PG2长变宽
pg2<-PG2%>%filter(!is.na(MRN) & TestCode=="PG2")%>%transmute(MRN,Order,TestResult)%>%pivot_wider(names_from = Order,values_from =TestResult )
names(pg2)[2:50]<-paste0('PGII.',1:49)
#5.合并
PG4<-Reduce(function(x,y)merge(x,y,by='MRN',all=TRUE),
list(baseline.1,Date,pg1,pg2))
#MRN缺失，以ExtIDPat为唯一识别
PG.EX<-PG1.4%>%filter(is.na(MRN) & !is.na(ExtIDPat))
names(PG3)
names(PG4)
names(PG4)[which(names(PG4) %in% "MRN")]
names(PG4)[which(names(PG4) %in% "MRN")]<"MRN1"
names(PG4)[which(names(PG4) %in% "MRN")]<-"MRN1"
###合并
PG4$MRN2<-NA
names(PG3)==names(PG4)
names(PG3)%in%names(PG4)
PG5<-rbind(PG3,PG4)
export(PG5,"~/data/hospital/20210319/HospitalPG.xlsx")
