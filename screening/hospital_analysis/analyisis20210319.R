rm(list=ls())
library(tidyverse)
library(rio)
library(openxlsx)
library(lubridate)
mytheme<-theme(plot.title=element_text(hjust=0.5),
               axis.title=element_text(family="serif",size=12,face="bold"),
               axis.text=element_text(family="serif",size=12,face="bold"),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black'),
               legend.title = element_text(family='serif',size=12),
               legend.text = element_text(family = 'serif',size=10),
               axis.text.x = element_text(angle = 45),
               legend.key = element_blank(),
               #legend.background = element_rect(colour = 'black')
               
)
###合并及分析医院PG及胃镜数据
gastroscopy<-read.xlsx("~/data/hospital/20210319/Gastroscpy(20210319版).xlsx",detectDates = TRUE)
PG<-import('~/data/hospital/20210319/HospitalPG.sav')
summary(gastroscopy)
summary(PG)
###PG分析
#1.PG检测次数分析
PG<-PG%>%mutate(n2=ifelse(n<=9,n,10))
with(PG,table(n2))
with(PG,round(prop.table(table(n2))*100,2))
PG%>%ggplot()+geom_bar(aes(x=factor(n2),fill=factor(n2)))+scale_x_discrete(labels=c('1','2','3','4','5','6','7','8','9','>=10'))+
  labs(x='检测次数')

#2.最后1次检测时间，检测结果
for (i in 1:nrow(PG)){
    n=PG[i,'n']
    PG$PGDate.Last[i]=as.character(PG[i,paste0('PGDate.',n)])
    PG$PGI.last[i]=PG[i,paste0('PGI.',n)]
    PG$PGII.last[i]=PG[i,paste0('PGI.',n)]
}
PG$PGDate.Last<-as.Date(PG$PGDate.Last)
#3.每人随访时间
PG$time<-as.numeric(floor(PG$PGDate.Last-PG$PGDate.1))
PG$time[PG$tim<0]<-0
median(PG$time)
#4.
PG2<-PG%>%transmute(MRN1,n=n,n2=n2,PGDate.1,PGI.1,PGII.1,PGDate.Last,PGI.last,PGII.last,time)
#5检测次数>=2次的
PG3<-PG2%>%filter(n>1)#8952人
#总体随访时间
summary(PG3$time)
#按检测次数分层
with(subset(PG3,n2==2),summary(time))
with(subset(PG3,n2==3),summary(time))
with(subset(PG3,n2==4),summary(time))
with(subset(PG3,n2==5),summary(time))
with(subset(PG3,n2==6),summary(time))
with(subset(PG3,n2==7),summary(time))
with(subset(PG3,n2==8),summary(time))
with(subset(PG3,n2==9),summary(time))
with(subset(PG3,n2==10),summary(time))
#计算PG的进展情况
PG4<-PG3%>%mutate(
  PG1.1_range=case_when(
    PGI.1<=30 ~ 1,
    PGI.1>30 & PGI.1<=50.0 ~ 2,
    PGI.1>50.0 & PGI.1<=70.0 ~ 3,
    PGI.1>70 ~ 4
  ),
  PG1.1_range=factor(PG1.1_range,levels=c(1,2,3,4),labels=c('<=30','30.01-50','50.01-70','>70')),
  PG1.last_range=case_when(
    PGI.last<=30 ~ 1,
    PGI.last>30 & PGI.last<=50.0 ~ 2,
    PGI.last>50.0 & PGI.last<=70.0 ~ 3,
    PGI.last>70 ~ 4
  ),
  PG1.last_range=factor(PG1.last_range,levels=c(1,2,3,4),labels=c('<=30','30.01-50','50.01-70','>70')),
  PG2.1_range=case_when(
    PGII.1<6.51 ti~ 1,
    PGII.1>=6.51 & PGII.1<9.8 ~ 2,
    PGII.1>=9.8 & PGII.1<15.3 ~ 3,
    PGII.1>=15.30 ~ 4
  ),
  PG2.1_range=factor(PG2.1_range,levels = c(1,2,3,4),labels=c('<6.5','6.51-9.79','9.8-15.29','>=15.30')),
  PG2.last_range=case_when(
    PGI.last<6.51 ~ 1,
    PGI.last>=6.51 & PGI.last<9.8 ~ 2,
    PGI.last>=9.8 & PGI.last<15.3 ~ 3,
    PGI.last>=15.30 ~ 4
  ),
  PG2.last_range=factor(PG2.last_range,levels = c(1,2,3,4),labels=c('<6.5','6.51-9.79','9.8-15.29','>=15.30')),
  
)
#PGI进展情况
with(PG4,table(PG1.1_range,PG1.last_range))
with(PG4,round(prop.table(table(PG1.1_range,PG1.last_range),1)*100,2))
#PGII进展情况
with(PG4,table(PG2.1_range,PG2.last_range))
with(PG4,round(prop.table(table(PG2.1_range,PG2.last_range),1)*100,2))

###############对所医院PG与胃镜数据进行合并#######################
##根据胃镜的IID和PG的ExtIDPat合并
gastroscopy$ID1<-gastroscopy$IID
PG$ID1<-PG$ExtIDPat
match1<-inner_join(gastroscopy,PG,by="ID1")

##根据胃镜的IID和PG的MRN8进行合并
gastroscopy$ID2<-gastroscopy$IID
PG$ID2<-PG$MRN8
match2<-inner_join(gastroscopy,PG,by="ID2")

##根据胃镜的住院号和PG的MRN6进行合并
gastroscopy$ID3<-gastroscopy$ID_H
PG$ID3<-PG$MRN6
match3<-inner_join(gastroscopy,PG,by="ID3")

##三个匹配数据集进行合并
names(match3)
match1.1<-match1%>%select(-ID1)
match2.1<-match2%>%select(-ID1.x,-ID2,-ID1.y)
match3.1<-match3%>%select(-ID1.x,-ID2.x,-ID3,-ID1.y,-ID2.y)
match_T<-rbind(match1.1,match2.1,match3.1)
#match_T[which(match_T$IID %in% unlist(match_T$IID[duplicated(match_T$IID)])),c('IID','Name','PtName','MRN6','ExtIDPat')]%>%arrange(IID)
match_T2<-match_T%>%filter(!duplicated(ExtIDPat))
export(match_T2,"~/data/hospital/20210319/GastroPG.xlsx")
#####################建库：胃镜结合PG(第一次胃镜无术后、放化疗、转移等关键)##########################
#手术？
table(match_T2$Postoperative1)
#放化疗？
table(match_T2$Chemotherapy1)
#转移？
table(match_T2$Mets1)
data<-match_T2%>%filter(Postoperative1=="No" & Chemotherapy1=="No",Mets1=="No")

#添加变量：最后一次PG
for (i in 1:nrow(data)){
  n=data[i,'n']
  data$PGDate.Last[i]=as.character(data[i,paste0('PGDate.',n)])
  data$PGI.last[i]=data[i,paste0('PGI.',n)]
  data$PGII.last[i]=data[i,paste0('PGI.',n)]
}
data$PGDate.Last<-as.Date(data$PGDate.Last)
#新建数据库
data2<-data%>%transmute(IID,Name,PtName,Sex,ID_H,ExtIDPat,MRN6,MRN8,gastroscopy,PGN=n,
                        GastDate1=Date1,GastDate2=Date2,GastDate3=Date3,GastDate_Last=Date_Last,Gastroscopy1,Gastroscopy2,Gastroscopy3,Gastroscopy_Last,
                        Digngastric1,Digngastric2,Digngastric3,Digngastric_Last,Dignclinic1,Dignclinic2,Dignclinic3,Dignclinic_Last,
                        Atrophy1,Atrophy2,Atrophy3,Atrophy_Last,ulcer1,ulcer2,ulcer3,ulcer_Last,
                        PGDate.1,PGDate.2,PGDate.3,PGDate.Last,PGI.1,PGI.2,PGI.3,PGI.last,PGII.1,PGII.2,PGII.3,PGII.last)
export(data2,"~/data/hospital/20210319/GastroPG.xlsx")
########
PG%>%ggplot(aes(x=PGI.1))+geom_histogram()
PG%>%filter(PGI.1<200)%>%ggplot(aes(x=log(PGI.1)))+geom_histogram()
shapiro.test(PG$PGI.1)
#1读取数据

########新建变量：PG检测在前，胃镜检查在后
data2$time2<-ifelse(data2$PGDate.1-data2$GastDate1<0,"PG在前","PG在后")
#data2[,c('PGDate.1','GastDate1','time2')]
#table(data2$time2)
########是否诊断为癌症
data3<-data2%>%mutate(cancer_gastri.1=case_when(
  str_detect(Digngastric1,paste(c("胃癌","胃Ca","结合部癌","结合部Ca","胃窦癌","胃窦Ca"),collapse="|")) ~ 1,
  str_detect(Dignclinic1,paste(c("残胃","术后"),collapse="|")) ~ 1,
)
                        )



##PG
PG_CA<-read.xlsx("~/data/hospital/20210319/GastroPG.xlsx",sheet=3,detectDates = TRUE)%>%
  transmute(
    PGR.1=round(log(PGI.1/PGII.1),2),PGI.1=round(log(PGI.1),2),PGII.1=round(log(PGII.1),2),
    type="gastric"
  )
PG1<-PG%>%filter(n==1 & !is.na(PGI.1) & !is.na(PGII.1))%>%transmute(
  PGR.1=round(log(PGI.1/PGII.1),2),PGI.1=round(log(PGI.1),2),PGII.1=round(log(PGII.1),2),type="Non-gastirc"
  
)
PG5<-rbind(PG_CA,PG1)
PG1%>%cut_number(PGI.1,20)%>%group_by(type)%>%summarise(n=n())

a<-PG5%>%mutate(vec_cut=cut_number(PGI.1,20))%>%group_by(type)%>%count(vec_cut)%>%group_by(vec_cut)%>%summarise(percent=round(n/sum(n),4)*100)
print(a,n=40)
PG5%>%ggplot()+geom_bar(aes(cut_number(PGI.1,20),fill=type),position = "fill")+mytheme+
  labs(x="log(PG1)")

b<-PG5%>%mutate(vec_cut=cut_number(PGR.1,20))%>%group_by(type)%>%count(vec_cut)%>%group_by(vec_cut)%>%summarise(percent=round(n/sum(n),4)*100)
print(b,n=40)

PG5%>%ggplot()+geom_bar(aes(cut_number(PGR.1,20),fill=type),position = "fill")+mytheme+labs(x="PGR")

c<-PG5%>%mutate(vec_cut=cut_number(PGII.1,20))%>%group_by(type)%>%count(vec_cut)%>%group_by(vec_cut)%>%summarise(percent=round(n/sum(n),4)*100)
print(c,n=40)

PG5%>%ggplot()+geom_bar(aes(cut_number(PGII.1,20),fill=type),position = "fill")+mytheme+labs(x="PGII")



