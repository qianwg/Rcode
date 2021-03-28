rm(list=ls())
library(tidyverse)
library(openxlsx)
library(rio)
##整理医院内镜数据
data<-read.xlsx('~/data/hospital/胃镜结果.xlsx',detectDates = TRUE)%>%
  transmute(IID=病人号,ID_H=住院号,Age=年龄,Sex=性别,Sex_new=factor(ifelse(Sex=="男",1,2),levels=c(1,2),labels=c('Male','Female')),
            Name=姓名,Self_report=主诉,Dignclinic=临床诊断,Type_old=词库种类,
            Gastroscopy=镜下所见,Digngastric=镜下诊断,Date=报告时间,ID_R=申请单号,
    Type_new=case_when(
      词库种类=="鼻咽镜" ~ 1,
      词库种类=='肠镜' ~2,
      词库种类=='超声内镜' ~3,
        词库种类=='电子喉镜' ~4,
        词库种类=='十二指肠镜' ~ 7,
        词库种类=='胃镜' ~5,
        词库种类=='支气管镜'~6,
        词库种类=='喉镜(普+特)'~4,
      词库种类=='结肠镜(普+特)' ~ 2,
    ),
    Type=ifelse(is.na(Type_new),7,Type_new),
    Type=factor(Type,levels = seq(7),labels=c('Nasopharyngoscope','Colonoscopy','Ultrasound','Videolaryngoscope','gastroscopy','Bronchoscopy',
                                              'Other')),
    IDH_CHECK=ifelse(is.na(ID_H) | (length(ID_H)<=3 & length(ID_H>=1)),1,0),
    IID_CHECK=ifelse(is.na(IID) | (length(IID)<=3 & length(IID>=1)),1,0),
    Year_CHECK=ifelse(is.na(Date),1,0),DATE_CHECK=ifelse(is.na(Date),1,0)
    
  )
#为每一个人标志一个唯一(针对于有IID号的)
#2、计算每一个人每项检查的检查次数
times<-data%>%group_by(IID,Type)%>%summarise(n=n())%>%pivot_wider(names_from = Type,values_from=n)%>%filter(IID!="")
data2<-left_join(data,times,by='IID') 
data2[,names(data2)[20:26]]<-apply(data2[,names(data2)[20:26]],2,function(x)x<-ifelse(is.na(x),0,x))
#3、创建变量：每个人每项检查是第几次检查
Order<-data%>%group_by(IID,Type,Date)%>%summarise(n=n())%>%filter(IID!="")%>%arrange(IID,Type,Date)%>%mutate(Order=cumsum(n))
data4<-left_join(data2,Order[,c('IID','Type','Date','Order')],by=c('IID','Type','Date'))
#4、创建变量：是否术后 OR 放化疗
#临床诊断：术后
#镜下所见：残胃
#镜下诊断：残胃、术后
#5、考虑转移
#临床诊断:转移
data4<-data4%>%mutate(
  Postoperative=case_when(
    str_detect(Dignclinic,paste(c("术后","手术史","外院术备后","左肺癌手术","手术治疗后","乙状结肠癌术","乳腺术区","左舌鳞癌根治术"),
                                collapse="|")) ~ 1,
    str_detect(Gastroscopy,paste(c("残胃","术后"),collapse="|")) ~ 1,
    str_detect(Digngastric,paste(c("残胃","术后"),collapse='|')) ~ 1,
    ),
  Postoperative=factor(ifelse(is.na(Postoperative),0,Postoperative),levels=c(0,1),labels=c('No','Yes')),
  Chemotherapy=case_when(
    str_detect(Gastroscopy,paste(c('放疗','化疗','放化疗'),collapse = '|')) ~ 1,
    str_detect(Dignclinic,paste(c('放疗','化疗','放化疗'),collapse = '|')) ~ 1,
    str_detect(Digngastric,paste(c('放疗','化疗','放化疗'),collapse = '|')) ~ 1,
  ),
  Chemotherapy=factor(ifelse(is.na(Chemotherapy),0,Chemotherapy),levels=c(0,1),labels=c('No','Yes')),
  Mets=case_when(
    str_detect(Dignclinic,"转移") ~ 1,
    str_detect(Gastroscopy,"转移") ~ 1,
    str_detect(Digngastric,"转移") ~ 2,
    ),
  Mets=factor(ifelse(is.na(Mets),0,Mets),levels=c(0,1,2),labels=c('No','Yes','Maybe'))
)
##检查
data[which(data$IID=="4187155287303041404"),'Gastroscopy']
export(data4,'~/data/hospital/20210319/Endoscope_Second.xlsx')



###2021-03-11：针对胃镜数据创建变量及长变宽
rm(list=ls())
data<-read.xlsx('~/data/hospital/20210319/Endoscope_Second.xlsx',detectDates = TRUE)%>%filter(Type=="gastroscopy")
#是否有萎缩性胃炎
data2<-data%>%mutate(
  Atrophy=factor(ifelse(str_detect(Digngastric,"萎缩"),ifelse(grepl("非",str_extract(Digngastric,".{0,1}萎缩.{0,1}")),0,1),0),levels=c(0,1),labels=c('No','Yes')),
  Reflux=factor(ifelse(str_detect(Digngastric,'反流性食管炎'),1,0),levels=c(0,1),labels=c('No','Yes')),
  Barrett=factor(ifelse(str_detect(Digngastric,paste(c('Barrett食管','SSBE','Barrett'),collapse='|')),1,0),levels=c(0,1),labels=c('No','Yes')),
  polyp=factor(ifelse(str_detect(Digngastric,c('息肉')),1,0),levels=c(0,1),labels=c('No','Yes')),
  ulcer=factor(ifelse(grepl("溃疡",str_extract(Digngastric,".{0,3}溃疡.{0,3}")),ifelse(grepl("非",str_extract(Digngastric,".{0,3}溃疡.{0,3}")),0,1),0),levels=c(0,1),labels=c('No','Yes')),
  
)%>%filter(IID!="")
####检查
#同一病人号，不同住院号
a<-data2%>%filter(IID!="" & ID_H!="" & !is.na(IID) & !is.na(ID_H))%>%group_by(IID,ID_H)%>%summarise(n=n())%>%arrange(IID,ID_H)
data2.1<-data2%>%filter(!IID %in% unlist(a[which(duplicated(a$IID)),'IID']))
#同一住院号不同病人号
b<-data2%>%filter(IID!="" & ID_H!="" & !is.na(IID) & !is.na(ID_H))%>%group_by(ID_H,IID)%>%summarise(n=n())%>%arrange(ID_H,IID)
data2.2<-data2.1%>%filter(!ID_H %in% unlist(b[which(duplicated(b$ID_H)),'ID_H']))
#同一病人号不同姓名
c<-data2%>%filter(IID!="" & Name!="" & !is.na(IID) & !is.na(Name))%>%group_by(IID,Name)%>%summarise(n=n())%>%arrange(IID,Name)
data2.3<-data2.2%>%filter(!IID %in% unlist(c[which(duplicated(c$IID)),'IID']))

export(data2.3,'~/data/hospital/20210319/Gastroscopy_First.xlsx')
#基本信息
data_baseline<-data2.3%>%transmute(IID,Name,Sex,ID_H,Type,
                                 Nasopharyngoscope,Colonoscopy,Ultrasound,Videolaryngoscope,gastroscopy,Bronchoscopy,Other)%>%filter(!duplicated(IID))
#1、针对内镜检查时间进行长变宽
data_Date<-data2.3%>%transmute(IID,Order,Date=as.character(Date))%>%pivot_wider(names_from = Order,values_from=Date)
names(data_Date)[2:20]<-paste0('Date',1:19)
#2、针对内镜下所见进行长变宽
data_Gastroscopy<-data2.3%>%transmute(IID,Order,Gastroscopy)%>%pivot_wider(names_from = Order,values_from=Gastroscopy)
names(data_Gastroscopy)[2:20]<-paste0('Gastroscopy',1:19)
#3、针对内镜下诊断长变宽
data_Digngastric<-data2.3%>%transmute(IID,Order,Digngastric)%>%pivot_wider(names_from = Order,values_from=Digngastric)
names(data_Digngastric)[2:20]<-paste0('Digngastric',1:19)
#4、临床诊断
data_Dignclinic<-data2.3%>%transmute(IID,Order,Dignclinic)%>%pivot_wider(names_from = Order,values_from=Dignclinic)
names(data_Dignclinic)[2:20]<-paste0('Dignclinic',1:19)
#5、术后
data_Postoperative<-data2.3%>%transmute(IID,Order,Postoperative)%>%pivot_wider(names_from = Order,values_from=Postoperative)
names(data_Postoperative)[2:20]<-paste0('Postoperative',1:19)
#6、放化疗
data_Chemotherapy<-data2.3%>%transmute(IID,Order,Chemotherapy)%>%pivot_wider(names_from = Order,values_from=Chemotherapy)
names(data_Chemotherapy)[2:20]<-paste0('Chemotherapy',1:19)
#7、转移
data_Mets<-data2.3%>%transmute(IID,Order,Mets)%>%pivot_wider(names_from = Order,values_from=Mets)
names(data_Mets)[2:20]<-paste0('Mets',1:19)
#8、萎缩性胃炎
data_Atrophy<-data2.3%>%transmute(IID,Order,Atrophy)%>%pivot_wider(names_from = Order,values_from=Atrophy)
names(data_Atrophy)[2:20]<-paste0('Atrophy',1:19)
#9、反流性食管炎
data_Reflux<-data2.3%>%transmute(IID,Order,Reflux)%>%pivot_wider(names_from = Order,values_from=Reflux)
names(data_Reflux)[2:20]<-paste0('Reflux',1:19)
#10、Barrett食管
data_Barrett<-data2.3%>%transmute(IID,Order,Barrett)%>%pivot_wider(names_from = Order,values_from=Barrett)
names(data_Barrett)[2:20]<-paste0('Barrett',1:19)
#11、息肉
data_polyp<-data2.3%>%transmute(IID,Order,polyp)%>%pivot_wider(names_from = Order,values_from=polyp)
names(data_polyp)[2:20]<-paste0('polyp',1:19)
#12、溃疡
data_ulcer<-data2.3%>%transmute(IID,Order,ulcer)%>%pivot_wider(names_from = Order,values_from=ulcer)
names(data_ulcer)[2:20]<-paste0('ulcer',1:19)
#合并
data3<-Reduce(function(x,y)merge(x,y,by='IID',all=TRUE),
              list(data_baseline,data_Date,data_Gastroscopy,data_Digngastric,data_Dignclinic,data_Postoperative,data_Chemotherapy
                   ,data_Mets,data_Atrophy,data_Reflux,data_Barrett,data_polyp,data_ulcer))
data3$Date_Last<-NA
data3$Gastroscopy_Last<-NA
data3$Digngastric_Last<-NA
data3$Dignclinic_Last<-NA
data3$Postoperative_Last<-NA
data3$Chemotherapy_Last<-NA
data3$Mets_Last<-NA
data3$Atrophy_Last<-NA
data3$Reflux_Last<-NA
data3$Barrett_Last<-NA
data3$polyp_Last<-NA
data3$ulcer_Last<-NA


for (i in 1:nrow(data3)){
  if(data3[i,'gastroscopy']<=3){
    data3$Date_Last[i]=NA
    data3$Gastroscopy_Last[i]=NA
    data3$Digngastric_Last[i]=NA
    data3$Dignclinic_Last[i]=NA
    data3$Postoperative_Last[i]=NA
    data3$Chemotherapy_Last[i]=NA
    data3$Mets_Last[i]=NA
    data3$Atrophy_Last[i]=NA
    data3$Reflux_Last[i]=NA
    data3$Barrett_Last[i]=NA
    data3$polyp_Last[i]=NA
    data3$ulcer_Last[i]=NA
    }else{
      n=data3[i,'gastroscopy']
      data3$Date_Last[i]=data3[i,paste0('Date',n)]
      data3$Gastroscopy_Last[i]=data3[i,paste0('Gastroscopy',n)]
      data3$Digngastric_Last[i]=data3[i,paste0('Digngastric',n)]
      data3$Dignclinic_Last[i]=data3[i,paste0('Dignclinic',n)]
      data3$Postoperative_Last[i]=data3[i,paste0('Postoperative',n)]
      data3$Chemotherapy_Last[i]=data3[i,paste0('Chemotherapy',n)]
      data3$Mets_Last[i]=data3[i,paste0('Mets',n)]
      data3$Atrophy_Last[i]=data3[i,paste0('Atrophy',n)]
      data3$Reflux_Last[i]=data3[i,paste0('Reflux',n)]
      data3$Barrett_Last[i]=data3[i,paste0('Barrett',n)]
      data3$polyp_Last[i]=data3[i,paste0('polyp',n)]
      data3$ulcer_Last[i]=data3[i,paste0('ulcer',n)]
      
  }
    
}

export(data3,'~/data/hospital/20210319/Gatroscopy_Second.xlsx')

###大肠镜检查数据整理
rm(list=ls())
data<-import('~/data/hospital/Colonscopy.sav')
data2<-data%>%mutate(
  #息肉
  polyp=factor(ifelse(str_detect(Digngastric,c('息肉')),1,ifelse(str_detect(Gastroscopy,c('息肉')),1,0)),levels=c(0,1),labels=c('No','Yes')),
  #息肉是否有蒂
  Pedicle=factor(ifelse(str_detect(Gastroscopy,"息肉"),ifelse(grepl("蒂",str_extract(Gastroscopy,".{0,5}息肉.{0,1}")),1,0),0),levels=c(0,1),labels=c('No','Yes')),
  #息肉直径
  #(?:([1-9]?[0-9])[a-zA-Z ]{0,5}(?:息肉))|(?:(?:息肉)[a-zA-Z ]{0,20}([1-9]?[0-9]))
)%>%filter(IID!="")%>%arrange(IID,Date)

#基本信息
data_baseline<-data2%>%transmute(IID,Name,Sex,ID_H,Type_Old,Type_New,Sex_new,
                                 Nasopharyngoscope,Colonoscopy,Ultrasound,Videolaryngoscope,gastroscic,Bronchoscopy,Other)%>%filter(!duplicated(IID))
#1、针对肠镜检查时间进行长变宽
data_Date<-data2%>%transmute(IID,Order,Date=as.character(Date))%>%pivot_wider(names_from = Order,values_from=Date)
names(data_Date)[2:20]<-paste0('Date',1:19)
#2、针对内镜下所见进行长变宽
data_Gastroscopy<-data2%>%transmute(IID,Order,Gastroscopy)%>%pivot_wider(names_from = Order,values_from=Gastroscopy)
names(data_Gastroscopy)[2:20]<-paste0('Gastroscopy',1:19)
#3、针对内镜下诊断长变宽
data_Digngastric<-data2%>%transmute(IID,Order,Digngastric)%>%pivot_wider(names_from = Order,values_from=Digngastric)
names(data_Digngastric)[2:20]<-paste0('Digngastric',1:19)
#4、临床诊断
data_Dignclinic<-data2%>%transmute(IID,Order,Dignclinic)%>%pivot_wider(names_from = Order,values_from=Dignclinic)
names(data_Dignclinic)[2:20]<-paste0('Dignclinic',1:19)
#5、术后
data_Postoperative<-data2%>%transmute(IID,Order,Postoperative)%>%pivot_wider(names_from = Order,values_from=Postoperative)
names(data_Postoperative)[2:20]<-paste0('Postoperative',1:19)
#6、放化疗
data_Chemotherapy<-data2%>%transmute(IID,Order,Chemotherapy)%>%pivot_wider(names_from = Order,values_from=Chemotherapy)
names(data_Chemotherapy)[2:20]<-paste0('Chemotherapy',1:19)
#7、转移
data_Mets<-data2%>%transmute(IID,Order,Mets)%>%pivot_wider(names_from = Order,values_from=Mets)
names(data_Mets)[2:20]<-paste0('Mets',1:19)
#11、息肉
data_polyp<-data2%>%transmute(IID,Order,polyp)%>%pivot_wider(names_from = Order,values_from=polyp)
names(data_polyp)[2:20]<-paste0('polyp',1:19)
#12、息肉是否有蒂
data_Pedicle<-data2%>%transmute(IID,Order,Pedicle)%>%pivot_wider(names_from = Order,values_from=Pedicle)
names(data_Pedicle)[2:20]<-paste0('Pedicle',1:19)
#合并
data3<-Reduce(function(x,y)merge(x,y,by='IID',all=TRUE),
              list(data_baseline,data_Date,data_Gastroscopy,data_Digngastric,data_Dignclinic,data_Postoperative,data_Chemotherapy
                   ,data_Mets,data_polyp,data_Pedicle))
export(data3,'~/Colonscopy.xlsx')


###
data<-import('~/data/hospital/Endoscope.sav')%>%
  mutate(Type=factor(Type_New,levels=seq(7),
                     labels=c('Nasopharyngoscope','Colonoscopy','Ultrasound','Videolaryngoscope','gastroscopy','Bronchoscopy',
                              'Other')))


#胃镜为每一个人标志一个唯一(针对于有无IID只有住院号的)##################################################
rm(list=ls())
#1、读取数据
data<-read.xlsx('~/data/hospital/20210319/Endoscope_Second.xlsx',detectDates = TRUE)
data1<-data%>%filter(is.na(IID) & !is.na(ID_H))%>%select(-Bronchoscopy,-gastroscopy,-Other,-Videolaryngoscope,-Ultrasound,-Colonoscopy,-Order)
#2、计算每一个人每项检查的检查次数
times<-data1%>%group_by(ID_H,Type)%>%dplyr::summarise(n=n())%>%pivot_wider(names_from = Type,values_from=n)
data2<-left_join(data1,times,by='ID_H') 
data2[,names(data2)[24:29]]<-apply(data2[,names(data2)[24:29]],2,function(x)x<-ifelse(is.na(x),0,x))
#3、创建变量：每个人每项检查是第几次检查
Order<-data1%>%group_by(ID_H,Type,Date)%>%dplyr::summarise(n=n())%>%group_by(ID_H,Type)%>%mutate(Order=cumsum(n))%>%arrange(ID_H,Type,Order)
data4<-left_join(data2,Order[,c('ID_H','Type','Date','Order')],by=c('ID_H','Type','Date'))
#4、创建变量：是否术后 OR 放化疗
#临床诊断：术后
#镜下所见：残胃
#镜下诊断：残胃、术后
#5、考虑转移
#临床诊断:转移
data4<-data4%>%mutate(
  Postoperative=case_when(
    str_detect(Dignclinic,paste(c("术后","手术史","外院术备后","左肺癌手术","手术治疗后","乙状结肠癌术","乳腺术区","左舌鳞癌根治术"),
                                collapse="|")) ~ 1,
    str_detect(Gastroscopy,paste(c("残胃","术后"),collapse="|")) ~ 1,
    str_detect(Digngastric,paste(c("残胃","术后"),collapse='|')) ~ 1,
  ),
  Postoperative=factor(ifelse(is.na(Postoperative),0,Postoperative),levels=c(0,1),labels=c('No','Yes')),
  Chemotherapy=case_when(
    str_detect(Gastroscopy,paste(c('放疗','化疗','放化疗'),collapse = '|')) ~ 1,
    str_detect(Dignclinic,paste(c('放疗','化疗','放化疗'),collapse = '|')) ~ 1,
    str_detect(Digngastric,paste(c('放疗','化疗','放化疗'),collapse = '|')) ~ 1,
  ),
  Chemotherapy=factor(ifelse(is.na(Chemotherapy),0,Chemotherapy),levels=c(0,1),labels=c('No','Yes')),
  Mets=case_when(
    str_detect(Dignclinic,"转移") ~ 1,
    str_detect(Gastroscopy,"转移") ~ 1,
    str_detect(Digngastric,"转移") ~ 2,
  ),
  Mets=factor(ifelse(is.na(Mets),0,Mets),levels=c(0,1,2),labels=c('No','Yes','Maybe'))
)
##5、针对胃镜数据创建变量及长变宽
#5.1创建变量
data5<-data4%>%filter(Type=="gastroscopy")%>%mutate(
  Atrophy=factor(ifelse(str_detect(Digngastric,"萎缩"),ifelse(grepl("非",str_extract(Digngastric,".{0,1}萎缩.{0,1}")),0,1),0),levels=c(0,1),labels=c('No','Yes')),
  Reflux=factor(ifelse(str_detect(Digngastric,'反流性食管炎'),1,0),levels=c(0,1),labels=c('No','Yes')),
  Barrett=factor(ifelse(str_detect(Digngastric,paste(c('Barrett食管','SSBE','Barrett'),collapse='|')),1,0),levels=c(0,1),labels=c('No','Yes')),
  polyp=factor(ifelse(str_detect(Digngastric,c('息肉')),1,0),levels=c(0,1),labels=c('No','Yes')),
  ulcer=factor(ifelse(grepl("溃疡",str_extract(Digngastric,".{0,3}溃疡.{0,3}")),ifelse(grepl("非",str_extract(Digngastric,".{0,3}溃疡.{0,3}")),0,1),0),levels=c(0,1),labels=c('No','Yes')),
  
)
#基本信息
data_baseline<-data5%>%transmute(IID,Name,Sex,ID_H,Type_old,Type_new,Sex_new,
                                Colonoscopy,Ultrasound,Videolaryngoscope,gastroscopy,Bronchoscopy,Other)%>%filter(!duplicated(ID_H))
#1、针对内镜检查时间进行长变宽
data_Date<-data5%>%transmute(ID_H,Order,Date=as.character(Date))%>%pivot_wider(names_from = Order,values_from=Date)
data_Date[,6:20]<-NA
names(data_Date)[2:20]<-paste0('Date',1:19)
#2、针对内镜下所见进行长变宽
data_Gastroscopy<-data5%>%transmute(ID_H,Order,Gastroscopy)%>%pivot_wider(names_from = Order,values_from=Gastroscopy)
data_Gastroscopy[,6:20]<-NA
names(data_Gastroscopy)[2:20]<-paste0('Gastroscopy',1:19)
#3、针对内镜下诊断长变宽
data_Digngastric<-data5%>%transmute(ID_H,Order,Digngastric)%>%pivot_wider(names_from = Order,values_from=Digngastric)
data_Digngastric[,6:20]<-NA
names(data_Digngastric)[2:20]<-paste0('Digngastric',1:19)
#4、临床诊断
data_Dignclinic<-data5%>%transmute(ID_H,Order,Dignclinic)%>%pivot_wider(names_from = Order,values_from=Dignclinic)
data_Dignclinic[,6:20]<-NA
names(data_Dignclinic)[2:20]<-paste0('Dignclinic',1:19)
#5、术后
data_Postoperative<-data5%>%transmute(ID_H,Order,Postoperative)%>%pivot_wider(names_from = Order,values_from=Postoperative)
data_Postoperative[,6:20]<-NA
names(data_Postoperative)[2:20]<-paste0('Postoperative',1:19)
#6、放化疗
data_Chemotherapy<-data5%>%transmute(ID_H,Order,Chemotherapy)%>%pivot_wider(names_from = Order,values_from=Chemotherapy)
data_Chemotherapy[,6:20]<-NA
names(data_Chemotherapy)[2:20]<-paste0('Chemotherapy',1:19)
#7、转移
data_Mets<-data5%>%transmute(ID_H,Order,Mets)%>%pivot_wider(names_from = Order,values_from=Mets)
data_Mets[,6:20]<-NA
names(data_Mets)[2:20]<-paste0('Mets',1:19)
#8、萎缩性胃炎
data_Atrophy<-data5%>%transmute(ID_H,Order,Atrophy)%>%pivot_wider(names_from = Order,values_from=Atrophy)
data_Atrophy[,6:20]<-NA
names(data_Atrophy)[2:20]<-paste0('Atrophy',1:19)
#9、反流性食管炎
data_Reflux<-data5%>%transmute(ID_H,Order,Reflux)%>%pivot_wider(names_from = Order,values_from=Reflux)
data_Reflux[,6:20]<-NA
names(data_Reflux)[2:20]<-paste0('Reflux',1:19)
#10、Barrett食管
data_Barrett<-data5%>%transmute(ID_H,Order,Barrett)%>%pivot_wider(names_from = Order,values_from=Barrett)
data_Barrett[,6:20]<-NA
names(data_Barrett)[2:20]<-paste0('Barrett',1:19)
#11、息肉
data_polyp<-data5%>%transmute(ID_H,Order,polyp)%>%pivot_wider(names_from = Order,values_from=polyp)
data_polyp[,6:20]<-NA
names(data_polyp)[2:20]<-paste0('polyp',1:19)
#12、溃疡
data_ulcer<-data5%>%transmute(ID_H,Order,ulcer)%>%pivot_wider(names_from = Order,values_from=ulcer)
data_ulcer[,6:20]<-NA
names(data_ulcer)[2:20]<-paste0('ulcer',1:19)
#合并
data6<-Reduce(function(x,y)merge(x,y,by='ID_H',all=TRUE),
              list(data_baseline,data_Date,data_Gastroscopy,data_Digngastric,data_Dignclinic,data_Postoperative,data_Chemotherapy
                   ,data_Mets,data_Atrophy,data_Reflux,data_Barrett,data_polyp,data_ulcer))

export(data6,'~/data/hospital/20210319/Gastroscopy(补充无IID有IDH).xlsx')

##合并
rm(list=ls())
data1<-read.xlsx('~/data/hospital/20210319/Gatroscopy_Second.xlsx',detectDates = TRUE)%>%
  mutate(Type_old=Type,Type_new=Type,Sex_new=factor(ifelse(Sex=="男",1,2),levels=c(1,2),labels=c('Male','Female')))
data2<-read.xlsx('~/data/hospital/20210319/Gastroscopy(补充无IID有IDH).xlsx',detectDates = TRUE)%>%
  mutate(Type=Type_new,Nasopharyngoscope=0,
         Date_Last=Date4,
         Gastroscopy_Last=Gastroscopy4,
         Digngastric_Last=Digngastric4,
         Dignclinic_Last=Dignclinic4,
         Postoperative_Last=Postoperative4,
         Chemotherapy_Last=Chemotherapy4,
         Mets_Last=Mets4,
         Atrophy_Last=Atrophy4,
         Reflux_Last=Reflux4,
         Barrett_Last=Barrett4,
         polyp_Last=polyp4,ulcer_Last=ulcer4,
         Chemotherapy_Last=Chemotherapy4)
data<-rbind(data1,data2)
data1[which(duplicated(data1$ID_H)),]
data2[which(duplicated(data2$ID_H)),]
data[which(duplicated(data$ID_H)),]
export(data,'~/data/hospital/20210319/Gastroscpy(20210319版).xlsx')
















