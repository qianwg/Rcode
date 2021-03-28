rm(list=ls())
library(stringr)
library(rio)
library(tidyverse)
###2017-2019肿瘤医院PG与胃镜的匹配结果分析
PG_hosp<-import('~/data/PG(处理1).xlsx')
apply(PG_hosp[,1:3],2,function(x)sum(!is.na(x)))
PG_hosp[which(duplicated(PG_hosp$MRN)),]
PG_hosp[which(duplicated(PG_hosp$PtName)),'PtName']#姓名重复的有2万多人。
gastroscopy[which(duplicated(gastroscopy$姓名)),]
##
match1<-inner_join(PG_hosp[which(!duplicated(PG_hosp$PtName)),],gastroscopy[which(!duplicated(gastroscopy$PtName)),],by='PtName')
match2<-inner_join(PG_hosp,gastroscopy,by='MRN')
match3<-inner_join(PG_hosp,gastroscopy,by='ExtIDPat')
write.xlsx(list(name=match1,MRN=match2,ExtIDPat=match3),'~/match_2020_11_26.xlsx')  
  
##################2020-12-05:针对所有内镜结果
rm(list=ls())
gastroscopy<-import('~/data/胃镜结果.xlsx')
gastroscopy2<-gastroscopy%>%mutate(a=ifelse(str_detect(词库种类,"胃镜"),2,1))%>%filter(a==2)%>%select(-a,-词库种类,-报告时间...12,-申请单号,-freq)
str(gastroscopy2)
apply(gastroscopy2,2,function(x)sum(is.na(x)))
b<-gastroscopy2[which(duplicated(gastroscopy2$病人号)),'病人号']
####
gastroscopy2.1<-gastroscopy2%>%arrange(报告时间...11)%>%transmute(病人号,诊断=paste0(临床诊断,"|" ,镜下所见,"|",镜下诊断 ,"|",报告时间...11))
gastroscopy2.1.split<-split(gastroscopy2.1,gastroscopy2.1$病人号)
#max(as.data.frame(lapply(gastroscopy2.1.split,function(x)nrow(x))))
gastroscopy2.1.split1<-lapply(gastroscopy2.1.split,function(x){
  x[1:19,'病人号']<-x[1,'病人号']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from = 诊断)
  return(x)
})
gastroscopy3.1<-do.call(rbind,gastroscopy2.1.split1) 
gastroscopy3.1$胃镜检查次数<-apply(gastroscopy3.1[,2:20],1,function(x)sum(!is.na(x)))
gastroscopy3.1<-as.data.frame(gastroscopy3.1)
gastroscopy3.1$最后1次胃镜检查<-NA
for(i in 1:nrow(gastroscopy3.1)){
  n<-as.numeric(gastroscopy3.1[i,'胃镜检查次数'])
  gastroscopy3.1[i,'最后1次胃镜检查']<-ifelse(n>3,gastroscopy3.1[i,n+1],NA)
}
export(gastroscopy3.1,'~/gastroscopy(诊断).xlsx')
#############个人信息
gastroscopy2.2<-gastroscopy2%>%transmute(病人号,个人信息=paste0(姓名,"|" ,性别 ,"|",年龄))
gastroscopy2.2.split<-split(gastroscopy2.2,gastroscopy2.2$病人号)
#max(as.data.frame(lapply(gastroscopy2.2.split,function(x)nrow(x))))
gastroscopy2.2.split1<-lapply(gastroscopy2.2.split,function(x){
  x[1:19,'病人号']<-x[1,'病人号']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from = 个人信息)
  return(x)
})
gastroscopy3.2<-do.call(rbind,gastroscopy2.2.split1) 
export(gastroscopy3.2,'~/gastroscopy(个人信息).xlsx')
#######住院信息
gastroscopy2.3<-gastroscopy2%>%transmute(病人号,住院号)
gastroscopy2.3.split<-split(gastroscopy2.3,gastroscopy2.3$病人号)
#max(as.data.frame(lapply(gastroscopy2.2.split,function(x)nrow(x))))
gastroscopy2.3.split1<-lapply(gastroscopy2.3.split,function(x){
  x[1:19,'病人号']<-x[1,'病人号']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from = 住院号)
  return(x)
})
gastroscopy3.3<-do.call(rbind,gastroscopy2.3.split1) 
export(gastroscopy3.3,'~/gastroscopy(住院号).xlsx')
#####
#summary(gastroscopy)
#names(gastroscopy)
#关键词：
#术后：术后
#已患癌：瘤	癌	ca	间质瘤	恶性肿瘤	非霍奇金淋巴瘤	Ca    c('瘤','癌','ca','Ca','间质瘤','恶性肿瘤','非霍奇金淋巴瘤')
#放化疗：化疗	放疗	放化疗
#治疗后：治疗后
#是否转移：转移
#萎缩性胃炎：萎缩
#Barrett食管:Barrett食管 SSBE Barrett
#反流性食管炎：反流性食管炎 反流性炎症
#息肉：息肉
#溃疡：溃疡
#str_detect(gastroscopy$第1次诊断,"癌")
rm(list=ls())
gastroscopy<-import('~/data/胃镜库(分析用).xlsx')
######

gastroscopy2<-gastroscopy%>%mutate(癌症类型1=str_extract(第1次胃镜结果, "[^;]{0,6}癌"),
                                  癌症类型2=str_extract(第1次胃镜结果, "[^;]{0,6}ca"),
                                  癌症类型3=str_extract(第1次胃镜结果, "[^;]{0,6}瘤"),
                                  癌症类型4=str_extract(第1次胃镜结果, "[^;]{0,6}肿物"),
                                  术后1=factor(ifelse(str_detect(第1次胃镜结果,"术后"),2,1),levels=c(1,2),labels=c('否','是')),
                                  已患癌1=factor(ifelse(str_detect(第1次胃镜结果,paste(c('瘤','癌','ca','Ca','间质瘤','恶性肿瘤','非霍奇金淋巴瘤'),collapse='|')),2,1),levels=c(1,2),labels=c('否','是')),
                                  放化疗1=factor(ifelse(str_detect(第1次胃镜结果,paste(c('放疗','化疗','放化疗'),collapse = '|')),2,1),levels=c(1,2),labels=c('否','是')),
                                  转移1=factor(ifelse(str_detect(第1次胃镜结果,c('转移')),2,1),levels=c(1,2),labels=c('否','是')),
                                  胃炎1=factor(ifelse(str_detect(第1次胃镜结果,c('胃炎')),2,1),levels=c(1,2),labels=c('否','是')),
                                  息肉1=factor(ifelse(str_detect(第1次胃镜结果,c('息肉')),2,1),levels=c(1,2),labels=c('否','是')),
                                  溃疡1=factor(ifelse(str_detect(第1次胃镜结果,c('溃疡')),2,1),levels=c(1,2),labels=c('否','是')),
                                  反流性食管炎1=factor(ifelse(str_detect(第1次胃镜结果,paste(c('反流性食管炎','反流性炎症'),collapse='|')),2,1),levels=c(1,2),labels=c('否','是')),
                                  Barrett食管1=factor(ifelse(str_detect(第1次胃镜结果,paste(c('Barrett食管','SSBE','Barrett'),collapse='|')),2,1),levels=c(1,2),labels=c('否','是')),
                                  
)




#apply(gastroscopy[,c('术后','已患癌','放化疗','转移','胃炎','息肉','溃疡')],2,table)
export(gastroscopy2,'~/gastroscopy.xlsx')

####################其他内镜#########################
rm(list=ls())
gastroscopy<-import('~/data/胃镜结果.xlsx')
gastroscopy2<-gastroscopy%>%mutate(a=ifelse(str_detect(词库种类,"鼻咽"),2,1))%>%filter(a==2)%>%select(-a,-词库种类,-报告时间...12,-申请单号,-freq)
#str(gastroscopy2)
#apply(gastroscopy2,2,function(x)sum(is.na(x)))
#b<-gastroscopy2[which(duplicated(gastroscopy2$病人号)),'病人号']
####
gastroscopy2.1<-gastroscopy2%>%arrange(报告时间...11)%>%transmute(病人号,诊断=paste0(临床诊断,"|" ,镜下所见,"|",镜下诊断 ,"|",报告时间...11))
gastroscopy2.1.split<-split(gastroscopy2.1,gastroscopy2.1$病人号)
#max(as.data.frame(lapply(gastroscopy2.1.split,function(x)nrow(x))))
gastroscopy2.1.split1<-lapply(gastroscopy2.1.split,function(x){
  x[1:4,'病人号']<-x[1,'病人号']
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  x<-pivot_wider(x,names_from = seq,values_from = 诊断)
  return(x)
})
gastroscopy3.1<-do.call(rbind,gastroscopy2.1.split1) 

  