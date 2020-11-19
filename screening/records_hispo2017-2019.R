rm(list = ls())
library(rio)
library(tidyverse)
records<-import('~/data/门诊查询202007.xls')
freq<-records%>%group_by(IDCARD)%>%summarise(freq=n())
records<-left_join(records,freq,by='IDCARD')
#inf<-records[which(!duplicated(records$IDCARD)),c('PATINAME','SEXNAME','IDCARD','BIRTHDATE','CLINICCARDID','freq')]
records1<-records%>%mutate(data_diganame=paste(DATE_INPUT,DIAGNAME,sep='|'))%>%arrange(PATINAME,DATE_INPUT)%>%
  select(PATINAME,SEXNAME,IDCARD,BIRTHDATE,CLINICCARDID,freq,data_diganame)
row.names(records1)<-NULL
#n<-freq$freq[!duplicated(freq$freq)]
records_split<-split(records1,records1$IDCARD)
records_split1<-lapply(records_split,function(x){
  x[1:205,1:6]<-x[1,1:6]
  x$seq<-seq(nrow(x)) 
  row.names(x)<-NULL
  return(x)
})
records_split2<-lapply(records_split1,function(x){
  x<-pivot_wider(x,names_from = seq,values_from = data_diganame)
  names(x)[7:211]<-paste("第",1:205,'次诊断',sep='')
  row.names(x)<-x$IDCARD
  return(x)
})
#records2<-unsplit(records_split2,records$IDCARD)
records2<-do.call(rbind,records_split2)
export(records2,'~/records2.xlsx')
