library(rio)
biomark2019<-biomark%>%filter(year==2019,source=='示范区')
biomark2019$PG1<-as.numeric(biomark2019$PG1)
biomark2019$PGR<-as.numeric(biomark2019$PGR)
check2019<-biomark2019%>%filter(PG1<=70 & PGR<=3)#所有符合胃镜复查要求的人群
checked<-import('~/已通知的胃镜名单(截值到10月10日).xlsx')
bio.checked<-checked%>%filter(source=='示范区')#肿瘤标志物已通知复查胃镜的
bio.checking<-anti_join(check2019,bio.checked,by=c('id','name'))
bio.checkde.id<-inner_join(biomark2019,bio.checked,by=c('id','name'))
export(bio.checking,'~/PG胃镜检查名单.xlsx')
rm(list=ls())
