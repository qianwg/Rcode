rm(list=ls())
source('~/Rcode/screening/gastric_screening2020/data2020.R')
biomarker<-pepsinogen2020%>%select(PG1,PG2,PGR)
Normal<-function(y,data){
  result1<-data.frame()
  for(i in y){
    ks<-ks.test(data[,i],pnorm)
    table<-data.frame(variable=i,statistics=ks$statistic,p=ks$p.value)
    result1 <- rbind(result1,table)
  }
  row.names(result1)=NULL
  return(result1)
}
Normal(y=c('PG1','PG2','PGR'),data=biomarker)


print(ks.test(biomarker[,'PGR'],pnorm),digits=5)
a
