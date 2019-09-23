#模拟数据
set.seed(1000)
data<-matrix(runif(100,0,1),nrow=10)
#定义1-3,4-6,7-10分别为三种类别
library(reshape2)
library(tidyverse)
colnames(data)<-c(paste0('sample',1:3),paste0('sample',4:6),paste0('sample',7:10))
data2<-melt(data,measure.vars=c(paste0('sample',1:10)),variable.name='sample',value.name = 'value')
data2$Var2<-as.character(data2$Var2)
group1<-c(paste0('sample',1:10))
group2<-data.frame(Var2=group1,group=c(1,1,1,2,2,2,3,3,3,3))
group2$Var2<-as.character(group2$Var2)
data3<-left_join(data2,group2,by='Var2')
data3
###均值
data3%>%group_by(Var1,group)%>%summarise(vag=mean(value))
#方差P值
func<-function(x){
  a<-aov(value~group,x)
  b<-summary(a)[[1]][,5][1]
}

b1<-split(data3,data3$Var1)
c<-do.call(rbind,lapply(b1,func))
c
