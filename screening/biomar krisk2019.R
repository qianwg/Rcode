library(rio)
biomark<-import('C:/Users/dell/Desktop/biomark.xlsx')
biomark2<-within(biomark,{
  CEA.risk<-vector()
  AFP.risk<-vector()
  CA199.risk<-vector()
  CA153.risk<-vector()
  CA125.risk<-vector()
  #CEA
  CEA.risk[CEA<=5]<-0
  CEA.risk[CEA>5 & CEA<=10]<-1
  CEA.risk[CEA>10 & CEA<=15]<-2
  CEA.risk[CEA>15 & CEA<=20]<-3
  CEA.risk[CEA>20]<-4
  #AFP
  AFP.risk[AFP<=7]<-0
  AFP.risk[AFP>7 & AFP<=14]<-1
  AFP.risk[AFP>14 & AFP<=21]<-2
  AFP.risk[AFP>21 & AFP<=28]<-3
  AFP.risk[AFP>28]<-4
  #CA199
  CA199.risk[CA199<=27]<-0
  CA199.risk[CA199>27 & CA199<=27*2]<-1
  CA199.risk[CA199>27*2 & CA199<=27*3]<-2
  CA199.risk[CA199>27*3 & CA199<=27*4]<-3
  CA199.risk[CA199>27*4]<-4
  #CA153
  CA153.risk[CA153<=30]<-0
  CA153.risk[CA153>30 & CA153<=60]<-1
  CA153.risk[CA153>60 & CA153<=90]<-2
  CA153.risk[CA153>90 & CA153<=120]<-3
  CA153.risk[CA153>120]<-4
  #CA125
  CA125.risk[CA125<=35]<-0
  CA125.risk[CA125>35 & CA125<=70]<-1
  CA125.risk[CA125>70 & CA125<=105]<-2
  CA125.risk[CA125>105 & CA125<=140]<-3
  CA125.risk[CA125>140]<-4
  
  
})
table(biomark2$CEA.risk)
table(biomark4[,1])
risk<-function(x){
  x2<-factor(x,levels = c(0,1,2,3,4),labels=c('正常','截值1-2倍','截值2-3倍','截值3-4倍','截值>4倍'))
  return(x2)
}
biomark2[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')]<-apply(biomark2[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,risk)
str(biomark2$CEA.risk)
##输出高危分布情况
###1.表格
library(tidyverse)
data.frame(biomark2%>%group_by(AFP.risk)%>%summarise(freq=n()))
##2.图像
with(subset(biomark2,CEA.risk!='正常'),{
  n<-data.frame(table(CEA.risk))[,2] 
  barplot(table(CEA.risk))
  text(n,pos=1,cex=1.5)})
ggplot()