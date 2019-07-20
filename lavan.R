library(lavaan)
library(rio)
## =~表示潜变量，~表示回归方程，~~表示方差和协方差，f~1表示截距项的回归方程
know<-import('know20190524_2.xlsx')
model<-'Y1=~know1+know2+know3+know4
Y2=~know5+know6+know7+know8
Y3=~know9+know10'
data<-data.frame(lapply(know,ordered))
result<-cfa(model,data,ordered = paste0('know',1:10))
summary(result,standardized=TRUE,fit.measure=TRUE)
