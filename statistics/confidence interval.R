####置信区间的绘制
or<-c(1.03,0.58,0.65,0.11,0.72,1.21,0.67,0.78,0.97)
low<-c(0.86,0.1,0.33,0.01,0.12,0.67,0.25,0.47,0.82)
up<-c(1.24,3.36,1.28,1.98,4.12,2.18,1.81,1.29,1.13)
CIs<-data.frame(CIlow=low,or=or,CIup=up)
plot(0,xlim=c(0,10),ylim=c(min(CIs)-0.4,max(CIs)+0.4),type="n",bty="l")
for(i in 1:nrow(CIs)){
  lines(x=rep(i,3),y=c(CIs[i,1],CIs[i,2],CIs[i,3]),lwd=2) 
  points(x=rep(i,3),y=c(CIs[i,1],CIs[i,2],CIs[i,3]),pch=16,col="blue") }
abline(h=1,lwd=2,col="black")