#multi-stage model and competing risk
rm(list=ls())
library(mstate)
library(survival)
path="http://www.uio.no/studier/emner/matnat/math/STK4080/h14/melanoma.txt"
mel=read.table(path,header = T)
summary(mel)
#status:1=death from the disease, 2=censored, 4=death from other cause
#lifetime: life time from operation in years
#ulcer:ulceration(1=present,2=absent)
#thicken: tumor thickness in mm
#grthick:grouped tumor thickness(1:0-1mm ,2:2-5mm, 3:5+mm)
#logthick:logarithm of tumor thickness
#nelson-aalen estimator#par(mfrow=c(1,1))
#par(mfrow=c(1,2))
#survl<-survfit(Surv(lifetime,status==1)~1,data=mel,type='fh')
#plot(survl,fun='cumhaz')
#comparing group graphically
#plot(survfit(Surv(lifetime,status==1)~grthick,data=mel,type='fh'),fun='cumhaz',lty=1:3)
#legend(0,0.87,lty=1:3,c('<2','2-5','>5'),bty='n')
#log-rank test
#survdiff(Surv(lifetime,status==1)~grthick,data=mel)
##竞争风险
mel$status2<-1*(mel$status==1)+2*(mel$status==4)
plot(survfit(Surv(lifetime,status2,type='mstate')~1,data=mel),lty=1:2)
legend(0,0.34,lty=1:2,c('Melanoma death','other cause'),bty='n')
#模拟数据
n<-100
timesick<-rweibull(n,2)
timedeath<-rweibull(n,2)
timesickdeath<-rexp(n)
censtime<-runif(n)*2
# Defines indicators of three events
D01<-1*(timesick<pmin(timedeath,censtime))
D02<-1*(timedeath<pmin(timesick,censtime))
D12<-1*(D01==1)*((timesick+timesickdeath)<censtime)
# Defines censored transition times to sickness and death states
obstimesick=timesick*D01+pmin(timedeath,censtime)*(1-D01)
obstimedeath=timedeath*D02+censtime*D01*(1-D12)+
  (timesick+timesickdeath)*D12+censtime*(1-D01-D02)
# Defines individual process and indicator of death
id=1:n
D2=D02+D01*D12
# Data are put in "wideformat" file with one line per individual
simdat=data.frame(cbind(id,obstimesick,D01,obstimedeath,D2))
head(simdat)
##













