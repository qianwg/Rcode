library(rio)
library(survival)
leukemia<-import('C:/Users/Administrator/Desktop/anderson.sav')
names(leukemia)<-tolower(names(leukemia))
leukemia$rx<-factor(leukemia$rx,levels = c(1,0),labels=c('placebo','treatment'))
leukemia$sex<-factor(leukemia$sex,levels = c(1,0),labels=c('male','female'))
head(leukemia)
#PH假定检验
#1.图形法
#1.1 PH假定方法1(log-log survival curves)
with(leukemia,plot(survfit(Surv(survt,status)~rx),fun='cloglog'))

#1.2 PH假定的图形法2（compares 'observed' with 'expected' survival curves）
#uses the stratified cox ph model to obtain observed plots
#1.2.1 KM(observed) plots by Rx group
a1<-survfit(Surv(survt,status)~rx,data=leukemia)
plot(a1,lwd=2)
#1.2.2 EXperted survival plots by Rx group Using cox PH model
model<-coxph(Surv(survt,status)~rx,data=leukemia)
newdat<-data.frame(rx=leukemia$rx)
par(new=TRUE)
plot(survfit(model,newdata=newdat),col='red',lwd=2)
title('Observed Versus Expected Plote by Rx')
legend('topright',c('Expected','Observed'),col=c('black','red'),lwd=2)
#2. the goodness of fit(拟合优度检验)
model2<-coxph(Surv(survt,status)~rx+logwbc+sex,data=leukemia)
p1<-cox.zph(model2,transform = 'rank')
plot(p1)
# time-dependent variates in cox model
additive<-survSplit(leukemia,cut=leukemia$survt[leukemia$status==1],
                    end='survt',event='status',start='start',id='id')
#3. additive[which(additive$id==11),] 
additive$logtwbc<-additive$logwbc*log(additive$survt)
coxph(Surv(start,survt,status)~sex+logwbc+logtwbc+cluster(id),method='breslow',data=additive)
##logtwbc的p值 is not signiicant,so it meet PH assumption

# stratified cox model


