library(rio)
library(survival)
#leukemia<-import('C:/Users/Administrator/Desktop/anderson.sav')
leukemia<-import('~/project/Rcode/survival/anderson.sav')
names(leukemia)<-tolower(names(leukemia))
#leukemia$rx<-factor(leukemia$rx,levels = c(1,0),labels=c('placebo','treatment'))
#leukemia$sex<-factor(leukemia$sex,levels = c(1,0),labels=c('female','male'))
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
model2<-coxph(Surv(survt,status)~rx+logwbc+sex,data=leukemia,method='breslow')
p1<-cox.zph(model2,transform = 'rank')
p1
plot(p1)
# time-dependent variates in cox model
additive<-survSplit(leukemia,cut=leukemia$survt[leukemia$status==1],
                    end='survt',event='status',start='start',id='id')
#3. additive[which(additive$id==11),] 
additive$logtwbc<-additive$logwbc*log(additive$survt)
coxph(Surv(start,survt,status)~sex+logwbc+logtwbc+cluster(id),method='breslow',data=additive)
##logtwbc的p值 is not signiicant,so it meet PH assumption

# stratified cox model(2019-9-01)
#the stratified cox model is a modification of cox ph model that allows for control by
# stratification if a predictor that does not satify th PH assumption.
#1.a single stratified cox model(stratified by sex,上述s结果sex不符合PH假定)=no-interaction model
model1.1<-coxph(Surv(survt,status)~logwbc+rx+strata(sex),data=leukemia,method='breslow') #method='breslow' 其结果与stata，SPSS等结果保保持一致
#2. the general stratified cox model(several predictors and several strata)
#3. no-interaction assumption
#in the stratified cox model ,the coefficients of X are same for the each stratum,estimates of hazard ratios 
#are the same for each stratum,this latter feature of SC model is "no-interaction" assumption
#3.1 how to evaluate the assumption:to test if corresponding coefficiedents are statistically different bewteen
#no-interaction model and the interaction model
#3.2 interaction model(by fitting separate models)
coxph(Surv(survt,status)~logwbc+rx,data=subset(leukemia,sex==1),method='breslow')##Females
coxph(Surv(survt,status)~logwbc+rx,data=subset(leukemia,sex==0),method='breslow')##Males
#3.3 basic analysis
model3.3<-coxph(Surv(survt,status)~logwbc+rx+sex:logwbc+sex:rx+strata(sex),data=leukemia,method="breslow")
#对于女性 在ineraction model中，logwbc的coef=1.639，在no-interaction model中,logwbc:sex的coef(0.4688)+logwbc的coef(1.1701)
#等于1.639,即等于interaction model 中logwbc的coef,同理对于女性中的rx也是相等,同理对于男性也是相等
#故对于interaction model 来说，可以用3.3的代码表示
#4 test the no-interaction assumption
#LR=-2ln(Lr)-(-2Ln(Lf));LR符合df=p(k-1),p=predictors的数量,l=变量的分层数
#Lr=reduced(no-interaction) model;Lf=full(interaction) model
#-2Ln(LR)=-2*ln()
anova(model1.1,model3.3)
# the p value is not significant at the o.o5 level for 2 degrees of freedom
# thus despite the numerical difference between corresponding coefficients in the female and male models.
#there is no statistically significant difference.the no-interaction model is acceptable


















