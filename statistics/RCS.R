library(ggplot2)
library(rms)
n <- 1000
set.seed(731)
age <- 50 + 12*rnorm(n)
label(age) <- "Age"
sex <- factor(sample(c('Male','Female'), n, rep=TRUE, prob=c(.6, .4)))
cens <- 15*runif(n)
h <- .02*exp(.04*(age-50)+.8*(sex=='Female'))
time<- -log(runif(n))/h
label(time) <- 'Follow-up Time'
death<- ifelse(time <= cens,1,0)
time <- pmin(time, cens)
units(time) <- "Year"
data<-data.frame(age,sex,time,death)
#######开始正式画图
dd <- datadist(data) #为后续程序设定数据环境
options(datadist='dd') #为后续程序设定数据环境
####拟合cox回归模型，注意这里的R命令是“cph”，而不是常见的生存分析中用到的“coxph"命令
fit<- cph(Surv(time,death) ~ rcs(age,4) + sex,data=data) 
dd$limits$age[2] <- 50 ###这里是设置参考点，也就是HR为1的点，常见的为中位数或者临床有意义的点 
fit=update(fit)
HR<-Predict(fit, age,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=HR, aes(age,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR, aes(age,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="age", y="HR (95%CI)") 

##RR
dd <- datadist(pepsinogen) 
options(datadist='dd')
fit<- lrm(PG_pos ~ rcs(年龄,4) + 性别,data=pepsinogen) 
#dd$limits$age[2] <- 50 ###这里是设置参考点，也就是HR为1的点，常见的为中位数或者临床有意义的点 
fit=update(fit)
RR<-Predict(fit, 年龄,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(年龄,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(年龄,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="age", y="RR (95%CI)") 
anova(fit)