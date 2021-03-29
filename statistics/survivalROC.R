rm(list = ls())

library(survival)
library(survivalROC)
library(timeROC)


Total <- read.csv("E:/ROC.csv" )

fit0 <- coxph( Surv(Total$time1,Total$event_c)
               ~ Total$age + Total$sex1 + Total$xybn_c3 + Total$a_E04_1+Total$a_E04_3, na.action=na.omit )
eta <- fit0$linear.predictor

year_survival <-  apply( data.frame( c(365, 365*3,365*5,365*6 ) ) ,1,function(x){
  year_survival <- survivalROC(Stime=Total$time1, status=Total$event_c,
                               marker=eta, predict.time=x , method="KM")
  year_survival <- data.frame(year_survival ) 
  return(year_survival)
})

ROC.bili<-timeROC(T=Total$time1,
                  delta=Total$event_c,marker=eta,
                  cause=1,weighting="marginal",
                  times=1*365  ,
                  iid=TRUE)


library( ggplot2 )
dat <- read.csv("D:/Desktop/year_survival.csv")
dat1 <- data.frame(TP=c(dat$TP,dat$TP.1,dat$TP.2,dat$TP.3 ),
                   FP=c(dat$FP,dat$FP.1,dat$FP.2,dat$FP.3 ),
                   Group=rep(c('1-year survival','3-year survival','5-year survival','6-year survival' ),each=213 ))
unique( c(dat$AUC,dat$AUC.1,dat$AUC.2,dat$AUC.3 ))

ggplot(dat1,aes(x=FP,y=TP,group= Group,color =Group))+
  geom_line(size=1)+
  # ggtitle("开滦肺癌风险Cox回归模型高危预测能力准确性比较") +
  xlab("1-Specificity") + ylab("Sensitivity")+
  theme_classic()+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black",linetype=4)+
  theme(axis.line = element_line(size=0.68, colour = "black"), ## 再加上坐标轴（无刻度、无标签）
        axis.text.x=element_text(size=15, color="black",face = "bold"),#hjust=0.5,vjust=0.3,angle =8,
        axis.text.y=element_text(size=15,hjust=0,vjust=0.8,color="black",face = "bold"),
        axis.title.x = element_text(size=15,vjust=1.8,color="black",face = "bold"),
        axis.title.y=element_text(size=15,vjust=1.8,color="black",face = "bold"),
        axis.ticks.length.y = unit(-0.15, "cm"),#内刻度
        axis.ticks.x = element_blank()) +
  scale_color_discrete(
    breaks=c('1-year survival','3-year survival','5-year survival','6-year survival' ),
    labels=c('AUC at 1 year  : 0.758','AUC at 3 years: 0.736','AUC at 5 years: 0.741','AUC at 6 years: 0.745' ))+
  theme(legend.position = c(0.75,0.25),
        legend.text = element_text( size=17,face = 'bold'),
        legend.title=element_text(size=18,face = 'bold'),
        legend.key.size = unit(0.8, "cm"),
        legend.background=element_rect( colour=NA))+#图例边框颜色
  guides(color=guide_legend(title='' ))



# calibrate ---------------------------------------------------------------
library(Hmisc)
library(lattice)
library(Formula)
library(rms)
fit1 <- cph( Surv(time1,event_c) ~ age + sex1 + xybn_c3 + a_E04_1+ a_E04_3, x=T,y=T,time.inc=365,na.action=na.omit, data =Total,surv=TRUE  )
cal <- calibrate(fit1,u=365,cmethod="KM", method="boot",m=28000,B=10000) 




plot(cal,xlim = c(0,0.0001),ylim= c(0,0.0001),
     errbar.col=c(rgb(0,0,0,maxColorValue=255)),
     col=c(rgb(255,0,0,maxColorValue=255)),
     xlab = "Predicated 1-year CSS", 
     ylab = "Actual 1-year CSS")
abline(0,1,lty=3,lwd=2,col=c(rgb(0,0,255,maxColorValue= 255)))


