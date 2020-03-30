mydata <- read.csv("randomforest.csv", header=TRUE,sep=",")##读取文件scv
mydata$group<-factor(mydata$group,levels=c(1,2),labels=c("IH","ID"))##将分组变量设为因子，1代表IH，2代表ID，我将原始文件中的IH改为了1，ID改为了2，方便分析。
library(pROC)##加载pROC包，若未安装，通过install.packages("pROC")命令安装
##1求AUC面积
roc(mydata$group,
    mydata$otu44+mydata$otu159+mydata$otu77+mydata$otu225+mydata$otu3+mydata$otu27+mydata$otu212+mydata$otu204+mydata$otu84+mydata$otu11995,
    col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)###roc函数即求出AUC的面积及其置信区间
##2画图
plot.roc(mydata$group,mydata$otu44+mydata$otu159+mydata$otu77+mydata$otu225+mydata$otu3+mydata$otu27+mydata$otu212+mydata$otu204+mydata$otu84+mydata$otu11995,
         add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)##mydata$group是结果变量outcome，后边的加号表示联合一系列变量作为自变量
##legacy.axes表示将横坐标变为1-特异度，col表示颜色设为红色，print.auc=T表示输出auc值，print.thres=T表示输出阈值。
##3标注
legend("bottomright",legend=c("AUC:0.9(0.8491,1)"),col="red",lwd=3)
###若发现45度斜线未通过原点，可通过移动rstudio左侧窗口进行调节，保存时，可选择按照当前尺寸。
