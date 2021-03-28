#install.packages("forestplot")
library(forestplot)
library(rio)
rs_forest<-import('~/data/血常规结果CRP与多种癌症风险2.xlsx',sheet=1)
rs_forest <- read.csv('~/data/森林图数据.csv',header = FALSE)
#tiff('~/Figure 1.tiff',height = 6000,width = 7000,res= 600)
forestplot(labeltext = as.matrix(rs_forest[,1:2]),
           mean = rs_forest$...3, #设置均值
           lower = rs_forest$...4, #设置均值的lowlimits限
           upper = rs_forest$...5, #设置均值的uplimits限
           #is.summary = c(T,T,T,F,F,T,F,F,T,F,F,T,F,F,T,F,F),
           #该参数接受一个逻辑向量，用于定义数据中的每一行是否是汇总值，若是，则在对应位置设置为TRUE，若否，则设置为FALSE；设置为TRUE的行则以粗体出现
           zero = 1, #设置参照值，此处我们展示的是HR值，故参照值是1，而不是0
           boxsize = 0.3, #设置点估计的方形大小
           xticks=c(0.2,1,5,1.5,3),
           #clop=c(0.01,30),
           xlog=TRUE,
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           col=fpColors(box='#458B00', summary= "#8B008B",lines = 'black',zero = '#7AC5CD'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           xlab="The estimates",#设置x轴标签
           lwd.xaxis=2,#设置X轴线的粗细
           lty.ci = "solid",
           graph.pos = 3,#设置森林图的位置，此处设置为4，则出现在第四列
           line.margin = 0.1
           )
#####另一种森林图做法####
#install.packages("forestplot")
rm(list=ls())
library(forestplot)
library(rio)
#####single_male_PCSK9####
SNP<-c('rs2479394','rs572512','rs11206510','rs2479409','rs2495477','rs10493176','rs585131','rs11206514',NA,'All - Inverse variance weighted')
OR<-c(6.95,6,3.22,2.58,1.94,1.74,1.12,0.65,NA,2.2)
lower<-c(0.98,0.95,1.02,0.76,0.55,0.32,0.26,0.14,NA,1.24)
upper<-c(49.33,38,10.19,8.73,6.81,9.47,4.74,3.05,NA,3.9)
P<-c(0.052,0.057,0.046,0.129,0.301,0.524,0.881,0.585,NA,0.007)
ORCI<-paste0(OR,'(',lower,'-',upper,')')
ORCI[9]<-NA
tabletext <- cbind(c("SNP",SNP),
                   c("Odd Ratios(95%CI)",ORCI),
                   c("P Value",P))
data<- structure(list(
  mean  = c(NA,6.95,6,3.22,2.58,1.94,1.74,1.12,0.65,NA,2.2), 
  lower = c(NA,0.98,0.95,1.02,0.76,0.55,0.32,0.26,0.14,NA,1.24),
  upper = c(NA,49.33,38,10.19,8.73,6.81,9.47,4.74,3.05,NA,3.9)),
  .Names = c("mean", "lower", "upper"), 
  row.names = c(NA, -11L), 
  class = "data.frame")
forestplot(tabletext,data,
           is.summary = c(T,F,F,F,F,F,F,F,F,T),
           zero = 1, 
           boxsize = 0.3, #设置点估计的方形大小
           xlog=TRUE,new_page = TRUE,
           xticks=c(0.12,0.5,1,2,5,32),
           hrzl_lines = list("2" = gpar(lwd=1,columns=c(1,2,3),col = "black")),
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
           lwd.zero = 4,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           col=fpColors(box="#444444",line="#444444", summary="#444444", hrz_lines = "#444444"),
           xlab=" ",
           lwd.xaxis=2,#设置X轴线的粗细
           txt_gp = fpTxtGp(ticks=gpar(cex=0.8)),
           lty.ci = "solid",
           graph.pos = 4,
           line.margin = 0.1
)
####single_female_PCSK9####
#
SNP<-c('rs118146573','rs12920974','rs12448528','rs1864163','rs289714','rs9989419','rs9929488','rs247616',NA,'All - Inverse variance weighted')
OR<-c(28.01,16.43,15.15,6.19,4.08,3.49,3.26,1.93,NA,4.31)
lower<-c(2.1,0.75,0.77,0.62,0.17,0.16,0.24,0.37,NA,1.19)
upper<-c(374.19,361.18,297.06,61.62,98.6,78.12,44.2,10.21,NA,15.58)
P<-c(0.012,0.076,0.073,0.12,0.387,0.43,0.375,0.437,NA,0.026)
ORCI<-paste0(OR,'(',lower,'-',upper,')')
ORCI[9]<-NA
tabletext <- cbind(c("SNP",SNP),
                   c("Odd Ratios(95%CI)",ORCI),
                   c("P Value",P))
data<- structure(list(
  mean  = c(NA,28.01,16.43,15.15,6.19,4.08,3.49,3.26,1.93,NA,4.31), 
  lower = c(NA,2.1,0.75,0.77,0.62,0.17,0.16,0.24,0.37,NA,1.19),
  upper = c(NA,374.19,361.18,297.06,61.62,98.6,78.12,44.2,10.21,NA,15.58)),
  .Names = c("mean", "lower", "upper"), 
  row.names = c(NA, -11L), 
  class = "data.frame")
forestplot(tabletext,data,
           is.summary = c(T,F,F,F,F,F,F,F,F,T),
           zero = 1, 
           boxsize = 0.3, #设置点估计的方形大小
           xlog=TRUE,new_page = TRUE,
           clip=c(0.5,35),
           xticks=c(0.12,1,6,15,35), xticks.digits = 2,
           hrzl_lines = list("2" = gpar(lwd=1,columns=c(1,2,3),col = "black")),
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
           lwd.zero = 4,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           col=fpColors(box="#444444",line="#444444", summary="#444444", hrz_lines = "#444444"),
           xlab=" ",
           lwd.xaxis=2,#设置X轴线的粗细
           txt_gp = fpTxtGp(ticks=gpar(cex=0.8)),
           lty.ci = "solid",
           graph.pos = 4,
           line.margin = 0.1
)



