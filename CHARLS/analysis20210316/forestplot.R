rm(list=ls())
library(forestplot)
library(openxlsx)
#####s读取数据####
Charls<-read.xlsx('~/data/CHARLS/analysis20210316/Results20210316.xlsx',sheet=5)
factors<-Charls$Factor
HR<-Charls$HR
lower<-Charls$LowerCI
upper<-Charls$UpperCI
P<-as.numeric(Charls$P.value)
ORCI<-Charls$`HR(95%CI)`
P2<-as.numeric(Charls$P.for.interaction)
tabletext <- cbind(c('Factors',factors),
                   c('HR(95%CI)',ORCI),
                   c('P value',P),
                   c('P for interaction',P2))
data<- structure(list(
  mean  = c(NA,Charls$HR) ,
  lower = c(NA,Charls$LowerCI),
  upper = c(NA,Charls$UpperCI)),
  .Names = c("mean", "lower", "upper"), 
  row.names = c(NA, -11L), 
  class = "data.frame")
pdf(file='~/tu/forestplot.pdf',height=12,width=8)
forestplot(tabletext,data,
           #is.summary = c(T,F,F,F,F,F,F,F,F,T),
           zero = 1, 
           boxsize = 0.3, #设置点估计的方形大小
           #xlog=TRUE,new_page = TRUE, 
           xticks=c(0,1,2,3,4,5,6,7),
           #xticks=c(0,0,25,0,65,1),
           hrzl_lines = list("2" = gpar(lwd=1,columns=c(1,2,3,4,5),col = "black")),
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
           lwd.zero = 4,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           col=fpColors(box="#444444",line="#444444", summary="#444444", hrz_lines = "#444444"),
           xlab=" ",
           lwd.xaxis=2,#设置X轴线的粗细
           txt_gp = fpTxtGp(ticks=gpar(cex=0.8),label=gpar(cex=0.8),xlab=gpar(cex=0.8)),
           lty.ci = "solid",
           graph.pos = 2,
           line.margin = 0.1
)
dev.off()
