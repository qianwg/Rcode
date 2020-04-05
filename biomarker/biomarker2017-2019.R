rm(list=ls())
library(rio)
library(ggplot2)
library(ggpubr)
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(face="bold",size=10),
               axis.text=element_text(face="bold",size=9),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               legend.position = 'none',
               strip.background = element_rect(
                 color = "white", fill = "white"),
               strip.text.x = element_blank(),
               panel.spacing.y=unit(0.05, "lines"))
biomarker<-import('~/data/2017-2019肿瘤标志物(剔除自身癌症).sav')
#一 不同肿瘤标志物的人群分布特征及其关联分析（按AFP，CEA，CA125，CA153，CA199，PG，HbsAg分别分析）
