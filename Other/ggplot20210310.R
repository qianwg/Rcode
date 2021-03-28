##力文师姐作图--2021-03-10
rm(list=ls())
library(patchwork)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(rio)
library(tidyverse)
data2<-import('~/data/Other/xiangshitu.csv')%>%select(-V1)
mytheme<-theme(plot.title=element_text(hjust=0.5),
               axis.title=element_text(face="bold",size=10),
               axis.text=element_text(face="bold",size=10),
               axis.text.x  = element_text(face="bold",size=10),
               #panel.grid.major = element_line(colour=NA),
               #panel.grid.major.x = element_line(color='grey'),
               #panel.grid.major.y = element_line(color='grey'),
               panel.background = element_blank(),
               axis.line = element_line(color='grey'),
               legend.title=element_text(face="bold",size=14),
               legend.text = element_text(face='bold',size=13),
               #legend.position = 'none',
               #strip.text.x =element_text(face='blod',color='red')
)
jpeg(filename = '~/data/Other/plot_sandian_hos_x1.jpeg',height = 1200,width =1800)
#jpeg(filename = '/Users/liulipeng/Desktop/plot_sandian_hos_x2.jpeg',height = 1200,width =1800)
p=list()
symnum.args<-list(cutpoints = c(0,0.001, 0.01, 0.05, 1), symbols = c("P<0.001", "P<0.01", "P<0.05", "ns"))
for (i in 1:(ncol(data2)-1)){
  y=colnames(data2)[i]
  p[[i]]=ggplot(data=data2)+aes_string(x='group',y=y)+
    geom_boxplot(aes(color=group))+
    mytheme+labs(x="")+scale_y_continuous(limits=c(min(data2[,y]),ifelse(max(data2[,y])<=0,0,max(data2[,y]))))+
    stat_compare_means(label = "p.signif",symnum.args = symnum.args,label.x=1.5,
                       label.y = ifelse(max(data2[,y])*0.75<=0,-0.05,max(data2[,y])*0.75),cex=3)
  
}
wrap_plots(p,nrow = 8,guides = 'collect',heights=1.5,widths=1.5)
while (!is.null(dev.list()))  dev.off()


