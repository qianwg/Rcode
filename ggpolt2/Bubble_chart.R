library(ggplot2)
library(gtable)
library(grid) 
pathway = import("~/data/GEO.xlsx")
names(pathway)[3]<-'Pathway'
pathway$ratio<-pathway$Count/550
p = ggplot(pathway,aes(ratio,reorder(Pathway,ratio)))
p=p + geom_point()  
# 修稿点的大小
p=p + geom_point(aes(size=Count))
# 展示三维数据
pbubble = p+ geom_point(aes(size=Count,color=-1*log10(Pvalue)))
# 设置渐变色
pr = pbubble+scale_color_gradient(low="blue",high = "red")
# 绘制p气泡图
pr = pr+labs(color=expression(-log[10](Pvalue)),size="Count",  
             x="Gene Ratio",y="",title="Pathway enrichment")
pr + theme_bw()
##
source('~/Rcode/two_y_axis.R')
source('~/Rcode/two_x_axis.R')

mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(face="bold",size=10),
               axis.text=element_text(face="bold",size=9),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA))
p1<-ggplot(pathway, aes(Pathway,Count)) +
  geom_bar(stat="identity", width=0.8,fill='red') + coord_flip() +
  theme_bw() +
  xlab("") + ylab("Gene Count")+ mytheme
p1
p2<-ggplot(pathway,aes(x=factor(Pathway),y=-1*log10(Pvalue),group=1))+geom_line(color='blue')+coord_flip()+
  xlab("") + ylab("-log10(Pvalue)")+mytheme
p2
ggplot2.two_x_axis(p1,p2)
