library(rio)
library(ggplot2)
library(patchwork)#该包需要从github上下载,install_github('thomasp85/patchwork')
biomark2019<-import('C:/Users/dell/Desktop/肿瘤标志物汇总2019表.xlsx')
biomark2018<-import('C:/Users/dell/Desktop/瘤标汇总原始2018表.xlsx')
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(face="bold",size=10),
               axis.text=element_text(face="bold",size=9),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA,color='black'))




##2018年
#CEA
biomark2018$CEA.ratio[biomark2018$CEA>5 & biomark2018$CEA<=10]<-1
biomark2018$CEA.ratio[biomark2018$CEA>10 & biomark2018$CEA<=15]<-2
biomark2018$CEA.ratio[biomark2018$CEA>15 & biomark2018$CEA<=20]<-3
biomark2018$CEA.ratio[biomark2018$CEA>20]<-4
a<-data.frame(table(biomark2018$CEA.ratio))
a$Var1<-factor(a$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p1<-ggplot(data=a,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CEA')+scale_y_continuous(expand = c(0,0),limits = c(0,600))
#AFP
biomark2018$AFP.ratio[biomark2018$AFP>7 & biomark2018$AFP<=14]<-1
biomark2018$AFP.ratio[biomark2018$AFP>14 & biomark2018$AFP<=21]<-2
biomark2018$AFP.ratio[biomark2018$AFP>21 & biomark2018$AFP<=28]<-3
biomark2018$AFP.ratio[biomark2018$AFP>28]<-4
a1<-data.frame(table(biomark2018$AFP.ratio))
a1$Var1<-factor(a1$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p2<-ggplot(data=a1,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='AFP',y='')+scale_y_continuous(expand = c(0,0),limits = c(0,400))
#CA199
biomark2018$CA199.ratio[biomark2018$CA199>27 & biomark2018$CA199<=54]<-1
biomark2018$CA199.ratio[biomark2018$CA199>54 & biomark2018$CA199<=81]<-2
biomark2018$CA199.ratio[biomark2018$CA199>81 & biomark2018$CA199<=108]<-3
biomark2018$CA199.ratio[biomark2018$CA199>108]<-4
a2<-data.frame(table(biomark2018$CA199.ratio))
a2$Var1<-factor(a2$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p3<-ggplot(data=a2,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CA199')+scale_y_continuous(expand = c(0,0),limits = c(0,600))
#CA153
biomark2018$CA153.ratio[biomark2018$CA153>25 & biomark2018$CA153<=50]<-1
biomark2018$CA153.ratio[biomark2018$CA153>50 & biomark2018$CA153<=75]<-2
biomark2018$CA153.ratio[biomark2018$CA153>75 & biomark2018$CA153<=100]<-3
biomark2018$CA153.ratio[biomark2018$CA153>100]<-4
a3<-data.frame(table(biomark2018$CA153.ratio))
a3$Var1<-factor(a3$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p4<-ggplot(data=a3,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity',width = 0.5)+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CA153',y='')+scale_y_continuous(expand = c(0,0),limits = c(0,70))
#CA125
biomark2018$CA125.ratio[biomark2018$CA125>35 & biomark2018$CA125<=70]<-1
biomark2018$CA125.ratio[biomark2018$CA125>70 & biomark2018$CA125<=105]<-2
biomark2018$CA125.ratio[biomark2018$CA125>105 & biomark2018$CA125<=140]<-3
biomark2018$CA125.ratio[biomark2018$CA125>140]<-4
a4<-data.frame(table(biomark2018$CA125.ratio))
a4$Var1<-factor(a4$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p5<-ggplot(data=a4,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CA125')+scale_y_continuous(expand = c(0,0),limits = c(0,60))
p6<-ggplot(data=a4,aes(x=Var1,y=Freq))+theme( panel.grid.major = element_line(colour=NA),
                                              panel.grid.minor = element_blank(),
                                              panel.background=element_rect(fill=NA),
                                              axis.text.x=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.title.y=element_blank(),
                                              axis.ticks = element_blank())
(p1 | p2) /
  (p3 | p4) /
  (p5 |p6)
##2018 and 2019
biomark<-import('C:/Users/dell/Desktop/biomark2018-2019.xlsx')
biomark$CEA.ratio[biomark$CEA>5 & biomark$CEA<=10]<-1
biomark$CEA.ratio[biomark$CEA>10 & biomark$CEA<=15]<-2
biomark$CEA.ratio[biomark$CEA>15 & biomark$CEA<=20]<-3
biomark$CEA.ratio[biomark$CEA>20]<-4
a<-data.frame(table(biomark$CEA.ratio))
a$Var1<-factor(a$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p1<-ggplot(data=a,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CEA')+scale_y_continuous(expand = c(0,0),limits = c(0,750))
#AFP
biomark$AFP.ratio[biomark$AFP>7 & biomark$AFP<=14]<-1
biomark$AFP.ratio[biomark$AFP>14 & biomark$AFP<=21]<-2
biomark$AFP.ratio[biomark$AFP>21 & biomark$AFP<=28]<-3
biomark$AFP.ratio[biomark$AFP>28]<-4
a1<-data.frame(table(biomark$AFP.ratio))
a1$Var1<-factor(a1$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p2<-ggplot(data=a1,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='AFP',y='')+scale_y_continuous(expand = c(0,0),limits = c(0,550))
#CA199
biomark$CA199.ratio[biomark$CA199>27 & biomark$CA199<=54]<-1
biomark$CA199.ratio[biomark$CA199>54 & biomark$CA199<=81]<-2
biomark$CA199.ratio[biomark$CA199>81 & biomark$CA199<=108]<-3
biomark$CA199.ratio[biomark$CA199>108]<-4
a2<-data.frame(table(biomark$CA199.ratio))
a2$Var1<-factor(a2$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p3<-ggplot(data=a2,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CA199')+scale_y_continuous(expand = c(0,0),limits=c(0,750))
#CA153
biomark$CA153.ratio[biomark$CA153>25 & biomark$CA153<=50]<-1
biomark$CA153.ratio[biomark$CA153>50 & biomark$CA153<=75]<-2
biomark$CA153.ratio[biomark$CA153>75 & biomark$CA153<=100]<-3
biomark$CA153.ratio[biomark$CA153>100]<-4
a3<-data.frame(table(biomark$CA153.ratio))
a3$Var1<-factor(a3$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p4<-ggplot(data=a3,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity',width = 0.5)+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CA153',y='')+scale_y_continuous(expand = c(0,0),limits=c(0,65))
#CA125
biomark$CA125.ratio[biomark$CA125>35 & biomark$CA125<=70]<-1
biomark$CA125.ratio[biomark$CA125>70 & biomark$CA125<=105]<-2
biomark$CA125.ratio[biomark$CA125>105 & biomark$CA125<=140]<-3
biomark$CA125.ratio[biomark$CA125>140]<-4
a4<-data.frame(table(biomark$CA125.ratio))
a4$Var1<-factor(a4$Var1,levels = c(1,2,3,4),labels = c('1-2倍','2-3倍','3-4倍','4倍以上'))
p5<-ggplot(data=a4,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+
  theme(legend.position = 'none')+geom_text(aes(label=Freq),vjust=0.05)+mytheme+labs(x='CA125')+scale_y_continuous(expand = c(0,0),limits=c(0,60))
p6<-ggplot(data=a4,aes(x=Var1,y=Freq))+theme( panel.grid.major = element_line(colour=NA),
                                              panel.grid.minor = element_blank(),
                                              panel.background=element_rect(fill=NA),
                                              axis.text.x=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.title.y=element_blank(),
                                              axis.ticks = element_blank())
(p1 | p2) /
  (p3 | p4) /
  (p5 |p6)
summary(biomark)
##
library(flextable)
library(xtable)
percent_value<-function(x){
  p1<-round(quantile(x,0.01,na.rm=TRUE),2)
  p2.5<-round(quantile(x,0.025,na.rm=TRUE),2)
  p5<-round(quantile(x,0.05,na.rm=TRUE),2)
  p10<-round(quantile(x,0.1,na.rm=TRUE),2)
  p25<-round(quantile(x,0.25,na.rm=TRUE),2)
  p50<-round(quantile(x,0.5,na.rm=TRUE),2)
  p75<-round(quantile(x,0.75,na.rm=TRUE),2)
  p90<-round(quantile(x,0.9,na.rm=TRUE),2)
  p95<-round(quantile(x,0.95,na.rm=TRUE),2)
  p97.5<-round(quantile(x,0.975,na.rm=TRUE),2)
  p98<-round(quantile(x,0.98,na.rm=TRUE),2)
  p99<-round(quantile(x,0.99,na.rm=TRUE),2)
  table<-c(p1,p2.5,p5,p10,p25,p50,p75,p90,p95,p97.5,p98,p99)
}
  table<-data.frame(t(apply(biomark[,c('AFP','CEA','CA199','CA153','CA125')],2,percent_value)))
  names(table)<-c('1%','2.5%','5%','10%','25%','50%','75%','90%','95%','97.5','98%','99%')
  rownames(table)<-c('AFP(0-7)','CEA(0-5)','CA199(0-27)','CA153(0-25)','CA125(0-35)')
  ft<-xtable_to_flextable(xtable(table,digits=2))
  ft
  