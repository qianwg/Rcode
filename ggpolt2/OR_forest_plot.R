rm(list=ls())
library(rio)
library(tidyverse)
OR<-import('~/data/forest_plot_data.xlsx')
mytheme<-theme(axis.title=element_text(face="bold",size=14),
               axis.text=element_text(face="bold",size=14),
               #axis.text.x= element_text(angle=45,face="bold",size=12,vjust=0.5),
               panel.background = element_blank(),
               axis.line = element_line(color='grey'),
               legend.text = element_text(face='bold',size=13),
               legend.title = element_text(face='bold',size=13),
               legend.background  = element_blank(),
               legend.key = element_blank(),
               legend.justification = 'top'
               
)
OR%>%ggplot(aes(x=`Drug target`,y=or,colour=Study,shape=Study))+geom_point(position = position_dodge(width=0.5),stat='identity')+geom_errorbar(aes(ymin=or_lci95,ymax=or_uci95),width=0.1,size=0.8,position = position_dodge(width=0.5))+
  mytheme+geom_hline(aes(yintercept=1),linetype='dashed',color='red')+labs(y='OR(95%CI)',x='')+coord_flip()+
  scale_y_log10()
