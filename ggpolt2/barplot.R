rm(list=ls())
data<-import('C:/Users/sheng/Desktop/fig2.xlsx')
data2<-data%>%pivot_longer(cols=c('n1','n2'),names_to = 'n',values_to = 'value')
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
ggplot(data2,aes(reorder(group3,n3),
y=value,fill = n))+geom_bar(stat='identity',position='fill',width = 0.8)+labs(x='',y='',fill='')+
coord_flip()+mytheme+facet_wrap(.~group1,nrow=3)
