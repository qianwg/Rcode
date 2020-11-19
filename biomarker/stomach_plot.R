library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
#数据读取
source('~/Rcode/biomarker/stomach_data.R')
#基本分布
PG1.plot<-pepsinogen%>%group_by(PG1_range)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI',label = 'percent2')+scale_y_continuous(limits=c(0,70))
PGR.plot<-pepsinogen%>%filter(PG1!=200)%>%group_by(PGR_range)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PGR_range',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI/II ratio',label = 'percent2')+scale_y_continuous(limits = c(0,100))
PG1R.plot<-pepsinogen%>%mutate(m='v')%>%filter(PG1!=200)%>%group_by(m,PG1_range,PGR_range)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range',y='percent',fill='PGR_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label='percent2')+labs(fill='PGI/II ratio')+scale_y_continuous(limits=c(0,45))
(PG1.plot |PGR.plot)/PG1R.plot +plot_annotation(tag_level = 'A')
#性别分布
median2.sex<-pepsinogen%>%group_by(性别)%>%summarise(median=median(PG1))
median3.sex<-pepsinogen%>%filter(PG2<100)%>%group_by(性别)%>%summarise(median=median(PG2))
median1.sex<-pepsinogen%>%filter(!is.na(PGR),PGR<30)%>%group_by(性别)%>%summarise(median=median(PGR))
#1
pg1.sex<-pepsinogen%>%ggviolin(x="性别", y="PG1", fill = "性别", 
                               palette = c("#00AFBB", "#E7B800"), 
                               add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(label.y = 260,label.x=1)+theme(legend.position = 'none')+labs(x='',title='PGI')+
  geom_point(data=median2.sex,aes(x=性别,y=median),color='red')+geom_line(data=median2.sex,aes(x=as.numeric(性别),y=median),color='red',size=1)
#2
pg1.sex2<-pepsinogen%>%group_by(PG1_range,性别) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = PG1_range, y = perc, fill =性别)) +
  geom_bar(stat="identity", width = 0.6,position = position_dodge()) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_dodge(width=0.7))+
  labs(x='',y='Percent',fill='Sex')
#3

pg2.sex<-pepsinogen%>%filter(PG2<100)%>%ggviolin(x="性别", y="PG2", fill = "性别", 
                                                 palette = c("#00AFBB", "#E7B800"), 
                                                 add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(label.y = 100,label.x=1)+theme(legend.position = 'none')+labs(x='',title='PGII')+
  geom_point(data=median3.sex,aes(x=性别,y=median),color='red')+geom_line(data=median3.sex,aes(x=as.numeric(性别),y=median),color='red',size=1)
#4
pgr.sex<-pepsinogen%>%filter(PGR<30,!is.na(PGR))%>%ggviolin(x="性别", y="PGR", fill = "性别", 
                                                            palette = c("#00AFBB", "#E7B800"), 
                                                            add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(label.y = 30,label.x=1)+theme(legend.position = 'none')+labs(x='',title='PGI/II ratio')+
  geom_point(data=median1.sex,aes(x=性别,y=median),color='red')+geom_line(data=median1.sex,aes(x=as.numeric(性别),y=median),color='red',size=1)
#5
pgr.sex2<-pepsinogen%>%filter(PGR!=30)%>%group_by(PGR_range,性别) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = PGR_range, y = perc, fill =性别)) +
  geom_bar(stat="identity", width = 0.6,position = position_dodge()) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_dodge(width=0.7))+
  labs(x='',y='Percent',fill='Sex')
#combine
(pg1.sex + pg1.sex2 +plot_annotation(title='PGI')) / (pgr.sex | pgr.sex2)#+plot_annotation(tag_levels = 'A')
##年龄分布
pg1.age<-pepsinogen%>%filter(年龄>=40,年龄<=74)%>%group_by(年龄)%>%
  summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75),median.pg2=median(PG2),Q1.pg2=quantile(PG2,0.25),Q3.pg2=quantile(PG2,0.75))%>%
  ggplot(aes(x=年龄))+geom_ribbon(aes(ymin=Q1,ymax=Q3), fill="#6699CC", alpha=.4)+mytheme+
  geom_line(aes(y=median,color="PGI"),size=1)+
  geom_ribbon(aes(ymin=Q1.pg2,ymax=Q3.pg2),fill="#FFCC00",alpha=0.4)+
  geom_line(aes(y=median.pg2,colour="PGII"),size=1)+
  labs(x = "Age",y='Median(Q1-Q3)',colour='PG')+scale_x_continuous(breaks=seq(42,74,5))+
  scale_y_log10()+scale_color_manual(values=c("PGI"="#003366","PGII"="#FFCC00"))
pg2.age<-pepsinogen%>%filter(年龄>=40,年龄<=74,!is.na(PGR))%>%group_by(年龄)%>%
  summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_ribbon(aes(x=年龄,ymin=Q1,ymax=Q3), fill="#FF9933", alpha=.6)+
  mytheme+geom_line(aes(x=年龄,y=median,colour='PGR'),size=1)+
  labs(x = "Age",y='',colour='')+scale_x_continuous(breaks=seq(42,74,5))+scale_color_manual(values=c('PGR'='#FF9933'))
(pg1.age | pg2.age) + plot_layout(guides='collect') & theme(legend.position = 'top')
##在年龄中PG的分布
pg1.age1<-pepsinogen%>%filter(!is.na(年龄分组3))%>%group_by(性别,年龄分组3,PG1_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = 年龄分组3, y = perc, fill =PG1_range)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_stack(vjust = 0.5))+
  labs(x='',y='Percent',fill='PGI')+facet_grid(.~性别)
pgr.age1<-pepsinogen%>%filter(!is.na(年龄分组3),!is.na(PGR))%>%group_by(性别,年龄分组3,PGR_range) %>% 
  summarise(count=n()) %>% 
  mutate(perc=round((count/sum(count))*100,2))%>%
  ggplot(aes(x = 年龄分组3, y = perc, fill =PGR_range)) +
  geom_bar(stat="identity", width = 0.7) +
  theme_minimal(base_size = 14)+geom_text(aes(label=perc),position = position_stack(vjust = 0.5))+
  labs(x='Age',y='Percent',fill='PGI/II ratio')+facet_grid(.~性别)
pg1.age1 /pgr.age1 #+plot_annotation(tag_levels = 'A')
##在吸烟中的分布
median.smoking1<-pepsinogen%>%group_by(吸烟)%>%summarise(median=median(PG1))
median.smoking2<-pepsinogen%>%group_by(吸烟)%>%summarise(median=median(PG2))
median.smoking3<-pepsinogen%>%filter(PGR<30)%>%group_by(吸烟)%>%summarise(median=median(PGR))
pg1.smoking<-pepsinogen%>%ggviolin(x="吸烟", y="PG1", fill = "吸烟", 
                                   palette = c("#00AFBB", "#E7B800"), 
                                   add = "boxplot", add.params = list(fill="white"))+ 
                                   stat_compare_means(label.y = 260,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+
                                   geom_point(data=median.smoking1,aes(x=吸烟,y=median),color='red')+
                                   geom_line(data=median.smoking1,aes(x=as.numeric(吸烟),y=median),color='red',size=1)+
                                   labs(y='PGI',title='PGI')+scale_x_discrete(labels=c('Never','Current/Ever'))

pg2.smoking<-pepsinogen%>%filter(PG2<100)%>%ggviolin(x="吸烟", y="PG2", fill = "吸烟", 
                                                     palette = c("#00AFBB", "#E7B800"), 
                                                     add = "boxplot", add.params = list(fill="white"))+ 
                                                     stat_compare_means(label.y = 100,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+
                                                     geom_point(data=median.smoking2,aes(x=吸烟,y=median),color='red')+
                                                     geom_line(data=median.smoking2,aes(x=as.numeric(吸烟),y=median),color='red',size=1)+
                                                     labs(y='PGII',title='PGII')+scale_x_discrete(labels=c('Never','Current/Ever'))

pgr.smoking<-ggviolin(pepsinogen[which(pepsinogen$PGR<30),],x="吸烟", y="PGR", fill = "吸烟", 
                      palette = c("#00AFBB", "#E7B800"), 
                      add = "boxplot", add.params = list(fill="white"))+
                      stat_compare_means(label.y = 30,label.x=0.7)+theme(legend.position = 'none')+labs(x='')+
                      geom_point(data=median.smoking3,aes(x=吸烟,y=median),color='red')+
                      geom_line(data=median.smoking3,aes(x=as.numeric(吸烟),y=median),color='red',size=1)+
                      labs(y='PGI/II ratio',title='PGI/II ratio')+scale_x_discrete(labels=c('Never','Current/Ever'))

(pg1.smoking |  pgr.smoking) 

##在饮酒中的分布
median.alcohol1<-pepsinogen%>%group_by(饮酒)%>%summarise(median=median(PG1))
median.alcohol2<-pepsinogen%>%group_by(饮酒)%>%summarise(median=median(PG2))
median.alcohol3<-pepsinogen%>%filter(PGR<30)%>%group_by(饮酒)%>%summarise(median=median(PGR))
pg1.alcohol<-pepsinogen%>%ggviolin(x="饮酒", y="PG1", fill = "饮酒", 
                                   palette = c("#00AFBB", "#E7B800"), 
                                   add = "boxplot", add.params = list(fill="white"))+ 
                                  theme(legend.position = 'none')+labs(x='',y='PGI',title='PGI')+
                                  geom_point(data=median.alcohol1,aes(x=饮酒,y=median),color='red')+geom_line(data=median.alcohol1,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
                                  stat_compare_means(label.y = 260,label.x=0.7)+
                                  scale_x_discrete(labels=c('No','Yes'))

pg2.alcohol<-pepsinogen%>%filter(PG2<100)%>%ggviolin(x="饮酒", y="PG2", fill = "饮酒", 
                                                     palette = c("#00AFBB", "#E7B800"), 
                                                     add = "boxplot", add.params = list(fill="white"))+ 
  theme(legend.position = 'none')+labs(x='Alcohol',y='PGII',title='PGII')+
  geom_point(data=median.alcohol2,aes(x=饮酒,y=median),color='red')+geom_line(data=median.alcohol2,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
  stat_compare_means(label.y = 100,label.x=0.7)+ scale_x_discrete(labels=c('No','Yes'))

pgr.alcohol<-ggviolin(pepsinogen[which(pepsinogen$PGR<30),],x="饮酒", y="PGR", fill = "饮酒", 
                      palette = c("#00AFBB", "#E7B800"), 
                      add = "boxplot", add.params = list(fill="white"))+
  theme(legend.position = 'none')+labs(x='',y='PGI/II ratio',title='PGI/II ratio')+
  geom_point(data=median.alcohol3,aes(x=饮酒,y=median),color='red')+
  geom_line(data=median.alcohol3,aes(x=as.numeric(饮酒),y=median),color='red',size=1)+
  stat_compare_means(label.y = 25,label.x=0.7)+ scale_x_discrete(labels=c('No','Yes'))
pg1.alcohol |  pgr.alcohol
##吸烟于饮酒合并
(pg1.smoking |  pgr.smoking) / (pg1.alcohol |  pgr.alcohol)

##胃癌及癌前病变
gastric<-import('~/data/胃癌及癌前病变.xlsx')
gastric2<-inner_join(gastric,pepsinogen,by='ID')
gastric2$type<-factor(gastric2$type,levels=c('萎缩性胃炎','肠上皮化生','异型增生','胃癌'))
#gastric2%>%group_by(type,PGR_range)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,4))
PG1.plot2<-gastric2%>%group_by(type,PG1_range)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,2),percent2=paste0(round(n/sum(n)*100,2),'%'))%>%
  ggbarplot(x='type',y='percent',fill='PG1_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='',lab.size = 3.5,label='percent2',x.text.angle= 50)+labs(fill='PGI')+
  scale_x_discrete(labels=c('Atrophic gastritis','intestinal metaplasia','dysplasia','cancer'))

PGR.plot2<-gastric2%>%group_by(type,PGR_range)%>%summarise(n=n())%>%group_by(type)%>%mutate(percent=round(n/sum(n)*100,2),percent2=paste0(round(n/sum(n)*100,2),'%'))%>%
  ggbarplot(x='type',y='percent',fill='PGR_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',lab.size =3.5,xlab='',label='percent2',x.text.angle=50)+labs(fill='PGI/II ratio')+
  scale_x_discrete(labels=c('Atrophic gastritis','intestinal metaplasia','dysplasia','cancer'))
PG1.plot2 | PGR.plot2 

#胃癌及癌前病变中：PG在性别中的分布
gastric2%>%ggboxplot(x='性别.y',y='PG1')
#年龄
gastric2%>%group_by(性别.y)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
wilcox.test(PG1~年龄分组,data=gastric2)
gastric2%>%filter(年龄.y>=40,年龄.y<=74)%>%ggboxplot(x='年龄分组2',y='PGR',facet.by = '性别.y')
gastric2%>%filter(年龄.y>=40,年龄.y<=74)%>%ggboxplot(x='性别.y',y='PGR')

pepsinogen%>%transmute(年龄=factor(ifelse(年龄>60,1,0),levels=c(0,1),labels=c('<=60','>60')),PG1=PG1)%>%ggboxplot(x='年龄',y='PG1')+
  stat_compare_means()
gastric2%>%transmute(年龄=factor(ifelse(年龄.y>60,1,0),levels=c(0,1),labels=c('<=60','>60')),PG1=PG1)%>%ggboxplot(x='年龄',y='PG1')+
  stat_compare_means()

##性别
gastric2%>%ggboxplot(x='性别.y',y='PG1')+
  stat_compare_means()



