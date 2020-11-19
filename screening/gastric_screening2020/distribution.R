rm(list=ls())
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
##2020-7-06
#数据读取
source('~/Rcode/screening/gastric_screening2020/stomach_data.R')
mytheme<-theme(plot.title=element_text(hjust=0.5),
               #axis.title=element_text(face="bold",size=10),
               #axis.text=element_text(face="bold",size=9),
               #panel.grid.major = element_line(colour=NA),
               #panel.grid.major.x = element_line(color='grey'),
               #panel.grid.major.y = element_line(color='grey'),
               panel.background = element_blank(),
               axis.line = element_line(color='grey'),
               #legend.position = 'none',
               axis.text.x = element_blank(),
               axis.title.x = element_blank(),
               axis.ticks.x = element_blank()
               #strip.text.x =element_text(face='blod',color='red')
)
mytheme2<-theme(plot.title=element_text(hjust=0.5),
                #axis.title=element_text(face="bold",size=10),
                #axis.text=element_text(face="bold",size=9),
                #panel.grid.major = element_line(colour=NA),
                #panel.grid.major.x = element_line(color='grey'),
                #panel.grid.major.y = element_line(color='grey'),
                panel.background = element_blank(),
                axis.line = element_line(color='grey'),
                #legend.position = 'none',
                #axis.text.x = element_blank(),
                #strip.text.x =element_text(face='blod',color='red')
)
##分布
#Hp 与 年龄
with(pepsinogen,table(年龄分组3,Hp_pos))
with(pepsinogen,prop.table(table(年龄分组3,Hp_pos),margin=1))

pepsinogen%>%group_by(年龄分组2,Hp_pos)%>%summarise(n=n())%>%filter(!is.na(Hp_pos))%>%group_by(年龄分组2)%>%mutate(percent=round(n/sum(n),4)*100)%>%
  filter(Hp_pos=='是')%>%ggplot(aes(x=年龄分组2,y=percent))+geom_point()+geom_line(aes(group=1))+mytheme2


#Hp and PG1 
pepsinogen%>%filter(!is.na(Hp_pos))%>%ggplot(aes(x=PG1,fill=Hp_pos))+geom_histogram(binwidth = 20,color='black',position = 'dodge')+mytheme2
#Hp and PG2
pepsinogen%>%filter(!is.na(Hp_pos))%>%ggplot(aes(x=PG2,fill=Hp_pos))+geom_histogram(binwidth = 10,color='black',position = 'dodge')+mytheme2
#Hp and PGR
pepsinogen%>%filter(!is.na(Hp_pos))%>%ggplot(aes(x=PGR,fill=Hp_pos))+geom_histogram(binwidth = 1,color='black',position = 'dodge')+mytheme2


















