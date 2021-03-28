rm(list=ls())
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
library(rms)
library(htmlTable)
library(nnet)
library(effects)
library(VGAM)
library(DT)
source('~/Rcode/screening/gastric_screening2020/data2020.R')
source('~/Rcode/statistics/two_y_axis.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
source('~/Rcode/statistics/linear_regression.R')
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
                axis.title=element_text(family="serif",size=14,face="bold"),
                axis.text=element_text(family="serif",size=14,face="bold"),
                panel.grid.major = element_line(colour=NA),
                panel.grid.minor = element_blank(),
                panel.background=element_rect(fill=NA),
                axis.line = element_line(color='black'),
                legend.title = element_text(family='serif',size=15,face='bold'),
                legend.text = element_text(family = 'serif',size=15,face='bold'),
                legend.position = 'top'
)
#1.1---PG的基本分布
#PG1
PG1.normal<-pepsinogen2020%>%ggplot(aes(x=PG1))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 57.10,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2020$PG1),sd=sd(pepsinogen2020$PG1)),col='red',size=1)+mytheme
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+
PG2.normal<-pepsinogen2020%>%ggplot(aes(x=PG2))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 9.80,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2020$PG2),sd=sd(pepsinogen2020$PG2)),col='red',size=1)+mytheme
  scale_x_continuous(breaks=c(0,6,10,15,20,50),limits = c(0,51),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+
PGR.normal<-pepsinogen2020%>%ggplot(aes(x=PGR))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 6.00,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(pepsinogen2020$PGR),sd=sd(pepsinogen2020$PGR)),col='red',size=1)+mytheme
  scale_x_continuous(breaks=c(0,1,2,3,6,10,15,20),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+
PG1.normal | PG2.normal | PGR.normal
#分类
pepsinogen2020%>%group_by(PG1_range3)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100)%>%ggbarplot(x='PG1_range3',y='percent',fill='lightblue',label=TRUE,lab.pos = 'out')
pepsinogen2020%>%group_by(PG2_range2)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100)%>%ggbarplot(x='PG2_range2',y='percent',fill='lightblue',label=TRUE,lab.pos = 'out')
pepsinogen2020%>%group_by(PGR_range2)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100)%>%ggbarplot(x='PGR_range2',y='percent',fill='lightblue',label=TRUE,lab.pos = 'out')

#####基本分布*年龄####
#年龄分布
age1<-pepsinogen2020%>%group_by(年龄分组2)%>%summarise(n=n())%>%ggbarplot(x='年龄分组2',y='n',fill='lightblue',label =TRUE,lab.pos="out",xlab='Age',ylab='N')+
    scale_y_continuous(breaks=c(0,500,1000,1500))+mytheme
#Hp感染在年龄中的分布
pepsinogen2020%>%group_by(年龄分组2,Hp_pos2)%>%summarise(n=n())%>%group_by(年龄分组2)%>%mutate(percent=round(n/sum(n),4)*100)%>%
    filter(Hp_pos2=="阳性")%>%ggbarplot(x='年龄分组2',y='percent',fill='lightblue',label = TRUE,lab.pos='out')
Hp1<-pepsinogen2020%>%group_by(年龄分组2,Hp_pos2)%>%summarise(n=n())%>%group_by(年龄分组2)%>%mutate(percent=round(n/sum(n),4)*100)%>%
    filter(Hp_pos2=="阳性")%>%ggline(x='年龄分组2',y='percent',color='red',label = TRUE,lab.pos='out')+mytheme
#年龄+Hp
ggplot2.two_y_axis(age1,Hp1)
#PG
pepsinogen2020%>%group_by(年龄分组2)%>%summarise(PG1.med=median(PG1),PG1.Q1=quantile(PG1,0.25),PG1.Q3=quantile(PG1,0.75),
                                             PG2.med=median(PG2),PG2.Q1=quantile(PG2,0.25),PG2.Q3=quantile(PG2,0.75),
                                             PGR.med=median(PGR),PGR.Q1=quantile(PGR,0.25),PGR.Q3=quantile(PGR,0.75))
  
pepsinogen2020%>%ggerrorplot(x='年龄分组2',y='PG1',desc_stat = 'median_iqr')
pepsinogen2020%>%group_by(年龄分组2)%>%summarise(PG1.med=median(PG1),PG1.Q1=quantile(PG1,0.25),PG1.Q3=quantile(PG1,0.75),
                                             PG2.med=median(PG2),PG2.Q1=quantile(PG2,0.25),PG2.Q3=quantile(PG2,0.75),
                                             PGR.med=median(PGR),PGR.Q1=quantile(PGR,0.25),PGR.Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_errorbar(aes(x=年龄分组2,ymin=PG1.Q1,ymax=PG1.Q3),width=0.1,size=0.5,color='blue')+geom_point(aes(x=年龄分组2,y=PG1.med),size=2,color='blue')+
  geom_line(aes(x=年龄分组2,y=PG1.med,group=1,color='PGI'))+
  geom_errorbar(aes(x=年龄分组2,ymin=PG2.Q1,ymax=PG2.Q3),width=0.1,size=0.5,color='#FF6600')+geom_point(aes(x=年龄分组2,y=PG2.med),size=2,color='#FF6600')+
  geom_line(aes(x=年龄分组2,y=PG2.med,group=1,color='PGII'))+scale_y_log10()+mytheme+
  scale_color_manual(values=c("PGI"="blue","PGII"="#FF6600"))+labs(x='',fill=' ',y="Median(Q1-Q3)")+
  guides(shape=guide_legend(override.aes=list(size=10)))

pepsinogen2020%>%group_by(年龄分组2)%>%summarise(n=n(),median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%filter(!is.na(年龄分组2))%>%
  ggplot()+geom_errorbar(aes(x=年龄分组2,ymin=Q1,ymax=Q3),width=0.1,size=0.5,color='#FF6600')+geom_point(aes(x=年龄分组2,y=median),size=2,color='#FF6600')+
  geom_line(aes(x=年龄分组2,y=median,group=1,color='PGR'))+scale_y_log10()+mytheme+
  scale_color_manual(values=c('PGR'='#FF6600'))+labs(x='',fill='',y="Median(Q1-Q3)")+
  guides(shape=guide_legend(override.aes=list(size=5)))
#PG异常率和年龄
pepsinogen2020%>%group_by(PG1_range3,年龄分组2)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100)%>%
  ggbarplot(x='PG1_range3',y='percent',fill='年龄分组2',palette = 'jco',label=TRUE,lab.pos = 'out')
#####Table1 PG基本分布
#PGI Level
make.table(dat= pepsinogen2020,
           strat        = "PG1_range3",
           cat.varlist  = c(variables11,'BMI_group','吸烟1','Hp_pos2'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
#PGII Level
make.table(dat= pepsinogen2020,
           strat        = "PG2_range2",
           cat.varlist  = c(variables11,'BMI_group','吸烟1','Hp_pos2'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
#PGI/II ratio 
make.table(dat= pepsinogen2020,
           strat        = "PGR_range2",
           cat.varlist  = c(variables11,'BMI_group','吸烟1','Hp_pos2'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
###wilcox test or kruskal.test
#PGI Level

continous_PG1<-pepsinogen2020%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1','Hp_pos2'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG1),2),Q1=round(quantile(PG1,0.25),2),Q3=round(quantile(PG1,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PG1,'~/continous_PG1.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1','Hp_pos2')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2020)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2020)$p.value,4)
  }
}
do.call(rbind,p)

#多元线性回归
#单因素
uni_linear<-linear(x=c(variables11,'BMI_group','吸烟1','Hp_pos2'),y='log(PG1)',data=pepsinogen2020)
export(uni_linear,"~/uni_linear.xlsx")
#多因素
multi_linear<-linear2(x=c(variables11,'BMI_group','吸烟1','Hp_pos2'),y='log(PG1)',data=pepsinogen2020)
export(multi_linear,"~/multi_linear.xlsx")



#PGII level

continous_PG2<-pepsinogen2020%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1','Hp_pos2'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG2),2),Q1=round(quantile(PG2,0.25),2),Q3=round(quantile(PG2,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PG2,'~/continous_PG2.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1','Hp_pos2')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2020)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2020)$p.value,4)
  }
}
do.call(rbind,p)

#多元线性回归
#单因素
uni_linear<-linear(x=c(variables11,'BMI_group','吸烟1','Hp_pos2'),y='log(PG2)',data=pepsinogen2020)
export(uni_linear,"~/uni_linear.xlsx")
#多因素
multi_linear<-linear2(x=c(variables11,'BMI_group','吸烟1','Hp_pos2'),y='log(PG2)',data=pepsinogen2020)
export(multi_linear,"~/multi_linear.xlsx")

#PGI/II ratio

continous_PGR<-pepsinogen2020%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1','Hp_pos2'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PGR),2),Q1=round(quantile(PGR,0.25),2),Q3=round(quantile(PGR,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PGR,'~/continous_PGR.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1','Hp_pos2')){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2020)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2020)$p.value,4)
  }
}
do.call(rbind,p)
#单因素
uni_linear<-linear(x=c(variables11,'BMI_group','吸烟1','Hp_pos2'),y='log(PGR)',data=pepsinogen2020)
export(uni_linear,"~/uni_linear.xlsx")
#多因素
multi_linear<-linear2(x=c(variables11,'BMI_group','吸烟1','Hp_pos2'),y='log(PGR)',data=pepsinogen2020)
export(multi_linear,"~/multi_linear.xlsx")


#####Table2 PG基本分布，按幽门螺杆菌感染状态分层(幽门螺杆菌感染阳性)
table1(~性别+年龄分组+胃癌家族史+家庭收入2+教育年数+就业状况+血型2+
           饮酒+喝茶+鲜奶+酸奶+咖啡+碳酸饮料+果味饮料+茶味饮料+
         蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+水产品+大蒜+菌类+油炸+烧烤+熏制+酱制+
         偏咸+晒制+腌制+偏辣+偏烫+偏酸+偏甜+运动+跑步+静态时间+十二指肠溃疡+胃息肉+胃溃疡+消化性溃疡+
         糖尿病+高血压+高血脂+冠心病+中风 | PG1_range3,data=subset(pepsinogen2020,Hp_pos2=="阳性")
         )
make.table(dat= subset(pepsinogen2020,Hp_pos2=="阳性"),
           strat        = "PG1_range3",
           cat.varlist  = c(variables11,'BMI_group','吸烟1'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
#PGII Level
make.table(dat= subset(pepsinogen2020,Hp_pos2=="阳性"),
           strat        = "PG2_range2",
           cat.varlist  = c(variables11,'BMI_group','吸烟1'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
#PGI/II ratio 
make.table(dat= subset(pepsinogen2020,Hp_pos2=="阳性"),
           strat        = "PGR_range2",
           cat.varlist  = c(variables11,'BMI_group','吸烟1'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
###wilcox test or kruskal.test
#PGI Level
continous_PG1<-pepsinogen2020%>%filter(Hp_pos2=="阳性")%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG1),2),Q1=round(quantile(PG1,0.25),2),Q3=round(quantile(PG1,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PG1,'~/continous_PG1.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阳性"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阳性"))$p.value,4)
  }
}
do.call(rbind,p)

#PGII level

continous_PG2<-pepsinogen2020%>%filter(Hp_pos2=="阳性")%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG2),2),Q1=round(quantile(PG2,0.25),2),Q3=round(quantile(PG2,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PG2,'~/continous_PG2.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阳性"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阳性"))$p.value,4)
  }
}
do.call(rbind,p)

#PGI/II ratio

continous_PGR<-pepsinogen2020%>%filter(Hp_pos2=="阳性")%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PGR),2),Q1=round(quantile(PGR,0.25),2),Q3=round(quantile(PGR,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PGR,'~/continous_PGR.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1')){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阳性"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阳性"))$p.value,4)
  }
}
do.call(rbind,p)
#####Table2 PG基本分布，按幽门螺杆菌感染状态分层(幽门螺杆菌感染阴性)
make.table(dat= subset(pepsinogen2020,Hp_pos2=="阴性"),
           strat        = "PG1_range3",
           cat.varlist  = c(variables11,'BMI_group','吸烟1'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
#PGII Level
make.table(dat= subset(pepsinogen2020,Hp_pos2=="阴性"),
           strat        = "PG2_range2",
           cat.varlist  = c(variables11,'BMI_group','吸烟1'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
#PGI/II ratio 
make.table(dat= subset(pepsinogen2020,Hp_pos2=="阴性"),
           strat        = "PGR_range2",
           cat.varlist  = c(variables11,'BMI_group','吸烟1'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
###wilcox test or kruskal.test
#PGI Level
continous_PG1<-pepsinogen2020%>%filter(Hp_pos2=="阴性")%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG1),2),Q1=round(quantile(PG1,0.25),2),Q3=round(quantile(PG1,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PG1,'~/continous_PG1.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阴性"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阴性"))$p.value,4)
  }
}
do.call(rbind,p)

p<-list()
anova(lm(log(PG1)~吸烟1*Hp_pos2,data=pepsinogen2020))



#PGII level

continous_PG2<-pepsinogen2020%>%filter(Hp_pos2=="阴性")%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG2),2),Q1=round(quantile(PG2,0.25),2),Q3=round(quantile(PG2,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PG2,'~/continous_PG2.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阴性"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阴性"))$p.value,4)
  }
}
do.call(rbind,p)

#PGI/II ratio

continous_PGR<-pepsinogen2020%>%filter(Hp_pos2=="阴性")%>%pivot_longer(cols=c(variables11,'BMI_group','吸烟1'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PGR),2),Q1=round(quantile(PGR,0.25),2),Q3=round(quantile(PGR,0.75)),2)%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))
export(continous_PGR,'~/continous_PGR.xlsx')
p<-list()
for(i in c(variables11,'BMI_group','吸烟1')){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen2020[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阴性"))$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=subset(pepsinogen2020,Hp_pos2=="阴性"))$p.value,4)
  }
}
do.call(rbind,p)
##幽门螺杆菌对PG的影响
pepsinogen2020%>%ggplot(aes(x=C14Value,y=log(PG1)))+geom_point()+geom_smooth(method='lm')
pepsinogen2020%>%ggplot(aes(x=C14Value,y=log(PG2)))+geom_point()+geom_smooth(method='lm')
pepsinogen2020%>%ggplot(aes(x=C14Value,y=log(PGR)))+geom_point()+geom_smooth(method='lm')
pg_hp1<-pepsinogen2020%>%ggboxplot(x='Hp_pos4',y='PG1',fill='HP_pos4',palette = 'jco')+stat_compare_means()+border()+stat_compare_means(comparisons = list(c(3,4),c(4,5)))+labs(x='C14 value')+mytheme
pg_hp2<-pepsinogen2020%>%ggboxplot(x='Hp_pos4',y='PG2',fill='HP_pos4',palette = 'jco')+stat_compare_means()+border()+stat_compare_means(comparisons = list(c(3,4),c(4,5)))+labs(x='C14 value')+mytheme
pg_hpr<-pepsinogen2020%>%ggboxplot(x='Hp_pos4',y='PGR',fill='HP_pos4',palette = 'jco')+stat_compare_means()+border()+stat_compare_means(comparisons = list(c(3,4),c(4,5)))+labs(x='C14 value')+mytheme
pg_hp1 | pg_hp2 | pg_hpr
#ABC
pg1_hp<-pepsinogen2020%>%mutate(m='v')%>%group_by(m,Hp_pos2,PG1_range3)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='Hp_pos2',y='percent',fill='PG1_range3',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='Hp infection',label=TRUE)+labs(fill='PGI')
pg2_hp<-pepsinogen2020%>%mutate(m='v')%>%group_by(m,Hp_pos2,PG2_range2)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='Hp_pos2',y='percent',fill='PG2_range2',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='Hp infection',label=TRUE)+labs(fill='PGI')
pgr_hp<-pepsinogen2020%>%mutate(m='v')%>%group_by(m,Hp_pos2,PGR_range2)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='Hp_pos2',y='percent',fill='PGR_range2',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='Hp infection',label=TRUE)+labs(fill='PGI')
(pg1_hp | pg2_hp)/pgr_hp +plot_annotation(tag_level = 'A')

pepsinogen2020%>%mutate(m='v')%>%group_by(m,Hp_pos2,PG_pos3)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='Hp_pos2',y='percent',fill='PG_pos3',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='Hp infection',label=TRUE)+labs(fill='PG test')+mytheme

pepsinogen2020%>%mutate(m='v')%>%group_by(m,年龄分组,Hp_pos2,PG_pos3)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='Hp_pos2',y='percent',fill='PG_pos3',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='Hp infection',label=TRUE)+labs(fill='PG test')+mytheme+facet_wrap(.~年龄分组,ncol=3)+border()

pepsinogen2020%>%mutate(m='v')%>%group_by(m,吸烟1,Hp_pos2,PG_pos3)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='Hp_pos2',y='percent',fill='PG_pos3',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='Hp infection',label=TRUE)+labs(fill='PG test')+mytheme+facet_wrap(.~吸烟1,ncol=3)+border()

##ABC+BMI
pepsinogen2020%>%mutate(m='v')%>%group_by(m,BMI_group4,Hp_pos2,PG_pos3)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='Hp_pos2',y='percent',fill='PG_pos3',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='Hp infection',label=TRUE)+labs(fill='PG test')+mytheme+facet_wrap(.~BMI_group4,ncol=3)+border()










###2020胃镜结果分析
gastrology<-import('~/data/2020/2020年胃镜结果.xlsx')
pepsinogen2020<-inner_join(pepsinogen2020,gastrology,by='ID')
pepsinogen2020%>%mutate(m='v')%>%group_by(m,PG_hp,胃镜结果)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',fill='胃镜结果',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='ABC方法',label=TRUE)+labs(fill='胃镜结果')+mytheme

pepsinogen2020%>%mutate(m='v')%>%group_by(m,PG_hp,胃镜结果)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',fill='胃镜结果',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='ABC方法',label=TRUE)+labs(fill='胃镜结果')+mytheme

pepsinogen2020%>%mutate(m='v')%>%group_by(m,PG_hp,胃镜结果)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',fill='胃镜结果',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='ABC方法',label=TRUE)+labs(fill='胃镜结果')+mytheme

###Hp感染与心血管疾病
make.table(dat= pepsinogen2020,
           strat        = "冠心病",
           cat.varlist  = c('高血压','Hp_pos2'),
           cat.rmstat   = list(c("row")),
           cat.ptype    = c("chisq"),
           output       = "html")
logit(x='Hp_pos',y='冠心病',data=pepsinogen2020)
logit(x=c('Hp_pos','性别','年龄分组'),y='冠心病',data=pepsinogen2020)
logit(x=c('Hp_pos','性别','年龄分组','BMI_group','吸烟2'),y='冠心病',data=pepsinogen2020)
##年龄分层
with(subset(pepsinogen2020,年龄<60),table(冠心病))
with(subset(pepsinogen2020,年龄>=60),table(冠心病))
logit(x=c('Hp_pos','性别','BMI_group','吸烟2'),y='冠心病',data=subset(pepsinogen2020,年龄>=60))
logit(x=c('Hp_pos','性别','BMI_group','吸烟2'),y='冠心病',data=subset(pepsinogen2020,年龄<60))
summary(glm(冠心病~Hp_pos*年龄分组+性别+BMI_group+吸烟2,data=pepsinogen2020,family='binomial'))
#性别分层
with(subset(pepsinogen2020,性别=="Female"),table(冠心病))
with(subset(pepsinogen2020,性别=="Male"),table(冠心病))
logit(x=c('Hp_pos','年龄分组','BMI_group','吸烟2'),y='冠心病',data=subset(pepsinogen2020,性别=="Male"))
logit(x=c('Hp_pos','年龄分组','BMI_group','吸烟2'),y='冠心病',data=subset(pepsinogen2020,性别=="Female"))
#吸烟分层

with(subset(pepsinogen2020,吸烟2=="从不吸烟"),table(冠心病))
with(subset(pepsinogen2020,吸烟2=="目前或过去吸烟"),table(冠心病))

logit(x=c('Hp_pos','性别','年龄分组','BMI_group'),y='冠心病',data=subset(pepsinogen2020,吸烟2=="从不吸烟"))
logit(x=c('Hp_pos','性别','年龄分组','BMI_group'),y='冠心病',data=subset(pepsinogen2020,吸烟2=="目前或过去吸烟"))


####2019年数据分析
PG1.plot<-pepsinogen2019%>%group_by(PG1_range3)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI',label =TRUE)+scale_y_continuous(limits=c(0,70))
PGR.plot<-pepsinogen2019%>%filter(PG1!=200)%>%group_by(PGR_range2)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PGR_range2',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI/II ratio',label =TRUE)+scale_y_continuous(limits = c(0,100))
PG1R.plot<-pepsinogen2019%>%mutate(m='v')%>%filter(PG1!=200)%>%group_by(m,PG1_range3,PGR_range2)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',fill='PGR_range2',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE)+labs(fill='PGI/II ratio')
(PG1.plot |PGR.plot)/PG1R.plot +plot_annotation(tag_level = 'A')


###
gastric<-import('~/data/胃癌及癌前病变.xlsx')
gastric2<-inner_join(gastric,pepsinogen2019,by='ID')
gastric2$type<-factor(gastric2$type,levels=c('萎缩性胃炎','肠上皮化生','异型增生','胃癌'))

plot2<-gastric2%>%mutate(m='v')%>%group_by(m,type,PG1_range3)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='type',y='percent',fill='PG1_range3',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE)+labs(fill='PGI/II ratio')

gastric2%>%mutate(m='v')%>%group_by(m,PG1_range3,type)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',fill='type',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE)+labs(fill='PGI/II ratio')

gastric2%>%mutate(m='v')%>%group_by(m,PG_pos3,type)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_pos3',y='percent',fill='type',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI & PGI/II ratio',label=TRUE)+labs(fill='病理诊断结果')



###2017-2019肿瘤医院胃癌患者与筛查人群病例对照研究
case_control<-import('~/data/hospital/case_control_PG.sav')
case_control$胃镜结果<-factor(case_control$胃镜结果)
case_control%>%gghistogram(x='PG1',y='..density..',fill='胃镜结果',palette = 'jco')
case_control%>%gghistogram(x='PG2',y='..density..',fill='胃镜结果',palette = 'jco')
case_control%>%gghistogram(x='PGR',y='..density..',fill='胃镜结果',palette = 'jco')

  
  
