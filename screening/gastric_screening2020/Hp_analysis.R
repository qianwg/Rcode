rm(list=ls())
library(rio)
library(tidyverse)
library(htmlTable)
library(table1)
library(rms)
source('~/Rcode/screening/questionnaire/questionnaire2020.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
variables<-c("胃癌家族史","性别","年龄分组","年龄分组3","年龄分组4","年龄分组5","婚姻",         
             "就业状况2","家庭收入2","教育年数"  ,        
              "血型1","运动","BMI_group","BMI_group2",
            "吸烟1","吸烟2","被动吸烟1","被动吸烟2","每天早餐","准点吃饭","吃饭速度",          
             "外出吃饭","静态时间","手机使用时间","睡眠时间","睡眠质量"  ,        
             "夜班","饮酒","喝茶","鲜奶","酸奶",              
            "咖啡","碳酸饮料","果味饮料","茶味饮料","蔬菜",              
             "水果","谷类","鸡蛋","杂粮","豆类" ,             
             "肉类","坚果","水产品","薯类","大蒜",
             "菌类","油炸","烧烤","熏制","酱制",              
              "偏咸","腌制","晒制","偏辣","偏烫" ,             
             "偏酸","偏甜","啤酒","低度白酒","高度白酒","十二指肠溃疡",      
              "胃食管反流性疾病","胃溃疡","胃息肉","幽门螺杆菌感染史","萎缩性胃炎","消化性溃疡","胃肠疾病",
             "糖尿病","高血压","高血脂","冠心病","中风" ,             
             "偏头疼")
#问卷
#summary(screening2020)
screening2020<-screening2020%>%filter(自身癌!="是" & 残胃!="是")
##1、问卷与Hp
match1<-inner_join(screening2020,data_Hp2020,by='persoID')
##Hp的因素素分析
make.table(dat=match1,
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c("癌症家族史",variables),
           cat.ptype    = c("chisq"),
           output       = "html"
           )
logit(x=variables,y='Hp_pos',data=match1)
##肥胖、冠心病、溃疡、教育年数、早餐、熏制、碳酸饮料、胃癌家族史、
#食管反流性疾病、血型、饮酒、高血脂、油炸、水产品、年龄
#一、查看基本人口学特征(性别、年龄、婚姻、家庭收入、教育年数、血型)与幽门螺旋杆菌感染的相关性。
make.table(dat=match1,
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c("性别",'年龄分组2',"婚姻",'家庭收入2','教育','教育年数','血型1'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
#二、查看吸烟、饮酒、BMI与Hp感染的相关性(吸烟2/饮酒/BMI_group)。
make.table(dat=match1,
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c("吸烟1",'吸烟2','吸烟年数分组2','包年分组','吸烟3',"饮酒",'饮酒2','低度白酒量','高度白酒量','BMI_group','BMI_group2'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
logit(x=c('性别','年龄','吸烟2','饮酒','BMI_group'),y='Hp_pos',data=match1)
#2.1性别分层
make.table(dat=subset(match1,性别=="Female"),
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c('吸烟2','吸烟3',"饮酒",'BMI_group'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
make.table(dat=subset(match1,性别=="Male"),
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c('吸烟2','吸烟3',"饮酒",'BMI_group'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
#2.2性别分层+年龄矫正
logit(x=c('年龄分组','吸烟2'),y='Hp_pos',data=subset(match1,性别=="Female"))
logit(x=c('年龄分组','饮酒'),y='Hp_pos',data=subset(match1,性别=="Female"))
logit(x=c('年龄分组','BMI_group'),y='Hp_pos',data=subset(match1,性别=="Female"))

logit(x=c('年龄分组','吸烟2'),y='Hp_pos',data=subset(match1,性别=="Male"))
logit(x=c('年龄分组','饮酒'),y='Hp_pos',data=subset(match1,性别=="Male"))
logit(x=c('年龄分组','BMI_group'),y='Hp_pos',data=subset(match1,性别=="Male"))
#2.3探究BMI与Hp感染的相关性
match1%>%group_by(Hp_pos)%>%summarise(n=n(),median=median(BMI),Q1=quantile(BMI,0.25),Q3=quantile(BMI,0.75))
wilcox.test(BMI~Hp_pos,data=match1)

match1.1<-match1[,c('Hp_pos','BMI')]
dd<-datadist(match1.1)
options(datadist='dd')
fit<-lrm(Hp_pos~rcs(BMI,3),data=match1.1)
OR<-Predict(fit,BMI,fun=exp,ref.zero=TRUE)
OR%>%ggplot()+geom_line(aes(x=BMI,y=yhat),linetype='solid',size=1,alpha=0.7,color='red')+
  geom_ribbon(aes(BMI,ymin=lower,ymax=upper),alpha=0.1,color='red')+geom_hline(yintercept = 1,linetype=1,size=1)+
  labs(x='BMI',y='OR(95%CI)')
###匹配
#1、问卷与Hp
match1<-inner_join(screening2020,data_Hp2020,by='persoID')
match2<-inner_join(match1,data_PG2020,by='ID')
#export(match2,'~/match2.xlsx')
###
variables<-c("胃癌家族史","性别","年龄分组","年龄分组3","年龄分组4","年龄分组5","婚姻",         
             "就业状况2","家庭收入2","教育年数"  ,        
             "血型1","运动","BMI_group","BMI_group2",
             "吸烟1","吸烟2","被动吸烟1","被动吸烟2","每天早餐","准点吃饭","吃饭速度",          
             "外出吃饭","静态时间","手机使用时间","睡眠时间","睡眠质量"  ,        
             "夜班","饮酒","喝茶","鲜奶","酸奶",              
             "咖啡","碳酸饮料","果味饮料","茶味饮料","蔬菜",              
             "水果","谷类","鸡蛋","杂粮","豆类" ,             
             "肉类","坚果","水产品","薯类","大蒜",
             "菌类","油炸","烧烤","熏制","酱制",              
             "偏咸","腌制","晒制","偏辣","偏烫" ,             
             "偏酸","偏甜","十二指肠溃疡",      
             "胃食管反流性疾病","胃溃疡","胃息肉","幽门螺杆菌感染史","萎缩性胃炎","消化性溃疡","胃肠疾病",
             "糖尿病","高血压","高血脂","冠心病","中风" ,             
             "偏头疼")
mytheme<-theme(plot.title=element_text(hjust=0.5),
               axis.title=element_text(face="bold",size=14),
               axis.text=element_text(face="bold",size=16),
               axis.text.x  = element_text(face="bold",size=14),
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
#问卷
#summary(screening2020)
match2<-match2%>%filter(自身癌!="是" & 残胃!="是")
#
match2%>%filter(C14Value!=0)%>%ggplot()+geom_histogram(aes(x=C14Value),bins=20,color='black',fill='lightblue')+
  scale_x_continuous(breaks=c(0,100,500,1000,1500,2000,2500))+mytheme
##Hp的因素素分析
make.table(dat=match2,
           strat        = "Hp_pos",
           cat.rmstat   = c("row"),
           cat.varlist  = c("癌症家族史",variables),
           cat.ptype    = c("chisq"),
           output       = "html"
)
logit(x=variables,y='Hp_pos',data=match1)

make.table(dat=match2,
           strat        = "Hp_pos2",
           cat.rmstat   = c("col"),
           cat.varlist  = c('Hp_pos',"癌症家族史",variables),
           cat.ptype    = c("chisq"),
           output       = "html"
)
a<-logit(x=variables,y='Hp_pos2',data=match2)
logit(x='年龄分组3',y='Hp_pos2',data=match2)
dd <- datadist(match2) 
options(datadist='dd')
forest_model(glm(Hp_pos2~relevel(BMI_group2,ref='正常'),family ='binomial',data=match2))
summary(glm(Hp_pos2~as.numeric(BMI_group2),family ='binomial',data=match2))
export(a,'~/a.xlsx')
make.table(dat=match2,
           strat        = "Hp_pos3",
           cat.rmstat   = c("col"),
           cat.varlist  = c("癌症家族史",variables),
           cat.ptype    = c("chisq"),
           output       = "html"
)
logit(x=variables,y='Hp_pos3',data=match2)


make.table(dat=match2,
           strat        = "Hp_pos4",
           cat.rmstat   = c("row"),
           cat.varlist  = c("癌症家族史",variables),
           cat.ptype    = c("chisq"),
           output       = "html"
)
make.table(dat=match2,
           strat        = "Hp_pos5",
           cat.rmstat   = c("row"),
           cat.varlist  = c("癌症家族史",variables),
           cat.ptype    = c("chisq"),
           output       = "html"
)
##肥胖、冠心病、溃疡、教育年数、早餐、熏制、碳酸饮料、胃癌家族史、
#食管反流性疾病、血型、饮酒、高血脂、油炸、水产品、年龄
#一、查看基本人口学特征(性别、年龄、婚姻、家庭收入、教育年数、血型)与幽门螺旋杆菌感染的相关性。
make.table(dat=match1,
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c("性别",'年龄分组2',"婚姻",'家庭收入2','教育','教育年数','血型1'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
#二、查看吸烟、饮酒、BMI与Hp感染的相关性(吸烟2/饮酒/BMI_group)。
make.table(dat=match1,
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c("吸烟1",'吸烟2','吸烟年数分组2','包年分组','吸烟3',"饮酒",'饮酒2','低度白酒量','高度白酒量','BMI_group','BMI_group2'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
logit(x=c('性别','年龄','吸烟2','饮酒','BMI_group'),y='Hp_pos',data=match1)
#2.1性别分层
make.table(dat=subset(match1,性别=="Female"),
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c('吸烟2','吸烟3',"饮酒",'BMI_group'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
make.table(dat=subset(match1,性别=="Male"),
           strat        = "Hp_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c('吸烟2','吸烟3',"饮酒",'BMI_group'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
#2.2性别分层+年龄矫正
logit(x=c('年龄分组','吸烟2'),y='Hp_pos',data=subset(match1,性别=="Female"))
logit(x=c('年龄分组','饮酒'),y='Hp_pos',data=subset(match1,性别=="Female"))
logit(x=c('年龄分组','BMI_group'),y='Hp_pos',data=subset(match1,性别=="Female"))

logit(x=c('年龄分组','吸烟2'),y='Hp_pos',data=subset(match1,性别=="Male"))
logit(x=c('年龄分组','饮酒'),y='Hp_pos',data=subset(match1,性别=="Male"))
logit(x=c('年龄分组','BMI_group'),y='Hp_pos',data=subset(match1,性别=="Male"))
#2.3探究BMI与Hp感染的相关性
match1%>%group_by(Hp_pos)%>%summarise(n=n(),median=median(BMI),Q1=quantile(BMI,0.25),Q3=quantile(BMI,0.75))
wilcox.test(BMI~Hp_pos,data=match1)

match1.1<-match1[,c('Hp_pos','BMI')]
dd<-datadist(match1.1)
options(datadist='dd')
fit<-lrm(Hp_pos~rcs(BMI,3),data=match1.1)
OR<-Predict(fit,BMI,fun=exp,ref.zero=TRUE)
OR%>%ggplot()+geom_line(aes(x=BMI,y=yhat),linetype='solid',size=1,alpha=0.7,color='red')+
  geom_ribbon(aes(BMI,ymin=lower,ymax=upper),alpha=0.1,color='red')+geom_hline(yintercept = 1,linetype=1,size=1)+
  labs(x='BMI',y='OR(95%CI)')


##########Hp与肿瘤标志物的关系
match2%>%group_by(Hp_pos)%>%summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
wilcox.test(PG1~Hp_pos,data=match2)
match2%>%group_by(Hp_pos)%>%summarise(median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))
wilcox.test(PG2~Hp_pos,data=match2)
match2%>%group_by(Hp_pos)%>%summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))
wilcox.test(PGR~Hp_pos,data=match2)

match2%>%group_by(Hp_pos4)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
match2%>%group_by(Hp_pos5)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
make.table(dat=match2,
           strat        = "Hp_pos",
           cat.rmstat   = c("row"),
           cat.varlist  = c('PG1_range3','PG2_range5','PGR_range2'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
logit(x=c('PG1_range3','PG2_range5','PGR_range2'),y='Hp_pos',data=match2)
summary(glm(Hp_pos~as.numeric(PG2_range5),family='binomial',data=match2))
logit(x=c('PG1_range3','PG2_range5','PGR_range2'),y='Hp_pos3',data=match2)
summary(glm(Hp_pos~as.numeric(PG2_range5),family='binomial',data=match2))
###BMI and Hp
logit(x=c('BMI'),y='Hp_pos2',data=match2)
forest_model(glm(Hp_pos~relevel(BMI_group2,ref='正常')+性别+年龄分组3+教育年数+家庭收入2,family='binomial',data=match2))
forest_model(glm(Hp_pos~BMI+性别+年龄分组3+教育年数+家庭收入2,family='binomial',data=match2))
#
forest_model(glm(Hp_pos~relevel(BMI_group2,ref='正常')+饮酒+吸烟2+碳酸饮料+油炸+熏制,family='binomial',data=match2))
forest_model(glm(Hp_pos~BMI+饮酒+吸烟2+碳酸饮料+油炸+熏制,family='binomial',data=match2))
##
forest_model(glm(Hp_pos~relevel(BMI_group2,ref='正常')+性别+年龄分组3+教育年数+家庭收入2+饮酒+吸烟2+碳酸饮料+油炸+熏制,family='binomial',data=match2))
forest_model(glm(Hp_pos~BMI+性别+年龄分组3+教育年数+家庭收入2+饮酒+吸烟2+碳酸饮料+油炸+熏制,family='binomial',data=match2))
##
make.table(dat=match2,
           strat        = "BMI_group4",
           cat.rmstat   = c("col"),
           cat.varlist  = c('Hp_pos','Hp_pos6',variables),
           cat.ptype    = c("chisq"),
           output       = "html"
)
#性别/年龄分组/婚姻/教育年数/运动/吸烟1/饮酒/喝茶/鲜奶/酸奶/油炸/烧烤/偏咸/腌制/糖尿病/高血脂/冠心病/
logit(x=c('Hp_pos','性别','年龄分组','教育年数'),y='BMI_group4',data=match2)
logit(x=c('Hp_pos','性别','年龄分组','教育年数','吸烟1','饮酒','糖尿病','高血压','冠心病'),y='BMI_group4',data=match2)
##
logit(x=c('Hp_pos6','性别','年龄分组','教育年数'),y='BMI_group4',data=match2)
logit(x=c('Hp_pos6','性别','年龄分组','教育年数','吸烟1','饮酒','糖尿病','高血压','冠心病'),y='BMI_group4',data=match2)




####饮酒与Hp感染
make.table(dat=match2,
           strat        = "Hp_pos",
           cat.rmstat   = c("row"),
           cat.varlist  = c('啤酒','白酒','低度白酒','高度白酒','葡萄酒'),
           cat.ptype    = c("chisq"),
           output       = "html"
)
dd <- datadist(match2) 
options(datadist='dd')
fit<- lrm(Hp_pos ~ rcs(白酒量,4),data=subset(match2,白酒=="是")) 
RR<-Predict(fit,白酒量,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(白酒量,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(白酒量,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+mytheme+ 
  labs( x="白酒量", y="OR (95%CI)") 
###PG与Hp
match2%>%ggplot()+geom_point(aes(x=PG2,y=PG1,fill=Hp_pos,color=Hp_pos),alpha=0.4)
match2%>%ggplot()+geom_point(aes(x=PG2,y=PGR,fill=Hp_pos,color=Hp_pos),alpha=0.3)+
  scale_x_continuous(breaks=c(0,6,10,20,30,40,50,100,150))+mytheme+
  scale_y_continuous(breaks=c(0,3,5,6,9,15,20))+geom_hline(aes(yintercept=6))+geom_vline(aes(xintercept=10))

###ROC
roc_PG1<-roc(match2$Hp_pos,match2$PG1,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc_PG2<-roc(match2$Hp_pos, match2$PG2,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
roc_PGR<-roc(match2$Hp_pos, match2$PGR,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
#曲线下面积的比较
roc.test(roc_PG1,roc_PG2)
roc.test(roc_PG1,roc_PGR,method = "bootstrap",boot.n=10000)#p=0.06
roc.test(roc_PG2,roc_PGR)
roc.test(roc_PGR,roc_PG1R,method = "bootstrap",boot.n=10000)#p=0.12
roc.test(roc_PG1,roc_PG1R,method = "bootstrap",boot.n=10000)#p<0.01
#plot
plot.roc(match2$Hp_pos, match2$PG1,add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(match2$Hp_pos, match2$PG2,add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
plot.roc(match2$Hp_pos, match2$PGR,add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)
#plot
plot.roc(match2$Hp_pos, match2$PG1,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="2")
#lines.roc(match2$type2, match2$PG2,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="3")
lines.roc(match2$Hp_pos, match2$PGR,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="4")
lines.roc(match2$Hp_pos, match2$PG2,percent=F, reuse.auc=TRUE,axes=TRUE, legacy.axes=T, col="5")
legend("bottomright",legend=c("PG1, AUC: 0.65(0.64,0.67)",
                              "PG2, AUC: 0.84(0.83-0.85)",
                              "PGR, AUC: 0.84(0.83,0.85)"
),
col=c("2","3","4","5"),lwd=3,cex=0.6)


#不同诊断标准的灵敏度和特异度

table<-match2%>%transmute(PG_pos1=ifelse(PG1>65.71,1,0),
                          PG_pos2=ifelse(PG2>10.17,1,0),
                          PG_pos3=ifelse(PGR<6,1,0),
                          type=Hp_pos)
#PG1>65
table(table$PG_pos1,table$type)
epi.tests(as.table(matrix(c(1190,684,919,1500), nrow = 2, byrow = TRUE)))
#PG2>10.17
table(table$PG_pos2,table$type)
epi.tests(as.table(matrix(c(1682,499,427,1685), nrow = 2, byrow = TRUE)))
# PGR<6
table(table$PG_pos3,table$type)
epi.tests(as.table(matrix(c(1669,431,440,1753), nrow = 2, byrow = TRUE)))





