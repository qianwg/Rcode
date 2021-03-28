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
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
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
##
screening2020<-screening2020%>%filter(自身癌!="是" & 残胃!="是")
match1<-inner_join(screening2020,data_Hp2020,by='persoID')
match2<-inner_join(match1,data_PG2020,by='ID')
#########基本分布
##年龄
##年龄分布1
pg1.age<-match2%>%filter(年龄>=40,年龄<=74)%>%group_by(年龄)%>%
  summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75),median.pg2=median(PG2),Q1.pg2=quantile(PG2,0.25),Q3.pg2=quantile(PG2,0.75))%>%
  ggplot(aes(x=年龄))+geom_ribbon(aes(ymin=Q1,ymax=Q3), fill="#6699CC", alpha=.4)+mytheme+
  geom_line(aes(y=median,color="PGI"),size=1)+
  geom_ribbon(aes(ymin=Q1.pg2,ymax=Q3.pg2),fill="#FFCC00",alpha=0.4)+
  geom_line(aes(y=median.pg2,colour="PGII"),size=1)+
  labs(x = "Age",y='Median(Q1-Q3)',colour='PG')+scale_x_continuous(breaks=seq(42,74,5))+
  scale_y_log10()+scale_color_manual(values=c("PGI"="#003366","PGII"="#FFCC00"))
pg2.age<-match2%>%filter(年龄>=40,年龄<=74,!is.na(PGR))%>%group_by(年龄)%>%
  summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_ribbon(aes(x=年龄,ymin=Q1,ymax=Q3), fill="#FF9933", alpha=.6)+
  mytheme+geom_line(aes(x=年龄,y=median,colour='PGR'),size=1)+
  labs(x = "Age",y='',colour='')+scale_x_continuous(breaks=seq(42,74,5))+scale_color_manual(values=c('PGR'='#FF9933'))
(pg1.age | pg2.age) + plot_layout(guides='collect') & theme(legend.position = 'top')
##性别分布
PG1.sex<-match2%>%group_by(性别)%>%summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGI',x='')+
  geom_line(aes(x=as.numeric(性别),y=c(92,92)),size=0.8)+geom_line(aes(x=c(1,1),y=c(86,87)),size=0.8)+geom_line(aes(x=c(2,2),y=c(86,87)),size=0.8)+
  annotate('text',label="***",x=1.5,y=93,size=10,color='black')

PG2.sex<-match2%>%group_by(性别)%>%summarise(median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGII',x=' ')+
  geom_line(aes(x=as.numeric(性别),y=c(18,18)),size=0.8)+geom_line(aes(x=c(1,1),y=c(15.7,16)),size=0.8)+geom_line(aes(x=c(2,2),y=c(15.7,16)),size=0.8)+
  annotate('text',label="***",x=1.5,y=19,size=10,color='black')

wilcox.test(PGR~性别,data=match2)
PGR.sex<-match2%>%group_by(性别)%>%summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot()+geom_bar(aes(x=性别,y=median,fill=性别),stat='identity',color='black',width = 0.5,size=0.8)+
  geom_errorbar(aes(x=性别,ymin=median,ymax=Q3),width=0.1,size=0.8)+mytheme+labs(fill='Sex',y='PGI/II ratio',x=' ')+
  geom_line(aes(x=as.numeric(性别),y=c(8.2,8.2)),size=0.8)+geom_line(aes(x=c(1,1),y=c(8.1,8.2)),size=0.8)+geom_line(aes(x=c(2,2),y=c(8.1,8.2)),size=0.8)+
  annotate('text',label="***",x=1.5,y=8.3,size=10,color='black')

(PG1.sex | PG2.sex | PGR.sex) +plot_layout(guides='collect') & theme(legend.position = 'top')
###幽门螺旋杆菌分布


###
make.table(dat= match2,
           strat        = "PG_pos",
           cat.varlist  = variables,
           cat.rmstat   = list(c("col")),
           cat.ptype    = c("chisq"),
           output       = "html")

##整体因素探索
model1<-glm(PG_pos~Hp_pos+胃癌家族史+性别+年龄+婚姻+       
              就业状况2+家庭收入2+教育+      
              血型2+运动+BMI_group2+吸烟1+ 
              被动吸烟1+每天早餐+准点吃饭+睡眠时间+睡眠质量+夜班+       
              吃饭速度+外出吃饭+静态时间+饮酒+   
              喝茶+鲜奶+酸奶+咖啡+碳酸饮料+     
              果味饮料+茶味饮料+蔬菜+水果+谷类+ 
              鸡蛋+杂粮+豆类+肉类+坚果+ 
              水产品+薯类+大蒜+菌类+油炸+   
              烧烤+熏制+酱制+偏咸+腌制+  
              晒制+偏辣+偏酸+偏烫+偏甜+偏硬+胃溃疡+胃息肉+
              萎缩性胃炎+胃肠上皮化生+  
              消化性溃疡+糖尿病+高血压+高血脂+
              冠心病,data=pepsinogen2020,family='binomial')
summary(model1)
vif(model1)
##向后逐步回归
step(model1,direction = 'backward')
#重新构建模型
model1.1<-glm(PG_pos ~ Hp_pos + 胃癌家族史 + 年龄 + 睡眠时间 + 
                静态时间 + 饮酒 + 喝茶 + 酸奶 + 水果 + 鸡蛋 + 
                烧烤 + 腌制 + 偏辣 + 胃息肉 + 糖尿病 + 高血脂, 
              family = "binomial", data = pepsinogen2020)
summary(model1.1)
##幽门螺旋杆菌感染分布
make.table(dat= pepsinogen2020,
           strat        = "PG_hp",
           cat.varlist  = variables,
           cat.rmstat   = list(c("col")),
           cat.ptype    = c("chisq"),
           output       = "html")
##adjusted OR
##Hp非感染者中发生萎缩性胃炎的因素分析
logit(y='PG_pos',x=c("性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                    '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('血型2',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('运动',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('吸烟1',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('被动吸烟1',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('饮酒',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('喝茶',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史','BMI',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('鲜奶',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('酸奶',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('咖啡',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('碳酸饮料',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('果味饮料',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('蔬菜',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('水果',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('谷类',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('鸡蛋',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('杂粮',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('豆类',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('坚果',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('大蒜',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('菌类',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('油炸',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('烧烤',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('熏制',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('酱制',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('偏咸',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('腌制',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('偏辣',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('偏烫',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('偏酸',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))
logit(y='PG_pos',x=c('偏甜',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))

logit(y='PG_pos',x=c('偏硬',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阳性'))




##Hp感染者中发生萎缩性胃炎的因素分析
logit(y='PG_pos',x=c("性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('血型2',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('运动',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('吸烟1',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('被动吸烟1',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('饮酒',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('喝茶',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史','BMI',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('鲜奶',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('酸奶',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('咖啡',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('碳酸饮料',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('果味饮料',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('蔬菜',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('水果',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('谷类',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('鸡蛋',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('杂粮',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('豆类',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('坚果',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('大蒜',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('菌类',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('油炸',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('烧烤',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('熏制',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('酱制',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('偏咸',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('腌制',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('偏辣',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('偏烫',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('偏酸',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(y='PG_pos',x=c('偏甜',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))

logit(y='PG_pos',x=c('偏硬',"性别","年龄分组5",'教育','家庭收入2','胃癌家族史',
                     '就业状况2','糖尿病','吸烟1'),data=subset(pepsinogen2020,Hp_pos!='阴性'))


###Hp感染与油炸的交互作用
with(subset(pepsinogen2020,Hp_pos=='阳性'),table(油炸,PG_pos))
with(subset(pepsinogen2020,Hp_pos=='阴性'),table(油炸,PG_pos))
logit(x=c('油炸',"性别","年龄分组5",'BMI_group',
          '糖尿病','吸烟1'),y='PG_pos',data=subset(pepsinogen2020,Hp_pos!='阴性'))
logit(x=c('油炸',"性别","年龄分组5",'BMI_group',
          '糖尿病','吸烟1'),y='PG_pos',data=subset(pepsinogen2020,Hp_pos!='阳性'))



###Hp感染分布
pepsinogen2020%>%pivot_longer(cols=c("PG1","PG2","PGR"),names_to = 'PG',values_to = 'value')%>%
  group_by(PG,Hp_pos)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))%>%
  ggplot(aes(x=PG,y=median,fill=Hp_pos))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='Hp_pos',x=' ')+
  annotate('text',label="***",x=1,y=92,size=10,color='black')+annotate('text',label="***",x=2,y=22,size=10,color='black')+
  annotate('text',label="***",x=3,y=9.2,size=10,color='black')+
  scale_fill_discrete(labels=c('Negative','Positive'))

##饮酒与PG
pepsinogen2020%>%pivot_longer(cols=c("PG1","PG2","PGR"),names_to = 'PG',values_to = 'value')%>%
  group_by(PG,饮酒)%>%summarise(median=median(value),Q1=quantile(value,0.25),Q3=quantile(value,0.75))%>%
  ggplot(aes(x=PG,y=median,fill=饮酒))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='alcohol',x=' ')+
  #annotate('text',label="***",x=1,y=81.5,size=9,color='black')+
  #annotate('text',label="***",x=2,y=16.1,size=9,color='black')+
  #annotate('text',label="NS",x=3,y=12.0,size=4,color='black')+
  scale_fill_discrete(labels=c('Occasional','Regular'))#+scale_y_continuous(limits=c(0,83))
##饮酒情况
with(pepsinogen2020,table(饮酒));with(pepsinogen2020,table(饮酒2))
pepsinogen2020%>%group_by(饮酒2)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  ggplot()+geom_bar(aes(x=饮酒2,y=median,fill=饮酒2),stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(x=饮酒2,ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='alcohol',x=' ')
pepsinogen2020%>%ggboxplot(x='饮酒2',y='PG1',fill='饮酒2',palette = 'jco')+stat_compare_means(
  comparisons = list(c(1,2),c(1,3),c(1,4),c(1,5))
)
##
pepsinogen2020%>%filter(饮酒2=="low alcoholic liquor" | 饮酒2=='strong liquor')%>%
  pivot_longer(cols=c('低度白酒量','高度白酒量'),names_to = '酒量',values_to = 'value')%>%
  transmute(value,PG1)%>%transmute(value_range=factor(case_when(
    value<=1 ~ 1,
    between(value,1.01,2) ~ 2,
    between(value,2.01,3) ~ 3,
    value>3 ~ 4
  )),PG1)%>%ggplot(aes(x=value_range,y=PG1))+geom_boxplot()



###2020-09-16
#model2
logit(y='PG_pos',x=c('Hp_pos','BMI','年龄分组','性别','就业状况2','吸烟3','饮酒'),data=pepsinogen2020)
#model3
logit(y='PG_pos',x=c('Hp_pos','BMI','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                       '消化性溃疡'),data=pepsinogen2020)
##
logit(y='PG_pos5',x=c('Hp_pos','BMI','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡'),data=pepsinogen2020)
##
#Hp与年龄
pepsinogen2020%>%group_by(年龄分组2,Hp_pos)%>%summarise(n=n())%>%
  group_by(年龄分组2)%>%mutate(percent=round(n/sum(n),2))%>%ggplot()+
  geom_bar(aes(x=年龄分组2,y=percent,fill=Hp_pos),stat='identity',position = 'dodge')+
  
  

ggplot(aes(x=年龄分组2,fill=Hp_pos))+geom_bar(position ='dodge')
pepsinogen2020%>%group_by(Hp_pos,年龄分组2)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))
Hp1<-pepsinogen2020%>%ggline(x='年龄分组2',y='PG1',color='Hp_pos',add='median')+labs(x='',y='PGI')+scale_fill_discrete(labels=c('Negative','Positive'))
Hp2<-pepsinogen2020%>%ggline(x='年龄分组2',y='PG2',color='Hp_pos',add='median')+labs(x='',y='PGII')+scale_fill_discrete(labels=c('Negative','Positive'))
Hpr<-pepsinogen2020%>%ggline(x='年龄分组2',y='PGR',color='Hp_pos',add='median')+labs(x='',y='PGI/II')+scale_fill_discrete(labels=c('Negative','Positive'))
(Hp1 | Hp2 | Hpr) + plot_layout(guides='collect') & theme(legend.position = 'top')
###吸烟与Hp
pepsinogen2020%>%filter(性别=='Male')%>%
  group_by(Hp_pos,吸烟2)%>%summarise(median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  ggplot(aes(x=Hp_pos,y=median,fill=吸烟2))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='smoking',x=' ')+
  scale_fill_discrete(labels=c('Never','Smoker'))

pepsinogen2020%>%
  group_by(Hp_pos,吸烟2)%>%summarise(median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  ggplot(aes(x=Hp_pos,y=median,fill=吸烟2))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='smoking',x=' ')+
  scale_fill_discrete(labels=c('Never','Smoker'))

pepsinogen2020%>%
  group_by(Hp_pos,吸烟2)%>%summarise(median=median(PGR),Q1=quantile(PGR,0.25),Q3=quantile(PGR,0.75))%>%
  ggplot(aes(x=Hp_pos,y=median,fill=吸烟2))+geom_bar(stat='identity',color='black',width = 0.5,size=0.8,position = 'dodge')+
  geom_errorbar(aes(ymin=median,ymax=Q3),width=0.1,size=0.8,position = position_dodge(0.5))+mytheme+labs(fill='smoking',x=' ')+
  scale_fill_discrete(labels=c('Never','Smoker'))


##吸烟、Hp、PG
with(pepsinogen2020,table(Hp_pos,吸烟1))
with(pepsinogen2020,prop.table(table(Hp_pos,吸烟1),margin = 2))
pepsinogen2020%>%filter(C14Value>0)%>%group_by(吸烟1)%>%summarise(median=median(C14Value),Q1=quantile(C14Value,0.25),Q3=quantile(C14Value,0.75))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阴性' & 性别=='Female'))
kruskal.test(PG1~吸烟1,data=subset(pepsinogen2020,Hp_pos=='阳性' & 性别=='Female'))
##奶奶了



