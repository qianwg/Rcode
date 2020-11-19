##与其他种类标志物的相关性分析
library(rio)
library(tidyverse)
library(table1)
library(ggpubr)
library(DT)
library(forestmodel)
library(patchwork)
library(rms)
library(ggstatsplot)
library(htmlTable)
library(nnet)
library(effects)
library(VGAM)
source('~/Rcode/screening/gastric_screening2019/sensitivity_data.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
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
#基本分布
#与PG1
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG1_range3",
           cat.varlist  = c('CEA_range','AFP_range','CA199_range','HBsAg_pos'),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           cat.ptype    = c("chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")
pepsinogen%>%pivot_longer(cols=c('CEA_range','AFP_range','CA199_range','HBsAg_pos'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('CEA_range','AFP_range','CA199_range','HBsAg_pos')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)     
##与PG2
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG2_range",
           cat.varlist  = c('CEA_range','AFP_range','CA199_range','HBsAg_pos'),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           cat.ptype    = c("chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")
pepsinogen%>%pivot_longer(cols=c('CEA_range','AFP_range','CA199_range','HBsAg_pos'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('CEA_range','AFP_range','CA199_range','HBsAg_pos')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p) 

##OR单因素和多因素
##Atrophic gastritis was defined as PG1<30
logit(x=c('CEA_range','咖啡','吸烟1','性别','年龄分组','BMI_group','油炸','饮酒','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('AFP_range','咖啡','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('CA199_range','咖啡','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
###Atrophic gastritis was defined as PG1<=70 & PGR<=3
logit(x=c('CEA_range','咖啡','吸烟1','性别','年龄分组','BMI_group','油炸','饮酒','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('AFP_range','咖啡','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('CA199_range','咖啡','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
#RCS
dd <- datadist(pepsinogen) 
options(datadist='dd')
fit<- lrm(PG_pos ~ rcs(CA199,4)+年龄分组+性别+饮酒+BMI_group+糖尿病+就业状况+高血压+高血脂+油炸+咖啡+教育年数,data=pepsinogen) 
RR<-Predict(fit,CA199,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(CA199,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(CA199,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="CA199", y="RR (95%CI)") +scale_x_continuous(breaks=c(0,1,2,3,4,5,10,15))
anova(fit)


  ###女性肿瘤标志物：CA125、CA153
#PG1
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG1_range3",
           cat.varlist  = c('CA125_range','CA153_range'),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           cat.ptype    = c("chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")

pepsinogen%>%pivot_longer(cols=c('CA125_range','CA153_range'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('CA125_range','CA153_range')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
##OR单因素和多因素
##Atrophic gastritis was defined as PG1<30
logit(x=c('CA125_range','咖啡','吸烟1','年龄分组','BMI_group','油炸','饮酒','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('CA153_range','咖啡','饮酒','吸烟1','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
###Atrophic gastritis was defined as PG1<=70 & PGR<=3
logit(x=c('CA125_range','咖啡','吸烟1','年龄分组','BMI_group','油炸','饮酒','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('CA153_range','咖啡','饮酒','吸烟1','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)


#pg2
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG2_range",
           cat.varlist  = c('CA125_range','CA153_range'),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           cat.ptype    = c("chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")
pepsinogen%>%pivot_longer(cols=c('CA125_range','CA153_range'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('CA125_range','CA153_range')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p) 

##CEA分层分析
##年龄
logit(x=c('CEA_range','BMI_group','性别','饮酒','油炸','吸烟1','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,年龄分组5=="<=60"))
logit(x=c('CEA_range','BMI_group','性别','饮酒','油炸','吸烟1','糖尿病'),y='PG_pos6',data=subset(pepsinogen,年龄分组5==">60"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+饮酒+性别+糖尿病+就业状况+教育年数,data=subset(pepsinogen,年龄分组5=="<=60"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+饮酒+性别+糖尿病+教育年数,data=subset(pepsinogen,年龄分组5==">60"),family='binomial'))

#P for interaction
model1<-glm(PG_pos6~年龄分组5*CEA_range+BMI_group2+性别+饮酒+油炸+吸烟1+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
##性别
#性别
logit(x=c('CEA_range','BMI_group','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,性别=="Male"))
logit(x=c('CEA_range','BMI_group','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,性别=="Female"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Male"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Female"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~性别*CEA_range+BMI_group+饮酒+油炸+吸烟1+年龄分组+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##吸烟
logit(x=c('CEA_range','BMI_group','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="从不吸烟"))
logit(x=c('CEA_range','BMI_group','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="目前或过去吸烟"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="从不吸烟"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="目前或过去吸烟"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~CEA_range*吸烟2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##饮酒
#饮酒
logit(x=c('CEA_range','BMI_group','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,饮酒=="否"))
logit(x=c('CEA_range','BMI_group','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,饮酒=="是"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="否"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="是"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~CEA_range*饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
##BMI分层
logit(x=c('CEA_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="正常"))
logit(x=c('CEA_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="超重"))
logit(x=c('CEA_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="肥胖"))

#P for trend 
summary(glm(PG_pos6~as.numeric(CEA_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="正常"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CEA_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="超重"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CEA_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="肥胖"),family='binomial'))

#P for interaction
model1<-glm(PG_pos6~CEA_range*BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况+饮酒,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##糖尿病
logit(x=c('CEA_range','BMI_group','油炸','吸烟1','性别','年龄分组','饮酒','就业状况'),y='PG_pos6',data=subset(pepsinogen,糖尿病=="否"))
logit(x=c('CEA_range','BMI_group','油炸','吸烟1','性别','年龄分组','饮酒','就业状况'),y='PG_pos6',data=subset(pepsinogen,糖尿病=="是"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+性别+年龄分组+饮酒+就业状况,data=subset(pepsinogen,糖尿病=="否"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CEA_range)+BMI_group+油炸+吸烟1+性别+年龄分组+饮酒+就业状况,data=subset(pepsinogen,糖尿病=="是"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~CEA_range*糖尿病+油炸+吸烟1+性别+年龄分组+饮酒+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##AFP分层分析
##年龄
logit(x=c('AFP_range','BMI_group','性别','饮酒','油炸','吸烟1','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,年龄分组5=="<=60"))
logit(x=c('AFP_range','BMI_group','性别','饮酒','油炸','吸烟1','糖尿病'),y='PG_pos6',data=subset(pepsinogen,年龄分组5==">60"))
#P for trend 
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+饮酒+性别+糖尿病+就业状况+教育年数,data=subset(pepsinogen,年龄分组5=="<=60"),family='binomial'))
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+饮酒+性别+糖尿病+教育年数,data=subset(pepsinogen,年龄分组5==">60"),family='binomial'))

#P for interaction
model1<-glm(PG_pos6~年龄分组5*AFP_range+BMI_group2+性别+饮酒+油炸+吸烟1+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
##性别
#性别
logit(x=c('AFP_range','BMI_group','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,性别=="Male"))
logit(x=c('AFP_range','BMI_group','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,性别=="Female"))
#P for trend 
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Male"),family='binomial'))
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Female"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~性别*AFP_range+BMI_group+饮酒+油炸+吸烟1+年龄分组+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##吸烟
logit(x=c('AFP_range','BMI_group','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="从不吸烟"))
logit(x=c('AFP_range','BMI_group','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="目前或过去吸烟"))
#P for trend 
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="从不吸烟"),family='binomial'))
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="目前或过去吸烟"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~AFP_range*吸烟2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##饮酒
#饮酒
logit(x=c('AFP_range','BMI_group','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,饮酒=="否"))
logit(x=c('AFP_range','BMI_group','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,饮酒=="是"))
#P for trend 
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="否"),family='binomial'))
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="是"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~AFP_range*饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
##BMI分层
logit(x=c('AFP_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="正常"))
logit(x=c('AFP_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="超重"))
logit(x=c('AFP_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="肥胖"))

#P for trend 
summary(glm(PG_pos6~as.numeric(AFP_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="正常"),family='binomial'))
summary(glm(PG_pos6~as.numeric(AFP_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="超重"),family='binomial'))
summary(glm(PG_pos6~as.numeric(AFP_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="肥胖"),family='binomial'))

#P for interaction
model1<-glm(PG_pos6~AFP_range*BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况+饮酒,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##糖尿病
logit(x=c('AFP_range','BMI_group','油炸','吸烟1','性别','年龄分组','饮酒','就业状况'),y='PG_pos6',data=subset(pepsinogen,糖尿病=="否"))
logit(x=c('AFP_range','BMI_group','油炸','吸烟1','性别','年龄分组','饮酒','就业状况'),y='PG_pos6',data=subset(pepsinogen,糖尿病=="是"))
#P for trend 
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+性别+年龄分组+饮酒+就业状况,data=subset(pepsinogen,糖尿病=="否"),family='binomial'))
summary(glm(PG_pos6~as.numeric(AFP_range)+BMI_group+油炸+吸烟1+性别+年龄分组+饮酒+就业状况,data=subset(pepsinogen,糖尿病=="是"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~AFP_range*糖尿病+油炸+吸烟1+性别+年龄分组+饮酒+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

###ca199分层分析

##年龄
logit(x=c('CA199_range','BMI_group','性别','饮酒','油炸','吸烟1','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,年龄分组5=="<=60"))
logit(x=c('CA199_range','BMI_group','性别','饮酒','油炸','吸烟1','糖尿病'),y='PG_pos6',data=subset(pepsinogen,年龄分组5==">60"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+饮酒+性别+糖尿病+就业状况+教育年数,data=subset(pepsinogen,年龄分组5=="<=60"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+饮酒+性别+糖尿病+教育年数,data=subset(pepsinogen,年龄分组5==">60"),family='binomial'))

#P for interaction
model1<-glm(PG_pos6~年龄分组5*CA199_range+BMI_group2+性别+饮酒+油炸+吸烟1+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
##性别
#性别
logit(x=c('CA199_range','BMI_group','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,性别=="Male"))
logit(x=c('CA199_range','BMI_group','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,性别=="Female"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Male"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Female"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~性别*CA199_range+BMI_group+饮酒+油炸+吸烟1+年龄分组+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##吸烟
logit(x=c('CA199_range','BMI_group','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="从不吸烟"))
logit(x=c('CA199_range','BMI_group','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="目前或过去吸烟"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="从不吸烟"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="目前或过去吸烟"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~CA199_range*吸烟2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##饮酒
#饮酒
logit(x=c('CA199_range','BMI_group','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,饮酒=="否"))
logit(x=c('CA199_range','BMI_group','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,饮酒=="是"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="否"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="是"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~CA199_range*饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
##BMI分层
logit(x=c('CA199_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="正常"))
logit(x=c('CA199_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="超重"))
logit(x=c('CA199_range','饮酒','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,BMI_group=="肥胖"))

#P for trend 
summary(glm(PG_pos6~as.numeric(CA199_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="正常"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CA199_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="超重"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CA199_range)+饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,BMI_group=="肥胖"),family='binomial'))

#P for interaction
model1<-glm(PG_pos6~CA199_range*BMI_group+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况+饮酒,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')

##糖尿病
logit(x=c('CA199_range','BMI_group','油炸','吸烟1','性别','年龄分组','饮酒','就业状况'),y='PG_pos6',data=subset(pepsinogen,糖尿病=="否"))
logit(x=c('CA199_range','BMI_group','油炸','吸烟1','性别','年龄分组','饮酒','就业状况'),y='PG_pos6',data=subset(pepsinogen,糖尿病=="是"))
#P for trend 
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+性别+年龄分组+饮酒+就业状况,data=subset(pepsinogen,糖尿病=="否"),family='binomial'))
summary(glm(PG_pos6~as.numeric(CA199_range)+BMI_group+油炸+吸烟1+性别+年龄分组+饮酒+就业状况,data=subset(pepsinogen,糖尿病=="是"),family='binomial'))
#P for interaction
model1<-glm(PG_pos6~CA199_range*糖尿病+油炸+吸烟1+性别+年龄分组+饮酒+就业状况+BMI_group,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')























