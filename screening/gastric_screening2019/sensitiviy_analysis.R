
###2020-08-13#按照戴老师得excel进行数据分析，专注于PG1与PG2，进行分类。
#0,20,40,60,80
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
dd <- datadist(pepsinogen) 
options(datadist='dd')
#summary(pepsinogen$PG1_range1);summary(pepsinogen$PG1_range2)
#summary(pepsinogen$PG2_range);summary(pepsinogen$PG2_range2)
##Table 1 :The Distribution of Demographic factors and Their associations with PGI and PGII levels
#PG1
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG1_range4",
           cat.varlist  = c("年龄分组3", "性别",'教育年数','地址','BMI_group','糖尿病','高血压','高血脂','冠心病','中风'),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           cat.ptype    = c("chisq", "chisq","chisq","fisher","chisq","chisq","chisq","chisq","chisq","chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")
pepsinogen%>%pivot_longer(cols=c("年龄分组3", "性别",'教育年数','地址','BMI_group','糖尿病','高血压','高血脂','冠心病','中风'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c("年龄分组3", "性别",'教育年数','地址','BMI_group','糖尿病','高血压','高血脂','冠心病','中风')){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#PG2
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG2_range",
           cat.varlist  = c("年龄分组3", "性别",'教育年数','地址','BMI_group','糖尿病','高血压','高血脂','冠心病','中风'),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           #cat.rmstat   = list(c("col"), c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col")),
           cat.ptype    = c("chisq", "chisq","chisq","fisher","chisq","chisq","chisq","chisq","chisq","chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")
pepsinogen%>%pivot_longer(cols=c("年龄分组3", "性别",'教育年数','地址','BMI_group','糖尿病','高血压','高血脂','冠心病','中风'),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c("年龄分组3", "性别",'教育年数','地址','BMI_group','糖尿病','高血压','高血脂','冠心病','中风')){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
###Table1-2,The distribution of lifestyle factors and their associations with PG1 and PG2 levels
#PG1
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG1_range3",
           cat.varlist  = c('吸烟1',variables4),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           #cat.rmstat   = list(c("col"), c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col")),
           cat.ptype    = c("chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")
pepsinogen%>%pivot_longer(cols=c('吸烟1',variables4),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('吸烟1',variables4)){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)
#PG2
make.table(dat= subset(pepsinogen,年龄>=40),
           strat        = "PG2_range",
           cat.varlist  = c('吸烟1',variables4),
           #cat.rmstat   = list(c("row"), c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row"),c("row")),
           #cat.rmstat   = list(c("col"), c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col"),c("col")),
           cat.ptype    = c("chisq"),
           #cont.varlist = c("bili", "copper"),
           #cont.rmstat  = list(c("count", "meansd", "q1q3"), c("minmax")),
           output       = "html")
pepsinogen%>%pivot_longer(cols=c('吸烟1',variables4),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(PG2),Q1=quantile(PG2,0.25),Q3=quantile(PG2,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
p<-list()
for(i in c('吸烟1',variables4)){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
do.call(rbind,p)

##Table 3. The Associations Between Life Style Factors and 
#Atrophic Gastritis Occurrence Estimated by Logistic Regression Modeling
#Atrophic gastritis was defined as PG1<=70 & PGR<=3
logit(x=c('性别','年龄分组','吸烟1','BMI_group','油炸','饮酒','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)

##Atrophic gastritis was defined as PG1<30
logit(x=c('吸烟1','性别','年龄分组','BMI_group','油炸','饮酒','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('咖啡','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('鲜奶','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('酸奶','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('喝茶','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('碳酸饮料','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('果味饮料','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('水果','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('谷类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('肉类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('鸡蛋','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('蔬菜','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('水产品','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('薯类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('杂粮','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('豆类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('坚果','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('大蒜','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('菌类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('烧烤','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('熏制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('腌制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('酱制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('晒制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)
logit(x=c('偏咸','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos6',data=pepsinogen)

###Atrophic gastritis was defined as PG1<=70 & PGR<=3

logit(x=c('吸烟1','性别','年龄分组','BMI_group','油炸','饮酒','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('咖啡','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('鲜奶','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('酸奶','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('喝茶','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('碳酸饮料','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('果味饮料','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('水果','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('谷类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('肉类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('鸡蛋','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('蔬菜','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('水产品','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('薯类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('杂粮','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('豆类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('坚果','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('大蒜','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('菌类','饮酒','吸烟1','性别','年龄分组','BMI_group','油炸','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('烧烤','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('熏制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('腌制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('酱制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('晒制','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)
logit(x=c('偏咸','油炸','饮酒','吸烟1','性别','年龄分组','BMI_group','糖尿病','就业状况'),y='PG_pos',data=pepsinogen)





###Table 4.The association between BMI level(or smk) and 
#PG-1/PG2 by age group (根据数据改成图,  J-shaped or U-shaped curve)
#PG1
make.table(dat= subset(pepsinogen,年龄分组3=="40-49"),
           strat        = "PG_pos7",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组3=="50-59"),
           strat        = "PG_pos7",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组3=="60-69"),
           strat        = "PG_pos7",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")

make.table(dat= subset(pepsinogen,年龄分组3==">=70"),
           strat        = "PG_pos7",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
#OR
fit<- lrm(PG_pos7 ~ rcs(BMI,4)+年龄分组+性别+饮酒+糖尿病+就业状况+高血压+高血脂+油炸+咖啡+教育年数,data=pepsinogen) 
RR<-Predict(fit,BMI,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="age", y="RR (95%CI)") 
anova(fit)

#40-49
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="40-49")))
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="40-49")))
#rcs
fit<- lrm(PG_pos7 ~ rcs(BMI,4) + 性别,data=subset(pepsinogen,年龄分组3=="40-49")) 
RR<-Predict(fit,BMI,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="age", y="RR (95%CI)") 
anova(fit)
#50-59
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="50-59")))
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="50-59")))

#60-69
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="60-69")))
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="60-69")))

#>=70
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3==">=70")))
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒,family='binomial',data=subset(pepsinogen,年龄分组3==">=70")))
##画图
BMI<-factor(c(1,2,3,4),levels=c(1,2,3,4),labels=c('<18.5','18.5-23.9','24-27.9','>=28'))
age_group<-factor(c(1,1,1,1,2,2,2,2,3,3,3,3),levels=c(1,1,1,1,2,2,2,2,3,3,3,3),
                  labels=c('<50','<50','<50','<50','50-60','50-60','50-60','50-60','>60','>60','>60','>60'))
aOR<-c(0.80,1,1.44,1.82,0.64,1,1.09,1.27,0.96,1,1.07,1.28)
LCL<-c(0.29,1,0.96,1.07,0.30,1,0.87,0.95,0.55,1,0.91,1.02)
UCL<-c(2.62,1,2.18,3.17,1.47,1,1.36,1.71,1.71,1,1.27,1.61)
OR.plot<-data.frame(age=age_group,BMI=rep(BMI,3),aOR,LCL,UCL)
#arrange(OR.plot,age)
OR.plot%>%ggplot()+geom_point(aes(x=BMI,y=aOR))+geom_point(aes(x=BMI,y=LCL),color='blue')+geom_point(aes(x=BMI,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=BMI,y=aOR,ymin=LCL,ymax=UCL))+
  facet_wrap(.~age,scales='free_y')+mytheme2+geom_smooth(aes(x=BMI,y=aOR,group=1),method='lm',se=FALSE)+
  scale_y_continuous(limits = c(0,4))
###OR


##分层 p for trend and P for interaction
#吸烟
logit(x=c('BMI_group2','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="从不吸烟"))
logit(x=c('BMI_group2','油炸','饮酒','性别','年龄分组','糖尿病','就业状况'),y='PG_pos6',data=subset(pepsinogen,吸烟2=="目前或过去吸烟"))
#P for trend 
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="从不吸烟"),family='binomial'))
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,吸烟2=="目前或过去吸烟"),family='binomial'))
#P for interaction
model1<-glm(PG_pos7~BMI_group2*吸烟2+油炸+饮酒+性别+年龄分组+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
#饮酒
logit(x=c('BMI_group2','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos7',data=subset(pepsinogen,饮酒=="否"))
logit(x=c('BMI_group2','油炸','吸烟1','性别','年龄分组','糖尿病','就业状况'),y='PG_pos7',data=subset(pepsinogen,饮酒=="是"))
#P for trend 
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="否"),family='binomial'))
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,饮酒=="是"),family='binomial'))
#P for interaction
model1<-glm(PG_pos7~BMI_group2*饮酒+油炸+吸烟1+性别+年龄分组+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')
#性别
logit(x=c('BMI_group2','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos7',data=subset(pepsinogen,性别=="Male"))
logit(x=c('BMI_group2','饮酒','油炸','吸烟1','年龄分组','糖尿病','就业状况'),y='PG_pos7',data=subset(pepsinogen,性别=="Female"))
#P for trend 
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Male"),family='binomial'))
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+吸烟1+饮酒+年龄分组+糖尿病+就业状况,data=subset(pepsinogen,性别=="Female"),family='binomial'))
#P for interaction
model1<-lrm(PG_pos7~BMI_group2*性别+饮酒+油炸+吸烟1+年龄分组+糖尿病+就业状况,data=pepsinogen)
latex(anova(model1),file=' ')
#年龄分层
logit(x=c('BMI_group2','性别','饮酒','油炸','吸烟1','糖尿病','就业状况'),y='PG_pos7',data=subset(pepsinogen,年龄分组=="<50"))
logit(x=c('BMI_group2','性别','饮酒','油炸','吸烟1','糖尿病','就业状况'),y='PG_pos7',data=subset(pepsinogen,年龄分组=="50-60"))
logit(x=c('BMI_group2','性别','饮酒','油炸','吸烟1','糖尿病','就业状况'),y='PG_pos7',data=subset(pepsinogen,年龄分组==">60"))
#P for trend 
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+吸烟1+饮酒+性别+糖尿病+就业状况,data=subset(pepsinogen,年龄分组=="<50"),family='binomial'))
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+吸烟1+饮酒+性别+糖尿病+就业状况,data=subset(pepsinogen,年龄分组=="50-60"),family='binomial'))
summary(glm(PG_pos7~as.numeric(BMI_group2)+油炸+吸烟1+饮酒+性别+糖尿病+就业状况,data=subset(pepsinogen,年龄分组==">60"),family='binomial'))

#P for interaction
model1<-glm(PG_pos7~BMI_group2*年龄分组+性别+饮酒+油炸+吸烟1+糖尿病+就业状况,data=pepsinogen,family='binomial')
anova(model1,test='Chisq')



########
##PG2
#总
make.table(dat= pepsinogen,
           strat        = "PG_pos8",
           cat.varlist  = c("包年分组","包年分组2"),
           cat.ptype    = c("chisq"),
           output       = "html")
logit(y='PG_pos8',x=c('包年分组','年龄分组','性别'),data=pepsinogen)
logit(y='PG_pos8',x=c('包年分组','年龄分组','性别','就业状况','饮酒','糖尿病','油炸','BMI_group'),data=pepsinogen)


#年龄分层
make.table(dat= subset(pepsinogen,年龄分组3=="40-49"),
           strat        = "PG_pos8",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组3=="50-59"),
           strat        = "PG_pos8",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组3=="60-69"),
           strat        = "PG_pos8",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")

make.table(dat= subset(pepsinogen,年龄分组3==">=70"),
           strat        = "PG_pos8",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
#OR
#40-49
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="40-49")))
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="40-49")))

#50-59
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="50-59")))
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="50-59")))

#60-69
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="60-69")))
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="60-69")))

#>=70
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3==">=70")))
forest_model(glm(PG_pos8~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒,family='binomial',data=subset(pepsinogen,年龄分组3==">=70")))
#PGR
make.table(dat= pepsinogen,
           strat        = "PG_pos7",
           cat.varlist  = c("包年分组","包年分组2"),
           cat.ptype    = c("chisq"),
           output       = "html")
logit(y='PG_pos7',x=c('包年分组','年龄分组','性别'),data=pepsinogen)
logit(y='PG_pos7',x=c('包年分组','年龄分组','性别','就业状况','饮酒','糖尿病','油炸','BMI_group'),data=pepsinogen)

make.table(dat= subset(pepsinogen,年龄分组3=="40-49"),
           strat        = "PG_pos5",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组3=="50-59"),
           strat        = "PG_pos5",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组3=="60-69"),
           strat        = "PG_pos5",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")

make.table(dat= subset(pepsinogen,年龄分组3==">=70"),
           strat        = "PG_pos5",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
#OR
#40-49
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="40-49")))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="40-49")))

#50-59
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="50-59")))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="50-59")))

#60-69
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3=="60-69")))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组3=="60-69")))

#>=70
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组3==">=70")))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒,family='binomial',data=subset(pepsinogen,年龄分组3==">=70")))
#根据数据改成图
#PG2
aOR<-c(1.20,1,0.95,0.66,1.52,1,0.88,0.83,1.48,1,1.01,0.75)
LCL<-c(0.34,1,0.61,0.34,0.66,1,0.70,0.61,0.85,1,0.85,0.59)
UCL<-c(3.38,1,1.46,1.21,3.27,1,1.11,1.13,2.54,1,1.21,0.96)
OR2.plot<-data.frame(age=age_group,BMI=rep(BMI,3),aOR,LCL,UCL)
#arrange(OR.plot,age)
OR2.plot%>%ggplot()+geom_point(aes(x=BMI,y=aOR))+geom_point(aes(x=BMI,y=LCL),color='blue')+geom_point(aes(x=BMI,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=BMI,y=aOR,ymin=LCL,ymax=UCL))+
  facet_wrap(.~age,scales='free_y')+mytheme2+geom_smooth(aes(x=BMI,y=aOR,group=1),method='lm',se=FALSE)+
  scale_y_continuous(limits = c(0,4))

##PGR定义的胃萎缩
#<50
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
#50-60
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))

#>=60
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄+性别,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄分组2+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组==">60")))

##画图
BMI<-factor(c(1,2,3,4),levels=c(1,2,3,4),labels=c('<18.5','18.5-23.9','24-27.9','>=28'))
age_group<-factor(c(1,1,1,1,2,2,2,2,3,3,3,3),levels=c(1,1,1,1,2,2,2,2,3,3,3,3),
                  labels=c('<50','<50','<50','<50','50-60','50-60','50-60','50-60','>60','>60','>60','>60'))
aOR<-c(1.12,1,0.81,1.79,1.33,1,1.31,1.03,1.71,1,0.90,0.86)
LCL<-c(0.06,1,0.33,0.66,0.21,1,0.86,0.95,0.74,1,0.66,0.56)
UCL<-c(6.06,1,1.89,4.52,4.77,1,2.03,1.11,3.62,1,1.22,1.29)
OR2.plot<-data.frame(age=age_group,BMI=rep(BMI,3),aOR,LCL,UCL)
OR2.plot%>%ggplot()+geom_point(aes(x=BMI,y=aOR))+geom_point(aes(x=BMI,y=LCL),color='blue')+geom_point(aes(x=BMI,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=BMI,y=aOR,ymin=LCL,ymax=UCL))+
  facet_wrap(.~age,scales='free_y')+mytheme2+geom_smooth(aes(x=BMI,y=aOR,group=1),method='lm',se=FALSE)#+
  #scale_y_continuous(limits = c(0,4))

#PG2与PGR定义的胃萎缩
#<50
#forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄+性别,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+饮酒+就业状况+咖啡,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
#50-60
#forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄+性别,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+饮酒+就业状况+糖尿病+咖啡,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))

#>=60
#forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄+性别,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组2+性别+吸烟1+油炸+糖尿病+饮酒+就业状况+咖啡,family='binomial',data=subset(pepsinogen,年龄分组==">60")))
aOR<-c(1.25,1,0.84,1.62,0.74,1,1.39,1.27,1.57,1,0.87,0.92)
LCL<-c(0.07,1,0.32,0.52,0.04,1,0.88,0.67,0.59,1,0.62,0.58)
UCL<-c(6.92,1,2.07,4.52,3.75,1,2.22,2.30,3.53,1,1.21,1.41)
OR2.plot<-data.frame(age=age_group,BMI=rep(BMI,3),aOR,LCL,UCL)
OR2.plot%>%ggplot()+geom_point(aes(x=BMI,y=aOR))+geom_point(aes(x=BMI,y=LCL),color='blue')+geom_point(aes(x=BMI,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=BMI,y=aOR,ymin=LCL,ymax=UCL))+
  facet_wrap(.~age,scales='free_y')+mytheme2+geom_smooth(aes(x=BMI,y=aOR,group=1),method='lm',se=FALSE)#+
#scale_y_continuous(limits = c(0,4))



##PG与吸烟

#PG1
make.table(dat= pepsinogen,
           strat        = "PG_pos7",
           cat.varlist  = c("包年分组","包年分组2"),
           cat.ptype    = c("chisq"),
           output       = "html")
forest_model(glm(PG_pos7~年龄+性别+包年分组,family='binomial',data=pepsinogen))
forest_model(glm(PG_pos7~BMI_group+年龄+性别+包年分组+油炸+糖尿病+饮酒+就业状况,family='binomial',data=pepsinogen))
#RCS
fit<- lrm(PG_pos7 ~ rcs(包年,4)+年龄分组+性别+饮酒+糖尿病+就业状况+高血压+高血脂+油炸+咖啡+教育年数,data=subset(pepsinogen,!is.na(包年) & 包年<55)) 
RR<-Predict(fit,包年,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(包年,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(包年,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="包年", y="RR (95%CI)") 
anova(fit)
##画图
baonian<-factor(c(1,2,3,4,5),levels=c(1,2,3,4,5),labels=c('<10','10-','20-','30-','>40'))
aOR<-c(1,0.87,0.89,0.90,0.82)
LCL<-c(1,0.67,0.66,0.65,0.59)
UCL<-c(1,1.14,1.21,1.25,1.14)
OR2.plot<-data.frame(baonian,aOR,LCL,UCL)
#arrange(OR.plot,age)
OR2.plot%>%ggplot()+geom_point(aes(x=baonian,y=aOR))+geom_point(aes(x=baonian,y=LCL),color='blue')+geom_point(aes(x=baonian,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=baonian,y=aOR,ymin=LCL,ymax=UCL))+
  mytheme2+geom_smooth(aes(x=baonian,y=aOR,group=1),method='lm',se=FALSE)
#PG2
make.table(dat= pepsinogen,
           strat        = "PG2_range4",
           cat.varlist  = c("包年分组","包年分组2"),
           cat.ptype    = c("chisq"),
           output       = "html")
forest_model(glm(PG2_range4~BMI_group+年龄+性别+包年分组,family='binomial',data=pepsinogen))
forest_model(glm(PG2_range4~BMI_group+年龄+性别+包年分组+油炸+糖尿病+饮酒+就业状况,family='binomial',data=pepsinogen))
##画图
baonian<-factor(c(1,2,3,4,5),levels=c(1,2,3,4,5),labels=c('<10','10-','20-','30-','>40'))
aOR<-c(1,0.78,0.93,0.91,0.78)
LCL<-c(1,0.58,0.67,0.64,0.54)
UCL<-c(1,1.05,1.29,1.30,1.12)
OR2.plot<-data.frame(baonian,aOR,LCL,UCL)
#arrange(OR.plot,age)
OR2.plot%>%ggplot()+geom_point(aes(x=baonian,y=aOR))+geom_point(aes(x=baonian,y=LCL),color='blue')+geom_point(aes(x=baonian,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=baonian,y=aOR,ymin=LCL,ymax=UCL))+
  mytheme2+geom_smooth(aes(x=baonian,y=aOR,group=1),method='lm',se=FALSE)

#RCS
fit<- lrm(PG_pos ~ rcs(包年,4) + 性别+年龄分组,data=subset(pepsinogen,吸烟1=="目前吸烟"| 吸烟2=="过去吸烟")) 
RR<-Predict(fit, 包年,fun=exp,ref.zero = TRUE) ####预测HR值
ggplot()+geom_line(data=RR, aes(包年,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(包年,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+ 
  labs(title = "RCS", x="age", y="RR (95%CI)") 
anova(fit)
logit(y="PG_pos",x=c("包年分组2","性别",'年龄分组'),data=subset(pepsinogen,吸烟1=="目前吸烟" | 吸烟2=="过去吸烟"))

##多元logistic回归
model1<-multinom(PG_pos4~油炸+性别+年龄分组,data=pepsinogen)
plot(Effect('油炸',model1))
plot(Effect('油炸',model1),multiline=T)
plot(Effect('油炸',model1),style='stacked')
##
model1<-multinom(relevel(PG1_range3,ref='>=70')~饮酒+性别+年龄分组,data=pepsinogen)
exp(model1$coefficients)
modle1$LCL<-exp(modle1$coefficients-modle1$standard.errors*1.96)
modlel1$UCL<-exp(modle1$coefficients+modlel1$standard.errors*1.96)


plot(Effect('饮酒',model1))
plot(Effect('饮酒',model1),multiline=T)
plot(Effect('饮酒',model1),style='stacked')
#R package BGAM
#吸烟
#PG1
model2<-vglm(PG1_range3~吸烟1+性别+鲜奶+喝茶+果味饮料+水果+谷类+肉类+鸡蛋+蔬菜+水产品+薯类+杂粮+豆类+坚果+大蒜+菌类+烧烤+熏制+腌制+酱制+晒制+偏咸+
               年龄分组+饮酒+油炸+BMI_group+就业状况+咖啡+酸奶+碳酸饮料,data=pepsinogen,family=multinomial(refLevel = 4))
round(cbind(as.data.frame(exp(coefvlm(model2))),exp(confintvglm(model2))),2)
#PG2
model2<-vglm(PG2_range~吸烟1+性别+鲜奶+喝茶+果味饮料+水果+谷类+肉类+鸡蛋+蔬菜+水产品+薯类+杂粮+豆类+坚果+大蒜+菌类+烧烤+熏制+腌制+酱制+晒制+偏咸+
               年龄分组+饮酒+油炸+BMI_group+就业状况+咖啡+酸奶+碳酸饮料,data=pepsinogen,family=multinomial(refLevel = 1))
round(cbind(as.data.frame(exp(coefvlm(model2))),exp(confintvglm(model2))),2)








##对于所有有胃镜/病理结果的人，其PG的分布与病变的分布曲线关系
gastroscopy<-import('~/data/示范区胃镜结果--2020-8-17.xlsx')
match<-left_join(gastroscopy,pepsinogen,by='ID')
table(match$type)
match$type<-factor(match$type,levels=1:9,labels=c('正常','消化性溃疡','胃息肉','胃切除术','慢性胃炎','萎缩性胃炎','肠上皮化生','不典型增生','胃癌'))
match%>%filter%>%filter(!is.na(type))%>%group_by(type)%>%summarise(n=n(),median=median(PG1),Q1=quantile(PG1,0.25),Q3=quantile(PG1,0.75))%>%
  transmute(level,n,median=paste0(median,"(",Q1,"-",Q3,")"))%>%datatable()
##PG1
PG1.normal<-data%>%ggplot(aes(x=PG1))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 69.60,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(data$PG1),sd=sd(data$PG1)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(0,20,50,70,100,150,200),expand = c(0,0))+scale_y_continuous(expand = c(0,0))
PG2.normal<-data%>%ggplot(aes(x=PG2))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 17.10,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(data$PG2),sd=sd(data$PG2)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(0,6,10,15,20,50),limits = c(0,51),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+labs(y=" ")
PGR.normal<-data%>%ggplot(aes(x=PGr))+geom_histogram(aes(y=..density..),bins=40,color='black',fill='lightblue')+
  geom_vline(xintercept = 5.65,color='blue',size=1)+stat_function(fun=dnorm,args=list(mean=mean(data$PGr),sd=sd(data$PGr)),col='red',size=1)+mytheme+
  scale_x_continuous(breaks=c(0,1,2,3,6,10,15,20),expand = c(0,0))+scale_y_continuous(expand = c(0,0))+labs(y=" ")
PG1.normal | PG2.normal | PGR.normal



