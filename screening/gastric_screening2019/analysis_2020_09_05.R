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
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
variables<-c("喝茶",    "鲜奶",    "酸奶",    "咖啡","碳酸饮料",        
             "蔬菜",    "水果",    "谷类",   
             "鸡蛋",    "杂粮",    "豆类",     "坚果",   
             "大蒜",    "菌类",    "油炸",   
             "烧烤",    "熏制",    "酱制",    "偏咸",    "腌制",   
             "偏辣",    "偏烫",    "偏酸",    "偏甜", "偏硬")
###数据：去除自身癌和胃部手术切除术
pepsinogen2019<-pepsinogen2019%>%
  filter(ID!=31030159,ID!=31060461,自身癌!='是',残胃!='是')
###PG1的基本分布
make.table(dat=pepsinogen2019,
           strat        = "PG1_range3",
           #strat        = "PG1_range4",
           cat.rmstat   = c("row"),
           cat.varlist  = c('糖尿病','高血压','高血脂','冠心病'),
           cat.ptype    = c("chisq"))
means_PG1<-pepsinogen2019%>%pivot_longer(cols=c('年龄分组2','年龄分组3','BMI_group',variables),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=round(median(PG1),1),Q1=round(quantile(PG1,0.25),1),
                                       Q3=round(quantile(PG1,0.75),1))
print(means_PG1,n=200)
p<-list()
for(i in c('年龄分组2','年龄分组3','BMI_group',variables)){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)

###PG2的基本分布
make.table(dat=pepsinogen2019,
           strat        = "PG2_range",
           cat.rmstat   = c("col"),
           cat.varlist  = c('年龄分组2','年龄分组3','BMI_group',variables),
           cat.ptype    = c("chisq"))
  means_PG2<-pepsinogen2019%>%pivot_longer(cols=c('年龄分组2','年龄分组3','BMI_group',variables),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=round(median(PG2),1),
                                Q1=round(quantile(PG2,0.25),1),Q3=round(quantile(PG2,0.75),1))%>%
                                 mutate(quantile=paste0(median,'(',Q1,'-',Q3,')'))
print(means_PG2,n=200)
p<-list()
for(i in c('年龄分组2','年龄分组3','BMI_group',variables)){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)

##############
with(pepsinogen2019,table(PG_pos7,PG1_range3))
#针对PG1的多因素分析
#model1矫正因素:c('BMI_group','年龄','性别')
#model2矫正因素:c('BMI_group','年龄','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒')
#model3矫正因素:c('BMI_group','年龄','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒','喝茶','咖啡','酸奶','碳酸饮料','果味饮料','油炸','烧烤','偏咸')
##BMI(三分类：正常、超重、肥胖)
#BMI(三分类):>70 vs 50-70
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1',
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))
#BMI(三分类):>70 vs 30-50
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1',
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
#BMI(三分类):>70 vs <30
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1',
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
##BMI(四分类：偏瘦、正常、超重、肥胖)
#BMI(四分类):>70 vs 50-70
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'),family = 'binomial'))
#BMI(四分类):>70 vs 30-50
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'),family = 'binomial'))
#BMI(四分类):>70 vs <30
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'),family = 'binomial'))

##吸烟(三分类)
#吸烟 (三分类):>70 vs 50-70
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))
#吸烟(三分类):>70 vs 30-50
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
#吸烟(三分类):>70 vs <30
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

##吸烟(五分类)
#吸烟 (五分类):>70 vs 50-70
logit(x=c('吸烟3','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))
#吸烟(五分类):>70 vs 30-50
logit(x=c('吸烟3','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
#吸烟(五分类):>70 vs <30
logit(x=c('吸烟3','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#饮食因素
#饮酒
logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#咖啡
logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#鲜奶
logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

##酸奶
logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
##喝茶
logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#碳酸饮料
logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#果味饮料
logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#水果
logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#谷类
logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#鸡蛋
logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#蔬菜
logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#杂粮
logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

#豆类
logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#坚果
logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#大蒜
logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#菌类
logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#油炸
logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

#烧烤
logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#熏制
logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

#腌制
logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

#酱制
logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

#偏咸
logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#每天早餐
logit(x=c('每天早餐','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('每天早餐','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('每天早餐','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#准点吃饭
logit(x=c('准点吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('准点吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('准点吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

#吃饭速度
logit(x=c('吃饭速度','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('吃饭速度','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('吃饭速度','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#外出吃饭
logit(x=c('外出吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('外出吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('外出吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#运动
logit(x=c('运动','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('运动','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('运动','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#睡眠时间
logit(x=c('睡眠时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('睡眠时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('睡眠时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#睡眠质量
logit(x=c('睡眠质量','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('睡眠质量','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('睡眠质量','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))

#夜班
logit(x=c('夜班','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('夜班','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('夜班','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#静态时间
logit(x=c('静态时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('静态时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('静态时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
#重度精神问题重度精神问题
logit(x=c('重度精神问题','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='30-49.9'))

logit(x=c('重度精神问题','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='<30' & PG1_range3!='50-69.9'))
logit(x=c('重度精神问题','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos7',data=subset(pepsinogen2019,PG1_range3!='30-49.9' & PG1_range3!='50-69.9'))
###PG2<<<<<<<<<<<<<<<<<
with(pepsinogen2019,table(PG_pos11,PG2_range))
#针对PG2的多因素分析
#model1矫正因素:c('BMI_group','年龄','性别')
#model2矫正因素:c('BMI_group','年龄','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒')
#model3矫正因素:c('BMI_group','年龄','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒','喝茶','咖啡','酸奶','碳酸饮料','果味饮料','油炸','烧烤','偏咸')
##BMI(三分类：正常、超重、肥胖)
#BMI(三分类):>=<6.5 vs 6.51-9.79
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1',
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))
#BMI(三分类):<6.5 vs 9.8-15.29
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1',
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
#BMI(三分类):<6.5 vs >=15.30
logit(x=c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1',
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
##BMI(四分类：偏瘦、正常、超重、肥胖)
#BMI(四分类):>=15.30 vs 9.8-15.29
forest_model(glm(PG_pos11~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'),family = 'binomial'))
#BMI(四分类):>=15.30 vs >=15.30
forest_model(glm(PG_pos11~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'),family = 'binomial'))
#BMI(四分类):>=15.30 vs 9.8-15.29
forest_model(glm(PG_pos11~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'),family = 'binomial'))
##吸烟(三分类)
#吸烟 (三分类):>=15.30 vs 9.8-15.29
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
'糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))
#吸烟(三分类):>=15.30 vs >=15.30
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
#吸烟(三分类):>70 vs 9.8-15.29
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

##吸烟(五分类)
#吸烟 (五分类):>=15.30 vs 9.8-15.29
logit(x=c('吸烟3','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))
#吸烟(五分类):>=15.30 vs >=15.30
logit(x=c('吸烟3','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='9.8-15.29'))
#吸烟(五分类):>=15.30 vs 9.8-15.29
logit(x=c('吸烟3','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病','饮酒'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#饮食因素
#饮酒
logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#咖啡
logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#鲜奶
logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

##酸奶
logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
##喝茶
logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#碳酸饮料
logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#果味饮料
logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#水果
logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#谷类
logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#鸡蛋
logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#蔬菜
logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#杂粮
logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

#豆类
logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#坚果
logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#大蒜
logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#菌类
logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#油炸
logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

#烧烤
logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#熏制
logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

#腌制
logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

#酱制
logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

#偏咸
logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#每天早餐
logit(x=c('每天早餐','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('每天早餐','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('每天早餐','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#准点吃饭
logit(x=c('准点吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('准点吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('准点吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

#吃饭速度
logit(x=c('吃饭速度','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('吃饭速度','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('吃饭速度','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#外出吃饭
logit(x=c('外出吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('外出吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('外出吃饭','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#运动
logit(x=c('运动','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('运动','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('运动','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#睡眠时间
logit(x=c('睡眠时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('睡眠时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('睡眠时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#睡眠质量
logit(x=c('睡眠质量','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('睡眠质量','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('睡眠质量','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))

#夜班
logit(x=c('夜班','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('夜班','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('夜班','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#静态时间
logit(x=c('静态时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('静态时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('静态时间','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))
#重度精神问题重度精神问题
logit(x=c('重度精神问题','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='9.8-15.29' & PG2_range!='>=15.30'))

logit(x=c('重度精神问题','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='>=15.30'))
logit(x=c('重度精神问题','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",
          
          '糖尿病','高血压','冠心病'),y='PG_pos11',data=subset(pepsinogen2019,PG2_range!='6.51-9.79' & PG2_range!='9.8-15.29'))



########胃萎缩的影响因素分析
#model1矫正因素:c('BMI_group','年龄分组','性别')
#model2矫正因素:c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒')
#model3矫正因素:c('BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒','喝茶','咖啡','酸奶','碳酸饮料','果味饮料','油炸','烧烤','偏咸')

#以PGI<28定义
#BMI(三分类)
logit(x=c('BMI_group','年龄分组3','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒'),y='PG_pos12',data=pepsinogen2019)
#BMI(四分类)
forest_model(glm(PG_pos12~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
forest_model(glm(PG_pos12~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=pepsinogen2019,family='binomial'))

#吸烟
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos12',data=pepsinogen2019)

#饮酒
logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#咖啡
logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#鲜奶
logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#鲜奶
logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#鲜奶
logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#碳酸饮料
logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#果味饮料
logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#水果
logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#谷类
logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#鸡蛋
logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#蔬菜
logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#杂粮
logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#豆类
logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#坚果
logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#大蒜
logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#菌类
logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#油炸
logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#烧烤
logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#熏制
logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#腌制
logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#酱制
logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)
#偏咸
logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos12',data=pepsinogen2019)








#以PGI≤70 & PGR≤3定义
#BMI(三分类)
logit(x=c('BMI_group','年龄分组3','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=pepsinogen2019)
#BMI(四分类)
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=pepsinogen2019,family='binomial'))


#吸烟
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=pepsinogen2019)

#饮酒
logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#咖啡
logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#鲜奶
logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#鲜奶
logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#鲜奶
logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#碳酸饮料
logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#果味饮料
logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#水果
logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#谷类
logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#鸡蛋
logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#蔬菜
logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#杂粮
logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#豆类
logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#坚果
logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#大蒜
logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#菌类
logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#油炸
logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#烧烤
logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#熏制
logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#腌制
logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#酱制
logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)
#偏咸
logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)


##PG<=3

#BMI(三分类)
logit(x=c('BMI_group','年龄分组3','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒'),y='PG_pos5',data=pepsinogen2019)
#BMI(四分类)
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+家庭收入2+血型2+吸烟1+糖尿病+高血压+冠心病+饮酒,data=pepsinogen2019,family='binomial'))


#吸烟
logit(x=c('吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病','饮酒'),y='PG_pos5',data=pepsinogen2019)

#饮酒
logit(x=c('饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#咖啡
logit(x=c('咖啡','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#鲜奶
logit(x=c('鲜奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#鲜奶
logit(x=c('酸奶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#鲜奶
logit(x=c('喝茶','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#碳酸饮料
logit(x=c('碳酸饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#果味饮料
logit(x=c('果味饮料','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#水果
logit(x=c('水果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#谷类
logit(x=c('谷类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#鸡蛋
logit(x=c('鸡蛋','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#蔬菜
logit(x=c('蔬菜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#杂粮
logit(x=c('杂粮','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#豆类
logit(x=c('豆类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#坚果
logit(x=c('坚果','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#大蒜
logit(x=c('大蒜','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#菌类
logit(x=c('菌类','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#油炸
logit(x=c('油炸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#烧烤
logit(x=c('烧烤','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#熏制
logit(x=c('熏制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#腌制
logit(x=c('腌制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#酱制
logit(x=c('酱制','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)
#偏咸
logit(x=c('偏咸','饮酒','吸烟1','BMI_group','年龄分组','性别','就业状况2','家庭收入2',"血型2",'糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)





###年龄
logit(x=c('BMI_group','年龄分组3','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,PG_pos4!='重度萎缩'))
logit(x=c('BMI_group','年龄分组3','性别','就业状况2','家庭收入2',"血型2",'吸烟1','糖尿病','高血压','冠心病','饮酒'),y='PG_pos',data=subset(pepsinogen2019,PG_pos4!='一般萎缩'))
##性别
#
logit(x=c('BMI_group','年龄分组','性别','就业状况2',
          '吸烟1','糖尿病','高血压','冠心病'),y='PG_pos',data=pepsinogen2019)

#model2
logit(x=c('BMI_group','年龄分组','性别','就业状况2',
          '吸烟1','糖尿病','高血压','冠心病','饮酒','咖啡','喝茶','油炸'),y='PG_pos',data=pepsinogen2019)
#model1
logit(x=c('BMI_group','年龄分组','性别','就业状况2','吸烟1','糖尿病','高血压',
          '冠心病','饮酒','咖啡','喝茶','油炸','消化性溃疡','幽门螺杆菌感染史','胃息肉'),y='PG_pos',data=pepsinogen2019)

##PGR≤3
logit(x=c('BMI_group','年龄分组','性别','就业状况2',
          '吸烟1','糖尿病','高血压','冠心病'),y='PG_pos5',data=pepsinogen2019)

#model2
logit(x=c('BMI_group','年龄分组','性别','就业状况2',
          '吸烟1','糖尿病','高血压','冠心病','饮酒','咖啡','喝茶','油炸'),y='PG_pos5',data=pepsinogen2019)

#model2
logit(x=c('BMI_group','年龄分组','性别','就业状况2',
          '吸烟1','糖尿病','高血压','冠心病','饮酒','咖啡','喝茶','油炸','消化性溃疡','胃息肉'),y='PG_pos5',data=pepsinogen2019)

##吸烟3与PG_pos
#model2
logit(x=c('BMI_group','年龄分组','性别','就业状况2',
          '吸烟3','糖尿病','高血压','冠心病','饮酒','咖啡','喝茶','油炸'),y='PG_pos',data=pepsinogen2019)
#model1
logit(x=c('BMI_group','年龄分组','性别','就业状况2',
          '吸烟3','糖尿病','高血压','冠心病','饮酒','咖啡','喝茶','油炸'),y='PG_pos5',data=pepsinogen2019)
##BMI与PG
#连续

#model1 
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
logit(y='PG_pos',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒'),data=pepsinogen2019)
#model2
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒,data=pepsinogen2019,family='binomial'))
#model3
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡'),data=pepsinogen2019)


#model4
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+咖啡+喝茶+油炸,data=pepsinogen2019,family='binomial'))

##PG_pos1 与BMI
#model1 
forest_model(glm(PG_pos1~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
#model2
forest_model(glm(PG_pos1~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos1',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒'),data=pepsinogen2019)

#model3
forest_model(glm(PG_pos1~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos1',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡'),data=pepsinogen2019)


#model4
forest_model(glm(PG_pos1~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+咖啡+喝茶+油炸,data=pepsinogen2019,family='binomial'))

##PG_pos2 与BMI
#model1 
forest_model(glm(PG_pos2~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
#model2
forest_model(glm(PG_pos2~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos2',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒'),data=pepsinogen2019)
#model3
forest_model(glm(PG_pos2~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos2',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡'),data=pepsinogen2019)

#model4
forest_model(glm(PG_pos2~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+咖啡+喝茶+油炸,data=pepsinogen2019,family='binomial'))
##PG_pos2 与BMI
#model1 
forest_model(glm(PG_pos12~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
#model2
forest_model(glm(PG_pos12~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos12',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒'),data=pepsinogen2019)

#model3
forest_model(glm(PG_pos12~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos12',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡'),data=pepsinogen2019)

#model4
forest_model(glm(PG_pos12~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+咖啡+喝茶+油炸,data=pepsinogen2019,family='binomial'))
##PG_pos5 与BMI
#model1 
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
#model2
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos5',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒'),data=pepsinogen2019)

#model3
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡,data=pepsinogen2019,family='binomial'))
#model4
forest_model(glm(PG_pos5~relevel(BMI_group2,ref='正常')+年龄分组+性别+就业状况2+吸烟3+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+咖啡+喝茶+油炸,data=pepsinogen2019,family='binomial'))
logit(y='PG_pos5',x=c('BMI','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                       '消化性溃疡'),data=pepsinogen2019)


###性别
logit(y='PG_pos',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)

logit(y='PG_pos5',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
###亚组分析BMI与PG_pos
make.table(dat=subset(pepsinogen2019,性别=="Male"),
           strat        = "PG_pos",
           #strat        = "PG1_range4",
           cat.rmstat   = c("col"),
           cat.varlist  = c('BMI_group2','年龄分组','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                            '消化性溃疡','油炸','咖啡','喝茶'),
           cat.ptype    = c("chisq"),
           output       = "html")
#性别
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+就业状况2+吸烟2+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data=subset(pepsinogen2019,性别=="Male"),family='binomial'))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+就业状况2+吸烟2+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,subset(pepsinogen2019,性别=="Female"),family='binomial'))
summary(glm(PG_pos~as.numeric(BMI_group3)+年龄分组+就业状况2+吸烟2+饮酒+
              糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data = subset(pepsinogen2019,性别=="Male"),family = 'binomial'))
anova(glm(PG_pos~BMI_group2*性别+年龄分组+就业状况2+吸烟2+饮酒+
              糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data = pepsinogen2019,family = 'binomial'),test='Chisq')
summary(glm(PG_pos~as.numeric(BMI_group3)+年龄分组+就业状况2+吸烟2+饮酒+
              糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data = subset(pepsinogen2019,性别=="Female"),family = 'binomial'))
logit(y='PG_pos',x=c('BMI_group3','年龄分组',"吸烟1",'就业状况2','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=="Male"))
logit(y='PG_pos',x=c('BMI_group3','年龄分组',"吸烟1",'就业状况2','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=="Female"))

#年龄
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+就业状况2+吸烟1+饮酒+
                   高血压+高血脂+冠心病+消化性溃疡+油炸+咖啡,data=subset(pepsinogen2019,年龄分组5=="<60"),family='binomial'))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+就业状况2+吸烟1+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,subset(pepsinogen2019,年龄分组5==">=60"),family='binomial'))
anova(glm(PG_pos~BMI_group2*年龄分组5+性别+就业状况2+吸烟1+饮酒+
            糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data = pepsinogen2019,family = 'binomial'),test='Chisq')
logit(y='PG_pos',x=c('BMI_group3','性别',"吸烟1",'就业状况2','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,年龄分组5=="<60"))
logit(y='PG_pos',x=c('BMI_group3','性别',"吸烟1",'就业状况2','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,年龄分组5==">=60"))


#吸烟
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+就业状况2+性别+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data=subset(pepsinogen2019,吸烟2=="从不吸烟"),family='binomial'))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+年龄分组+就业状况2+性别+饮酒+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,subset(pepsinogen2019,吸烟2=="目前或过去吸烟"),family='binomial'))
anova(glm(PG_pos~BMI_group2*吸烟2+性别+就业状况2+年龄分组+饮酒+
            糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data = pepsinogen2019,family = 'binomial'),test='Chisq')
logit(y='PG_pos',x=c('BMI_group3','年龄分组','性别','就业状况2','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,吸烟2=="从不吸烟"))

logit(y='PG_pos',x=c('BMI_group3','性别',"年龄分组",'就业状况2','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,吸烟2=="目前或过去吸烟"))




#饮酒
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')*+年龄分组+就业状况2+性别+吸烟3+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,data=subset(pepsinogen2019,饮酒=="否"),family='binomial'))
forest_model(glm(PG_pos~BMI_group+年龄分组+就业状况2+性别+吸烟3+
                   糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸,subset(pepsinogen2019,饮酒=="是"),family='binomial'))
logit(y='PG_pos',x=c('BMI_group3','性别',"吸烟1",'就业状况2','年龄分组','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,饮酒=="是"))

#Hp_pos感染分层

logit(y='PG_pos',x=c('BMI_group3','性别',"吸烟1",'饮酒','就业状况2','年龄分组','糖尿病','高血压','高血脂','冠心病',
                     '油炸','咖啡','喝茶'),data=subset(pepsinogen2020,Hp_pos=="阴性"))
logit(y='PG_pos',x=c('BMI_group3','性别',"吸烟1",'饮酒','就业状况2','年龄分组','糖尿病','高血压','高血脂','冠心病',
                     '油炸','咖啡','喝茶'),data=subset(pepsinogen2020,Hp_pos=="阳性"))


##慢性病史
##PGI≤70 and PGI/II≤3
logit(y='PG_pos',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
##PGI/II≤3
logit(y='PG_pos5',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)



##########分层下吸烟与PG_pos的关系
#性别分层
logit(y='PG_pos',x=c('BMI_group','年龄分组','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=="Male"))
logit(y='PG_pos',x=c('BMI_group','年龄分组','就业状况2','吸烟2','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=="Female"))
#年龄分组
#幽门螺杆菌感染分层
#饮酒分层


##喝茶、鲜奶、酸奶、咖啡、碳酸饮料、果味饮料
#PGI≤70 & PGR≤3
logit(y='PG_pos',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('鲜奶','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('酸奶','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('碳酸饮料','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)

logit(y='PG_pos',x=c('果味饮料','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)

##蔬菜、水果、谷类、豆类、鸡蛋、杂粮、豆类、坚果
logit(y='PG_pos',x=c('蔬菜','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('水果','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('谷类','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('鸡蛋','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('杂粮','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('豆类','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('坚果','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)

#大蒜、菌类、油炸、烧烤、熏制、腌制、酱制、偏咸
logit(y='PG_pos',x=c('大蒜','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('菌类','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('烧烤','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('熏制','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('腌制','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('酱制','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('偏咸','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)

##偏咸、偏辣、偏烫、偏酸、偏甜、偏硬

logit(y='PG_pos',x=c('偏辣','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('偏烫','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('偏酸','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('偏甜','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos',x=c('偏硬','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)


##PGR≤3
logit(y='PG_pos5',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('鲜奶','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('酸奶','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('碳酸饮料','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)

logit(y='PG_pos5',x=c('果味饮料','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
##蔬菜、水果、谷类、豆类、鸡蛋、杂粮、豆类、坚果
logit(y='PG_pos5',x=c('蔬菜','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('水果','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('谷类','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('鸡蛋','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('杂粮','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('豆类','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('坚果','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)

#大蒜、菌类、油炸、烧烤、熏制、腌制、酱制、偏咸
logit(y='PG_pos5',x=c('大蒜','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('菌类','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('烧烤','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('熏制','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('腌制','BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡','油炸','咖啡','喝茶'),data=pepsinogen2019)


###分层下吸烟与PG_pos的关系
#年龄分组
logit(y='PG_pos5',x=c('BMI_group','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,年龄分组5=='<60'))
logit(y='PG_pos5',x=c('BMI_group','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,年龄分组5=='>=60'))

##性别
logit(y='PG_pos5',x=c('BMI_group','年龄分组','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=='Male'))
logit(y='PG_pos5',x=c('BMI_group','年龄分组','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=='Female'))

#Hp
logit(y='PG_pos5',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2020,Hp_pos=='阴性'))
logit(y='PG_pos5',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2020,Hp_pos=='阳性'))

#BMI与PG_pos
#MOdel1
logit(y='PG_pos',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡'),data=pepsinogen2019)
#
logit(y='PG_pos3',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                     '消化性溃疡'),data=pepsinogen2019)
logit(y='PG_pos1',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡'),data=pepsinogen2019)
logit(y='PG_pos2',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡'),data=pepsinogen2019)
logit(y='PG_pos5',x=c('BMI_group','年龄分组','性别','就业状况2','吸烟3','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡'),data=pepsinogen2019)




##油炸与PG
make.table(dat=pepsinogen2019,
           strat        = "PG_pos",
           #strat        = "PG1_range4",
           cat.rmstat   = c("col"),
           cat.varlist  = c('年龄分组5','性别'),
           cat.ptype    = c("chisq"),
           output       = "html")
#年龄分组
logit(y='PG_pos',x=c('BMI_group','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,年龄分组5=='<60'))
logit(y='PG_pos',x=c('BMI_group','性别','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,年龄分组5=='>=60'))
#性别分层
logit(y='PG_pos',x=c('BMI_group','年龄分组','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=='Male'))
logit(y='PG_pos',x=c('BMI_group','年龄分组','就业状况2','吸烟1','饮酒','糖尿病','高血压','高血脂','冠心病',
                      '消化性溃疡','油炸','咖啡','喝茶'),data=subset(pepsinogen2019,性别=='Female'))
##Hp
logit(y='PG_pos',x=c('BMI_group3','性别',"吸烟1",'饮酒','就业状况2','年龄分组','糖尿病','高血压','高血脂','冠心病',
                     '油炸','咖啡','喝茶'),data=subset(pepsinogen2020,Hp_pos=="阴性"))
logit(y='PG_pos',x=c('BMI_group3','性别',"吸烟1",'饮酒','就业状况2','年龄分组','糖尿病','高血压','高血脂','冠心病',
                     '油炸','咖啡','喝茶'),data=subset(pepsinogen2020,Hp_pos=="阳性"))

##BMI and RCS
dd <- datadist(pepsinogen2019) 
options(datadist='dd')
fit<- lrm(PG_pos12 ~ rcs(BMI,4)+年龄分组+就业状况2+性别+吸烟1+
            糖尿病+高血压+高血脂+冠心病+消化性溃疡+油炸+喝茶,data=pepsinogen2019) 
RR<-Predict(fit,BMI,fun=exp,ref.zero = TRUE) ####预测HR值
p1<-ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+mytheme+ 
  labs( x="BMI", y="OR (95%CI)") 
p2<-ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+mytheme+ 
  labs( x="BMI", y="OR (95%CI)") 
p3<-ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+mytheme+ 
  labs( x="BMI", y="OR (95%CI)") 
p4<-ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+mytheme+ 
  labs( x="BMI", y="OR (95%CI)") 
ggplot()+geom_line(data=RR, aes(BMI,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=RR, aes(BMI,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+mytheme+ 
  labs( x="BMI", y="OR (95%CI)") 

anova(fit)
(p1 | p2) / (p3 | p4)


###BMI与萎缩性胃炎
#1、PGI≤70 & PGI/II≤3
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄分组+吸烟2+饮酒,data=pepsinogen2019,family='binomial'))
forest_model(glm(PG_pos~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
#2、PGI≤50 & PGI/II≤3
forest_model(glm(PG_pos3~relevel(BMI_group2,ref='正常'),data=pepsinogen2019,family='binomial'))
forest_model(glm(PG_pos3~relevel(BMI_group2,ref='正常')+性别+年龄分组+吸烟2+饮酒,data=pepsinogen2019,family='binomial'))
forest_model(glm(PG_pos3~relevel(BMI_group2,ref='正常')+性别+年龄分组+吸烟2+饮酒+ 糖尿病+高血压+高血脂+冠心病+消化性溃疡+喝茶,data=pepsinogen2019,family='binomial'))



###
variables_ppt<-c('性别','年龄分组','教育年数','就业状况2','家庭收入2','血型1','饮酒','吸烟1','BMI_group2',
                 '碳酸饮料','果味饮料','大蒜','油炸','咖啡','喝茶')
#PG1
means_PG1<-pepsinogen2019%>%pivot_longer(cols=c(variables_ppt),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=round(median(PG1),1),Q1=round(quantile(PG1,0.25),1),
                                       Q3=round(quantile(PG1,0.75),1))
print(means_PG1,n=200)
p<-list()
for(i in c(variables_ppt)){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)
#PG2
means_PG2<-pepsinogen2019%>%pivot_longer(cols=c(variables_ppt),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=round(median(PG2),1),
                                       Q1=round(quantile(PG2,0.25),1),Q3=round(quantile(PG2,0.75),1))%>%
  mutate(quantile=paste0(median,'(',Q1,'-',Q3,')'))
print(means_PG2,n=200)
p<-list()
for(i in c(variables_ppt)){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)
#PGR
means_PGR<-pepsinogen2019%>%pivot_longer(cols=c(variables_ppt),names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=round(median(PGR),1),
                                       Q1=round(quantile(PGR,0.25),1),Q3=round(quantile(PGR,0.75),1))%>%
  mutate(quantile=paste0(median,'(',Q1,'-',Q3,')'))
print(means_PGR,n=200)
p<-list()
for(i in c(variables_ppt)){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)


##幽门螺杆菌数据
pepsinogen2020%>%group_by(Hp_pos)%>%summarise(n=n(),median=median(PG1),Q1=round(quantile(PG1,0.25),1),Q3=round(quantile(PG1,0.75),1))
wilcox.test(PG1~Hp_pos,data=pepsinogen2020)
pepsinogen2020%>%group_by(Hp_pos)%>%summarise(n=n(),median=median(PG2),Q1=round(quantile(PG2,0.25),1),Q3=round(quantile(PG2,0.75),1))
wilcox.test(PG2~Hp_pos,data=pepsinogen2020)
pepsinogen2020%>%group_by(Hp_pos)%>%summarise(n=n(),median=median(PGR),Q1=round(quantile(PGR,0.25),1),Q3=round(quantile(PGR,0.75),1))
wilcox.test(PGR~Hp_pos,data=pepsinogen2020)

##
variables<-c('年龄分组','性别','吸烟1','饮酒','BMI_group','家庭收入2','血型1','就业状况2','糖尿病','高血压','冠心病',
             '高血脂','喝茶','鲜奶','酸奶','咖啡','碳酸饮料','果味饮料',
              '蔬菜','水果','谷类','鸡蛋','杂粮','豆类','坚果','大蒜',
              '菌类','油炸','烧烤','熏制','酱制','偏咸','腌制','偏辣','偏烫',
             '偏酸','偏甜','偏硬')
p<-list()
for(i in c(variables)){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)
#PG2
p<-list()
for(i in c(variables)){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)
#PGR
p<-list()
for(i in c(variables)){
  formula_uni<-as.formula(paste('PGR','~', i))
  if(length(table(pepsinogen2019[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen2019)$p.value,4)
  }
}
do.call(rbind,p)

##
summary(lm(PG1~吸烟,data=pepsinogen2019))




