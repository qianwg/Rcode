##PG定义的胃萎缩与年龄的关系
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
library(glmnet)
source('~/Rcode/screening/gastric_screening2019/sensitivity_data.R')
dd <- datadist(pepsinogen) 
options(datadist='dd')
##整体人群分布
make.table(dat= pepsinogen,
           strat        = "PG1_range5",
           cat.varlist  = c('年龄分组','年龄分组5','年龄分组7'),
           cat.ptype    = c("chisq"),
           output       = "html")

##基线在年龄分组5中的分布
make.table(dat= pepsinogen,
           strat        = "年龄分组5",
           cat.rmstat   = list(c("row")),
           cat.varlist  = c('PG_pos4',"性别",'PG_pos9','PG_pos11'),
           cat.ptype    = c("chisq"),
           output       = "html")
#PG在年龄分组中的分布
make.table(dat= pepsinogen,
           strat        = "年龄分组",
           cat.varlist  = c('PG_pos4','PG_pos9','PG_pos11'),
           cat.ptype    = c("chisq"),
           output       = "html")


###年老者定义为>=60岁，重度萎缩定义为PGI<=30 & PGII<=3>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##单因素分析(年龄分组5,PG_pos4)
make.table(dat= subset(pepsinogen,年龄分组5=='<60'),
           strat        = "PG_pos4",
           cat.varlist  = c("性别","年龄分组3", '年龄分组2','BMI_group','教育年数','家庭收入2','婚姻',
                            '就业状况2','胃癌家族史','幽门螺杆菌感染史','消化性溃疡','糖尿病','高血压','高血脂','冠心病','中风',
                            '吸烟2',variables4),
           cat.ptype    = c("chisq"),
           output       = "html")

make.table(dat= subset(pepsinogen,年龄分组5=='>=60'),
           strat        = "PG_pos4",
           cat.varlist  = c("性别","年龄分组3", '年龄分组2','BMI_group','教育年数','家庭收入2','婚姻',
                            '就业状况2','胃癌家族史','幽门螺杆菌感染史','消化性溃疡','糖尿病','高血压','高血脂','冠心病','中风',
                            '吸烟2',variables4),
           cat.ptype    = c("chisq"),
           output       = "html")


##多因素矫正
#1] "饮酒"     "喝茶"     "鲜奶"     "酸奶"     "咖啡"     "碳酸饮料" "果味饮料" "茶味饮料"
#[9] "蔬菜"     "水果"     "谷类"     "肉类"     "鸡蛋"     "水产品"   "薯类"     "杂粮"    
#[17] "豆类"     "坚果"     "大蒜"     "菌类"     "油炸"     "烧烤"     "熏制"     "腌制"    
#[25] "酱制"     "晒制"     "偏咸"     "偏辣"     "偏烫"     "偏酸"     "偏甜"     "偏硬" 


#年龄<60岁
model2<-vglm(PG_pos4~就业状况2+性别+年龄分组3+BMI_group+家庭收入2+吸烟2+饮酒+咖啡+喝茶+腌制+偏咸,
             data=subset(pepsinogen,年龄分组5=="<60"),family=multinomial(refLevel = 1))
round(cbind(as.data.frame(exp(coefvlm(model2))),exp(confintvglm(model2))),2)
summary(model2)
#年龄≥60
model2<-vglm(PG_pos4~中风+就业状况2+糖尿病+性别+年龄分组3+BMI_group+吸烟2+饮酒+咖啡+喝茶,
             data=subset(pepsinogen,年龄分组5==">=60"),family=multinomial(refLevel = 1))
round(cbind(as.data.frame(exp(coefvlm(model2))),exp(confintvglm(model2))),2)
summary(model2)


##与其他肿瘤标志物的关系
make.table(dat= subset(pepsinogen,年龄分组5=='<60'),
           strat        = "PG_pos4",
           cat.varlist  = c('CEA_range','CEA_range2','CA199_range2'),
           cat.ptype    = c("chisq"),
           output       = "html")

make.table(dat= subset(pepsinogen,年龄分组5=='>=60'),
           strat        = "PG_pos4",
           cat.varlist  = c('CEA_range','CEA_range2','CA199_range2'),
           cat.ptype    = c("chisq"),
           output       = "html")
#年龄<60岁
  model2<-vglm(PG_pos4~CEA_range2+就业状况2+性别+年龄分组3+吸烟2+饮酒+咖啡+喝茶+腌制+偏咸,
             data=subset(pepsinogen,年龄分组5=="<60"),family=multinomial(refLevel = 1))
round(cbind(as.data.frame(exp(coefvlm(model2))),exp(confintvglm(model2))),2)
summary(model2)
#年龄≥60
model2<-vglm(PG_pos4~CEA_range2+就业状况2+糖尿病+性别+年龄分组3+BMI_group+吸烟2+饮酒+咖啡+喝茶,
             data=subset(pepsinogen,年龄分组5==">=60"),family=multinomial(refLevel = 1))
round(cbind(as.data.frame(exp(coefvlm(model2))),exp(confintvglm(model2))),2)
summary(model2)


####2020-8-26
#不同程度胃萎缩
make.table(dat= pepsinogen,
           strat        = "PG_pos4",
           cat.varlist  = c("性别","年龄分组5", '年龄分组2','BMI_group','教育年数','家庭收入2','婚姻',
                            '就业状况2','胃癌家族史','幽门螺杆菌感染史','消化性溃疡','糖尿病','高血压','高血脂','冠心病','中风',
                            '吸烟1',variables4),
           cat.rmstat   = list(c("col")),
           cat.ptype    = c("chisq"),
           output       = "html")
##
logit(y='PG_pos4',x=c('中风',"就业状况2","性别","年龄分组5","吸烟1","饮酒","咖啡","喝茶",'糖尿病',
                      "腌制","偏咸","家庭收入2","碳酸饮料","油炸"),data=subset(pepsinogen,PG_pos4!='重度萎缩'))

logit(y='PG_pos4',x=c('中风',"就业状况2","性别","年龄分组5","吸烟1",'BMI_group','糖尿病'),data=subset(pepsinogen,PG_pos4!='轻度萎缩'))
logit(y='PG_pos4',x=c('冠心病','家庭收入2','性别','BMI_group',"就业状况2","年龄分组5","吸烟1"),data=subset(pepsinogen,PG_pos4!='正常'))


##不同年龄段，不同胃萎缩的分布
make.table(dat= subset(pepsinogen,PG_pos10!="一般萎缩"),
           strat        = "PG_pos10",
           cat.varlist  = c("性别","年龄分组5", '年龄分组2','BMI_group','教育年数','家庭收入2','婚姻',
                            '就业状况2','胃癌家族史','幽门螺杆菌感染史','消化性溃疡','糖尿病','高血压','高血脂','冠心病','中风',
                            '吸烟1',variables4),
           cat.rmstat   = list(c("col")),
           cat.ptype    = c("chisq"),
           output       = "html")
logit(y='PG_pos',x=c('薯类','糖尿病','油炸','BMI_group','就业状况2','性别','家庭收入2','吸烟1','饮酒'),data=subset(pepsinogen,PG_pos10!='年老者重度萎缩' & PG_pos10!='一般萎缩'))

logit(y='PG_pos',x=c('冠心病','油炸','BMI_group','就业状况2','性别','家庭收入2','吸烟1','饮酒'),data=subset(pepsinogen,PG_pos10!='年轻者重度萎缩' & PG_pos10!='一般萎缩'))
logit(y='PG_pos10',x=c('冠心病','油炸','BMI_group','就业状况2','性别','家庭收入2','吸烟1','饮酒'),data=subset(pepsinogen,PG_pos10!='正常' & PG_pos10!='一般萎缩'))


###验证
pepsinogen2<-pepsinogen%>%filter(PG_pos10!='正常' & PG_pos10!='一般萎缩')%>%mutate(PG_pos11=ifelse(PG_pos10=="年轻者重度萎缩",0,1))
logit(y='PG_pos11',x=c('冠心病','油炸','BMI_group','就业状况2','性别','家庭收入2','吸烟1','饮酒'),data=pepsinogen2)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#年龄以60岁划分，<60为中青年，≥60岁为老年人

#胃萎缩以PGI≤70 & PGI/II≤3定义
#中青年人-总体模型
model1<-glm(PG_pos~胃癌家族史+性别+年龄+婚姻+       
              就业状况2+家庭收入2+教育+      
              血型2+运动+BMI_group2+吸烟1+ 
              被动吸烟1+每天早餐+准点吃饭+睡眠时间+睡眠质量+夜班+重大精神创伤+       
              吃饭速度+外出吃饭+静态时间+饮酒+   
              喝茶+鲜奶+酸奶+咖啡+碳酸饮料+     
              果味饮料+茶味饮料+蔬菜+水果+谷类+ 
              鸡蛋+杂粮+豆类+肉类+坚果+ 
              水产品+薯类+大蒜+菌类+油炸+   
              烧烤+熏制+酱制+偏咸+腌制+  
              晒制+偏辣+偏酸+偏烫+偏甜+偏硬+胃溃疡+胃息肉+脂肪肝+ 
              萎缩性胃炎+胃肠上皮化生+  
              消化性溃疡+糖尿病+高血压+高血脂+
              冠心病,data=subset(pepsinogen,年龄<60),family='binomial')

summary(model1)
vif(model1)
##向后逐步回归
step(model1,direction = 'backward')
#重新构建模型
model1.1<-glm(PG_pos ~ 性别 + 年龄 + 家庭收入2 + 吸烟1 + 
                咖啡 + 坚果 + 大蒜 + 菌类 + 油炸 + 烧烤 + 腌制 + 偏酸+
                晒制 + 胃息肉, family = "binomial", data = subset(pepsinogen, 
                                                             年龄 < 60))
summary(model1.1)
logit(x=c('性别' , '年龄', '家庭收入2' , '吸烟1 ',
              '咖啡','坚果' , '大蒜' , '菌类' ,'油炸','烧烤', '腌制','偏酸',
              '晒制' ,'胃息肉'),y='PG_pos',data=subset(pepsinogen,年龄<60))
#LASSO回归
x<- model.matrix(PG_pos~胃癌家族史+性别+年龄+婚姻+       
                   就业状况2+家庭收入2+教育+      
                   血型2+运动+BMI_group2+吸烟1+ 
                   被动吸烟1+每天早餐+准点吃饭+睡眠时间+睡眠质量+夜班+重大精神创伤+       
                   吃饭速度+外出吃饭+静态时间+饮酒+   
                   喝茶+鲜奶+酸奶+咖啡+碳酸饮料+     
                   果味饮料+茶味饮料+蔬菜+水果+谷类+ 
                   鸡蛋+杂粮+豆类+肉类+坚果+ 
                   水产品+薯类+大蒜+菌类+油炸+   
                   烧烤+熏制+酱制+偏咸+腌制+  
                   晒制+偏辣+偏酸+偏烫+偏甜+偏硬+胃溃疡+胃息肉+脂肪肝+ 
                   萎缩性胃炎+胃肠上皮化生+  
                   消化性溃疡+糖尿病+高血压+高血脂+
                   冠心病,data=subset(pepsinogen, 年龄<60), contrasts.arg = lapply(pepsinogen[ ,sapply(pepsinogen, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y<-pepsinogen[which(pepsinogen$年龄<60), c('PG_pos')]
#lasso regression
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
print(lasso)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
best_lam<-cv_output$lambda.min#选择lambda值
coef(cv_output,s='lambda.min')#系数

##老年人-总体模型
model2<-glm(PG_pos~胃癌家族史+性别+年龄+婚姻+       
              就业状况2+家庭收入2+教育+      
              血型2+运动+BMI_group2+吸烟1+ 
              被动吸烟1+每天早餐+准点吃饭+睡眠时间+睡眠质量+夜班+重大精神创伤+       
              吃饭速度+外出吃饭+静态时间+饮酒+   
              喝茶+鲜奶+酸奶+咖啡+碳酸饮料+     
              果味饮料+茶味饮料+蔬菜+水果+谷类+ 
              鸡蛋+杂粮+豆类+肉类+坚果+ 
              水产品+薯类+大蒜+菌类+油炸+   
              烧烤+熏制+酱制+偏咸+腌制+  
              晒制+偏辣+偏酸+偏烫+偏甜+偏硬+胃溃疡+胃息肉+脂肪肝+ 
              萎缩性胃炎+胃肠上皮化生+  
              消化性溃疡+糖尿病+高血压+高血脂+
              冠心病,data=subset(pepsinogen,年龄>=60),family='binomial')

summary(model2)
vif(model2)
##向后逐步回归
step(model2,direction = 'backward')
step(model2)
#LASSO回归
x<- model.matrix(PG_pos~胃癌家族史+性别+年龄+婚姻+       
                   就业状况2+家庭收入2+教育+      
                   血型2+运动+BMI_group2+吸烟1+ 
                   被动吸烟1+每天早餐+准点吃饭+睡眠时间+睡眠质量+夜班+重大精神创伤+       
                   吃饭速度+外出吃饭+静态时间+饮酒+   
                   喝茶+鲜奶+酸奶+咖啡+碳酸饮料+     
                   果味饮料+茶味饮料+蔬菜+水果+谷类+ 
                   鸡蛋+杂粮+豆类+肉类+坚果+ 
                   水产品+薯类+大蒜+菌类+油炸+   
                   烧烤+熏制+酱制+偏咸+腌制+  
                   晒制+偏辣+偏酸+偏烫+偏甜+偏硬+胃溃疡+胃息肉+脂肪肝+ 
                   萎缩性胃炎+胃肠上皮化生+  
                   消化性溃疡+糖尿病+高血压+高血脂+
                   冠心病,data=subset(pepsinogen, 年龄 >= 60), contrasts.arg = lapply(pepsinogen[ ,sapply(pepsinogen, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y<-pepsinogen[which(pepsinogen$年龄>=60), c('PG_pos')]
#lasso regression
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
print(lasso)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
best_lam<-cv_output$lambda.min#选择lambda值
coef(cv_output,s='lambda.min')#系数
#重新构建模型
#性别、被动吸烟、糖尿病
model2.1<-glm(PG_pos ~ 性别 + 家庭收入2 + 被动吸烟1 + 
                睡眠时间 + 夜班 + 喝茶 + 油炸 + 烧烤 + 偏烫 + 
                偏硬 + 消化性溃疡 + 糖尿病, family = "binomial", 
              data = subset(pepsinogen, 年龄 >= 60))
summary(model2.1)
#有意义的：被动吸烟、油炸、糖尿病
model2.2<-glm(PG_pos~性别+吸烟1+被动吸烟1+喝茶+血型2+
                油炸+消化性溃疡+糖尿病,data=subset(pepsinogen, 年龄 >= 60),family='binomial')
summary(model2.2)
####<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##年龄<=60 | 年龄>60
##PGI--30,70 PGR--2、3







