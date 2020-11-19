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
#########以PG定义的胃萎缩
#中青年者
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
#人口学特征
model1<-glm(PG_pos~性别+教育+家庭收入2+吸烟2+饮酒+BMI_group,data=subset(pepsinogen,年龄<60),family='binomial')
summary(model1)
#饮食相关因素
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('水果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
#饮食习惯
logit(x=c('熏制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
##饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60))

###将胃萎缩分为一般萎缩和重度萎缩
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

#饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄<60 & PG_pos4!='一般萎缩'))


#####老年人

#饮食相关因素
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('水果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
#饮食习惯
logit(x=c('熏制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
##饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60))

###将胃萎缩分为一般萎缩和重度萎缩
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

#饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen,年龄>=60 & PG_pos4!='一般萎缩'))



##向后逐步回归
step(model1,direction = 'backward')
