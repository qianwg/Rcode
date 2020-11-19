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
###基本分布情况
table1(~年龄+性别+吸烟1+家庭收入2+PG1+PG2+PGR+PG_pos+咖啡+饮酒+PG1_range4+PG_pos4+PG_pos5+PG_pos9 | BMI_group4,data=pepsinogen,render.categorical=my.render.cat)
##对于总体
######low:PGI≤70 & PGII≤3
#model1:
model1<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤50 & PGII≤3
#model1:
model1<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤30 & PGII≤2
model1<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤70
#model1:
model1<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI<30
#model1:
model1<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))

######low:PGR≤3
#model1:
model1<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))


######中-青年人

######low:PGI≤70 & PGII≤3
#model1:
model1<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤50 & PGII≤3
#model1:
model1<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤30 & PGII≤2
model1<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤70
#model1:
model1<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI<30
#model1:
model1<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))

######low:PGR≤3
#model1:
model1<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))


#######老年人

######low:PGI≤70 & PGII≤3
#model1:
model1<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤50 & PGII≤3
#model1:
model1<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤30 & PGII≤2
model1<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤70
#model1:
model1<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI<30
#model1:
model1<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))

######low:PGR≤3
#model1:
model1<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))




































