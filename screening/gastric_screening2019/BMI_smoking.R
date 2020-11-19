#PG-1/PG2 by age group (根据数据改成图,  J-shaped or U-shaped curve)
#PG1
make.table(dat= subset(pepsinogen,年龄分组=="<50"),
           strat        = "PG_pos7",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组=="50-60"),
           strat        = "PG_pos7",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组==">60"),
           strat        = "PG_pos7",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")


#40-49
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
#50-60
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))

#>60
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组==">60")))
forest_model(glm(PG_pos7~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组==">60")))

###画图
BMI<-factor(c(1,2,3,4),levels=c(1,2,3,4),labels=c('<18.5','18.5-23.9','24-27.9','>=28'))
age_group<-factor(c(1,1,1,1,2,2,2,2,3,3,3,3),levels=c(1,1,1,1,2,2,2,2,3,3,3,3),
                  labels=c('<50','<50','<50','<50','50-60','50-60','50-60','50-60','>60','>60','>60','>60'))
aOR<-c(0.82,1,1.42,1.81,0.64,1,1.10,1.28,0.96,1,1.07,1.27)
LCL<-c(0.30,1,0.95,1.07,0.30,1,0.88,0.96,0.56,1,0.90,1.01)
UCL<-c(2.68,1,2.13,3.15,1.46,1,1.37,1.72,1.68,1,1.26,1.60)
OR.plot<-data.frame(age=age_group,BMI=rep(BMI,3),aOR,LCL,UCL)
#arrange(OR.plot,age)
OR.plot%>%ggplot()+geom_point(aes(x=BMI,y=aOR))+geom_point(aes(x=BMI,y=LCL),color='blue')+geom_point(aes(x=BMI,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=BMI,y=aOR,ymin=LCL,ymax=UCL))+
  facet_wrap(.~age,scales='free_y')+mytheme2


########
##PG2
#总
make.table(dat= pepsinogen,
           strat        = "PG2_prange3",
           cat.varlist  = c("BMI_group","BMI_group2"),
           cat.ptype    = c("chisq"),
           output       = "html")
logit(y='PG_pos8',x=c('BMI_group2','年龄分组','性别'),data=pepsinogen)
logit(y='PG_pos8',x=c('BMI_group2','年龄分组','性别','就业状况','饮酒','糖尿病','油炸','BMI_group'),data=pepsinogen)


#年龄分层
make.table(dat= subset(pepsinogen,年龄分组=="<50"),
           strat        = "PG2_range4",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组=="50-60"),
           strat        = "PG2_range4",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")
make.table(dat= subset(pepsinogen,年龄分组==">60"),
           strat        = "PG2_range4",
           cat.varlist  = c("BMI_group2","BMI_group"),
           cat.ptype    = c("chisq"),
           output       = "html")


#OR
#<50
forest_model(glm(PG2_range4~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))
forest_model(glm(PG2_range4~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组=="<50")))

#50-60
forest_model(glm(PG2_range4~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))
forest_model(glm(PG2_range4~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组=="50-60")))

#>60
forest_model(glm(PG2_range4~relevel(BMI_group2,ref='正常')+年龄,family='binomial',data=subset(pepsinogen,年龄分组==">60")))
forest_model(glm(PG2_range4~relevel(BMI_group2,ref='正常')+年龄+性别+吸烟1+油炸+糖尿病+饮酒+就业状况,family='binomial',data=subset(pepsinogen,年龄分组==">60")))
##画图
BMI<-factor(c(1,2,3,4),levels=c(1,2,3,4),labels=c('<18.5','18.5-23.9','24-27.9','>=28'))
age_group<-factor(c(1,1,1,1,2,2,2,2,3,3,3,3),levels=c(1,1,1,1,2,2,2,2,3,3,3,3),
                  labels=c('<50','<50','<50','<50','50-60','50-60','50-60','50-60','>60','>60','>60','>60'))
aOR<-c(1.37,1,0.98,0.72,1.21,1,0.97,0.92,1.18,1,1.07,0.76)
LCL<-c(0.51,1,0.69,0.43,0.55,1,0.79,0.71,0.69,1,0.92,0.61)
UCL<-c(3.34,1,1.41,1.17,2.55,1,1.20,1.21,1.98,1,1.26,0.94)
OR2.plot<-data.frame(age=age_group,BMI=rep(BMI,3),aOR,LCL,UCL)
#arrange(OR.plot,age)
OR2.plot%>%ggplot()+geom_point(aes(x=BMI,y=aOR))+geom_point(aes(x=BMI,y=LCL),color='blue')+geom_point(aes(x=BMI,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=BMI,y=aOR,ymin=LCL,ymax=UCL))+
  facet_wrap(.~age,scales='free_y')+mytheme2


###smoking
#PG1
baonian<-factor(c(1,2,3,4,5),levels=c(1,2,3,4,5),labels=c('<10','10-','20-','30-','>40'))
aOR<-c(1,0.88,0.89,0.9,0.83)
LCL<-c(1,0.67,0.66,0.65,0.6)
UCL<-c(1,1.15,1.2,1.25,1.15)
OR2.plot<-data.frame(baonian,aOR,LCL,UCL)
#arrange(OR.plot,age)
OR2.plot%>%ggplot()+geom_point(aes(x=baonian,y=aOR))+geom_point(aes(x=baonian,y=LCL),color='blue')+geom_point(aes(x=baonian,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=baonian,y=aOR,ymin=LCL,ymax=UCL))+
  mytheme2+geom_line(aes(x=baonian,y=aOR,group=1),color='blue')
#PG2
baonian<-factor(c(1,2,3,4,5),levels=c(1,2,3,4,5),labels=c('<10','10-','20-','30-','>40'))
aOR<-c(1,0.97,1.11,0.97,1.45)
LCL<-c(1,0.74,0.83,0.71,1.05)
UCL<-c(1,1.26,1.49,1.34,2.01)
OR2.plot<-data.frame(baonian,aOR,LCL,UCL)
#arrange(OR.plot,age)
OR2.plot%>%ggplot()+geom_point(aes(x=baonian,y=aOR))+geom_point(aes(x=baonian,y=LCL),color='blue')+geom_point(aes(x=baonian,y=UCL),color='blue')+
  geom_hline(yintercept = 1,color='grey')+geom_pointrange(aes(x=baonian,y=aOR,ymin=LCL,ymax=UCL))+
  mytheme2+geom_line(aes(x=baonian,y=aOR,group=1),color='blue')


###2020-8-20



















