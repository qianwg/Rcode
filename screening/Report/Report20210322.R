rm(list=ls())
###针对20210324日的课题汇报进行筛查PG及医院数据(胃镜及PG结果)
##1、筛查数据
library(rio)
library(tidyverse)
library(ggpubr)
library(blandr)
library(htmlTable)
library(patchwork)
library(compareGroups)
library(MatchIt)
library(survival)
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(family="serif",size=14,face="bold"),
               axis.text=element_text(family="serif",size=14,face="bold"),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black'),
               legend.title = element_text(family='serif',size=15,face='bold'),
               legend.text = element_text(family = 'serif',size=15,face='bold'),
               legend.position = 'none'
)
#读取数据
source('~/Rcode/statistics/OR.R')
source('~/Rcode/screening/gastric_screening2019/PAD2019.R')
source('~/Rcode/screening/gastric_screening2020/data2020.R')
screening2019_2020<-inner_join(screening2019,screening2020,by='persoID')
match_id<-unlist(inner_join(screening2019,screening2020,by='persoID')%>%transmute(ID.y))%>%as.vector()
screening2020_2<-screening2020[-which(screening2020$ID %in% match_id),names(screening2019)]
screening2019_2020_2<-rbind(screening2019,screening2020_2)


###2019和2020年数据进行合并
#0.2020年性别和年龄的分布情况
descrTable(性别~年龄分组3,data=screening2020,digits = 2,show.all = TRUE)
descrTable(年龄分组3~PG_pos+Hp_pos2,data=screening2020,digits = 2,show.all = TRUE)

#1、查看PG的基本分布
PG1.plot<-screening2019_2020_2%>%group_by(PG1_range3)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI',label=TRUE,lab.pos="in")
PG2.plot<-screening2019_2020_2%>%group_by(PG2_range5)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG2_range5',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGII',label=TRUE,lab.pos="in")
PGR.plot<-screening2019_2020_2%>%filter(PG1!=200)%>%group_by(PGR_range)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PGR_range',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI/II ratio',label=TRUE,lab.pos="in")

PG1R.plot<-screening2019_2020_2%>%mutate(m='v')%>%filter(PG1!=200)%>%group_by(m,PG1_range3,PGR_range)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',fill='PGR_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE)+labs(fill='PGI/II ratio')
(PG1.plot|PG2.plot |PGR.plot)/PG1R.plot +plot_annotation(tag_level = 'A')

####PG的分布在问卷因素中
#PGI
PG1_range.base<-descrTable(PG1_range3~性别+年龄分组+教育+家庭收入+BMI_group+吸烟1+饮酒+血型+喝茶+鲜奶+碳酸饮料+
           果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
             高血脂+冠心病,data=screening2019_2020_2,show.all = TRUE,digits=2)
export2xls(PG1_range.base,'~/PG1_range.base.xlsx')
#PGII
PG2_range.base<-descrTable(PG2_range5~性别+年龄分组+教育+家庭收入+BMI_group+吸烟1+饮酒+血型+喝茶+鲜奶+碳酸饮料+
                             果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
                             高血脂+冠心病,data=screening2019_2020_2,show.all = TRUE,digits=2)
export2xls(PG2_range.base,'~/PG2_range.base.xlsx')
#PGI/II
PGR_range.base<-descrTable(PGR_range2~性别+年龄分组+教育+家庭收入+BMI_group+吸烟1+饮酒+血型+喝茶+鲜奶+碳酸饮料+
                             果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
                             高血脂+冠心病,data=screening2019_2020_2,show.all = TRUE,digits=2)
export2xls(PGR_range.base,'~/PGR_range.base.xlsx')
#PG在幽门螺杆菌感染中的分布情况
HP_freq<-descrTable(Hp_pos2~PG1+PG2+PGR+PG1_range3+PG2_range5+PGR_range,data=screening2020,show.all = TRUE,digits=2,method=2,show.ratio = TRUE,ref=c(PG1_range3=4,PG2_range5=1,PGR_range=3))
export2xls(HP_freq,'~/HP_freq.xlsx')

##连续性变量
variables<-c('性别','年龄分组','教育','家庭收入','BMI_group','吸烟1','饮酒','血型','喝茶',
               '鲜奶','碳酸饮料',
                 '果味饮料','酸奶','咖啡','蔬菜','水果','胃溃疡','消化性溃疡','糖尿病','高血压',
                 '高血脂','冠心病')
##PG1
means_PG1<-screening2019_2020_2%>%pivot_longer(cols=variables,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=round(median(PG1),1),Q1=round(quantile(PG1,0.25),1),
                                       Q3=round(quantile(PG1,0.75),1))
print(means_PG1,n=200)
p<-list()
for(i in variables){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(screening2019_2020_2[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=screening2019_2020_2)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=screening2019_2020_2)$p.value,4)
  }
}
do.call(rbind,p)
screening2020%>%group_by(Hp_pos2)%>%summarise(median=round(median(PG1),1),Q1=round(quantile(PG1,0.25),1),
                                       Q3=round(quantile(PG1,0.75),1))
##PG2
means_PG2<-screening2019_2020_2%>%pivot_longer(cols=variables,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(median=round(median(PG2),1),Q1=round(quantile(PG2,0.25),1),
                                       Q3=round(quantile(PG2,0.75),1))
screening2020%>%group_by(Hp_pos2)%>%summarise(median=round(median(PG2),1),Q1=round(quantile(PG2,0.25),1),
                                              Q3=round(quantile(PG2,0.75),1))

print(means_PG2,n=200)
p<-list()
for(i in variables){
  formula_uni<-as.formula(paste('PG2','~', i))
  if(length(table(screening2019_2020_2[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=screening2019_2020_2)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=screening2019_2020_2)$p.value,4)
  }
}
do.call(rbind,p)

###按年龄,性别分层看PG的分布情况
age_sexPG1男<-descrTable(PG1_range3~年龄分组,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
export2xls(age_sexPG1男,"~/age_sexPG1男.xlsx")
age_sexPG1女<-descrTable(PG1_range3~年龄分组,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")
export2xls(age_sexPG1女,"~/age_sexPG1女.xlsx")
#BMI
descrTable(PG1_range3~BMI_group,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
descrTable(PG1_range3~BMI_group,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")
#家族史
descrTable(PG1_range3~胃癌家族史,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
descrTable(PG1_range3~胃癌家族史,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")
#消化性溃疡
descrTable(PG1_range3~消化性溃疡,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
descrTable(PG1_range3~消化性溃疡,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")
#糖尿病
descrTable(PG1_range3~糖尿病,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
descrTable(PG1_range3~糖尿病,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")
#吸烟
descrTable(PG1_range3~吸烟4,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
descrTable(PG1_range3~吸烟4,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")
#血型
descrTable(PG1_range3~血型1,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
descrTable(PG1_range3~血型1,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")

##性别、年龄分层后，PG1在吸烟中的分布
with(subset(screening2019_2020_2,性别=="Male" & 年龄>60),table(吸烟4,PG1_range3))
with(subset(screening2019_2020_2,性别=="Male" & 年龄>60),round(prop.table(table(吸烟4,PG1_range3),1)*100,2))

with(subset(screening2019_2020_2,性别=="Male" & 年龄>60),round(prop.table(table(吸烟4,PG1_range3),1)*100,2))
with(subset(screening2019_2020_2,性别=="Male" & 年龄<60),round(prop.table(table(吸烟4,PG1_range3),1)*100,2))

##PG2在性别分层后，在各因素中的分布情况
sexPG2男<-descrTable(PG2_range5~年龄分组+吸烟4+BMI_group4+胃癌家族史+消化性溃疡+糖尿病+血型1,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Male")
export2xls(sexPG2男,"~/sexPG2男.xlsx")
sexPG2女<-descrTable(PG2_range5~年龄分组+吸烟4+BMI_group4+胃癌家族史+消化性溃疡+糖尿病+血型1,data=screening2019_2020_2,show.all = TRUE,digits=2,subset=性别=="Female")
export2xls(sexPG2女,"~/sexPG2女.xlsx")



###幽门螺旋杆菌感染分层
#年龄
descrTable(PG1_range3~年龄分组,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阴性")
descrTable(PG1_range3~年龄分组,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阳性")
#BMI
descrTable(PG1_range3~BMI_group,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阴性")
descrTable(PG1_range3~BMI_group,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阳性")
#家族史
descrTable(PG1_range3~胃癌家族史,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阴性")
descrTable(PG1_range3~胃癌家族史,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阳性")
#消化性溃疡
descrTable(PG1_range3~消化性溃疡,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阴性")
descrTable(PG1_range3~消化性溃疡,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阳性")
#糖尿病
descrTable(PG1_range3~糖尿病,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阴性")
descrTable(PG1_range3~糖尿病,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阳性")
#吸烟
descrTable(PG1_range3~吸烟4,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阴性")
descrTable(PG1_range3~吸烟4,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阳性")
#血型
descrTable(PG1_range3~血型1,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阴性")
descrTable(PG1_range3~血型1,data=screening2020,show.all = TRUE,digits=2,subset=Hp_pos2=="阳性")

##性别、幽门螺杆菌分层后，PG1在吸烟中的分布
with(subset(screening2020,性别=="Male" & Hp_pos2=="阳性"),table(吸烟4,PG1_range3))
with(subset(screening2020,性别=="Male" & Hp_pos2=="阳性"),round(prop.table(table(吸烟4,PG1_range3),1)*100,2))

with(subset(screening2020,性别=="Male" & Hp_pos2=="阴性"),round(prop.table(table(吸烟4,PG1_range3),1)*100,2))
with(subset(screening2020,性别=="Male" & Hp_pos2=="阴性"),round(prop.table(table(吸烟4,PG1_range3),1)*100,2))




###
age_sexPG2<-descrTable(PG2_range5~吸烟2,data=screening2019_2020_2,show.all = TRUE,digits=2)
strataTable(age_sexPG2, "性别")
age_sexPGR<-descrTable(PGR_range~年龄分组,data=screening2019_2020_2,show.all = TRUE,digits=2)
strataTable(age_sexPGR, "性别")


##PG在癌及钱病变的人群中分布情况
data2019<-read.xlsx('~/data/Report/Report20210324/癌及癌前病变.xlsx',sheet=1)%>%transmute(
  ID
)
data2020<-read.xlsx('~/data/Report/Report20210324/癌及癌前病变.xlsx',sheet=2)%>%transmute(
  ID=筛查编号
)
data2019<-inner_join(data2019,screening2019,by="ID")
data2020<-inner_join(data2020,screening2020,by="ID")
data<-rbind(data2019,data2020[,names(data2019)])
data2020%>%mutate(m="v")%>%group_by(m,PG_hp)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',position = position_stack(),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE,lab.pos = "in")+labs(fill='')+
  scale_x_discrete(labels=c('PG- AND Hp-','PG- AND Hp+','PG+ AND Hp+','PG+ AND Hp-'))
diff<-descrTable(PG_pos~性别+胃癌家族史+年龄分组+教育+家庭收入+BMI_group+吸烟1+饮酒+血型1+喝茶+鲜奶+碳酸饮料+
             果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
             高血脂+冠心病,data=data,show.all = TRUE,digits=2)
export2xls(diff,'~/diff.xlsx')
data2020%>%select(C14Value,Hp_pos2,PG_pos,PG_hp)%>%group_by(PG_hp)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',position = position_stack(),palette = 'jco',ylab='Percent',xlab='')+labs(fill='')+
  scale_x_discrete(labels=c('A','B','C','D'))

###

  
#PG分布情况
#1.基本分布
PG1.plot<-data%>%group_by(PG1_range3)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI',label=TRUE,lab.pos="out")
PGR.plot<-data%>%filter(PG1!=200)%>%group_by(PGR_range)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PGR_range',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI/II ratio',label=TRUE,lab.pos="out")
PG1R.plot<-data%>%mutate(m='v')%>%filter(PG1!=200)%>%group_by(m,PG1_range3,PGR_range)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',fill='PGR_range',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE)+labs(fill='PGI/II ratio')
(PG1.plot |PGR.plot)/PG1R.plot +plot_annotation(tag_level = 'A')


#2.人群分布
base_PG1<-descrTable(PG1_range3~性别+年龄分组+教育+家庭收入+BMI_group+吸烟1+饮酒+血型+喝茶+鲜奶+碳酸饮料+
             果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
             高血脂+冠心病,data=data,show.all = TRUE,digits=2)
export2xls(base_PG1,'~/base_PG1.xlsx')

base_PG2<-descrTable(PG2_range5~性别+年龄分组+教育+家庭收入+BMI_group+吸烟1+饮酒+血型+喝茶+鲜奶+碳酸饮料+
                       果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
                       高血脂+冠心病,data=data,show.all = TRUE,digits=2)
export2xls(base_PG2,'~/base_PG2.xlsx')

base_PGR<-descrTable(PGR_range2~性别+年龄分组+教育+家庭收入+BMI_group+吸烟1+饮酒+血型+喝茶+鲜奶+碳酸饮料+
                       果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
                       高血脂+冠心病,data=data,show.all = TRUE,digits=2)
export2xls(base_PGR,'~/base_PGR.xlsx')
##PG在幽门螺杆菌的分布情况
descrTable(PG1_range3~Hp_pos2,data=screening2020,show.all = TRUE,digits=2)

descrTable(PG2_range5~Hp_pos2,data=screening2020,show.all = TRUE,digits=2)

###ABC方法在人群中的分布(针对2020年数据)
screening2020%>%group_by(PG_hp)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
ggbarplot(x='PG_hp',y='percent',fill='PG_hp',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='',label=TRUE)+labs(fill='ABC method')+
  scale_x_discrete(labels=c('A','B','C','D'))+theme( legend.position = 'none')
##ABC+年龄
screening2020%>%mutate(m="v")%>%group_by(m,年龄分组3,PG_hp)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='年龄分组3',y='percent',fill='PG_hp',position = position_fill(),palette = 'jco',ylab='Percent',xlab='年龄',label=TRUE,lab.pos = "in")+labs(fill='ABC')+
  scale_x_discrete(labels=c('40-49','50-59','60-69','70-74'))


#ABC+性别
screening2020%>%mutate(m="v")%>%group_by(m,性别,PG_hp)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',fill='性别',position = position_stack(),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE,lab.pos = "in")+labs(fill='性别')+
  scale_x_discrete(labels=c('PG- AND Hp-','PG- AND Hp+','PG+ AND Hp+','PG+ AND Hp-'))
##ABC+C14值
screening2020%>%mutate(m="v")%>%group_by(m,Hp_pos4,PG_hp)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',fill='Hp_pos4',position = position_stack(),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE,lab.pos = "in")+labs(fill='C14 value')+
  scale_x_discrete(labels=c('PG- AND Hp-','PG- AND Hp+','PG+ AND Hp+','PG+ AND Hp-'))
screening2020%>%mutate(m="v")%>%group_by(m,PG_hp)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG_hp',y='percent',position = position_stack(),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE,lab.pos = "in")+labs(fill='C14 value')+
  scale_x_discrete(labels=c('PG- AND Hp-','PG- AND Hp+','PG+ AND Hp+','PG+ AND Hp-'))


####ABCD分组在人群中的分布情况
ABC<-descrTable(PG_hp~性别+年龄分组+教育+家庭收入+BMI_group+吸烟4+饮酒+血型1+喝茶+油炸+鲜奶+碳酸饮料+
             果味饮料+酸奶+咖啡+蔬菜+水果+胃溃疡+消化性溃疡+糖尿病+高血压+
             高血脂+冠心病+Hp_pos4,data=screening2020,show.all = TRUE,digits=2)
export2xls(ABC,'~/ABC.xlsx')



####2019-2020年PG的进展
#PGI进展情况
with(screening2019_2020,table(PG1_range3.x))
with(screening2019_2020,table(PG1_range3.y))
with(screening2019_2020,table(PG1_range3.x,PG1_range3.y))
with(screening2019_2020,round(prop.table(table(PG1_range3.x,PG1_range3.y),1)*100,2))
#PGII进展情况
with(screening2019_2020,table(PG2_range5.x))
with(screening2019_2020,table(PG2_range5.y))
with(screening2019_2020,table(PG2_range5.x,PG2_range5.y))
with(screening2019_2020,round(prop.table(table(PG2_range5.x,PG2_range5.y),1)*100,2))
#PG比值进展
with(screening2019_2020,table(PGR_range2.x))
with(screening2019_2020,table(PGR_range2.y))
with(screening2019_2020,table(PGR_range2.x,PGR_range2.y))
with(screening2019_2020,round(prop.table(table(PGR_range2.x,PGR_range2.y),1)*100,2))


###肿瘤医院PG分布的分析
HosPG<-import("~/data/Report/Report20210324/HospitalPG.sav")
#2.最后1次检测时间，检测结果
for (i in 1:nrow(HosPG)){
  n=HosPG[i,'n']
  HosPG$PGDate.Last[i]=as.character(HosPG[i,paste0('PGDate.',n)])
  HosPG$PGI.last[i]=HosPG[i,paste0('PGI.',n)]
  HosPG$PGII.last[i]=HosPG[i,paste0('PGI.',n)]
}
HosPG$PGDate.Last<-as.Date(HosPG$PGDate.Last)
#summary(HosPG$PGI.1)
HosPG2<-HosPG%>%filter(Source=="医院人群")%>%transmute(n,
  PGR.1=round(PGI.1/PGII.1,2),
  PG1_range3=case_when(
    PGI.1<=30 ~ 1,
    PGI.1>30 & PGI.1<=50.0 ~ 2,
    PGI.1>50.0 & PGI.1<=70.0 ~ 3,
    PGI.1>70 ~ 4
  ),
  PG1_range3=factor(PG1_range3,levels=c(1,2,3,4),labels=c('<=30','30.01-50','50.01-70','>70')),
  PG2_range5=case_when(
    PGII.1<=7.5 ~ 1,
    PGII.1>7.5 & PGII.1<=12.5 ~ 2,
    PGII.1>12.5 & PGII.1<=17.5 ~ 3,
    PGII.1>17.5 & PGII.1<=24.5 ~ 4,
    PGII.1>24.5 ~ 5
  ),
  PG2_range5=factor(PG2_range5,levels = c(1,2,3,4,5),labels=c('<=7.5','7.6-12.5','12.6-17.5','17.5-24.5','>24.5')),
  PGR_range2=case_when(
    PGR.1<=3 ~ 1,
    PGR.1>3 &  PGR.1<=6 ~ 2,
    PGR.1>6 &  PGR.1<=9  ~ 3,
    PGR.1>9   ~ 4
  ),
  PGR_range2=factor(PGR_range2,levels=c(1,2,3,4),labels=c('<=3','3.01-6','6.01-9','>9')),
  PG1.last_range=case_when(
    PGI.last<=30 ~ 1,
    PGI.last>30 & PGI.last<=50.0 ~ 2,
    PGI.last>50.0 & PGI.last<=70.0 ~ 3,
    PGI.last>70 ~ 4
  ),
  PG1.last_range=factor(PG1.last_range,levels=c(1,2,3,4),labels=c('<=30','30.01-50','50.01-70','>70')),
  
  PG2.last_range=case_when(
    PGII.last<=7.5 ~ 1,
    PGII.last>7.5 & PGII.last<=12.5 ~ 2,
    PGII.last>12.5 & PGII.last<=17.5 ~ 3,
    PGII.last>17.5 & PGII.last<=24.5 ~ 4,
    PGII.last>24.50 ~ 5
  ),
  PG2.last_range=factor(PG2.last_range,levels = c(1,2,3,4,5),labels=c('<=7.5','7.6-12.5','12.6-17.5','17.5-24.5','>24.5')),
  
)
##PG基本分布
HosPG.1<-HosPG2%>%filter(!is.na(PG1_range3))%>%group_by(PG1_range3)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI',label=TRUE,lab.pos="out")
HosPG.2<-HosPG2%>%filter(!is.na(PGR_range2))%>%group_by(PGR_range2)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PGR_range2',y='percent',palette ='jco',fill='lightblue',ylab='Percent',xlab='PGI/II ratio',label=TRUE,lab.pos="out")
HosPG.R<-HosPG2%>%filter(!is.na(PG1_range3) & !is.na(PGR_range2))%>%mutate(m='v')%>%group_by(m,PG1_range3,PGR_range2)%>%summarise(n=n())%>%group_by(m)%>%mutate(percent=round(n/sum(n),4)*100,percent2=paste0(round(n/sum(n),4)*100,'%'))%>%
  ggbarplot(x='PG1_range3',y='percent',fill='PGR_range2',position = position_dodge(width=0.8),palette = 'jco',ylab='Percent',xlab='PGI',label=TRUE)+labs(fill='PGI/II ratio')
(HosPG.1 |HosPG.2)/HosPG.R +plot_annotation(tag_level = 'A')
#PGI进展情况
with(subset(HosPG2,n>1),table(PG1_range3,PG1.last_range))
with(subset(HosPG2,n>1),round(prop.table(table(PG1.1_range,PG1.last_range),1)*100,2))
#PGII进展情况
with(subset(HosPG2,n>1),table(PG2_range5,PG2.last_range))
with(subset(HosPG2,n>1),round(prop.table(table(PG2_range5,PG2.last_range),1)*100,2))


##医院
PGPath6<-import('~/data/hospital/Path_PG20210323/PGPath2.2 (5).xlsx')%>%filter(lauren!="混合型")%>%
  transmute(id_card,name,Age,性别=factor(ifelse(Sex=="男",1,2),levels=c(1,2),labels=c('Male','Female')),lauren,PGI.1,PGII.1,PGR.1=round(PGI.1/PGII.1,2),
            年龄分组=factor(ifelse(Age<=49,0,ifelse(Age>60,2,1)),levels=c(0,1,2),labels=c('<50','50-60','>60')),
            PG1_range3=case_when(
              PGI.1<=30 ~ 1,
              PGI.1>30 & PGI.1<=50.0 ~ 2,
              PGI.1>50.0 & PGI.1<=70.0 ~ 3,
              PGI.1>70 ~ 4
            ),
            PG1_range3=factor(PG1_range3,levels=c(1,2,3,4),labels=c('<=30','30.01-50','50.01-70','>70')),
            PG2_range5=case_when(
              PGII.1<=7.5 ~ 1,
              PGII.1>7.5 & PGII.1<=12.5 ~ 2,
              PGII.1>12.5 & PGII.1<=17.5 ~ 3,
              PGII.1>17.5 & PGII.1<=24.5 ~ 4,
              PGII.1>24.5 ~ 5
            ),
            PG2_range5=factor(PG2_range5,levels = c(1,2,3,4,5),labels=c('<=7.5','7.6-12.5','12.6-17.5','17.5-24.5','>24.5')),
            PGR_range2=case_when(
              PGR.1<=3 ~ 1,
              PGR.1>3 &  PGR.1<=6 ~ 2,
              PGR.1>6 &  PGR.1<=9  ~ 3,
              PGR.1>9   ~ 4
            ),
            PGR_range2=factor(PGR_range2,levels=c(1,2,3,4),labels=c('<=3','3.01-6','6.01-9','>9')),
            group=1
            )%>%filter(!is.na(PGR.1))

summary(PGPath6)
summary(PGPath6[which(PGPath6$lauren=="肠型"),c('PGI.1','PGII.1','PGR.1')])
summary(PGPath6[which(PGPath6$lauren=="弥漫型"),c('PGI.1','PGII.1','PGR.1')])
#PGI
PG1_range.base<-descrTable(PG1_range3~性别+年龄分组,data=PGPath6,show.all = TRUE,digits=2)
export2xls(PG1_range.base,'~/PG1_range.base.xlsx')
#PGII
PG2_range.base<-descrTable(PG2_range5~性别+年龄分组,data=PGPath6,show.all = TRUE,digits=2)
export2xls(PG2_range.base,'~/PG2_range.base.xlsx')
#PGI/II
PGR_range.base<-descrTable(PGR_range2~性别+年龄分组,data=PGPath6,show.all = TRUE,digits=2)


###
control=screening2019%>%transmute(
  id_card=ID,name,Age=年龄,性别,lauren=0,PGI.1=PG1,PGII.1=PG2,PGR.1=PGR,
  年龄分组,
  PG1_range3,
  PG2_range5,
  PGR_range2,
  PGR_range2,
  group=0
)
data<-rbind(control,PGPath6)

###根据性别年龄1：2匹配
set.seed(12345)
m.out2<-matchit(group~性别+Age,data=data,method='nearest',exact=c('性别','Age'),ratio=2)
m.data<-match.data(m.out2)
summary(m.data)
table(m.data$group)
table(m.data$lauren)

forest_model(clogit(group~strata(subclass)+relevel(PG1_range3,ref='>70')+性别+年龄分组,data=m.data))
clogit(group~strata(subclass)+as.numeric(PG1_range3)+性别+年龄分组,data=m.data)
forest_model(clogit(group~strata(subclass)+relevel(PG1_range3,ref='>70')+性别+年龄分组,data=subset(m.data,lauren!="弥漫型")))
forest_model(clogit(group~strata(subclass)+relevel(PG1_range3,ref='>70')+性别+年龄分组,data=subset(m.data,lauren!="肠型")))

forest_model(clogit(group~strata(subclass)+PG2_range5+性别+年龄分组,data=m.data))
forest_model(clogit(group~strata(subclass)+PG2_range5+性别+年龄分组,data=subset(m.data,lauren!="弥漫型")))
forest_model(clogit(group~strata(subclass)+PG2_range5+性别+年龄分组,data=subset(m.data,lauren!="肠型")))


forest_model(clogit(group~strata(subclass)+relevel(PGR_range2,ref='>9'),data=m.data))
forest_model(clogit(group~strata(subclass)+relevel(PGR_range2,ref='>9'),data=subset(m.data,lauren!="弥漫型")))
forest_model(clogit(group~strata(subclass)+relevel(PGR_range2,ref='>9'),data=subset(m.data,lauren!="肠型")))

##
match2<-descrTable(lauren~性别+年龄分组+PGI.1+PGII.1+PGR.1+PG1_range3+PG2_range5+PGR_range2,data=m.data,show.all = TRUE,digits=2,method=2)
export2xls(match,'~/match2.xlsx')

#
diffuse_data<-m.data%>%filter(lauren!="肠型")
forest_model(clogit(group~strata(subclass)+PG2_range5+性别+Age,data=subset(diffuse_data,Age<=60)))
forest_model(clogit(group~strata(subclass)+PG2_range5+性别+Age,data=subset(diffuse_data,Age>60)))

forest_model(clogit(group~strata(subclass)+relevel(PGR_range2,ref='>9')+性别+Age,data=subset(diffuse_data,Age<=60)))
forest_model(clogit(group~strata(subclass)+relevel(PGR_range2,ref='>9')+性别+Age,data=subset(diffuse_data,Age>60)))

###绘制ROC曲线
#AUC=0.67,灵敏度：0.72，特异度，0.548
plot.roc(m.data$group,m.data$PGII.1,direction='<',add=F,legacy.axes=T,las=1,col="red",print.thres=T)
#AUC=0.70，灵敏度：0.72，特异度：0.60
plot.roc(m.data[which(m.data$lauren!="肠型"),'group'],m.data[which(m.data$lauren!="肠型"),'PGII.1'],direction='<',add=F,legacy.axes=T,las=1,col="red",print.thres=T)
#AUC=0.65，灵敏度：0.72，特异度：0.52
plot.roc(m.data[which(m.data$lauren!="弥漫型"),'group'],m.data[which(m.data$lauren!="弥漫型"),'PGII.1'],direction='<',add=F,legacy.axes=T,las=1,col="red",print.thres=T)

###确定切点值问题
m.data%>%filter(group==0)%>%mutate(v="m",vec_cut=cut_number(log(PGI.1),20))%>%count(v,vec_cut)%>%group_by(v)%>%summarise(percent=round(n/sum(n),4)*100)
m.data%>%transmute(
  PG
)











