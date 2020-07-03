library(rio)
library(tidyverse)
library(ggpubr)
library(gtable)
library(grid)
rm(list=ls())
screening<-import('~/data/baseline2019(剔除自身癌).sav')
#筛查居民基本信息
screening%>%group_by(sex_check)%>%summarise(n=n(),age_mean=mean(age_check),age_sd=sd(age_check))%>%transmute(sex_check=factor(sex_check,labels=c('男','女')),
                                                               n=n,labs=paste0(sex_check,'(',round(n/48216,4)*100,'%)'),age_mean=age_mean,age_sd=age_sd)
#年龄
plot1<-screening%>%select(sex_check,age_check)%>%transmute(sex_check=factor(sex_check,labels = c('男','女')),age_check=age_check)%>%
  gghistogram(x='age_check',y='..density..',binwidth = 1,xlab='年龄(岁)',ylab='密度',fill='green')+
  scale_y_continuous(expand = c(0,0),breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06),limits = c(0,0.06))+border()+
  scale_x_continuous(breaks=c(42,48,54,60,66,72,78),limits = c(36,78))+labs(fill='性别',title='社区人群年龄分布直方图')+  
  stat_overlay_normal_density(color = "red", linetype = "dashed")+theme(legend.position = 'right',plot.title = element_text(hjust = 0.5))

#性别
plot2<-screening%>%group_by(sex_check)%>%summarise(n=n())%>%transmute(sex_check=factor(sex_check,labels=c('男','女')),
                   n=n,labs=paste0(sex_check,'(',round(n/48216,4)*100,'%)'))%>%
  ggpie(x='n',lab.pos = 'in',label = 'labs',fill='sex_check',lab.font = c(3,'bold','black'))+
  labs(fill='性别')+theme(legend.position = 'right',plot.title = element_text(hjust = 0.5))+labs(title='社区人群性别分布饼图')
#年龄与性别
plot3<-screening%>%select(sex_check,age_check)%>%transmute(sex_check=factor(sex_check,labels = c('男','女')),age_check=age_check)%>%
  gghistogram(x='age_check',y='..density..',binwidth = 1,fill='sex_check',xlab='年龄(岁)',ylab='密度',palette = c("#00AFBB", "#E7B800"),alpha=0.2,merge=TRUE,facet.by = 'sex_check')+
  scale_y_continuous(expand = c(0,0),breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06),limits = c(0,0.06))+
  scale_x_continuous(breaks=c(42,48,54,60,66,72,78),limits = c(36,78))+labs(fill='性别',title='社区人群不同性别年龄分布直方图')+  
  stat_overlay_normal_density(color = "red", linetype = "dashed")+theme(legend.position = 'right',plot.title = element_text(hjust = 0.5))
plot12<-ggarrange(plot1,plot2,nrow=1)
plot123<-ggarrange(plot12,plot3,nrow=2)
plot123                   
##婚姻、血型、就业状况、保险、职业类型、家庭收入、文化程度分布
table<-screening%>%select(marriag,educati,employm,occupat,income,bloodtp,insuran)%>%
  transmute(marriag=factor(ifelse(marriag<=4,marriag,NA),labels=c('已婚','未婚','离婚','丧偶')),
educati=factor(ifelse(educati<=6,educati,NA),labels=c('未受教育','小学','初中','高中/中专/技校','大专/大学','研究生')),
 employm=factor(ifelse(employm<=4,employm,NA),labels=c('在业','离退休','失业/下岗/待业','家务/无业')),
occupat=factor(ifelse(occupat<=8,occupat,NA),labels=c('机关/企事业单位负责人','专业技术人员','办事人员和相关人员','商业/服务业人员','农林牧鱼水利业生产人员','生产运输设备操作人员','军人','其他')),
income=factor(ifelse(income<=4,income,NA),labels=c('<3000元/月','3000-4999元/月','5000-9999元/月','>=10000元/月')),
bloodtp=factor(ifelse(bloodtp<=5,bloodtp,NA),labels=c('A型','B型','O型','AB型','不详')),
insuran=factor(ifelse(insuran<=7,insuran,NA),labels=c('城镇职工基本医疗保险','城镇居民基本医疗保险','新型农村合作医疗','商业医疗保险','全公费','全自费','其他')))%>%
  pivot_longer(cols=c('marriag','educati','employm','occupat','income','bloodtp','insuran'),names_to='variables',values_to='levels')%>%filter(!is.na(levels))%>%group_by(variables,levels)%>%summarise(n=n())
table2<-as_tibble(table)
table3<-table2%>%transmute(variables=variables,levels=levels,n=n,N=case_when(
  variables=='bloodtp' ~ 48214,
  variables=='educati' ~ 48213,
  variables=='income' ~ 48213,
  variables=='employm' ~ 48207,
  variables=='insuran' ~ 48214,
  variables=='marriag' ~48211,
  variables=='occupat' ~43304
))%>%transmute(variables=variables,levels=levels,n=n,N=N,percent=round(n/N,4),labs=paste0(round(n/N,4)*100,'%'))
plot4<-facet(ggbarplot(data=subset(table3,variables=='bloodtp' | variables=='employm' |variables=='marriag' | variables=='educati' ),
                x='levels',y='percent',label = 'labs',lab.pos = 'out',fill='levels',xlab = '',ylab='百分比',x.text.angle=50),
      facet.by = 'variables',scales='free',panel.labs=list(variables=c('血型','文化程度','就业状况','婚姻状况')),
      nrow=1)+theme(legend.position = 'none')

plot5<-facet(ggbarplot(data=subset(table3,variables=='income' | variables=='occupat' |variables=='insuran'),
                x='levels',y='percent',label = 'labs',lab.pos = 'out',fill='levels',xlab = '',ylab='百分比',x.text.angle=50),
      facet.by = 'variables',scales='free',panel.labs=list(variables=c('家庭收入','医疗费用支付方式','主要职业')),
      nrow=1)+theme(legend.position = 'none')
ggarrange(plot4,plot5,nrow=2)
##示范区肿瘤标志物描述性分析
rm(list=ls())
#基本
biomarker<-import('~/data/biomarker/biomarker2019.xlsx')
biomarker2<-biomarker%>%filter(source=='示范区')
mean(biomarker$age);sd(biomarker$age)
biomarker2%>%group_by(sex)%>%summarise(n=n(),age_mean=mean(age),age_sd=sd(age))%>%
  transmute(sex=sex,n=n,percent=round(n/sum(n),4)*100,age_mean=age_mean,age_sd=age_sd)
#各街道性别百分比
table_sex<-as_tibble(biomarker2%>%group_by(社区,sex)%>%summarise(n=n())%>%
transmute(area=社区,sex=factor(sex,labels=c('男','女')),n=n))
biomarker2%>%group_by(社区)%>%summarise(n=n())
table_sex2<-table_sex%>%transmute(area=area,sex=sex,n=n,N=case_when(
  area=='下瓦房街' ~ 1009,
  area=='友谊路街' ~ 998,
  area=='尖山街' ~ 1003,
  area=='桃园街' ~ 1003,
  area=='越秀路街' ~ 1014,
  area=='陈塘庄街' ~ 1003,
))%>%transmute(area=area,sex=sex,n=n,N=N,labs=paste0(sex,' ',round(n/N,4)*100,'%'))
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
ggplot(table_sex2, aes(x="", y=n, fill=sex))+labs(fill='性别')+
  geom_bar(width = 1, stat = "identity",position = 'fill')+facet_wrap(area~.)+ coord_polar("y")+
  blank_theme +scale_fill_brewer("Blues")+
  theme(axis.text.x=element_blank(),legend.title = element_blank())
 ##性别
biomarker2%>%select(sex,age)%>%transmute(sex_check=factor(sex,labels = c('男','女')),age_check=age)%>%
  gghistogram(x='age_check',y='..density..',binwidth = 1,fill='sex_check',xlab='年龄(岁)',ylab='密度',palette = c("#00AFBB", "#E7B800"),alpha=0.2,merge=TRUE,facet.by = 'sex_check')+
  scale_y_continuous(expand = c(0,0),breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06),limits = c(0,0.07))+
  scale_x_continuous(breaks=c(42,48,54,60,66,72,78),limits = c(36,78))+labs(fill='性别',title='示范区人群不同性别年龄分布直方图')+  
  stat_overlay_normal_density(color = "red", linetype = "dashed")+theme(legend.position = 'right',plot.title = element_text(hjust = 0.5))
##蓟州区高危情况
jizhou_risk<-data.frame(
  area=c('别山镇','马伸桥镇','东二营镇','官庄镇','桑梓镇','下营镇'),
  肺癌高危人数=c(88,66,5,3,1,6),
  乳腺癌高危人数=c(7,1,0,1,0,2),
  胃癌高危人数=c(68,76,0,3,1,2),
  肝癌高危人数=c(64,61,3,8,0,7)
)
jizhou_risk2<-jizhou_risk%>%pivot_longer(cols=c('肺癌高危人数','乳腺癌高危人数','胃癌高危人数','肝癌高危人数'),names_to = 'item',values_to = 'value')
 
facet(ggbarplot(jizhou_risk2,y='value',x='item',fill='item',x.text.angle=20,label='value',xlab='街道',ylab='人数',lab.pos = 'out',position = position_dodge()),facet.by = 'area',scales='free_x')+
  theme(legend.position = 'none')
#肿瘤标志物界限分布
risk<-function(x){
  x2<-factor(x,levels = c(0,1,2,3,4),labels=c('正常','超出截值1-2倍','超出截值2-3倍','超出截值3-4倍','超出截值4倍'))
  return(x2)
}
biomark6.3<-within(biomarker2,{
  CEA.risk<-vector()
  AFP.risk<-vector()
  CA199.risk<-vector()
  CA153.risk<-vector()
  CA125.risk<-vector()
  #CEA
  CEA.risk[CEA<=5]<-0
  CEA.risk[CEA>5 & CEA<=10]<-1
  CEA.risk[CEA>10 & CEA<=15]<-2
  CEA.risk[CEA>15 & CEA<=20]<-3
  CEA.risk[CEA>20]<-4
  #AFP
  AFP.risk[AFP<=7]<-0
  AFP.risk[AFP>7 & AFP<=14]<-1
  AFP.risk[AFP>14 & AFP<=21]<-2
  AFP.risk[AFP>21 & AFP<=28]<-3
  AFP.risk[AFP>28]<-4
  #CA199
  CA199.risk[CA199<=27]<-0
  CA199.risk[CA199>27 & CA199<=27*2]<-1
  CA199.risk[CA199>27*2 & CA199<=27*3]<-2
  CA199.risk[CA199>27*3 & CA199<=27*4]<-3
  CA199.risk[CA199>27*4]<-4
  #CA153
  CA153.risk[CA153<=25]<-0
  CA153.risk[CA153>25 & CA153<=50]<-1
  CA153.risk[CA153>50 & CA153<=75]<-2
  CA153.risk[CA153>75 & CA153<=100]<-3
  CA153.risk[CA153>100 ]<-4
  #CA125
  CA125.risk[CA125<=35]<-0
  CA125.risk[CA125>35 & CA125<=70]<-1
  CA125.risk[CA125>70 & CA125<=105]<-2
  CA125.risk[CA125>105 & CA125<=140]<-3
  CA125.risk[CA125>140]<-4
})
biomark6.3[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')]<-apply(biomark6.3[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,risk)
apply(biomark6.3[which(biomark6.3$sex==2),c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,table)
##
biomarker2%>%select(sex,CEA,AFP,CA199,CA125,CA153)%>%group_by(sex)%>%
  summarise(CEA_mean=median(CEA,na.rm = T),CEA_sd=IQR(CEA,na.rm = T),
            AFP_mean=mean(AFP,na.rm = T),CEA_sd=sd(AFP,na.rm = T),
            CA199_mean=mean(CA199,na.rm = T),CA199_sd=sd(CA199,na.rm = T),
            CA125_mean=mean(CA125,na.rm = T),CA125_sd=sd(CA125,na.rm = T),
            CA153_mean=mean(CA153,na.rm = T),CA153_sd=sd(CA153,na.rm = T))
##
man_cea<-biomarker2%>%filter(sex==1)%>%gghistogram(x='CEA',binwidth = 0.1,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,20))

man_afp<-biomarker2%>%filter(sex==1)%>%gghistogram(x='AFP',binwidth = 0.1,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,30))
man_ca199<-biomarker2%>%filter(sex==1)%>%gghistogram(x='CA199',binwidth = 0.2,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,50))
ggarrange(man_cea,man_afp,man_ca199,nrow=3)

woman_cea<-biomarker2%>%filter(sex==2)%>%gghistogram(x='CEA',binwidth = 0.1,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,20))

woman_afp<-biomarker2%>%filter(sex==2)%>%gghistogram(x='AFP',binwidth = 0.1,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,30))
woman_ca199<-biomarker2%>%filter(sex==2)%>%gghistogram(x='CA199',binwidth = 0.2,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,50))
woman_ca153<-biomarker2%>%filter(sex==2)%>%gghistogram(x='CA153',binwidth = 0.2,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,50))
woman_ca125<-biomarker2%>%filter(sex==2)%>%gghistogram(x='CA125',binwidth = 0.2,fill='green',y='..density..',ylab='密度')+border()+
  stat_overlay_normal_density(color = "red", linetype = "dashed")+scale_x_continuous(limits = c(0,50))


ggarrange(woman_cea,woman_afp,woman_ca199,woman_ca125,woman_ca153,ncol=5)


