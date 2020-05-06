rm(list=ls())
library(tidyverse)
library(rio)
library(table1)
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
                        axis.title=element_text(face="bold",size=10),
                        axis.text=element_text(face="bold",size=9),
                        panel.grid.major = element_line(colour=NA),
                        panel.grid.minor = element_blank(),
                        panel.background=element_rect(fill=NA),
                        axis.line = element_line(color='black'),
               strip.background = element_rect(
                 color = "white", fill = "white"),
               legend.key = element_rect(fill='white'),
               strip.text = element_text(
                 size = 11, face = "bold")
)
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
check<-function(data){
  attach(data)
  #5.女性生理与生育方面
  check39<-ifelse(agemenarch>age_check,1,0)#初潮年龄大于年龄
  check40<-ifelse(agemenopau>age_check,1,0)#绝经年龄大于年龄
  check41<-ifelse(agemenopau<agemenarch,1,0)#绝经年龄小于初潮年龄
  check42<-ifelse(agefirdeli>age_check,1,0)#首次生育年龄大于年龄 
  check43<-ifelse(agefirdeli<agemenarch,1,0)#首次生育年龄小于初潮年龄
  check44<-ifelse(agefirdeli>agemenopau,1,0)#首次生育年龄大于绝经年龄
  check<-case_when(
    check39==1 ~ '初潮年龄大于年龄',
    check40==1 ~ '绝经年龄大于年龄',
    check41==1 ~ '绝经年龄小于初潮年龄',
    check42==1 ~ '首次生育年龄大于年龄',
    check43==1 ~ '首次生育年龄小于初潮年龄',
    check44==1 ~ '首次生育年龄大于绝经年龄'
  )
  check_table<-cbind(data,check39,check40,check41,check42,check43,check44,check)
  return(check_table)
}
source('~/Rcode/statistics/PersoID_inf.R')
#读取数据
screening<-import('~/data/女性生理与生育年龄趋势.sav')
#分析逻辑错误
screening2<-check(screening)
table(screening2$check)
screening2[which(!is.na(screening2$check)),c('age_check','agemenopau',"agefirdeli")]
#出生年的提取
screening2$YOB_check2<-apply(matrix(screening2[,c('persoID_check')],nrow=1),1,function(id){
  if(nchar(id)==15)
    paste('19',substr(id,7,8),sep="")
  else
    substr(id,7,10)
})
screening2[,c('YOB_check','YOB_check2')]
screening2$match_YOB<-screening2$YOB_check2==screening2$YOB_check
summary(screening2$match_YOB);table(screening2$match_YOB)#出生年正确81760，匹配错误479人，缺失118人
screening2[which(screening2$match_YOB=="FALSE"),c('ID_BLAST','YOB_check','YOB_check2','persoID_check')]
screening2[which(is.na(screening2$match_YOB)),c('ID_BLAST','age_check','YOB_check','YOB_check2','persoID_check')]
summary(screening2$DOB_check)#出生年月日缺失情况
summary(screening2$YOB_check)#出生年缺失情况
summary(screening2$YOB_check2)#出生年缺失情况
summary(screening2$persoID_check)#出生年缺失情况

#分析出生年月日与年龄的对应关系
screening$DOB_check2<-apply(matrix(screening[,c('persoID_check')],nrow=1),1,afunc)
screening[,c('YOB_check','DOB_check','DOB_check2')]
##年龄检查
screening$age_chekc2<-ifelse(round((screening$DOI_check-screening$DOB_check)/365)-age_check<2,0,1)
summary(screening$age_chekc2);table(screening$age_chekc2)
screening[which(is.na(screening$age_chekc2)),c('age_check','DOI_check','DOB_check')]
#数据处理
screening3<-screening2%>%filter(CASelf==1)%>%transmute(
  筛查年份=Year,婚姻=factor(ifelse(marriag<5,marriag,NA)),
  height=ifelse(height<quantile(screening2$height,0.001,na.rm = T) | height>quantile(screening2$height,0.999,na.rm = T),NA,height),
  weight=ifelse(weight<quantile(screening2$weight,0.001,na.rm = T) | weight>quantile(screening2$weight,0.999,na.rm = T),NA,weight),
 
  ID=ID_BLAST,year=Year,出生年份=YOB_check,
  地区=factor(ifelse(region=='下营' | region=='下营镇' | region=='东二营镇' | region=='别山' | region=='别山镇' |
              region=='官庄' | region=='官庄镇' | region=='桑梓镇' | region=='马伸桥' | region=='马伸桥镇',2,1),levels=c(1,2),labels = c('市区','郊区')),
  初筛年龄=age_check,
  #婚姻=factor(marriag,levels = c(1,2,3,4),labels = c('已婚','未婚','离婚','丧偶')),
  education_risk=case_when(
    educati==1 | educati==2 ~ 1,
    educati==3 ~ 2,
    educati==4 |  educati==5 | educati==6 ~ 3,
    ),
  教育=factor(education_risk,levels=c(1,2,3),labels=c('小学及以下','初中','高中及以上')),
  就业状况=factor(employm,levels = c(1,2,3,4),labels = c('在业','离退休','失业/下岗/待业','家务/无业')),
  职业=factor(occupat),
  初潮年龄=ifelse(agemenarch<quantile(agemenarch,0.001,na.rm = TRUE) | agemenarch>quantile(agemenarch,0.999,na.rm = TRUE),NA,agemenarch),
  绝经年龄=ifelse(agemenopau<quantile(agemenopau,0.001,na.rm = TRUE) | agemenopau>quantile(agemenopau,0.999,na.rm = TRUE),NA,agemenopau),
  绝经=factor(ifelse(is.na(绝经年龄),1,2),levels = c(1,2),labels=c('否','是')),
  生育=factor(deliver,levels = c(1,2),labels=c('否','是')),
  生育次数=delivertim,
  生育次数=ifelse(delivertim<quantile(delivertim,0.1,na.rm = TRUE) | delivertim>quantile(delivertim,0.999,na.rm = TRUE),NA,delivertim),
  初次生育年龄=agefirdeli,哺乳=factor(breastfeed,levels = c(1,2),labels=c('否','是')),
  哺乳月份=ifelse(brstfedmth<quantile(brstfedmth,0.001,na.rm = TRUE) | brstfedmth>quantile(brstfedmth,0.99,na.rm = TRUE),NA,brstfedmth),
  流产=factor(abortion,levels = c(1,2),labels = c('否','是')),
  人工流产=factor(ifelse(is.na(induabort),1,2),levels = c(1,2),labels = c('否','是')),
  自然流产=factor(ifelse(is.na(sponabort),1,2),levels = c(1,2),labels = c('否','是')),
  人工流产次数=ifelse(induabort<quantile(induabort,0.01,na.rm = TRUE) |  induabort>quantile(induabort,0.99,na.rm = TRUE),NA,induabort),
  生育年份=出生年份+初次生育年龄,
  流产情况分组=case_when(
    abortion==1 ~ 1,#无流产
    abortion==2 & !is.na(induabort) ~ 2,#有人工流产史
    abortion==2 & !is.na(sponabort) ~ 2,#有自然流产史
  ),
)%>%transmute(筛查年份,婚姻, BMI=weight/((height/100)^2),
  出生年份,地区,初筛年龄,教育,就业状况,职业,初潮年龄,绝经,绝经年龄,生育,生育次数,
  初次生育年龄,哺乳,哺乳月份,流产,人工流产,自然流产,人工流产次数,生育年份,流产情况分组,
  出生队列=factor(case_when(
    出生年份<1950 ~ 1, 
    between(出生年份,1950,1959) ~ 2,
    between(出生年份,1960,1969) ~ 3,
    between(出生年份,1970,1979) ~ 4,
  ),levels = c(1,2,3,4),labels=c('<1950','1950-1959','1960-1969','1970-1979')),
  初潮年龄分组=factor(ifelse(初潮年龄<13,1,ifelse(初潮年龄<=14,2,ifelse(初潮年龄<=16,3,ifelse(初潮年龄<=18,4,5)))),
                levels = c(1,2,3,4,5),labels = c('<13','13-14','15-16','17-18','>=19')),
  生育次数分组=case_when(
    生育次数==1 ~ 1,
    生育次数==2 ~ 2,
    生育次数==3 |  生育次数==4 ~ 3,
    生育次数>=5 ~ 4,
  ),
  生育次数分组=factor(生育次数分组),
  
  首次生育年龄分组=case_when(
    初次生育年龄<20 ~ 1,
    between(初次生育年龄,20,21) ~ 2,
    between(初次生育年龄,22,23) ~ 3,
    between(初次生育年龄,24,25) ~ 4,
    between(初次生育年龄,26,27) ~ 5,
    初次生育年龄>27 ~ 6,
  ),
  首次生育年龄分组=factor(首次生育年龄分组,levels = c(1,2,3,4,5,6),labels = c('<20','20-21','22-23','24-25','26-27','>=28')) ,
  哺乳时间分组=case_when(
    哺乳==1 ~ 1,
    between(哺乳月份,1,6) ~ 2,
    between(哺乳月份,7,12) ~ 3,
    between(哺乳月份,13,18) ~ 4,
    between(哺乳月份,19,24) ~ 5,
    哺乳月份>24 ~ 6,
  ),
  哺乳时间分组=factor(哺乳时间分组,levels = c(1,2,3,4,5,6),labels = c('未哺乳','1-6','7-12','13-18','19-24','>24')),
  绝经年龄分组=case_when(
    绝经年龄<43 ~ 1,
    between(绝经年龄,43,47) ~ 2,
    between(绝经年龄,48,53) ~ 3,
    绝经年龄>=54~ 4,
  ),
  绝经年龄分组=factor(绝经年龄分组,levels = c(1,2,3,4),labels=c('<43','43-47','48-53','>=54')),
  出生年份分组=case_when(
    between(出生年份,1945,1949) ~ 1,
    between(出生年份,1950,1954) ~ 2,
    between(出生年份,1955,1959) ~ 3,
    between(出生年份,1960,1964) ~ 4,
    between(出生年份,1965,1969) ~ 5,
    between(出生年份,1970,1974) ~ 6,
    between(出生年份,1975,1979) ~ 7,
  ),
  BMI分组=case_when(
    BMI<24 & BMI>=18.5 ~ 0,#正常
    BMI<18.5 ~ 1,#偏瘦
    BMI<28 & BMI>=24 ~ 2,#超重
    BMI>=28 ~ 3#肥胖
  )
)%>%filter(出生年份<=1979 & 出生年份>=1945,!is.na(绝经),!is.na(初潮年龄))
#分析异常值
summary(screening3)
with(screening3,table(出生年份))
with(screening3,table(初潮年龄))
with(screening3,table(绝经年龄))
with(screening3,table(生育次数))
with(screening3,table(哺乳月份))
with(screening3,table(流产))
with(screening3,table(人工流产))
with(screening3,table(自然流产))
with(screening3,table(人工流产次数))
with(screening3,table(教育))
with(screening3,table(地区))
with(screening3,table(流产))
with(screening3,table(生育次数,生育))
with(screening3,table(绝经年龄,绝经))
with(screening3,table(哺乳月份,哺乳))
with(screening3,table(出生年份,地区))
with(screening3,table(生育年份,地区))#
with(screening3,table(出生年份,绝经年龄))
with(screening3,table(出生年份,生育))
with(screening3,table(生育年份,初次生育年龄))
#基线描述(以地区分组)
table1::table1(~初筛年龄+出生队列++地区+教育+就业状况+职业+初潮年龄分组+初潮年龄+绝经+婚姻+
                    绝经年龄分组+绝经年龄+生育+生育次数分组+生育次数+首次生育年龄分组+初次生育年龄+
                     哺乳+哺乳时间分组+哺乳月份+流产情况分组+流产+人工流产+自然流产+人工流产次数 ,data=screening3,render.categorical=my.render.cat)

#出生年龄分组
screening3%>%group_by(出生年份分组)%>%summarise(n=n())%>%mutate(percenr=n/sum(n))
#初潮年龄
summary(screening3$初潮年龄)
screening3%>%group_by(出生年份分组)%>%summarise(n=n(),mean=mean(初潮年龄,na.rm=T),sd=sd(初潮年龄,na.rm=T))
#绝经
summary(screening3$绝经年龄)
with(screening3,table(绝经,is.na(绝经年龄)))
with(subset(screening3,筛查年份==2017),table(绝经,is.na(绝经年龄)))
with(screening3,table(绝经,出生年份分组))
prop.table(with(screening3,table(绝经,出生年份分组)),margin = 2)
screening3%>%group_by(出生年份分组)%>%summarise(mean=mean(绝经年龄,na.rm = T),sd=sd(绝经年龄,na.rm = T))
#教育
summary(screening3$教育)#缺失33个
with(screening3,table(教育,出生年份分组))
prop.table(with(screening3,table(教育,出生年份分组)),margin = 2)
#地区
summary(screening3$地区)
with(screening3,table(地区,出生年份分组))
prop.table(with(screening3,table(地区,出生年份分组)),margin = 2)
#生育
summary(screening3$生育)
with(screening3,table(生育,出生年份分组))
prop.table(with(screening3,table(生育,出生年份分组)),margin = 2)
#生育次数
summary(screening3$生育次数)
with(screening3,table(生育,is.na(生育次数)))#有278个生育的女性的初次生育年龄缺失。
summary(screening3$生育次数分组)
prop.table(table(screening3$生育次数分组))
with(screening3,table(生育次数分组,出生年份分组))
prop.table(with(screening3,table(生育次数分组,出生年份分组)),margin = 2)
screening3%>%group_by(出生年份)
#初次生育年龄
summary(screening3$初次生育年龄)
with(screening3,table(is.na(生育次数),is.na(初次生育年龄)))
screening3%>%filter(!is.na(生育次数))%>%group_by(首次生育年龄分组)%>%summarise(n=n())
#哺乳
with(screening3,table(is.na(生育次数),is.na(哺乳)))
screening3%>%filter(!is.na(生育次数))%>%group_by(哺乳)%>%summarise(n=n())
#哺乳时间

with(screening3,table(is.na(生育次数),is.na(哺乳月份)))
screening3%>%filter(!is.na(生育次数))%>%group_by(哺乳时间分组)%>%summarise(n=n())
screening3%>%filter(!is.na(生育次数))%>%group_by(哺乳时间分组)%>%summarise(n=n())
screening3%>%filter(!is.na(生育次数),哺乳=='是')%>%group_by(哺乳时间分组)%>%summarise(n=n())
mean(screening3[which(!is.na(screening3$生育次数) & screening3$哺乳=='是'),c('哺乳月份')],na.rm=T)
sd(screening3[which(!is.na(screening3$生育次数) & screening3$哺乳=='是'),c('哺乳月份')],na.rm=T)
a<-screening3%>%filter(生育年份<2011,生育年份>=1965)%>%group_by(生育年份)%>%summarise(mean=mean(哺乳月份,na.rm=T))
print(a,n=47)
a1<-screening3%>%filter(地区=="市区",生育年份<2011,生育年份>=1965)%>%group_by(生育年份)%>%summarise(mean=mean(哺乳月份,na.rm=T))
print(a1,n=47)
a2<-screening3%>%filter(地区=="郊区",生育年份<2011,生育年份>=1965)%>%group_by(生育年份)%>%summarise(n=n(),mean=mean(哺乳月份,na.rm=T))
print(a2,n=47)


#教育，地区，年份
with(subset(screening3,地区=='市区'),table(出生年份分组,教育))
with(subset(screening3,地区=='市区'),prop.table(table(出生年份分组,教育),margin = 2))
with(subset(screening3,地区=='郊区'),table(出生年份分组,教育))
with(subset(screening3,地区=='郊区'),prop.table(table(出生年份分组,教育),margin = 2))
#趋势性分析
#初潮年龄
#初潮年龄与出生年份:14.9-13.4
screening3%>%filter(初潮年龄>7,出生年份<=1975)%>%group_by(出生年份)%>%summarise(mean=mean(初潮年龄,na.rm=TRUE))%>%
ggplot(aes(x=出生年份,y=mean))+geom_smooth(method='loess',se=FALSE)+geom_point()+
  scale_x_continuous(breaks = seq(1945,1978,5))+mytheme+labs(y='初潮年龄(均值)',title='初潮年龄\n(n=79927)')
#初潮年龄与出生年份(教育水平分层)
with(screening3,table(is.na(初潮年龄),is.na(教育)))

screening3%>%filter(初潮年龄>7,!is.na(教育),出生年份<=1975)%>%group_by(教育,出生年份)%>%summarise(mean=mean(初潮年龄,na.rm=TRUE))%>%
  ggplot(aes(x=出生年份,y=mean,color=教育,linetype=教育))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1945,1975,5))+mytheme+labs(y='初潮年龄(均值)',title='教育水平分层\n(n=79918)' )
##初潮年龄与出生年份(教育水平分层,地区分面)
with(subset(screening3,地区=='市区'),table(is.na(初潮年龄),is.na(教育)))#65063
with(subset(screening3,地区=='郊区'),table(is.na(初潮年龄),is.na(教育)))#14855
region<-c('市区\n(n=65063)','郊区\n(n=14855)')
names(region)<-c('市区','郊区')
screening3%>%filter(初潮年龄>7,!is.na(教育),出生年份<=1975)%>%group_by(地区,教育,出生年份)%>%summarise(mean=mean(初潮年龄,na.rm=TRUE))%>%
  ggplot(aes(x=出生年份,y=mean,color=教育,linetype=教育))+
  geom_smooth(method='loess',se=FALSE)+facet_wrap(.~地区,nrow=1,labeller = labeller(地区=region))+
  scale_x_continuous(breaks = seq(1945,1975,5))+mytheme+labs(y='初潮年龄(均值)')
#初潮年龄与出生年份(地区分层)
with(screening3,table(is.na(初潮年龄),is.na(地区)))#79927

screening3%>%filter(初潮年龄>7,出生年份<=1975)%>%group_by(出生年份,地区)%>%summarise(mean=mean(初潮年龄,na.rm=TRUE))%>%
  ggplot(aes(x=出生年份,y=mean,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+geom_point()+
  scale_x_continuous(breaks = seq(1945,1975,5))+mytheme+labs(y='初潮年龄(均值)',title='地区分层\n(n=79927)')


#生育次数
with(screening3,table(生育年份,生育次数))
with(subset(screening3,生育年份<2011 & 生育年份>=1965),table(is.na(生育年份),is.na(生育次数)))
screening3%>%filter(生育年份<2011,生育年份>=1965)%>%group_by(生育年份)%>%summarise(mean=mean(生育次数,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean))+geom_smooth(method='loess',se=FALSE)+geom_point()+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='生育次数(均值)',title='生育次数\n(n=75987)')

#生育次数与生育年份(地区分层)
screening3%>%filter(生育年份<2011,生育年份>=1965)%>%group_by(生育年份,地区)%>%summarise(mean=mean(生育次数,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='生育次数(均值)',title='地区分层\n(n=75987)')
#生育次数与生育年份(教育分层)
screening3%>%filter(生育年份<2011,生育年份>=1965,!is.na(教育))%>%group_by(生育年份,教育)%>%summarise(mean=mean(生育次数,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean,color=教育,linetype=教育))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='生育次数(均值)',title='教育分层\n(n=75987)')

#初次生育年龄
with(subset(screening3,生育年份<2011 & 生育年份>=1965),table(is.na(生育年份),is.na(初次生育年龄)))
b1<-screening3%>%filter(生育年份<2011,生育年份>=1965)%>%group_by(生育年份)%>%summarise(mean=mean(初次生育年龄,na.rm=TRUE))
print(b1,n=46)
b2<-screening3%>%filter(生育年份<2011,生育年份>=1965,地区=='市区')%>%group_by(生育年份)%>%summarise(mean=mean(初次生育年龄,na.rm=TRUE))
print(b2,n=46)
b3<-screening3%>%filter(生育年份<2011,生育年份>=1965,地区=='郊区')%>%group_by(生育年份)%>%summarise(mean=mean(初次生育年龄,na.rm=TRUE))
print(b3,n=46)

screening3%>%filter(生育年份<2011,生育年份>=1965)%>%group_by(生育年份)%>%summarise(mean=mean(初次生育年龄,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean))+geom_smooth(method='loess',se=FALSE)+geom_point()+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='初次生育年龄(均值)',title='初次生育年龄\n(n=76158)')
#初次生育年龄与生育年份(地区分层)
with(screening3,table)
screening3%>%filter(生育年份<2011,生育年份>=1965)%>%group_by(生育年份,地区)%>%summarise(mean=mean(初次生育年龄,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='初次生育年龄(均值)',title='地区分层\n(n=76158)')
#初次生育年龄与生育年份(教育分层)
screening3%>%filter(生育年份<2011,!is.na(教育))%>%group_by(生育年份,教育)%>%summarise(mean=mean(初次生育年龄,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean,color=教育,linetype=教育))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='初次生育年龄(均值)')
#初次生育年龄与生育年份(教育分层,地区分面)
screening3%>%filter(生育年份<2011,生育年份>=1965)%>%filter(!is.na(教育))%>%group_by(地区,教育,生育年份)%>%summarise(mean=mean(初次生育年龄,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean,color=教育,linetype=教育))+geom_smooth(method='loess',se=FALSE)+facet_wrap(.~地区,nrow=1)+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='初次生育年龄(均值)')


#哺乳时间
with(subset(screening3,生育年份<2011 & 生育年份>=1965),table(is.na(生育年份),is.na(哺乳月份)))
screening3%>%filter(生育年份<2011,生育年份>=1965)%>%group_by(生育年份)%>%summarise(mean=mean(哺乳月份,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean))+geom_smooth(method='loess',se=FALSE)+geom_point()+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='哺乳月份(均值)',title='哺乳月份\n(n=65187)')

#哺乳月份与生育年份(地区分层)
screening3%>%filter(生育年份<2011  & 生育年份>=1965)%>%group_by(生育年份,地区)%>%summarise(mean=mean(哺乳月份,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1960,2019,5))+mytheme+labs(y='哺乳月份(均值)',title='地区分层\n(n=65187)')
#哺乳月份与生育年份(教育分层,地区分面)
screening3%>%filter(!is.na(教育),生育年份<2011,生育年份>=1965)%>%group_by(地区,教育,生育年份)%>%summarise(mean=mean(哺乳月份,na.rm=TRUE))%>%
  ggplot(aes(x=生育年份,y=mean,color=教育,linetype=教育))+geom_smooth(method='loess',se=FALSE)+facet_wrap(.~地区,nrow=1)+mytheme+
  scale_x_continuous(breaks = seq(1960,2019,5))+labs(y='哺乳月份(均值)')


#流产情况
#流产率与出生年份(地区分层)
screening3%>%filter(!is.na(流产),出生年份<1975)%>%group_by(地区,出生年份,流产)%>%summarise(n=n())%>%group_by(出生年份,地区)%>%
  mutate(percent=n/sum(n))%>%filter(流产=='是')%>%ggplot(aes(x=出生年份,y=percent,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1944,1978,5))+mytheme+labs(y='流产率')
#人工流产率与出生年份(地区分层)
screening3%>%filter(出生年份<1965)%>%group_by(地区,出生年份,人工流产)%>%summarise(n=n())%>%group_by(出生年份,地区)%>%
  mutate(percent=n/sum(n))%>%filter(人工流产=='是')%>%ggplot(aes(x=出生年份,y=percent,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1944,1978,5))+mytheme+labs(y='人工流产率')
#自然流产率与出生年份
screening3%>%filter(出生年份<1965)%>%group_by(地区,出生年份,自然流产)%>%summarise(n=n())%>%group_by(出生年份,地区)%>%
  mutate(percent=n/sum(n))%>%filter(自然流产=='是')%>%ggplot(aes(x=出生年份,y=percent,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1944,1978,5))+mytheme+labs(y='自然流产率')


#绝经年龄
#绝经年龄与出生年份

num<-screening3%>%filter(绝经年龄>39)%>%group_by(出生年份)%>%summarise(n=n(),mean=mean(绝经年龄,na.rm=TRUE))
print(num,n=35)
with(subset(screening3,绝经年龄>39 & 出生年份<=1970),table(is.na(出生年份),is.na(绝经年龄)))
screening3%>%filter(绝经年龄>39,出生年份<=1970)%>%group_by(出生年份)%>%summarise(mean=mean(绝经年龄,na.rm=TRUE))%>%
  ggplot(aes(x=出生年份,y=mean))+geom_smooth(method='loess',se=FALSE)+geom_point()+
  scale_x_continuous(breaks = seq(1945,1979,5))+mytheme+labs(y='绝经年龄(均值)',title='绝经年龄\n(n=58438)')
#绝经年龄与出生年份(地区分层)
#with(subset(screening3,绝经年龄>39 & 出生年份<=1970 & 地区=='市区'),table(is.na(出生年份),is.na(绝经年龄)))#48965
#with(subset(screening3,绝经年龄>39 & 出生年份<=1970 & 地区=='郊区'),table(is.na(出生年份),is.na(绝经年龄)))#9473
#region<-c('市区\n(n=65063)','郊区\n(n=14855)')
#names(region)<-c('市区','郊区')
with(subset(screening3,绝经年龄>39 & 出生年份<=1970),table(is.na(出生年份),is.na(绝经年龄)))
screening3%>%filter(绝经年龄>39,出生年份<=1970)%>%group_by(出生年份,地区)%>%summarise(mean=mean(绝经年龄,na.rm=TRUE))%>%
  ggplot(aes(x=出生年份,y=mean,color=地区,linetype=地区))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1944,1978,5))+mytheme+labs(y='绝经年龄(均值)',title='地区分层\n(n=58438)')
#绝经年龄与出生年份(教育分层)
screening3%>%filter(绝经年龄>39,出生年份<=1970,!is.na(教育))%>%group_by(出生年份,教育)%>%summarise(mean=mean(绝经年龄,na.rm=TRUE))%>%
  ggplot(aes(x=出生年份,y=mean,color=教育,linetype=教育))+geom_smooth(method='loess',se=FALSE)+
  scale_x_continuous(breaks = seq(1944,1978,5))+mytheme+labs(y='绝经年龄(均值)')


#生理生育因素与BMI的关系性研究
#初潮与BMI
screening3%>%group_by(初潮年龄)%>%summarise(BMI=mean(BMI,na.rm=T))%>%
ggplot(aes(x=初潮年龄,y=BMI))+geom_smooth(method='loess',se=FALSE)+
mytheme+labs(y='BMI',title='初潮年龄')
#绝经年龄与BMI
screening3%>%group_by(绝经年龄)%>%summarise(BMI=mean(BMI,na.rm=T))%>%
  ggplot(aes(x=绝经年龄,y=BMI))+geom_smooth(method='loess',se=FALSE)+
  mytheme+labs(y='BMI',title='初潮年龄')
















