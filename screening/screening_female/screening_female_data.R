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
#读取数据
screening<-import('~/data/女性生理与生育年龄趋势.sav')
#数据处理
screening2<-screening%>%filter(CASelf==1)%>%transmute(
  ID_BLAST=ID_BLAST,
  筛查年份=Year,婚姻=factor(ifelse(marriag<5,marriag,NA)),
  height=ifelse(height<quantile(screening$height,0.001,na.rm = T) | height>quantile(screening$height,0.999,na.rm = T),NA,height),
  weight=ifelse(weight<quantile(screening$weight,0.001,na.rm = T) | weight>quantile(screening$weight,0.999,na.rm = T),NA,weight),
  
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
  绝经=factor(ifelse(!is.na(agemenopau) | menopause==2,2,1),levels = c(1,2),labels=c('否','是')),
  生育=factor(ifelse(delivertim>=1 | deliver==2 ,2,1),levels = c(1,2),labels=c('否','是')),
  生育次数=ifelse(delivertim<quantile(delivertim,0.1,na.rm = TRUE) | delivertim>quantile(delivertim,0.999,na.rm = TRUE),NA,delivertim),
  初次生育年龄=agefirdeli,
  哺乳=ifelse(!is.na(brstfedmth) | breastfeed==2,2,1),
  哺乳=factor(ifelse(is.na(哺乳),1,哺乳),levels=c(1,2),labels=c('否','是')),
  哺乳月份=ifelse(brstfedmth<quantile(brstfedmth,0.001,na.rm = TRUE) | brstfedmth>quantile(brstfedmth,0.99,na.rm = TRUE),NA,brstfedmth),
  流产=factor(ifelse(abortion==2 | !is.na(induabort) | !is.na(sponabort),2,1),levels = c(1,2),labels = c('否','是')),
  人工流产次数=ifelse(induabort<quantile(induabort,0.01,na.rm = TRUE) |  induabort>quantile(induabort,0.999,na.rm = TRUE),NA,induabort),
  自然流产次数=ifelse(sponabort<quantile(sponabort,0.01,na.rm = TRUE) |  sponabort>quantile(sponabort,0.999,na.rm = TRUE),NA,sponabort),
  人工流产=factor(ifelse(is.na(人工流产次数),1,2),levels = c(1,2),labels = c('否','是')),
  自然流产=factor(ifelse(is.na(自然流产次数),1,2),levels = c(1,2),labels = c('否','是')),
  生育年份=出生年份+初次生育年龄,
  流产情况分组=case_when(
    abortion==1 ~ 1,#无流产
    abortion==2 & !is.na(induabort) ~ 2,#有人工流产史
    abortion==2 & !is.na(sponabort) ~ 2,#有自然流产史
  ),
  绝育手术=factor(ifelse(is.na(sterilizat),1,sterilizat),levels = c(1,2),labels = c('否','是')),
  子宫摘除术=factor(ifelse(is.na(hysterecto),1,hysterecto),levels = c(1,2),labels = c('否','是')),
  卵巢摘除术=factor(ifelse(is.na(ovariectom),1,ovariectom),levels = c(1,2),labels = c('否','是'))
  
)%>%transmute(ID_BLAST=ID_BLAST,筛查年份,婚姻, BMI=round(weight/((height/100)^2),2),
                  出生年份,地区,初筛年龄,教育,就业状况,职业,初潮年龄,绝经,绝经年龄,生育,生育次数,
                  初次生育年龄,哺乳,哺乳月份,流产,人工流产,自然流产,人工流产次数,自然流产次数,生育年份,流产情况分组,绝育手术,子宫摘除术,卵巢摘除术,
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
                  生育次数分组=factor(生育次数分组,levels = c(1,2,3,4),labels=c('1次','2次','3-4次','>=5次')),
                  
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
                    出生年份<1945 ~ 0,
                    between(出生年份,1945,1949) ~ 1,
                    between(出生年份,1950,1954) ~ 2,
                    between(出生年份,1955,1959) ~ 3,
                    between(出生年份,1960,1964) ~ 4,
                    between(出生年份,1965,1969) ~ 5,
                    between(出生年份,1970,1974) ~ 6,
                    出生年份>=1975 ~ 7,
                  ),
                出生年份分组=factor(出生年份分组,levels=c(0,1,2,3,4,5,6,7),
                                    labels=c('<1945','1945-1949','1950-1954',
                                             '1955-1959','1960-1964','1965-1969','1970-1974','>=1975')),
                  BMI分组=case_when(
                    BMI<24 & BMI>=18.5 ~ 0,#正常
                    BMI<18.5 ~ 1,#偏瘦
                    BMI<28 & BMI>=24 ~ 2,#超重
                    BMI>=28 ~ 3#肥胖
                  ),
              BMI分组=factor(BMI分组,levels = c(0,1,2,3),labels = c('18.5-23.9','<18.5','24-27.9','>=28'))
)
#%>%filter(出生年份<=1979 & 出生年份>=1945,!is.na(绝经),!is.na(初潮年龄))
