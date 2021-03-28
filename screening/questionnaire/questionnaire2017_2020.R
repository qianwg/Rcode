###2017-2020年基线危险因素评分计算
library(tidyverse)
library(rio)
library(openxlsx)
library(lubridate)
source("~/Rcode/screening/riskscore/riskscore2020_3.R")
source("~/Rcode/screening/riskscore/riskscore2017_2019.R")
##读取2017-2020年数据
screening2020<-read.xlsx("~/data/questionnaire/合并文件.xlsx",detectDates = TRUE)
screening2017_2019<-import("~/data/questionnaire/基础问卷2017-2019.sav")
#整理数据
#2020年
screening2020<-screening2020%>%transmute(
  #个人信息
  ID_BLAST=ID20,Year=year(as.Date(A0)),name_check=NAME,persoID=ID18,
  #自身癌症史
  CASelf=factor(ifelse(B2a_1==1 & !is.na(B2a_1),2,1),levels=c(1,2),labels=c('否','是')),
  #癌症家族史
  cancerFH=factor(ifelse(B2==2,2,1),levels=c(1,2),labels=c('否','是')),
  catpfath=B2a2a,catpmoth=B2a3a,catpbrot1=B2a4a,catpbrot2=B2a5a,
  catpsist1=B2a6a,catpsist2=B2a7a,catpchil1=B2a8a,catpchil2=B2a9a,
   catpgrandpa1=B2a10a,catpgrandmo1=B2a11a,catpgrandpa2=B2a12a,
  catpgrandmo1=B2a13a,catpuncle=B2a14a,catpaunt=B2a15a,couosin1=B2a16a,couosin2=B2a17a,
  age=AGE1,
  sex=A1,
  #人口学特征
   #吸烟与被动吸烟
  smoking=B4,passivesmk=B5,psmkyrs=B5_X,
  cpd=B4a,smkyrs=B4b,
  baonian=cpd*smkyrs/20,
  #BMI
  BMI=10000*A15/(A14*A14),
 #饮食
  alcohol=ifelse(B6_1==1,2,1),
  tea=ifelse(B6_2==1,2,1),
  coffee=ifelse(B6_5==1,2,1),#咖啡
  milk=ifelse(B6_3==1,2,1),#鲜奶
  yogurt=ifelse(B6_4==1,2,1),#酸奶
  sodas=ifelse(B7_1==1,2,1),#碳酸饮料
  juice=ifelse(B7_2==1,2,1),#果汁果味饮料
  teadr=ifelse(B7_3==1,2,1),#茶味饮料
  veget=ifelse(B8_1==1,2,1),#蔬菜
  fruit=ifelse(B8_2==1,2,1),#水果
  grain=ifelse(B8_3==1,2,1),  #谷类
  meat=ifelse(B8_4==1,2,1),#肉类
  egg=ifelse(B8_5==1,2,1),  #鸡蛋
  seafd=ifelse(B9_1==1,2,1),#水产品
  potato=ifelse(B9_2==1,2,1),#薯类
  cereal=ifelse(B9_3==1,2,1),  #杂粮
  beans=ifelse(B9_4==1,2,1),  #豆类
  nuts=ifelse(B9_5==1,2,1),  #坚果
  garlic=ifelse(B9_6==1,2,1),  #大蒜
  fungus=ifelse(B9_7==1,2,1),  #菌类
  fried=ifelse(B10_1==1,2,1),  #油炸
  barbecued=ifelse(B10_2==1,2,1),  #烧烤
  smked=ifelse(B10_3==1,2,1),  #熏制
  sauced=ifelse(B10_5==1,2,1),  #酱治
  salted=ifelse(B10_4==1,2,1),  #腌制
  dried=ifelse(B10_6==1,2,1),  #晒制
  salty=ifelse(B11_1==1,2,1),  #偏咸
  spicy=ifelse(B11_2==1,2,1),  #偏辣
  hot=ifelse(B11_3==1,2,1),  #偏烫
  sour=ifelse(B11_4==1,2,1),  #偏酸
  sweet=ifelse(B11_5==1,2,1),  #偏甜
  hard=ifelse(B11_6==1,2,1),  #偏硬
  #基础性疾病
 disea1=ifelse(B3a1_1==1 & !is.na(B3a1_1),2,1),#弥漫性肺间质纤维化(2019)
 disea2=ifelse(B3a1_2==1 & !is.na(B3a1_2),2,1),#肺结核
 disea3=ifelse(B3a1_3==1 & !is.na(B3a1_3),2,1),#慢性支气管炎
 disea4=ifelse(B3a1_4==1 & !is.na(B3a1_4),2,1),#肺气肿
 disea5=ifelse(B3a1_5==1 & !is.na(B3a1_5),2,1),#哮喘支气管扩张
 disea6=ifelse(B3a1_6==1 & !is.na(B3a1_6),2,1),#矽肺或尘肺
 disea7=ifelse(B3b1_1==1 & !is.na(B3b1_1),2,1),#胆囊息肉
 disea8=ifelse(B3b1_2==1 & !is.na(B3b1_2),2,1),#胆结石
 disea9=ifelse(B3b1_3==1 & !is.na(B3b1_3),2,1),#脂肪肝
 disea10=ifelse(B3b1_4==1 & !is.na(B3b1_4),2,1),#肝硬化
 disea11=ifelse(B3b1_5==1 & !is.na(B3b1_5),2,1),#慢性乙型肝炎
 disea12=ifelse(B3b1_6==1 & !is.na(B3b1_6),2,1),#慢性丙型肝炎
 disea13=ifelse(B3b1_7==1 & !is.na(B3b1_7),2,1),#血吸虫病感染史
 disea14=ifelse(B3c1_1==1 & !is.na(B3c1_1),2,1),#食管或胃上皮内瘤变(2019)
 disea15=ifelse(B3c1_2==1 & !is.na(B3c1_2),2,1),#十二指肠溃疡(2019)
 disea16=ifelse(B3c1_3==1 & !is.na(B3c1_3),2,1),#胃食管反流性疾病(2019)
 disea17=ifelse(B3c1_4==1 & !is.na(B3c1_4),2,1),#萎缩性胃炎
 disea18=ifelse(B3c1_5==1 & !is.na(B3c1_5),2,1),#胃溃疡
 disea19=ifelse(B3c1_6==1 & !is.na(B3c1_6),2,1),#胃息肉
 disea20=ifelse(B3c1_7==1 & !is.na(B3c1_7),2,1),#幽门螺杆菌感染史
 disea22=ifelse(B3c1_9==1 & !is.na(B3c1_9),2,1),#胃粘膜异性或不典型增生
 disea23=ifelse(B3c1_10==1 & !is.na(B3c1_10),2,1),#胃肠上皮化生
 disea24=ifelse(B3c1_11==1 & !is.na(B3c1_11),2,1),#残胃
 disea28=ifelse(B3f_1==1 & !is.na(B3f_1),2,1),#糖尿病
 disea29=ifelse(B3f_2==1 & !is.na(B3f_2),2,1),#高血压
 disea30=ifelse(B3f_3==1 & !is.na(B3f_3),2,1),#高血脂
 disea31=ifelse(B3f_4==1 & !is.na(B3f_4),2,1),#冠心病
 disea32=ifelse(B3f_5==1 & !is.na(B3f_5),2,1),#中风
 disea33=ifelse(B3f_6==1 & !is.na(B3f_6),2,1),#偏头疼
 #锻炼
 #体育锻炼
 jog=ifelse(B16a_1==1 & !is.na(B16a_1),2,1),#快走
 taichi=ifelse(B16a_2==1 & !is.na(B16a_2),2,1),#太极
 fitdance=ifelse(B16a_3==1 & !is.na(B16a_3),2,1),#广场舞
 yoga=ifelse(B16a_4==1 & !is.na(B16a_4),2,1),#瑜伽
 swim=ifelse(B16a_5==1 & !is.na(B16a_5),2,1),#游泳
 run=ifelse(B16a_6==1 & !is.na(B16a_6),2,1),#跑步
 ball=ifelse(B16a_7==1 & !is.na(B16a_7),2,1),#球类
 apparatus=ifelse(B16a_8==1 & !is.na(B16a_8),2,1),#器械
 #职业暴露
 cadmium=ifelse(B24_2==1 & !is.na(B24_2),2,1),#镉
 asbestos=ifelse(B24_1==1 & !is.na(B24_1),2,1),#石棉
 nickel=ifelse(B24_3==1 & !is.na(B24_3),2,1),#镍
 arsenic=ifelse(B24_4==1 & !is.na(B24_4),2,1),#砷
 radon=ifelse(B24_5==1 & !is.na(B24_5),2,1),#氡
 chloroethy=ifelse(B24_6==1 & !is.na(B24_6),2,1),#氯乙烯
 Xray=ifelse(B24_7==1 & !is.na(B24_7),2,1),#X射线
 benzene=ifelse(B24_8==1 & !is.na(B24_7),2,1),#苯
 stress=ifelse(B23==2,2,1),
 #女性生理生育因素
  agemenarch=B32,agemenopau=B33_X,menopause=B33,
  deliver=B34,
  agefirdeli=B34a,
  breastfeed=B34b,
  brstfedmth=B34b_X,
  abortion=B35,
  induabort=B35a_1_X,
  oralcontra=B36,
  hrt=B37,
 #(8)空气污染
 cookingfum=B27,#室内油烟较多或很多
 hometraffi=B28,#家庭住所临街
 worktraffi=B29,#工作场所临街
)
risk2020<-risk_function(screening2020)










