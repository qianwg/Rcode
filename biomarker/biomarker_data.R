biomarker<-import('~/data/biomark2017-2019(剔除自身癌).sav')
pepsinogen<-biomarker%>%filter(!is.na(PGI))%>%transmute(
  ID=ID_BLAST,name=name,
  PG_pos=factor(ifelse(PGI<=70 & PG_ratio<=3,1,0)),
  PG1=PGI,PG2=PGII,PGR=PG_ratio,CA199=CA199,CA199_pos=ifelse(CA199>27,1,0),
  gastric_sim=ifelse(CATPFath==16 | CATPMoth==16 | CATPBrot==16 | Catpbrot1==16 |Catpbrot2==16 |
                       CATPSist==16 | Catpsist1==16 | Catpsist2==16 | CATPChil==16 | Catpchil1==16 | Catpchil2==16,1,0),
  gastric_sim=ifelse(is.na(gastric_sim),0,gastric_sim),
  age=age,
  age_risk=factor(ifelse(age<=49,0,ifelse(age>=60,2,1))),
  sex_risk=ifelse(sex_check==1,0,1),
  marriage_risk=case_when(
    marriag==1 ~ 1,
    marriag==2 ~ 2,
    marriag==3  | marriag==4 ~ 3
  ),
  marriage_risk=factor(marriage_risk,levels = c(1,2,3),labels=c('已婚','未婚','离婚或丧偶')),
  education_risk=case_when(
    educati==1 | educati==2 | educati==3 ~ 1,
    educati==4 ~ 2,
    educati==5 | educati==6 ~ 3,
  ),
  education_risk=factor(education_risk,levels=c(1,2,3),labels=c('初中及以下','高中/中专/技校','大学及以上')),
  income_risk=factor(income,levels = c(1,2,3,4),labels=c('<3000','3000-4999','5000-9999','>10000')),
  employm_risk=factor(employm,levels=c(1,2,3,4),labels=c('在业','离退休','失业/下岗/待业','家务/无业')),
  smk_risk=ifelse(is.na(smk_risk),0,smk_risk),
  BMI=10000*weight/(height*height),
  BMI_risk=case_when(
    BMI<24 & BMI>=18.5 ~ 0,#正常
    BMI<18.5 ~ 1,#偏瘦
    BMI<28 & BMI>=24 ~ 2,#超重
    BMI>=28 ~ 3#肥胖
  ),
  BMI_risk=factor(BMI_risk,levels = c(0,1,2,3),labels=c('正常','偏瘦','超重','肥胖')),
  #BMI_risk=factor(ifelse(10000*weight/(height*height)<24,0,ifelse(10000*weight/(height*height)<28,1,2))),
  #饮食
  alcohol_risk=ifelse(alcohol==2 & !is.na(alcohol),1,0),
  tea_risk=ifelse(tea==2 & !is.na(tea),1,0),
  coffee_risk=ifelse(coffee==2 & !is.na(coffee),1,0),#咖啡
  yogurt_risk=ifelse(yogurt==2 & !is.na(yogurt),1,0),#酸奶
  veget_risk=ifelse(veget==2 & !is.na(veget),1,0),#蔬菜
  fruit_risk=ifelse(fruit==2 & !is.na(fruit),1,0),#水果
  grain_risk=ifelse(grain==2 & !is.na(grain),1,0),  #谷类
  egg_risk=ifelse(egg==2 & !is.na(egg),1,0),  #鸡蛋
  cereal_risk=ifelse(cereal==2 & !is.na(cereal),1,0),  #杂粮
  beans_risk=ifelse(beans==2 & !is.na(beans),1,0),  #豆类
  nuts_risk=ifelse(nuts==2 & !is.na(nuts),1,0),  #坚果
  garlic_risk=ifelse(garlic==2 & !is.na(garlic),1,0),  #大蒜
  fungus_risk=ifelse(fungus==2 & !is.na(fungus),1,0),  #菌类
  fried_risk=ifelse(fried==2 & !is.na(fried),1,0),  #油炸
  barbecued_risk=ifelse(barbecued==2 & !is.na(barbecued),1,0),  #烧烤
  smked_risk=ifelse(smked==2 & !is.na(smked),1,0),  #熏制
  sauced_risk=ifelse(sauced==2 & !is.na(sauced),1,0),  #酱治
  salty_risk=ifelse(salty==2 & !is.na(salty),1,0),  #偏咸
  salted_risk=ifelse(salted==2 & !is.na(salted),1,0),  #腌制
  #手机使用时间
  cellphoneh_risk=factor(cellphoneh,levels=c(1,2,3,4),labels=c('少于3小时','3-6小时','7-12小时','13小时及以上')),
  #基础性疾病
  disea14_risk=ifelse(Disea14==2 & !is.na(Disea14),1,0),#食管或胃上皮内瘤变
  disea15_risk=ifelse(Disea15==2 & !is.na(Disea15),1,0),#十二指肠溃疡
  disea17_risk=ifelse(Disea17==2 & !is.na(Disea17),1,0),#萎缩性胃炎
  disea18_risk=ifelse(Disea18==2 & !is.na(Disea18),1,0),#胃溃疡
  disea19_risk=ifelse(Disea19==2 & !is.na(Disea19),1,0),#胃息肉
  disea20_risk=ifelse(Disea20==2 & !is.na(Disea20),1,0),#幽门螺杆菌感染史
  disea22_risk=ifelse(Disea22==2 & !is.na(Disea22),1,0),#胃粘膜异性增生
  disea23_risk=ifelse(Disea23==2 & !is.na(Disea23),1,0),#胃肠上皮化生
  disea24_risk=ifelse(Disea24==2 & !is.na(Disea24),1,0),#残胃
  disea28_risk=ifelse(Disea28==2 & !is.na(Disea28),1,0),#糖尿病
  disea29_risk=ifelse(Disea29==2 & !is.na(Disea29),1,0),#高血压
  disea30_risk=ifelse(Disea30==2 & !is.na(Disea30),1,0),#高血脂
  disea31_risk=ifelse(Disea31==2 & !is.na(Disea31),1,0),#冠心病
  disea32_risk=ifelse(Disea32==2 & !is.na(Disea32),1,0),#中风
)%>%transmute(ID=ID,name=name,PG1,PG2,PGR,CA199=CA199,CA199_pos=CA199_pos,
                                 n=as.character(1:length(PG1)),PG_pos=PG_pos,
                                 胃癌家族史=gastric_sim,年龄=age_risk,性别=sex_risk,婚姻=marriage_risk,
                                 教育=education_risk,#萎缩性胃炎=disea17_risk,
                                 家庭收入=income_risk,BMI=BMI_risk,饮酒=alcohol_risk,喝茶=tea_risk,
                                 酸奶=yogurt_risk,咖啡=coffee_risk,蔬菜=veget_risk,水果=fruit_risk,谷类= grain_risk,
                                 鸡蛋=egg_risk,杂粮=cereal_risk,豆类=beans_risk,坚果=nuts_risk,
                                 大蒜=garlic_risk,菌类=fungus_risk,油炸=fried_risk,烧烤=barbecued_risk,
                                 熏制=smked_risk,酱制=sauced_risk,偏咸=salty_risk,腌制=salted_risk,
                                 吸烟=factor(smk_risk),手机使用时间=cellphoneh_risk,十二指肠溃疡=disea15_risk,
                                 胃溃疡=disea18_risk,胃息肉=disea19_risk,幽门螺杆菌感染史=disea20_risk,
                                 癌前病变=ifelse(disea14_risk==1 |disea22_risk==1 | disea23_risk==1 | disea17_risk==1,1,0),
                                 #胃肠上皮化生=disea23_risk,
                                 残胃=disea24_risk,糖尿病=disea28_risk,高血压=disea29_risk,
                                 高血脂=disea30_risk,冠心病=disea31_risk,年龄2=age
)

