biomarker<-import('~/data/biomark2017-2019(剔除自身癌).sav')
#baseline risk factors
CA153<-biomarker%>%filter(sex_check==2 & !is.na(CA153) & CA153!=0)%>%transmute(
  CA153=CA153,CA153_pos=factor(ifelse(CA153>25,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  FATH_sim=ifelse(cancerFH==2 | !is.na(CATPFath) |!is.na(CATPMoth) | !is.na(CATPBrot) |
                    !is.na(Catpbrot1) |!is.na(Catpbrot2)| !is.na(Catpsist1) | !is.na(CATPSist) |
                    !is.na(Catpsist2) |!is.na(CATPChil) | !is.na(Catpchil1) | !is.na(Catpchil2),1,0),
  FATH_sim=factor(ifelse(is.na(FATH_sim),0,FATH_sim),levels=c(0,1),labels = c('否','是')),
  breast_sim=ifelse(CATPMoth==47 |  CATPSist==47 | Catpsist1==47 | Catpsist2==47 | CATPChil==47 | Catpchil2==47,1,0),
  breast_sim=factor(ifelse(is.na(breast_sim),0,breast_sim),levels=c(0,1),labels=c('否','是')),
  age_risk=ifelse(age<=49,'0',ifelse(age>=60,'2','1')),
  age_risk=factor(age_risk,levels = c(0,1,2),labels = c('<49','50-60','>60')), 
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
  blood_risk=factor(bloodtp,levels = c(1,2,3,4,5),labels=c('A','B','O','AB','不详')),
  smk_risk=factor(smoking,levels=c(1,2,3),labels=c('从不吸烟','目前仍在吸烟','以前吸烟')),
  psmk_risk=case_when(
    passivesmk==1 | is.na(passivesmk)~ 0,
    passivesmk==2 & psmkyrs<10 ~ 1,
    passivesmk==2 & psmkyrs>=10 ~ 2,
  ),
  psmk_risk=factor(psmk_risk),
  BMI_risk=case_when(
    BMI<24 & BMI>=18.5 ~ 0,#正常
    BMI<18.5 ~ 1,#偏瘦
    BMI<28 & BMI>=24 ~ 2,#超重
    BMI>=28 ~ 3#肥胖
  ),
  BMI_risk=factor(BMI_risk,levels = c(0,1,2,3),labels=c('正常','偏瘦','超重','肥胖')),
  #饮食
  alcohol_risk=ifelse(alcohol==2 & !is.na(alcohol),1,0),#饮酒
  tea_risk=ifelse(tea==2 & !is.na(tea),1,0),#饮茶
  yogurt_risk=ifelse(yogurt==2 & !is.na(yogurt),1,0),#酸奶
  coffee_risk=ifelse(coffee==2 & !is.na(coffee),1,0),#coffee
  veget_risk=ifelse(veget==2 & !is.na(veget),1,0),#蔬菜
  fruit_risk=ifelse(fruit==2 & !is.na(fruit),1,0),#水果
  grain_risk=ifelse(grain==2 & !is.na(grain),1,0),#谷类
  egg_risk=ifelse(egg==2 & !is.na(egg),1,0),#鸡蛋
  cereal_risk=ifelse(cereal==2 & !is.na(cereal),1,0),#杂粮
  beans_risk=ifelse(beans==2 & !is.na(beans),1,0),#豆类
  nuts_risk=ifelse(nuts==2 & !is.na(nuts),1,0),#坚果
  fungus_risk=ifelse(fungus==2 & !is.na(fungus),1,0),#菌类  
  #饮食偏好
  salty_risk=ifelse(salty==2 & !is.na(salty),1,0),
  salted_risk=ifelse(salted==2 & !is.na(salted),1,0),
  fried_risk=factor(ifelse(fried==2 & !is.na(fried),1,0),levels=c(0,1),labels=c('否','是')),#油炸
  barbecued_risk=factor(ifelse(barbecued==2 & !is.na(barbecued),1,0),levels=c(0,1),labels=c('否','是')),#烧烤
  smked_risk=factor(ifelse(smked==2 & !is.na(smked),1,0),levels=c(0,1),labels=c('否','是')),#熏制
  #体育锻炼
  exercise_risk=factor(ifelse(exercise==2 & !is.na(exercise),1,0),levels=c(0,1),labels=c('否','是')),
  jog_risk=factor(ifelse(jog==2,1,0),levels=c(0,1),labels=c('否','是')),#快走
  taichi_risk=factor(ifelse(taichi==2,1,0),levels=c(0,1),labels=c('否','是')),#太极
  fitdance_risk=factor(ifelse(fitdance==2,1,0),levels=c(0,1),labels=c('否','是')),#广场舞
  yoga_risk=factor(ifelse(yoga==2,1,0),levels=c(0,1),labels=c('否','是')),#瑜伽
  swim_risk=factor(ifelse(swim==2,1,0),levels=c(0,1),labels=c('否','是')),#游泳
  run_risk=factor(ifelse(run==2,1,0),levels=c(0,1),labels=c('否','是')),#跑步
  ball_risk=factor(ifelse(ball==2,1,0),levels=c(0,1),labels=c('否','是')),#球类
  apparatus_risk=factor(ifelse(apparatus==2,1,0),levels=c(0,1),labels=c('否','是')),#器械
  #静态时间
  sedentaryh_risk=ifelse(is.na(sedentaryh),1,sedentaryh),
  #手机使用时间
  cellphoneh_risk=factor(cellphoneh,levels=c(1,2,3,4),labels=c('少于3小时','3-6小时','7-12小时','13小时及以上')),
  #基础性疾病
  disea25_risk=factor(ifelse(Disea25==2 & !is.na(Disea25) & Year==2019,1,0),levels=c(0,1),labels=c('否','是')),#乳腺小叶不典型增生2019
  disea26_risk=factor(ifelse(Disea26==2 & !is.na(Disea26) & Year==2019,1,0),levels=c(0,1),labels=c('否','是')),#乳腺导管不典型增生2019
  disea27_risk=factor(ifelse(Disea27==2 & !is.na(Disea27) & Year==2019,1,0),levels=c(0,1),labels=c('否','是')),#乳腺小叶原位癌2019
  disea19_2_risk=factor(ifelse(Disea19_2==2 & !is.na(Disea19_2) & Year<2019,1,0),levels=c(0,1),labels=c('否','是')),#乳腺重度不典型增生2017-2018
  disea20_2_risk=factor(ifelse(Disea20_2==2 & !is.na(Disea20_2) & Year<2019,1,0),levels=c(0,1),labels=c('否','是')),#乳腺纤维瘤2017-2018
  disea28_risk=factor(ifelse(Disea28==2 & !is.na(Disea28),1,0),levels=c(0,1),labels=c('否','是')),#糖尿病
  disea29_risk=factor(ifelse(Disea29==2 & !is.na(Disea29),1,0),levels=c(0,1),labels=c('否','是')),#高血压
  disea30_risk=factor(ifelse(Disea30==2 & !is.na(Disea30),1,0),levels=c(0,1),labels=c('否','是')),#高血脂
  disea31_risk=factor(ifelse(Disea31==2 & !is.na(Disea31),1,0),levels=c(0,1),labels=c('否','是')),#冠心病
  disea32_risk=factor(ifelse(Disea32==2 & !is.na(Disea32),1,0),levels=c(0,1),labels=c('否','是')),#中风
  #职业暴露
  cadmium_risk=factor(ifelse(cadmium==2,1,0),levels=c(0,1),labels=c('否','是')),#镉
  asbestos_risk=factor(ifelse(asbestos==2,1,0),levels=c(0,1),labels=c('否','是')),#石棉
  nickel_risk=factor(ifelse(nickel==2,1,0),levels=c(0,1),labels=c('否','是')),#镍
  arsenic_risk=factor(ifelse(arsenic==2,1,0),levels=c(0,1),labels=c('否','是')),#砷
  radon_risk=factor(ifelse(radon==2,1,0),levels=c(0,1),labels=c('否','是')),#氡
  chloroethy_risk=factor(ifelse(chloroethy==2,1,0),levels=c(0,1),labels=c('否','是')),#氯乙烯
  Xray_risk=factor(ifelse(Xray==2,1,0),levels=c(0,1),labels=c('否','是')),#X射线
  benzene_risk=factor(ifelse(benzene==2,1,0),levels=c(0,1),labels=c('否','是')),#苯
  #女性生理与生育
  agemen_risk=factor(ifelse(agemenarch<13 & !is.na(agemenarch),1,0),levels=c(0,1),labels=c('>=13','<13')),#初潮年龄
  #agepau_risk=ifelse(agemenopau>=55 & agemenopau<99 & !is.na(agemenopau),1,0),#绝经年龄menopause
  agepau_risk=case_when(
    menopause==1 | is.na(menopause) ~ 0,
    menopause==2 & agemenopau>=55 ~ 1,
    menopause==2 & agemenopau<55 ~ 2
  ),
  agepau_risk=factor(agepau_risk,levels = c(0,1,2),labels = c('未绝经','绝经且绝经年龄<55','绝经且绝经年龄>=55')),
  oratra_risk=factor(ifelse(oralcontra==2 & !is.na(oralcontra),1,0),levels=c(0,1),labels=c('否','是')),#口服避孕药
  hrt_risk=factor(ifelse(HRT==2 & !is.na(HRT),1,0),levels=c(0,1),labels=c('否','是')),#激素代替治疗
  indabt_risk=factor(ifelse(induabort>=2 & !is.na(induabort),1,0),levels=c(0,1),labels=c('否','是')),#人工流产大于1次
  #妇科手术
  sterilizat_risk=factor(ifelse(sterilizat==2 & !is.na(sterilizat),1,0)),#绝育手术
  hysterecto_risk=factor(ifelse(hysterecto==2 & !is.na(hysterecto),1,0)),#子宫摘除术
  ovariectom_risk=factor(ifelse(ovariectom==2 & !is.na(ovariectom),1,0)),#卵巢摘除术
  #结局
  CA=factor(ifelse(CA==1,1,0),levels = c(0,1),labels=c('Negative','Positive')),CA_type=CA_type,
  
)%>%transmute(CA153=CA153,CA153_pos=CA153_pos,癌症家族史=FATH_sim,乳腺癌家族史=breast_sim,
              年龄=age_risk,
              家庭收入=income_risk,就业状况=employm_risk,BMI=BMI_risk,
              偏咸=factor(salty_risk),腌制=factor(salted_risk),饮酒=factor(alcohol_risk),喝茶=factor(tea_risk),酸奶=factor(yogurt_risk),
              吸烟=smk_risk,被动吸烟=factor(psmk_risk),婚姻=marriage_risk,教育=education_risk,
              血型=blood_risk,蔬菜=factor(veget_risk),水果=factor(fruit_risk),谷类=factor(grain_risk),鸡蛋=factor(egg_risk),
              杂粮=factor(cereal_risk),豆类=factor(beans_risk),坚果=factor(nuts_risk),菌类=factor(fungus_risk),油炸=fried_risk,
              烧烤=barbecued_risk,熏制=smked_risk,
              #运动=factor(exercise_risk),快走=jog_risk,太极=taichi_risk,广场舞=fitdance_risk,瑜伽=yoga_risk,游泳=swim_risk,跑步=run_risk,球类=ball_risk,器械=apparatus_risk,
              静态时间=factor(sedentaryh_risk),手机使用时间=cellphoneh_risk,乳腺小叶不典型增生=disea25_risk,
              乳腺导管不典型增生=disea26_risk,乳腺重度不典型增生=disea19_2_risk,乳腺纤维瘤=disea20_2_risk,
              糖尿病=disea28_risk,高血压=disea29_risk,高血脂=disea30_risk,
              冠心病=disea31_risk,中风=disea32_risk,
              #镉=cadmium_risk,石棉=asbestos_risk,镍=nickel_risk,砷=arsenic_risk,氡=radon_risk,乳腺小叶原位癌=disea27_risk,
              #氯乙烯=chloroethy_risk,X射线=Xray_risk,苯=benzene_risk,
              初潮年龄=agemen_risk,绝经年龄=agepau_risk,口服避孕药=oratra_risk,
              激素治疗=hrt_risk,人工流产次数=indabt_risk,
              绝育手术=sterilizat_risk,子宫摘除术=hysterecto_risk,卵巢摘除术=ovariectom_risk,
              CA=CA,CA_type=CA_type,
              CA_type_female=ifelse(CA_type=='宫颈癌' | CA_type=='乳腺癌' | CA_type=='输卵管癌' | CA_type=='子宫内膜癌',1,0),
              CA_breast=ifelse(CA_type=='乳腺癌',1,0)
)%>%filter(!is.na(BMI))
#summary(CA125)
