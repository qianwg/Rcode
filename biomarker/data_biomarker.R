#baseline risk factors
rm(list=ls())
biomarker<-import('~/data/biomark2017-2019(剔除自身癌).sav')
baseline<-biomarker%>%transmute(ID_BLAST=ID_BLAST,Year=Year,出生年份=YOB_check,街道=region,
  地区=factor(ifelse(region=='下营' | region=='下营镇' | region=='东二营镇' | region=='别山' | region=='别山镇' |
  region=='官庄' | region=='官庄镇' | region=='桑梓镇' | region=='马伸桥' | region=='马伸桥镇',2,1),levels=c(1,2),labels = c('市内六区','蓟州区')),
  AFP_pos=factor(ifelse(AFP>7,1,0),levels = c(0,1),labels=c('阴性','阳性')),
  CEA_pos=factor(ifelse(CEA>5,1,0),levels = c(0,1),labels=c('阴性','阳性')),
  CA199_pos=factor(ifelse(CA199>27,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  CA125_pos=factor(ifelse(CA125>35,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  CA153_pos=factor(ifelse(CA153>25,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  HBsAg_pos=factor(ifelse(HBsAg>0,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  AFP=AFP,CEA=CEA,CA199=CA199,CA125=CA125,CA153=CA153,
  FATH_sim=ifelse(cancerFH==2 | !is.na(CATPFath) |!is.na(CATPMoth) | !is.na(CATPBrot) |
                    !is.na(Catpbrot1) |!is.na(Catpbrot2)| !is.na(Catpsist1) | !is.na(CATPSist) |
                    !is.na(Catpsist2) |!is.na(CATPChil) | !is.na(Catpchil1) | !is.na(Catpchil2),1,0),
  FATH_sim=factor(ifelse(is.na(FATH_sim),0,FATH_sim),levels=c(0,1),labels = c('否','是')),
  lung_sim=ifelse(CATPFath==34 | CATPMoth==34 | CATPBrot==34 | Catpbrot1==34 |Catpbrot2==34 |
                    CATPSist==34 | Catpsist1==34 | Catpsist2==34 | CATPChil==34 | Catpchil1==34 | Catpchil2==34,1,0),
  lung_sim=factor(ifelse(is.na(lung_sim),0,lung_sim),levels=c(0,1),labels=c('否','是')),
  breast_sim=ifelse(CATPMoth==47 |  CATPSist==47 | Catpsist1==47 | Catpsist2==47 | CATPChil==47 | Catpchil2==47,1,0),
  breast_sim=factor(ifelse(is.na(breast_sim),0,breast_sim),levels=c(0,1),labels=c('否','是')),
  liver_sim=ifelse(CATPFath==24 | CATPMoth==24 | CATPBrot==24 | Catpbrot1==24 |Catpbrot2==24 |
                     CATPSist==24 | Catpsist1==24 | Catpsist2==24 | CATPChil==24 | Catpchil1==24 | Catpchil2==24,1,0),
  liver_sim=factor(ifelse(is.na(liver_sim),0,liver_sim),levels=c(0,1),labels=c('否','是')),
  gastric_sim=ifelse(CATPFath==16 | CATPMoth==16 | CATPBrot==16 | Catpbrot1==16 |Catpbrot2==16 |
                       CATPSist==16 | Catpsist1==16 | Catpsist2==16 | CATPChil==16 |  Catpchil1==16 | Catpchil2==16,1,0),
  gastric_sim=factor(ifelse(is.na(gastric_sim),0,gastric_sim),levels = c(0,1),labels=c('否','是')),
  
  ovaries_sim=ifelse(CATPMoth==72 |  CATPSist==72 | Catpsist1==72 | Catpsist2==72 | CATPChil==72 | Catpchil2==72,1,0),
  ovaries_sim=factor(ifelse(is.na(ovaries_sim),0,ovaries_sim),levels=c(0,1),labels=c('否','是')),
  

 age=age,
  age_risk=ifelse(age<=49,'0',ifelse(age>=60,'2','1')),
  sex_risk=ifelse(sex_check==1,0,1),
  marriage_risk=case_when(
    marriag==1 ~ 1,
    marriag==2 ~ 2,
    marriag==3  | marriag==4 ~ 3
  ),
  marriage_risk=factor(marriage_risk,levels = c(1,2,3),labels=c('已婚','未婚','离婚或丧偶')),
  education_risk=case_when(
    educati==1 | educati==2 ~ 1,
    educati==3 ~ 2,
    educati==4 |  educati==5 | educati==6 ~ 3,
  ),
  education_risk=factor(education_risk,levels=c(1,2,3),labels=c('小学及以下','初中','高中及以上')),
  income_risk=factor(income,levels = c(1,2,3,4),labels=c('<3000','3000-4999','5000-9999','>10000')),
  employm_risk=factor(employm,levels=c(1,2,3,4),labels=c('在业','离退休','失业/下岗/待业','家务/无业')),
  occupat_risk=factor(occupat,levels=c(1,2,3,4,5,6,7,8),labels=c('机关/企事业单位负责人','专业技术人员',
                                                                 '办事人员和相关人员','商业/服务业人员',
                                                                 '农林牧渔水利业生产人员','生产运输设备操作人员',
                                                                 '军人','其它')),
  blood_risk=factor(bloodtp,levels = c(1,2,3,4,5),labels=c('A','B','O','AB','不详')),
  smk_risk=factor(smoking,levels=c(1,2,3),labels=c('从不吸烟','目前仍在吸烟','以前吸烟')),
  smk_risk2=case_when(
    smoking==1 ~ 1,
    smoking==2 | smoking==3  ~ 2,

      ),
  smk_risk2=factor(smk_risk2,levels = c(1,2),
                   labels = c('从不吸烟','目前或过去吸烟')),
  psmk_risk=factor(passivesmk,levels=c(1,2),labels=c('否','是')),
  BMI=10000*weight/(height*height),
  BMI_risk=case_when(
    BMI<18.5 ~ 1, #偏瘦
    BMI<24 & BMI>=18.5  ~ 2,#正常
    BMI<28 & BMI>=24 ~ 3,#超重
    BMI>=28 ~ 4#肥胖
  ),
  BMI_risk=factor(BMI_risk,levels = c(1,2,3,4),labels=c('偏瘦','正常',"超重",'肥胖')),
  #女性生理生育因素
  #女性生理生育因素
  初潮年龄=ifelse(agemenarch<quantile(agemenarch,0.001,na.rm = TRUE) | agemenarch>quantile(agemenarch,0.999,na.rm = TRUE),NA,agemenarch),
  绝经年龄=ifelse(agemenopau<quantile(agemenopau,0.001,na.rm = TRUE) | agemenopau>quantile(agemenopau,0.999,na.rm = TRUE),NA,agemenopau),
  绝经=factor(ifelse(!is.na(agemenopau) | menopause==2,2,1),levels = c(1,2),labels=c('否','是')),
  生育=factor(ifelse(delivertim>=1 | deliver==2 ,2,1),levels = c(1,2),labels=c('否','是')),
  生育次数=ifelse(delivertim<quantile(delivertim,0.1,na.rm = TRUE) | delivertim>quantile(delivertim,0.999,na.rm = TRUE),NA,delivertim),
  初次生育年龄=agefirdeli,
  哺乳=ifelse(!is.na(brstfedmth) | breastfeed==2,2,1),
  哺乳=factor(ifelse(is.na(哺乳),1,哺乳),levels=c(1,2),labels=c('否','是')),
  哺乳月份=ifelse(brstfedmth<quantile(brstfedmth,0.001,na.rm = TRUE) | brstfedmth>quantile(brstfedmth,0.99,na.rm = TRUE),NA,brstfedmth),
  流产=case_when(
    abortion==2 ~ 1,
    !is.na(induabort) ~ 1,
    !is.na(sponabort) ~ 1,
    is.na(abortion) & is.na(sponabort) & is.na(abortion) ~ 0,
    abortion==1 ~ 0
  ),
  流产=factor(流产,levels = c(0,1),labels = c('否','是')),

  人工流产次数=ifelse(induabort<quantile(induabort,0.01,na.rm = TRUE) |  induabort>quantile(induabort,0.999,na.rm = TRUE),NA,induabort),
  自然流产次数=ifelse(sponabort<quantile(sponabort,0.01,na.rm = TRUE) |  sponabort>quantile(sponabort,0.999,na.rm = TRUE),NA,sponabort),
  人工流产=factor(ifelse(is.na(induabort),1,2),levels = c(1,2),labels = c('否','是')),
  自然流产=factor(ifelse(is.na(sponabort),1,2),levels = c(1,2),labels = c('否','是')),
  流产情况分组=case_when(
    abortion==1 ~ 1,#无流产
    abortion==2 & !is.na(induabort) ~ 2,#有人工流产史
    abortion==2 & !is.na(sponabort) ~ 2,#有自然流产史
  ),
  口服避孕药=factor(oralcontra,levels=c(1,2),labels=c('否','是')),雌激素代替治疗=factor(HRT,levels=c(1,2),labels=c('否','是')),
  绝育手术=factor(ifelse(is.na(sterilizat),1,sterilizat),levels = c(1,2),labels = c('否','是')),
  子宫摘除术=factor(ifelse(is.na(hysterecto),1,hysterecto),levels = c(1,2),labels = c('否','是')),
  卵巢摘除术=factor(ifelse(is.na(ovariectom),1,ovariectom),levels = c(1,2),labels = c('否','是')),
  
  #饮食
  alcohol_risk=ifelse(alcohol==2 & !is.na(alcohol),1,0),
  tea_risk=ifelse(tea==2 & !is.na(tea),1,0),
  yogurt_risk=ifelse(yogurt==2 & !is.na(yogurt),1,0),
  veget_risk=ifelse(veget==2 & !is.na(veget),1,0),
  fruit_risk=ifelse(fruit==2 & !is.na(fruit),1,0),
  grain_risk=ifelse(grain==2 & !is.na(grain),1,0),
  egg_risk=ifelse(egg==2 & !is.na(egg),1,0),
  cereal_risk=ifelse(cereal==2 & !is.na(cereal),1,0),
  beans_risk=ifelse(beans==2 & !is.na(beans),1,0),
  nuts_risk=ifelse(nuts==2 & !is.na(nuts),1,0),
  fungus_risk=ifelse(fungus==2 & !is.na(fungus),1,0),
  #饮食偏好
  salty_risk=ifelse(salty==2 & !is.na(salty),1,0),#偏咸
  spicy_risk=ifelse(spicy==2 & !is.na(spicy),1,0),#偏辣
  hot_risk=ifelse(hot==2 & !is.na(hot),1,0),#偏汤
  sour_risk=ifelse(sour==2 & !is.na(sour),1,0),#偏酸
  sweet_risk=ifelse(sweet==2 & !is.na(sweet),1,0),#偏酸
  hard_risk=ifelse(hard==2 & !is.na(hard),1,0),#偏硬
  salted_risk=ifelse(salted==2 & !is.na(salted),1,0),#腌制
  smked_risk=ifelse(smked==2 & !is.na(smked),1,0),#熏制
  sauced_risk=ifelse(sauced==2 & !is.na(sauced),1,0),#酱制
  dried_risk=ifelse(dried==2 & !is.na(dried),1,0),#晒制
  #体育锻炼
  exercise_risk=factor(ifelse(exercise==2 & !is.na(exercise),1,0)),
  jog_risk=factor(ifelse(jog==2 & !is.na(jog),1,0)),#快走
  taichi_risk=factor(ifelse(taichi==2 & !is.na(taichi),1,0)),#太极
  fitdance_risk=factor(ifelse(fitdance==2 & !is.na(fitdance),1,0)),#广场舞
  yoga_risk=factor(ifelse(yoga==2 & !is.na(yoga),1,0)),#瑜伽
  swim_risk=factor(ifelse(swim==2 & !is.na(swim),1,0)),#游泳
  run_risk=factor(ifelse(run==2 & !is.na(run),1,0)),#跑步
  ball_risk=factor(ifelse(ball==2 & !is.na(ball),1,0)),#球类
  apparatus_risk=factor(ifelse(apparatus==2 & !is.na(apparatus),1,0)),#器械
 #出行方式
 出行方式=factor(traffic,levels=c(1,2,3,4,5),labels=c('步行','自行车','公交车/班车/地铁','私家车/出租车','电动车/摩托车')),
 #静态时间
  sedentaryh_risk=case_when(
    sedentaryh==1 ~ 1,
    sedentaryh==2 ~ 2,
    sedentaryh==3 |sedentaryh==4  ~3
  ),
  sedentaryh_risk=factor(sedentaryh_risk,levels=c(1,2,3),labels=c('少于3小时','3-6小时','>=7')),
  #手机使用时间
  cellphoneh_risk=case_when(
    cellphoneh==1 ~ 1,
    cellphoneh==2 ~ 2,
    cellphoneh==3 | cellphoneh==3 ~ 3,
  ),
  cellphoneh_risk=factor(cellphoneh_risk,levels=c(1,2,3),labels=c('少于3小时','3-6小时','7小时及以上')),
  #负性生活事件
  stress_risk=factor(ifelse(stress==2,1,0),levels=c(0,1),labels = c('否','是')),
  depress_risk=factor(ifelse(depress==2,1,0),levels=c(0,1),labels = c('否','是')),
  #基础性疾病
  disea1_risk=factor(ifelse(Disea1==2 & !is.na(Disea1),1,0),levels=c(0,1),labels=c('否','是')),#弥漫性肺间质纤维化
  disea2_risk=factor(ifelse(Disea2==2 & !is.na(Disea2),1,0),levels=c(0,1),labels=c('否','是')),#肺结核
  disea3_risk=factor(ifelse(Disea3==2 & !is.na(Disea3),1,0),levels=c(0,1),labels=c('否','是')),#慢性支气管炎
  disea4_risk=factor(ifelse(Disea4==2 & !is.na(Disea4),1,0),levels=c(0,1),labels=c('否','是')),#肺气肿
  disea5_risk=factor(ifelse(Disea5==2 & !is.na(Disea5),1,0),levels=c(0,1),labels=c('否','是')),#哮喘支气管扩张
  disea6_risk=factor(ifelse(Disea6==2 & !is.na(Disea6),1,0),levels=c(0,1),labels=c('否','是')),#矽肺或尘肺
  disea7_risk=factor(ifelse(Disea7==2 & !is.na(Disea7),1,0),levels=c(0,1),labels=c('否','是')),#胆囊息肉
  disea8_risk=factor(ifelse(Disea8==2 & !is.na(Disea8),1,0),levels=c(0,1),labels=c('否','是')),#胆结石
  disea9_risk=factor(ifelse(Disea9==2 & !is.na(Disea9),1,0),levels=c(0,1),labels=c('否','是')),#脂肪肝
  disea10_risk=factor(ifelse(Disea10==2 & !is.na(Disea10),1,0),levels=c(0,1),labels=c('否','是')),#肝硬化
  disea11_risk=factor(ifelse(Disea11==2 & !is.na(Disea11),1,0),levels=c(0,1),labels=c('否','是')),#慢性乙型肝炎
  disea12_risk=factor(ifelse(Disea12==2 & !is.na(Disea12),1,0),levels=c(0,1),labels=c('否','是')),#慢性丙型肝炎
  disea13_risk=factor(ifelse(Disea13==2 & !is.na(Disea13),1,0),levels=c(0,1),labels=c('否','是')),#血吸虫病感染史
  disea14_risk=factor(ifelse(Disea14==2 & !is.na(Disea14),1,0),levels=c(0,1),labels=c('否','是')),#食管或胃上皮内瘤变
  disea15_risk=factor(ifelse(Disea15==2 & !is.na(Disea15),1,0),levels=c(0,1),labels=c('否','是')),#十二指肠溃疡
  disea16_risk=factor(ifelse(Disea16==2 & !is.na(Disea16),1,0),levels=c(0,1),labels=c('否','是')),#Barrett食管
  disea17_risk=factor(ifelse(Disea17==2 & !is.na(Disea17),1,0),levels=c(0,1),labels=c('否','是')),#萎缩性胃炎
  disea18_risk=factor(ifelse(Disea18==2 & !is.na(Disea18),1,0),levels=c(0,1),labels=c('否','是')),#胃溃疡
  disea19_risk=factor(ifelse(Disea19==2 & !is.na(Disea19),1,0),levels=c(0,1),labels=c('否','是')),#胃息肉
  disea20_risk=factor(ifelse(Disea20==2 & !is.na(Disea20),1,0),levels=c(0,1),labels=c('否','是')),#幽门螺杆菌感染史
  disea21_risk=factor(ifelse(Disea21==2 & !is.na(Disea21),1,0),levels=c(0,1),labels=c('否','是')),#EB病毒感染史
  disea22_risk=factor(ifelse(Disea22==2 & !is.na(Disea22),1,0),levels=c(0,1),labels=c('否','是')),#胃粘膜异性增生
  disea23_risk=factor(ifelse(Disea23==2 & !is.na(Disea23),1,0),levels=c(0,1),labels=c('否','是')),#胃肠上皮化生
  disea24_risk=factor(ifelse(Disea24==2 & !is.na(Disea24),1,0),levels=c(0,1),labels=c('否','是')),#残胃
  disea25_risk=factor(ifelse(Disea20_2==2  & !is.na(Disea20_2),1,0),levels=c(0,1),labels=c('否','是')),#乳腺纤维瘤
  
  disea261_risk=factor(ifelse(Disea25==2 & !is.na(Disea25),1,0),levels=c(0,1),labels=c('否','是')),#乳腺小叶不典型增生
  disea262_risk=factor(ifelse(Disea26==2 & !is.na(Disea26),1,0),levels=c(0,1),labels=c('否','是')),#乳腺导管不典型增生
  disea263_risk=factor(ifelse(Disea19_2==2 & !is.na(Disea19_2),1,0),levels=c(0,1),labels=c('否','是')),#乳腺中重度不典型增生

  disea26_risk=factor(ifelse(disea261_risk=="是" | disea262_risk=="是" | disea263_risk=="是",1,0),levels=c(0,1),labels=c('否','是')),#乳腺不典型增生
  disea27_risk=factor(ifelse(Disea27==2 & !is.na(Disea27),1,0),levels=c(0,1),labels=c('否','是')),#乳腺小叶原位癌
  
  disea28_risk=factor(ifelse(Disea28==2 & !is.na(Disea28),1,0),levels=c(0,1),labels=c('否','是')),#糖尿病
  disea29_risk=factor(ifelse(Disea29==2 & !is.na(Disea29),1,0),levels=c(0,1),labels=c('否','是')),#高血压
  disea30_risk=factor(ifelse(Disea30==2 & !is.na(Disea30),1,0),levels=c(0,1),labels=c('否','是')),#高血脂
  disea31_risk=factor(ifelse(Disea31==2 & !is.na(Disea31),1,0),levels=c(0,1),labels=c('否','是')),#冠心病
  disea32_risk=factor(ifelse(Disea32==2 & !is.na(Disea32),1,0),levels=c(0,1),labels=c('否','是')),#中风
  disea_common=factor(ifelse(disea28_risk=="是" | disea29_risk=="是" | disea30_risk=="是" | disea31_risk=="是" | disea32_risk=="是",2,1),levels=c(1,2),labels=c('否','是')),
  disea_female=factor(ifelse(disea26_risk=="是" | disea25_risk=="是" ,1,0),levels=c(0,1),labels=c('否','是')),#女性良性病变史
  #职业暴露
  cadmium_risk=factor(ifelse(cadmium==2 & !is.na(cadmium),1,0)),#镉
  asbestos_risk=factor(ifelse(asbestos==2 & !is.na(asbestos),1,0)),#石棉
  nickel_risk=factor(ifelse(nickel==2 & !is.na(nickel),1,0)),#镍
  arsenic_risk=factor(ifelse(arsenic==2 & !is.na(arsenic),1,0)),#砷
  radon_risk=factor(ifelse(radon==2 & !is.na(radon),1,0)),#氡
  chloroethy_risk=factor(ifelse(chloroethy==2 & !is.na(chloroethy),1,0)),#氯乙烯
  Xray_risk=factor(ifelse(Xray==2 & !is.na(Xray),1,0)),#X射线
  benzene_risk=factor(ifelse(benzene==2,1,0)),#苯
  #结局
  BI_rads=factor(ifelse(ultrBIRADS>=5,1,0),levels = c(0,1),labels=c('BI-RADS<4A','BI-RADS 4-5')),
  CA=factor(ifelse(CA==1 | is.na(CA),1,0),levels = c(0,1),labels=c('Negative','Positive')),CA_type=CA_type,
  CA_type_breast=ifelse(CA_type=='乳腺癌' | BI_rads=="BI-RADS 4-5",1,0),
)%>%transmute(#指标
  Year=Year,ID_BLAST,出生年份=出生年份,year_group=factor(ifelse(Year==2017,1,2),levels = c(1,2),labels=c('2017年','2018-2019年')),街道=街道,地区=地区,
              CEA=CEA,AFP=AFP,CA199=CA199,CA153=CA153,CA125=CA125,
              CEA_pos=CEA_pos,AFP_pos=AFP_pos,CA199_pos=CA199_pos,CA153_pos=CA153_pos,CA125_pos=CA125_pos,HBsAg_pos=HBsAg_pos,
              #基本人群特征
              癌症家族史=FATH_sim,肺癌家族史=lung_sim,乳腺癌家族史=breast_sim,肝癌家族史=liver_sim,胃癌家族史=gastric_sim,
              卵巢癌=ovaries_sim,年龄连续=age,
              年龄=factor(age_risk,levels=c(0,1,2),labels=c('<50','50-59','>=60')),
              性别=factor(sex_risk,levels=c(0,1),labels=c('男性','女性')),家庭收入=income_risk,
              就业状况=employm_risk,职业类型=occupat_risk,婚姻=marriage_risk,教育=education_risk,血型=blood_risk,
             
              ##女性生理生育因素
              初潮年龄,绝经,绝经年龄,生育,生育次数,初次生育年龄,哺乳,哺乳月份,流产,
              人工流产次数,自然流产次数,人工流产,自然流产,流产情况分组,绝育手术,
              子宫摘除术,卵巢摘除术,生育年份=出生年份+初次生育年龄,
              ##
              初潮年龄分组=case_when(
                初潮年龄<=13 ~ 1,
                初潮年龄>=14 ~ 2,
              ),
              初潮年龄分组=factor(初潮年龄分组,levels=c(1,2),labels=c('<=13','>=14')),
              生育次数分组=case_when(
                生育次数<=2 ~ 1,
                生育次数>=3 ~ 2,
                
              ),
              生育次数分组=factor(生育次数分组,levels = c(1,2),labels=c('<=2次','>=3次')),
              
              首次生育年龄分组=case_when(
                初次生育年龄<30 ~ 1,
                生育=="否"  | 初次生育年龄>=30 ~ 2,
              ),
              首次生育年龄分组=factor(首次生育年龄分组,levels = c(1,2),labels = c('<30','未生育或生育年龄>=30')) ,
              哺乳时间分组=case_when(
                哺乳=="否" | 哺乳月份<=6~ 1,
                哺乳月份>6~ 2,
            
              ),
              哺乳时间分组=factor(哺乳时间分组,levels = c(1,2),labels = c('未哺乳或哺乳时间<6个月','>=6')),
             人工流产次数分组=factor(ifelse(人工流产次数<2 | 人工流产=="否",1,2),levels=c(1,2),labels = c('人工流产少于2次','人工流产2次或更多')),
             绝经年龄分组=case_when(
              绝经年龄<55 ~ 1,
              绝经年龄>=55~ 2,
                ),
              绝经年龄分组=factor(绝经年龄分组,levels = c(1,2),labels=c('<55','>=55')),
              #女性特异性危险因素
              口服避孕药,雌激素代替治疗,
              #吸烟相关
              吸烟=factor(smk_risk),吸烟2=smk_risk2,被动吸烟=factor(psmk_risk),
              #BMI
              BMI=BMI,BMI_group=BMI_risk,
              #饮食相关因素
              饮酒=factor(alcohol_risk),喝茶=factor(tea_risk),酸奶=factor(yogurt_risk),
              蔬菜=factor(veget_risk),水果=factor(fruit_risk),谷类=factor(grain_risk),
              鸡蛋=factor(egg_risk),杂粮=factor(cereal_risk),豆类=factor(beans_risk),
              坚果=factor(nuts_risk),菌类=factor(fungus_risk),
              #饮食偏好
              偏咸=factor(salty_risk),偏辣=factor(spicy_risk),偏烫=factor(hot_risk),偏酸=factor(sour_risk),偏甜=factor(sweet_risk),偏硬=factor(hard_risk),
              腌制=factor(salted_risk),熏制=factor(smked_risk),酱制=factor(sauced_risk),晒制=factor(dried_risk),
              #体育锻炼
              运动=factor(exercise_risk),快走=jog_risk,太极=taichi_risk,
              广场舞=fitdance_risk,瑜伽=yoga_risk,游泳=swim_risk,跑步=run_risk,球类=ball_risk,器械=apparatus_risk,
              出行方式=出行方式,
              #生活习惯
              静态时间=factor(sedentaryh_risk),手机使用时间=cellphoneh_risk,
              #特殊疾病史
              弥漫性肺间质纤维化=disea1_risk,肺结核=disea2_risk,慢性支气管炎=disea3_risk,肺气肿=disea4_risk,哮喘支气管扩张=disea5_risk,
              矽肺或尘肺=disea6_risk,胆囊息肉=disea7_risk,胆结石=disea8_risk,脂肪肝=disea9_risk,肝硬化=disea10_risk,慢性乙型肝炎=disea11_risk,
              慢性丙型肝炎=disea12_risk,血吸虫病感染史=disea13_risk,食管或胃上皮内瘤变=disea14_risk,十二指肠溃疡=disea15_risk,Barrett食管=disea16_risk,
              萎缩性胃炎=disea17_risk,胃溃疡=disea18_risk,胃息肉=disea19_risk,幽门螺杆菌感染史=disea20_risk,EB病毒感染史=disea21_risk,
              胃粘膜异性增生=disea22_risk,胃肠上皮化生=disea23_risk,残胃=disea24_risk,乳腺纤维瘤=disea25_risk,
              乳腺不典型增生=disea26_risk,乳腺小叶原位癌=disea27_risk,女性良性病变史=disea_female,
              #慢性病史
              糖尿病=disea28_risk,高血压=disea29_risk,高血脂=disea30_risk,
              冠心病=disea31_risk,中风=disea32_risk,
              基础疾病史=disea_common,
              #负性生活事件
              重大精神创伤=stress_risk,精神压抑=depress_risk,
              #职业暴露史
              镉=cadmium_risk,石棉=asbestos_risk,镍=nickel_risk,砷=arsenic_risk,氡=radon_risk,
              氯乙烯=chloroethy_risk,X射线=Xray_risk,苯=benzene_risk,
              #结局
              BI_rads=BI_rads,CA_type_breast=CA_type_breast,
              CA=CA,CA_type=CA_type
)
rm(biomarker)



