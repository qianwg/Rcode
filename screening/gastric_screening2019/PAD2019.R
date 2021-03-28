library(openxlsx)
pepsinogen2019<-read.xlsx('~/data/2019/示范区2019年.xlsx',detectDates=TRUE)

pepsinogen2019[,-which(names(pepsinogen2019) %in% c('NAME','ID18','Name2'))]<-as.data.frame(
  apply(pepsinogen2019[,-which(names(pepsinogen2019) %in% c('NAME','ID18','Name2'))],2,as.numeric))
#apply(pepsinogen[,-which(names(pepsinogen) %in% c('NAME','ID18','Name2'))],2,table)
pepsinogen2019<-pepsinogen2019%>%transmute(
  #个人信息
  ID=ID20,name=NAME,persoID=ID18,F_ID,
  #自身癌症史
  cancer_self=factor(ifelse(B2a_1==1 & !is.na(B2a_1),1,0),levels=c(0,1),labels=c('否','是')),
  #癌症家族史
  cancer_fam=factor(ifelse(B2==2,1,0),levels=c(0,1),labels=c('否','是')),
  #胃癌家族史
  gastric_sim=ifelse(B2a2a==4 | B2a3a==4  | B2a4a==4 |B2a5a==4 |
                       B2a6a==4 | B2a7a==4  | B2a8a==4 | B2a9a==4,1,0),
  gastric_sim=factor(ifelse(is.na(gastric_sim),0,gastric_sim),levels=c(0,1),labels=c('阴性','阳性')),
  #年龄
  age=AGE1,
  age_risk=factor(ifelse(AGE1<=49,0,ifelse(AGE1>60,2,1)),levels=c(0,1,2),labels=c('<50','50-60','>60')),
  age_risk2=case_when(
    AGE1<=44 ~ 1,
    between(AGE1,45,49) ~ 2,
    between(AGE1,50,54) ~ 3,
    between(AGE1,55,59) ~ 4,
    between(AGE1,60,64) ~ 5,
    between(AGE1,65,69) ~ 6,
    AGE1>=70 ~ 7
  ),
  age_risk2=factor(age_risk2,levels=seq(7),labels=c('40-44','45-49','50-54','55-59',
                                                    '60-64','65-69','>=70')),
  age_risk3=case_when(
    between(AGE1,40,49) ~ 1,
    between(AGE1,50,59) ~ 2,
    between(AGE1,60,69) ~ 3,
    AGE1>=70 ~ 4
  ),
  age_risk3=factor(age_risk3,levels=seq(4),labels=c('40-49','50-59',
                                                    '60-69','>=70')),
  age_risk4=case_when(
    between(AGE1,40,49) ~ 1,
    between(AGE1,50,69) ~ 2,
    AGE1>=70 ~ 3
  ),
  age_risk4=factor(age_risk4,levels=seq(3),labels=c('40-49','50-69','>=70')),
  age_risk5=factor(ifelse(age>=60,1,0),levels=c(0,1),labels=c('<60','>=60')),                                                 
  #性别
  sex_risk=factor(A1,levels=c(1,2),labels=c('Male','Female')),
  #人口学特征
  marriage_risk=case_when(
    A6==1 ~ 1,
    A6==2 ~ 2,
    A6==3  | A6==4 ~ 3
  ),
  marriage_risk=factor(marriage_risk,levels = c(1,2,3),labels=c('已婚','未婚','离婚或丧偶')),
  education_risk=case_when(
    A7==1 | A7==2  ~ 1,
    A7==3 ~ 2,
    A7==4 | A7==5 | A7==6 ~ 3,
  ),
  education_risk2=case_when(
    A7==1 | A7==2 | A7==3 | A7==4~ 1,
    A7==5 | A7==6 ~ 2,
  ),
  education_risk=factor(education_risk,levels=c(1,2,3),labels=c('小学及以下','初中','高中及以上')),
  education_risk2=factor(education_risk2,levels=c(1,2),labels=c('≤12年','>12年')),
  income_risk=factor(A10,levels = c(1,2,3,4),labels=c('<3000','3000-4999','5000-9999','>10000')),
  income_risk2=factor(ifelse(A10>2,1,0),levels = c(0,1),labels=c('<5000','>=5000')),
  employm_risk=factor(A8,levels=c(1,2,3,4),labels=c('在业','离退休','失业/下岗/待业','家务/无业')),
  employm_risk2=factor(ifelse(A8>=3,3,A8),levels=c(1,2,3),labels=c('在业','离退休','失业/下岗/待业')),
  
  blood_risk=factor(A11,levels = c(1,2,3,4,5),labels=c('A','B','O','AB','不详')),
  blood_risk1=case_when(
    A11==3 ~ 1,
    A11==1 | A11==2 | A11==4 ~ 2 ,
    A11==5 ~ 3
  ),
  blood_risk1=factor( blood_risk1,levels = c(1,2,3),labels=c('O','A/B/AB','不详')),
  blood_risk2=case_when(
    A11==1 ~ 1,
    A11==2 | A11==3 | A11==4 ~ 2 ,
    A11==5 ~ 3
  ),
  blood_risk2=factor(blood_risk2,levels = c(1,2,3),labels=c('A','B/AB/O','不详')),
  #吸烟与被动吸烟
  
  cpd=B4a,smkyrs=B4b,
  吸烟年数分组=case_when(
    smkyrs<10 ~ 1,
    between(smkyrs,10,14) ~ 2,
    between(smkyrs,15,19) ~ 3,
    between(smkyrs,20,24) ~ 4,
    between(smkyrs,25,29) ~ 5,
    between(smkyrs,30,34) ~ 6,
    between(smkyrs,35,39) ~ 7,
    between(smkyrs,40,44) ~ 8,
    between(smkyrs,45,49) ~ 9,
    smkyrs>=50 ~ 10,
    ),
  #吸烟年数分组=factor(吸烟年数分组,levels=seq(12),labels=c('','','','','','','','','','','','')),
  吸烟年数分组2=case_when(
    smkyrs<10 ~ 1,
    between(smkyrs,10,19) ~ 2,
    between(smkyrs,20,29) ~ 3,
    between(smkyrs,30,39) ~ 4,
    between(smkyrs,40,49) ~ 5,
   smkyrs>=50 ~ 6,
  ),
  
  包年=cpd*smkyrs/20,
  包年分组=case_when(
    包年<10 ~ 1,
    between(包年,10,20) ~ 2,
    between(包年,20.01,30) ~ 3,
    between(包年,30.01,40) ~ 4,
    between(包年,40.01,50) ~ 5,
    包年>50 ~ 6,
  ),
  包年分组=factor(包年分组,levels=c(1,2,3,4,5,6),labels=c('<10','-20','-30','-40','-50','>50')),
  smk_risk1=factor(B4,levels=c(1,2,3),labels=c('从不吸烟','目前吸烟','过去吸烟')),
  smk_risk2=factor(ifelse(B4>=2,1,0),levels=c(0,1),labels=c('从不吸烟','目前或过去吸烟')),
  smk_risk4=factor(ifelse(B4==2,1,0),levels=c(0,1),labels=c('从不吸烟/过去吸烟','目前吸烟')),
  smk_risk3=case_when(
    B4==1 ~ 1,
    B4==2 & 包年<20 ~2,
    B4==2 & 包年>=20 ~3,
    B4==3 & 包年<20 ~4,
    B4==3 & 包年>=20 ~5,
    ),
  smk_risk3=factor(smk_risk3,levels = c(1,2,3,4,5),labels=c('从不吸烟','目前吸烟&包年<20','目前吸烟&包年>=20','过去吸烟&包年<20','过去吸烟&包年>=20')),
  psmk_risk2=case_when(
    B5==1 ~ 0,
    B5==2 & B5_X<10 ~ 1,
    B5==2 & B5_X>=10 ~ 2,
  ),
  psmk_risk1=factor(B5,levels=c(1,2),labels=c('否','是')),
  psmk_risk2=factor(psmk_risk2,levels=c(0,1,2),labels=c('否','是且<=10年','是且>10年')),
  
  #BMI
  BMI=10000*A15/(A14*A14),
  BMI_risk=case_when(
    BMI<24  ~ 1,#正常
    BMI>=24 & BMI<28 ~ 2,#超重
    BMI>=28 ~ 3#肥胖
  ),
  BMI_risk=factor(BMI_risk,levels = c(1,2,3),labels=c('正常','超重','肥胖')),
  BMI_risk2=case_when(
    BMI<18.5 ~ 1, #偏瘦
    BMI<24 & BMI>=18.5  ~ 2,#正常
    BMI<28 & BMI>=24 ~ 3,#超重
    BMI>=28 ~ 4#肥胖
  ),
  BMI_risk2=factor(BMI_risk2,levels = c(1,2,3,4),labels=c('偏瘦','正常',"超重",'肥胖')),
  BMI_risk3=case_when(
    BMI<18.5 ~ 2, #偏瘦
    BMI<24 & BMI>=18.5  ~ 1,#正常
    BMI<28 & BMI>=24 ~ 3,#超重
    BMI>=28 ~ 4#肥胖
  ),
  BMI_risk3=factor(BMI_risk3,levels = c(1,2,3,4),labels=c('正常','偏瘦',"超重",'肥胖')),
  BMI_risk4=case_when(
    BMI<24  ~ 1,#正常
    BMI>=24 ~ 2,#超重或肥胖
  ),
  BMI_risk4=factor(BMI_risk4,levels = c(1,2),labels=c('正常','超重/肥胖')),
  
  #BMI_risk=factor(ifelse(10000*weight/(height*height)<24,0,ifelse(10000*weight/(height*height)<28,1,2))),
  #饮食
  alcohol_risk=factor(B6_1,levels=c(0,1),labels=c('否','是')),#饮酒
  tea_risk=factor(B6_2,levels=c(0,1),labels=c('否','是')),#饮茶
  coffee_risk=factor(B6_5,levels=c(0,1),labels=c('否','是')),#咖啡
  milk_risk=factor(B6_3,levels=c(0,1),labels=c('否','是')),#鲜奶
  yogurt_risk=factor(B6_4,levels=c(0,1),labels=c('否','是')),#酸奶
  sodas_risk=factor(B7_1,levels=c(0,1),labels=c('否','是')),#碳酸饮料
  juice_risk=factor(B7_2,levels=c(0,1),labels=c('否','是')),#果汁果味饮料
  teadr_risk=factor(B7_3,levels=c(0,1),labels=c('否','是')),#茶味饮料
  veget_risk=factor(B8_1,levels=c(0,1),labels=c('否','是')),#蔬菜
  fruit_risk=factor(B8_2,levels=c(0,1),labels=c('否','是')),#水果
  grain_risk=factor(B8_3,levels=c(0,1),labels=c('否','是')),  #谷类
  meat_risk=factor(B8_4,levels=c(0,1),labels=c('否','是')),#肉类
  egg_risk=factor(B8_5,levels=c(0,1),labels=c('否','是')),  #鸡蛋
  seafd_risk=factor(B9_1,levels=c(0,1),labels=c('否','是')),#水产品
  potato_risk=factor(B9_2,levels=c(0,1),labels=c('否','是')),#薯类
  cereal_risk=factor(B9_3,levels=c(0,1),labels=c('否','是')),  #杂粮
  beans_risk=factor(B9_4,levels=c(0,1),labels=c('否','是')),  #豆类
  nuts_risk=factor(B9_5,levels=c(0,1),labels=c('否','是')),  #坚果
  garlic_risk=factor(B9_6,levels=c(0,1),labels=c('否','是')),  #大蒜
  fungus_risk=factor(B9_7,levels=c(0,1),labels=c('否','是')),  #菌类
  fried_risk=factor(B10_1,levels=c(0,1),labels=c('否','是')),  #油炸
  barbecued_risk=factor(B10_2,levels=c(0,1),labels=c('否','是')),  #烧烤
  smked_risk=factor(B10_3,levels=c(0,1),labels=c('否','是')),  #熏制
  sauced_risk=factor(B10_5,levels=c(0,1),labels=c('否','是')),  #酱治
  salted_risk=factor(B10_4,levels=c(0,1),labels=c('否','是')),  #腌制
  dried_risk=factor(B10_6,levels=c(0,1),labels=c('否','是')),  #晒制
  salty_risk=factor(B11_1,levels=c(0,1),labels=c('否','是')),  #偏咸
  spicy_risk=factor(B11_2,levels=c(0,1),labels=c('否','是')),  #偏辣
  hot_risk=factor(B11_3,levels=c(0,1),labels=c('否','是')),  #偏烫
  sour_risk=factor(B11_4,levels=c(0,1),labels=c('否','是')),  #偏酸
  sweet_risk=factor(B11_5,levels=c(0,1),labels=c('否','是')),  #偏甜
  hard_risk=factor(B11_6,levels=c(0,1),labels=c('否','是')),  #偏硬
  #饮食习惯
  breakfast_risk=factor(ifelse(B12==2,1,0),levels=c(0,1),labels = c('否','是')),#每天都吃早饭
  dalayeat_risk=factor(ifelse(B13==2,1,0),levels=c(0,1),labels = c('否','是')),
  speedeat_risk=factor(B14,levels=c(1,2,3),labels = c('慢','适中','快')),
  outeat_risk=factor(ifelse(B15>3,3,B15),levels=c(1,2,3),labels = c('无或少于1次','1-7次','8次及以上')),
  
  #体育锻炼
  exercise_risk=factor(ifelse(B16==2,1,0),levels=c(0,1),labels = c('否','是')),
  jog_risk=factor(ifelse(B16a_1==1 & !is.na(B16a_1),1,0),levels=c(0,1),labels = c('否','是')),#快走
  taichi_risk=factor(ifelse(B16a_2==1 & !is.na(B16a_2),1,0),levels=c(0,1),labels = c('否','是')),#太极
  fitdance_risk=factor(ifelse(B16a_3==1 & !is.na(B16a_3),1,0),levels=c(0,1),labels = c('否','是')),#广场舞
  yoga_risk=factor(ifelse(B16a_4==1 & !is.na(B16a_4),1,0),levels=c(0,1),labels = c('否','是')),#瑜伽
  swim_risk=factor(ifelse(B16a_5==1 & !is.na(B16a_5),1,0),levels=c(0,1),labels = c('否','是')),#游泳
  run_risk=factor(ifelse(B16a_6==1 & !is.na(B16a_6),1,0),levels=c(0,1),labels = c('否','是')),#跑步
  ball_risk=factor(ifelse(B16a_7==1 & !is.na(B16a_7),1,0),levels=c(0,1),labels = c('否','是')),#球类
  apparatus_risk=factor(ifelse(B16a_8==1 & !is.na(B16a_8),1,0),levels=c(0,1),labels = c('否','是')),#器械
  #睡眠时间
  sleephrs_risk=factor(case_when(
    B18==1 ~ 1,
    B18==2 | B18==3 ~ 2,
    B18==4 ~ 3
  )),
  sleephrs_risk=factor(sleephrs_risk,levels = c(1,2,3),labels=c('<5小时','5-8小时','>8小时')),
  #睡眠质量
  sleepquali_risk=factor(B19,levels=c(1,2,3),labels=c('优','中','差')),
  #夜班
  nightshift_risk=factor(B20,levels=c(1,2),labels=c('否','是')),
  
  
  #静态时间
  sedentaryh_risk=case_when(
    B21==1 ~ 1,
    B21==2 ~ 2,
    B21==3 |B21==4  ~3
  ),
  sedentaryh_risk=factor(sedentaryh_risk,levels=c(1,2,3),labels=c('少于3小时','3-6小时','>=7')),
  #手机使用时间
  cellphoneh_risk=case_when(
    B22==1 ~ 1,
    B22==2 ~ 2,
    B22==3 |  B22==4 ~ 3,
  ),
  cellphoneh_risk=factor(cellphoneh_risk,levels=c(1,2,3),labels=c('少于3小时','3-6小时','7小时及以上')),
  #基础性疾病
  #stomach_disea=factor(ifelse(B3c==2,1,0),levels=c(0,1),labels=c('否','是')),#食管、胃、或十二指肠疾病
  disea14_risk=factor(ifelse(B3_14==1 & !is.na(B3_14),1,0),levels=c(0,1),labels=c('否','是')),#食管或胃上皮内瘤变
  disea15_risk=factor(ifelse(B3_15==1 & !is.na(B3_15),1,0),levels=c(0,1),labels=c('否','是')),#十二指肠溃疡
  disea16_risk=factor(ifelse(B3_16==1 & !is.na(B3_16),1,0),levels=c(0,1),labels=c('否','是')),#胃食管反流性疾病
  
  disea17_risk=factor(ifelse(B3_17==1 & !is.na(B3_17),1,0),levels=c(0,1),labels=c('否','是')),#萎缩性胃炎
  disea18_risk=factor(ifelse(B3_18==1 & !is.na(B3_18),1,0),levels=c(0,1),labels=c('否','是')),#胃溃疡
  disea19_risk=factor(ifelse(B3_19==1 & !is.na(B3_19),1,0),levels=c(0,1),labels=c('否','是')),#胃息肉
  disea20_risk=factor(ifelse(B3_20==1 & !is.na(B3_20),1,0),levels=c(0,1),labels=c('否','是')),#幽门螺杆菌感染史
  disea22_risk=factor(ifelse(B3_22==1 & !is.na(B3_22),1,0),levels=c(0,1),labels=c('否','是')),#胃粘膜异性或不典型增生
  disea23_risk=factor(ifelse(B3_23==1 & !is.na(B3_23),1,0),levels=c(0,1),labels=c('否','是')),#胃肠上皮化生
  disea24_risk=factor(ifelse(B3_24==1 & !is.na(B3_24),1,0),levels=c(0,1),labels=c('否','是')),#残胃
  #慢性病
  disea28_risk=factor(ifelse(B3_28==1 & !is.na(B3_28),1,0),levels=c(0,1),labels=c('否','是')),#糖尿病
  disea29_risk=factor(ifelse(B3_29==1 & !is.na(B3_29),1,0),levels=c(0,1),labels=c('否','是')),#高血压
  disea30_risk=factor(ifelse(B3_30==1 & !is.na(B3_30),1,0),levels=c(0,1),labels=c('否','是')),#高血脂
  disea31_risk=factor(ifelse(B3_31==1 & !is.na(B3_31),1,0),levels=c(0,1),labels=c('否','是')),#冠心病
  disea32_risk=factor(ifelse(B3_32==1 & !is.na(B3_32),1,0),levels=c(0,1),labels=c('否','是')),#中风
  disea33_risk=factor(ifelse(B3_33==1 & !is.na(B3_33),1,0),levels=c(0,1),labels=c('否','是')),#偏头疼
  #代谢综合征=factor(ifelse(BMI>=25 & disea28_risk=='是' & disea29_risk=='是',1,0),levels=c(0,1),labels = c('否','是')),
 #  代谢紊乱相关因素=factor(ifelse(BMI>=25 | disea28_risk=='是' | disea29_risk=='是',1,0),levels=c(0,1),labels = c('否','是')),
  #职业暴露
  cadmium_risk=factor(ifelse(B24_2==1 & !is.na(B24_2),1,0),levels=c(0,1),labels=c('否','是')),#镉
  asbestos_risk=factor(ifelse(B24_1==1 & !is.na(B24_1),1,0),levels=c(0,1),labels=c('否','是')),#石棉
  nickel_risk=factor(ifelse(B24_3==1 & !is.na(B24_3),1,0),levels=c(0,1),labels=c('否','是')),#镍
  arsenic_risk=factor(ifelse(B24_4==1 & !is.na(B24_4),1,0),levels=c(0,1),labels=c('否','是')),#砷
  radon_risk=factor(ifelse(B24_5==1 & !is.na(B24_5),1,0),levels=c(0,1),labels=c('否','是')),#氡
  chloroethy_risk=factor(ifelse(B24_6==1 & !is.na(B24_6),1,0),levels=c(0,1),labels=c('否','是')),#氯乙烯
  Xray_risk=factor(ifelse(B24_7==1 & !is.na(B24_7),1,0),levels=c(0,1),labels=c('否','是')),#X射线
  benzene_risk=factor(ifelse(B24_8==1 & !is.na(B24_7),1,0),levels=c(0,1),labels=c('否','是')),#苯
  stress_risk=factor(ifelse(B23==2,1,0),levels=c(0,1),labels = c('否','是')),
  #女性生理生育因素
  初潮年龄=B32,绝经年龄=B33_X,绝经=factor(B33,levels=c(1,2),labels=c('否','是')),
  绝经分类=case_when(
    sex_risk=='Male' ~ 1,
    sex_risk=='Female' & 绝经=="否" ~ 2,
    sex_risk=='Female' & 绝经=="是" ~ 3,
  ),
  绝经分类=factor(绝经分类,levels=c(1,2,3),labels=c('男性','未绝经女性','绝经女性')),
  生育=factor(B34,levels = c(1,2),labels=c('否','是')),
  生育次数=B34_X,初次生育年龄=B34a,
  哺乳=factor(B34b,levels=c(1,2),labels=c('否','是')),
  哺乳月份=B34b_X,
  流产=factor(B35,levels = c(1,2),labels = c('否','是')),
  人工流产次数=B35a_1_X,
  自然流产次数=B35a_2_X,
  人工流产=factor(B35a_1,levels = c(0,1),labels = c('否','是')),
  自然流产=factor(B35a_2,levels = c(0,1),labels = c('否','是')),
  口服避孕药=factor(B36,levels=c(1,2),labels=c('否','是')),
  雌激素代替治疗=factor(B37,levels=c(1,2),labels=c('否','是')),
  绝育手术=factor(B38_1,levels = c(0,1),labels = c('否','是')),
  子宫摘除术=factor(B38_2,levels = c(0,1),labels = c('否','是')),
  卵巢摘除术=factor(B38_3,levels = c(0,1),labels = c('否','是')),
  妇科手术史=case_when(
    子宫摘除术=="否" & 卵巢摘除术=="否" ~ 1,
    子宫摘除术=="是" & 卵巢摘除术=="否" ~ 2,
    卵巢摘除术=="是" ~ 3
    
  ),
  妇科手术史=factor(妇科手术史,levels=c(1,2,3),labels=c('无子宫摘除或卵巢摘除','有子宫摘除无卵巢摘除','有卵巢摘除')),
 胃癌问卷阳性=factor(ifelse(B43==5,1,0),levels=c(0,1),labels=c('阴性','阳性'))
)%>%transmute(
  #匹配信息
  ID=ID,F_ID,name,persoID,
  #癌症家族史相关
  自身癌=cancer_self,癌症家族史=cancer_fam,胃癌家族史=gastric_sim,
  #性别和年龄
  性别=sex_risk,年龄=age,年龄分组=age_risk,年龄分组2=age_risk2,年龄分组3=age_risk3,年龄分组4=age_risk4,年龄分组5=age_risk5,
  #人口学特征
  婚姻=marriage_risk,就业状况=employm_risk, 就业状况2=employm_risk2,家庭收入=income_risk,家庭收入2=income_risk2,
  教育=education_risk,教育年数=education_risk2,血型=blood_risk,血型1=blood_risk1,血型2=blood_risk2,
  #运动
  运动=exercise_risk,快走=jog_risk,太极=taichi_risk,
  广场舞=fitdance_risk,瑜伽=yoga_risk,游泳=swim_risk,跑步=run_risk,球类=ball_risk,器械=apparatus_risk,
  #BMI
  BMI=BMI,BMI_group=BMI_risk,BMI_group2=BMI_risk2, BMI_group3=BMI_risk3,BMI_group4=BMI_risk4,
  #吸烟相关
  包年,包年分组,cpd,smkyrs,吸烟年数分组,吸烟年数分组2,
  吸烟1=smk_risk1,吸烟2=smk_risk2,吸烟3=smk_risk3,吸烟4=smk_risk4,
  被动吸烟1=psmk_risk1,被动吸烟2=psmk_risk2,
  #生活习惯
  每天早餐=breakfast_risk,准点吃饭=dalayeat_risk,吃饭速度=speedeat_risk,外出吃饭=outeat_risk,
  静态时间=sedentaryh_risk,手机使用时间=cellphoneh_risk,睡眠时间=sleephrs_risk,睡眠质量=sleepquali_risk,夜班=nightshift_risk,
  #饮食相关
  饮酒=alcohol_risk,喝茶=tea_risk,鲜奶=milk_risk,碳酸饮料=sodas_risk,#
  果味饮料=juice_risk,#果汁
  酸奶=yogurt_risk,咖啡=coffee_risk,蔬菜=veget_risk,水果=fruit_risk,谷类= grain_risk,
  鸡蛋=egg_risk,杂粮=cereal_risk,豆类=beans_risk,坚果=nuts_risk,
  大蒜=garlic_risk,菌类=fungus_risk,油炸=fried_risk,烧烤=barbecued_risk,
  熏制=smked_risk,酱制=sauced_risk,偏咸=salty_risk,腌制=salted_risk,
  偏辣=spicy_risk,
  偏烫=hot_risk,
  偏酸=sour_risk, 
  偏甜=sweet_risk,
  偏硬=hard_risk, 
  #食管、胃、十二指肠疾病史
  十二指肠溃疡=disea15_risk,胃食管反流性疾病=disea16_risk,
  胃溃疡=disea18_risk,胃息肉=disea19_risk,幽门螺杆菌感染史=disea20_risk,
  萎缩性胃炎=disea17_risk,胃肠上皮化生=disea23_risk,胃粘膜异性增生=disea22_risk,残胃=disea24_risk,
  消化性溃疡=factor(ifelse(十二指肠溃疡=="是" | 胃溃疡=="是" ,1,0),levels=c(0,1),labels=c('否','是')),
  
  #胃肠疾病=stomach_disea,
  #慢性病史
  糖尿病=disea28_risk,高血压=disea29_risk,高血脂=disea30_risk,冠心病=disea31_risk,偏头疼=disea33_risk,
  #代谢综合征,代谢紊乱相关因素,
  #职业暴露
  镉=cadmium_risk,石棉=asbestos_risk,镍=nickel_risk,砷=arsenic_risk,氡=radon_risk,
  氯乙烯=chloroethy_risk,X射线=Xray_risk,苯=benzene_risk,
  重度精神问题=stress_risk,
  #生理生育因素
  初潮年龄,绝经年龄,绝经,生育,生育次数,绝经分类,
  初次生育年龄,哺乳,哺乳月份,流产,人工流产次数,
  自然流产次数,人工流产,自然流产,绝育手术,子宫摘除术,卵巢摘除术,
  
  初潮年龄分组=case_when(
    初潮年龄<13 ~ 1 ,
    between(初潮年龄,13,14) ~ 2,
    between(初潮年龄,15,16) ~ 3,
    初潮年龄>=17 ~ 4
  ),
  初潮年龄分组=factor(初潮年龄分组,levels = c(1,2,3,4),labels = c('<13岁','13-14岁','15-16',">=17")),
  生育次数分组=factor(ifelse(生育次数>2,1,0),levels = c(0,1),labels=c('<=2次','>2次')),
  
  首次生育年龄分组=case_when(
    初次生育年龄<=20 ~ 1,
    between(初次生育年龄,21,25) ~ 2,
    between(初次生育年龄,26,30) ~ 3,
    初次生育年龄>30 ~ 4
    
  ),
  首次生育年龄分组=factor(首次生育年龄分组,levels = c(1,2,3,4),labels = c('<=20','21-25','26-30','>=31')) ,
  哺乳时间分组=case_when(
    哺乳==1 ~ 1,
    哺乳月份<=12 ~ 2,         
    哺乳月份>12 ~ 3
  ),
  哺乳时间分组=factor(哺乳时间分组,levels = c(1,2,3),labels = c('未哺乳','<=12','>12')),
  绝经年龄分组=case_when(
    绝经年龄<45 ~ 1,
    between(绝经年龄,45,49) ~ 2,
    between(绝经年龄,50,54) ~ 3,
    绝经年龄>54 ~ 4
  ),
  绝经年龄分组=factor(绝经年龄分组,levels=c(1,2,3,4),labels=c('<45','45-49','50-54','>54')),
  口服避孕药,
  雌激素代替治疗,
  雌激素影响时间=绝经年龄-初潮年龄,
  雌激素影响时间分组=case_when(
    雌激素影响时间<33 ~ 1,
    雌激素影响时间>=33 & 雌激素影响时间<36 ~ 2,
    雌激素影响时间>=36 & 雌激素影响时间<39 ~ 3,
    雌激素影响时间>=39 ~ 4
  ),
  雌激素影响时间分组=factor(雌激素影响时间分组,levels=c(1,2,3,4),labels=c('<33','33-35.9','36-38.9','>=39')),
  妇科手术史,胃癌问卷阳性
)
#基本分布
#my.render.cat <- function(x) {
##  c("", sapply(stats.default(x), function(y) with(y,
#                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
#}
#table1(~性别+婚姻+就业状况+家庭收入+教育+血型+运动+快走+  
#         太极+广场舞+瑜伽+游泳+跑步+球类+器械+吸烟1+ 
#         被动吸烟1+每天早餐+准点吃饭+吃饭速度+外出吃饭+静态时间+手机使用时间+饮酒+   
#         喝茶+鲜奶+酸奶+咖啡+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+ 
#         大蒜+菌类+油炸+烧烤+熏制+酱制+偏咸+腌制+
#         十二指肠溃疡+胃溃疡+胃息肉+幽门螺杆菌感染史+萎缩性胃炎+胃肠上皮化生+残胃+  
##         +糖尿病+高血压+高血脂+冠心病+镉+石棉+  
#         镍+砷+氡+氯乙烯+X射线+苯,data=pepsinogen,render.categorical=my.render.cat)


##2019标志物数据
data_PG2019<-import('~/data/2019/标志物示范区2019.xlsx')
#names(data_PG2019)
data_PG2019<-data_PG2019%>%transmute(
  ID=ID,age_PG=年龄,name_PG2019=姓名,persoID_PG2019=身份证号,#AFP=AFP, CA199=`CA19-9`,#CA125=`CA-125`,CA153=`CA15-3`,CEA=CEA,
  PG1=PGⅠ,PG2=PGⅡ,PGR=`PGⅠ/PGⅡ`,
  hbsag=HBsAg,
  PG1_range=case_when(
    PG1<20 ~ 1,
    between(PG1,20,70) ~ 2,
    between(PG1,70.01,199.9) ~ 3,
    PG1>=200 ~ 4
  ),
  PG1_range=factor(PG1_range,levels = c(1,2,3,4),labels=c('<20','20-70','70.01-199.99','≥200')),
  PG1_range1=case_when(
    PG1<40.2 ~ 1,
    PG1>=40.2 & PG1<51.1 ~ 2,
    PG1>=51.1 & PG1<63.9 ~ 3,
    PG1>=63.9 & PG1<81.9 ~ 4,
    PG1>=81.9 ~5
  ),
  PG1_range1=factor(PG1_range1,levels=c(1,2,3,4,5),labels=c('<40.2','40.2-51.0','51.1-63.8','63.9-81.8','>=81.9')),
  PG1_range2=case_when(
    PG1<43.4 ~ 1,
    PG1>=43.4 & PG1<57.0 ~ 2,
    PG1>=57.0 & PG1<76.5 ~ 3,
    PG1>=76.5 ~ 4
  ),
  PG1_range2=factor(PG1_range2,levels=c(1,2,3,4),labels=c('<43.4','43.4-56.9','57.0-76.5','>=76.5')),
  PG1_range3=case_when(
    PG1<=30 ~ 1,
    PG1>30 & PG1<=50.0 ~ 2,
    PG1>50.0 & PG1<=70.0 ~ 3,
    PG1>70 ~ 4
  ),
  PG1_range3=factor(PG1_range3,levels=c(1,2,3,4),labels=c('<=30','30.01-50','50.01-70','>70')),
  PG1_range4=case_when(
    PG1<20 ~ 1,
    PG1>=20 & PG1<=50.0 ~ 2,
    PG1>50.0 & PG1<=70.0 ~ 3,
    PG1>=70 ~ 4
  ),
  PG1_range4=factor(PG1_range4,levels=c(1,2,3,4),labels=c('<20','20-49.9','50-69.9','>=70')),
  
  PG2_range=case_when(
    PG2<6.51 ~ 1,
    PG2>=6.51 & PG2<9.8 ~ 2,
    PG2>=9.8 & PG2<15.3 ~ 3,
    PG2>=15.30 ~ 4
  ),
  PG2_range=factor(PG2_range,levels = c(1,2,3,4),labels=c('<6.5','6.51-9.79','9.8-15.29','>=15.30')),
  PG2_range2=case_when(
    PG2<6 ~ 1,
    PG2>=6 & PG2<8.4 ~ 2,
    PG2>=8.4 & PG2<11.7 ~ 3,
    PG2>=11.7 & PG2<16.8 ~ 4,
    PG2>=16.8 ~ 5
  ),
  PG2_range2=factor(PG2_range2,levels = c(1,2,3,4,5),labels=c('<6','6-8.39','8.4-11.69','11.7-16.8','>=16.8')),
  
  PG2_range3=factor(ifelse(PG2<=12,0,1),levels=c(0,1),labels=c('<=12','>12')),
  PG2_range4=factor(ifelse(PG2<=15,0,1),levels=c(0,1),labels=c('<=12','>12')),
  PG2_range5=case_when(
    PG2<=7.5 ~ 1,
    PG2>7.5 & PG2<=12.5 ~ 2,
    PG2>12.5 & PG2<=17.5 ~ 3,
    PG2>17.5 & PG2<=24.5 ~ 4,
    PG2>24.5 ~ 5
  ),
  PG2_range5=factor(PG2_range5,levels = c(1,2,3,4,5),labels=c('<=7.5','7.6-12.5','12.6-17.5','17.5-24.5','>24.5')),
  
  PGR_range=case_when(
    PGR<=3 ~ 1,
    between(PGR,3.01,7) ~ 2,
    PGR>7 ~ 3
  ),
  PGR_range=factor(PGR_range,levels=c(1,2,3),labels=c('<3','3-7','>7')),
  PGR_range2=case_when(
    PGR<=3 ~ 1,
    PGR>3 & PGR<=6 ~ 2,
    PGR>6 & PGR<=9  ~ 3,
    PGR>9   ~ 4
  ),
  PGR_range2=factor(PGR_range2,levels=c(1,2,3,4),labels=c('<=3','3.01-6','6.01-9','>9')),
  PG_pos=factor(ifelse(PG1<=70 & PGR<=3,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  
  #PG_pos_1=factor(ifelse(PG1<70 & PGR<3,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos1=factor(ifelse(PG1<=50 & PGR<=3,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos2=factor(ifelse(PG1<=30 & PGR<=2,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos3<-NA,
  PG_pos3[PG1<=70 & PGR<=3]<-2,
  PG_pos3[PG1<=50 & PGR<=3]<-3,
  PG_pos3[PG1<=30 & PGR<=2]<-4,
  PG_pos3[is.na(PG_pos3)]<-1,
  PG_pos3=factor(PG_pos3,levels=c(1,2,3,4),labels=c('阴性','弱阳性','中度阳性','极度阳性')),
  PG_pos4<-NA,
  PG_pos4[PG1<=70 & PGR<=3]<-2,
  PG_pos4[PG1<=30 & PGR<=2]<-3,
  PG_pos4[is.na(PG_pos4)]<-1,
  PG_pos4=factor(PG_pos4,levels=c(1,2,3),labels=c('正常','一般萎缩','重度萎缩')),
  PG_pos5=factor(ifelse(PGR<=3,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos6=factor(ifelse(PG1<30,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos7=factor(ifelse(PG1<=70,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos8=factor(ifelse(PG2>10,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos9=factor(ifelse(PG1<20,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  PG_pos10<-NA,
  PG_pos10[PG1<=70 & PGR<=3]<-3,
  PG_pos10[PG1<=30 & PGR<=2 & age_PG<60]<-4,
  PG_pos10[PG1<=30 & PGR<=2 & age_PG>=60]<-5,
  PG_pos10[is.na(PG_pos10) & age_PG<60]<-1,
  PG_pos10[is.na(PG_pos10) & age_PG>=60]<-2,
  PG_pos10=factor(PG_pos10,levels=c(1,2,3,4,5),labels=c('年轻者正常','年老者正常','一般萎缩','年轻者重度萎缩','年老者重度萎缩')),
  PG_pos11=factor(ifelse(PG2>=6.51,1,0),levels=c(0,1),labels=c('<6.51','>=6.51')),
  PG_pos12=factor(ifelse(PG1<28,1,0),levels=c(0,1),labels=c('PG1>=28','PG1<28')),
  PG_pos13=case_when(
    PG1<=70 & PGR>3 ~ 1,
    PG1>70 & PGR>3 ~ 2,
    PG1>70 & PGR<=3 ~ 3,
    PG1<=70 & PG1>50 & PGR<=3 ~ 4,
    PG1<=50 & PG1>30 & PGR<=3 ~ 5,
    PG1<=30 & PGR<=3 ~ 6
  ),
  PG_pos13=factor(PG_pos13,levels=c(1,2,3,4,5,6),labels=c('PGa','PGb','PGc','PGd','PGe','PGf')),
  #PG_pos14=factor(ifelse(PG1<25,1,0),levels=c(0,1),labels=c('PG1>=25','PG1<25')),
  #CA199_pos=factor(ifelse(CA199>27,1,0),levels=c(0,1),labels=c('阴性','阳性')),
  )%>%transmute(
  ID,PG1,PG2,PGR,hbsag,#AFP,
  PG1_range,PG1_range1,PG1_range2,PG1_range3,PG1_range4,
  PG2_range,PG2_range2,PG2_range3,PG2_range4,PG2_range5,
  PGR_range,PGR_range2,PG_pos12, PG_pos13,#PG_pos_1,#PG_pos14,
  PG_pos,PG_pos1,PG_pos2,PG_pos3,PG_pos4,PG_pos5,PG_pos6,PG_pos7,PG_pos8,PG_pos9,PG_pos10,PG_pos11,#CA199,CA199_pos
)
screening2019<-inner_join(pepsinogen2019,data_PG2019,by='ID')
screening2019<-screening2019%>%
  filter(ID!=31030159,ID!=31060461,自身癌!='是',残胃!='是')
rm(data_PG2019)
rm(pepsinogen2019)


















