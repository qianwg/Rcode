
risk_function2<-function(data){
  data$baonian<-ifelse(data$B4==2,(data$B4a*data$B4b)/20,
                       ifelse(data$B4==3,(data$B4a*data$B4b)/20,0))
  data$bmi<-data$A15/((data$A14/100)^2)
  attach(data)
  #(2)吸烟
  #（1）从不吸烟（0分）
  #（2）吸烟<20包年 a)目前已戒烟，且戒烟大于15年（1分）
  #                 b)目前已戒烟，且戒烟不足15年（2分）
  #                 c)目前仍在吸烟（3分）
  
  #（3）吸烟≥20包年 a)目前已戒烟，且戒烟大于15年（4分）
  #                 b)目前已戒烟，且戒烟不足15年（5分）
  #                 c)目前仍在吸烟（6分）
  smoking_risk<-vector()
  smoking_risk[data$B4==1]<-0
  smoking_risk[data$B4==2 & is.na(data$baonian)]<-0
  smoking_risk[data$B4==3 & is.na(data$baonian)]<-0
  smoking_risk[data$B4==2 & data$baonian<20]<-2
  smoking_risk[data$B4==2 & data$baonian>=20]<-3.5
  smoking_risk[data$B4==3 & data$baonian<20 & data$B4_X>15 & data$B4_X<99]<-1
  smoking_risk[data$B4==3 & data$baonian<20 & data$B4_X<=15 ]<-1.5
  smoking_risk[data$B4==3 & data$baonian>=20 & data$B4_X>15 & data$B4_X<99]<-2.5
  smoking_risk[data$B4==3 & data$baonian>=20 & data$B4_X<=15]<-3
  #(3)被动吸烟
  #1、从未暴露过（0分）
  #2、暴露过，但暴露时间<10年（1分）
  #3、暴露过，但暴露时间≥10年（2分）
  passivesmk_risk<-ifelse(is.na(B5) | is.na(B5_X),0,ifelse(B5==2 & B5_X>=10 & B5_X<99,2,1))
  #(4)BMI
  #	BMI: ≤ 23.9 kg/m2（0分）
  #	BMI: 24.0 – 27.9 kg/m2（1分）
  #	BMI: ≥ 28.0 kg/m2（2分） 
  bmi_risk<-ifelse(bmi<=23.9 | is.na(bmi),0,ifelse(bmi>=28,2,1))
  #(5)饮食、饮酒、饮茶等相关因素
  #每周至少3次喝酒（1分）
  #每周至少3次喝茶、鲜奶、酸奶或其他富含益生菌饮品、咖啡（-1分/个）
  #每周至少1次喝碳酸饮料、果汁/果味饮料、茶味饮料（1分/个）
  #每天都吃新鲜蔬菜、新鲜水果、谷类、蛋类（-1分/个）
  #每周至少3次新鲜水产品、薯类、杂粮、豆类、坚果、大蒜、菌类（-1分/个）
  #每周至少1次油炸、烧烤、熏制、腌制、酱制、晒制食品（1分/个）
  food_risk1<-ifelse(is.na(B6_1),0,ifelse(B6_1==1,1,0))#饮酒
  food_risk2<-ifelse(is.na(B6_2),0,ifelse(B6_2==1,-1,0))#茶
  food_risk3<-ifelse(is.na(B6_3),0,ifelse(B6_3==1,-1,0))#鲜奶
  food_risk4<-ifelse(is.na(B6_4),0,ifelse(B6_4==1,-1,0))#酸奶
  food_risk5<-ifelse(is.na(B6_5),0,ifelse(B6_5==1,-1,0))#咖啡
  food_risk6<-ifelse(is.na(B7_1),0,ifelse(B7_1==1,1,0))#碳酸饮料
  food_risk7<-ifelse(is.na(B7_2),0,ifelse(B7_2==1,1,0))#果汁饮料
  food_risk8<-ifelse(is.na(B7_3),0,ifelse(B7_3==1,1,0))#茶味饮料
  food_risk9<-ifelse(is.na(B8_1),0,ifelse(B8_1==1,-1,0))#蔬菜
  food_risk10<-ifelse(is.na(B8_2),0,ifelse(B8_2==1,-1,0))#水果
  food_risk11<-ifelse(is.na(B8_3),0,ifelse(B8_3==1,-1,0))#谷类
  food_risk12<-ifelse(is.na(B8_5),0,ifelse(B8_5==1,-1,0))#鸡蛋
  food_risk13<-ifelse(is.na(B9_1),0,ifelse(B9_1==1,-1,0))#水产品
  food_risk14<-ifelse(is.na(B9_2),0,ifelse(B9_2==1,-1,0))#薯类
  food_risk15<-ifelse(is.na(B9_3),0,ifelse(B9_3==1,-1,0))#杂粮
  food_risk16<-ifelse(is.na(B9_4),0,ifelse(B9_4==1,-1,0))#豆类
  food_risk17<-ifelse(is.na(B9_5),0,ifelse(B9_5==1,-1,0))#坚果
  food_risk18<-ifelse(is.na(B9_6),0,ifelse(B9_6==1,-1,0))#大蒜
  food_risk19<-ifelse(is.na(B9_7),0,ifelse(B9_7==1,-1,0))#菌类
  food_risk20<-ifelse(is.na(B10_1),0,ifelse(B10_1==1,1,0))#烧烤
  food_risk21<-ifelse(is.na(B10_2),0,ifelse(B10_2==1,1,0))
  food_risk22<-ifelse(is.na(B10_3),0,ifelse(B10_3==1,1,0))
  food_risk23<-ifelse(is.na(B10_4),0,ifelse(B10_4==1,1,0))
  food_risk24<-ifelse(is.na(B10_5),0,ifelse(B10_5==1,1,0))
  food_risk25<-ifelse(is.na(B10_6),0,ifelse(B10_6==1,1,0))
  food_risk<-food_risk1+food_risk2+food_risk3+food_risk4+food_risk5+food_risk6+food_risk7+food_risk8+food_risk9+food_risk10+
    food_risk11+food_risk12+food_risk13+food_risk14+food_risk15+food_risk16+food_risk17+food_risk18+food_risk19+
    food_risk20+food_risk21+food_risk22+food_risk23+food_risk24+food_risk25
  #(6)身体活动相关因素
  #每周至少3次，且每次至少30分钟的快走、太极拳、广场舞、瑜伽、游泳、跑步、球类、器械、或其他锻炼方式（-1分/个）
  #主要出行方式：步行或自行车，-1分；公交车/班车/地铁、私家车/出租车、电动车/摩托车，1分
  #每天累计坐着、靠着或躺着的时间：少于 3小时，0分；3-6小时，1分；7-12小时，2分；13小时及以上，3分
  body_risk1<-ifelse(is.na(B16a_1),0,ifelse(B16a_1==1,-1,0))
  body_risk2<-ifelse(is.na(B16a_2),0,ifelse(B16a_2==1,-1,0))
  body_risk3<-ifelse(is.na(B16a_3),0,ifelse(B16a_3==1,-1,0))
  body_risk4<-ifelse(is.na(B16a_4),0,ifelse(B16a_4==1,-1,0))
  body_risk5<-ifelse(is.na(B16a_5),0,ifelse(B16a_5==1,-1,0))
  body_risk6<-ifelse(is.na(B16a_6),0,ifelse(B16a_6==1,-1,0))
  body_risk7<-ifelse(is.na(B16a_7),0,ifelse(B16a_7==1,-1,0))
  body_risk8<-ifelse(is.na(B16a_8),0,ifelse(B16a_8==1,-1,0))
  body_risk9<-ifelse(is.na(B16a_9),0,ifelse(B16a_9==1,-1,0))
  #body_risk12<-ifelse(is.na(cellphoneh),0,cellphoneh-1)
  body_risk<-body_risk1+body_risk2+body_risk3+body_risk4+body_risk5+body_risk6+body_risk7+body_risk8+body_risk9
  #(7)重度精神问题并接受治疗≥3个月（1分）
  stress_risk<-ifelse(is.na(B23),0,ifelse(B23==2,1,0))
  #共同因素得分之和
  common_risk<-smoking_risk+passivesmk_risk+bmi_risk+food_risk+body_risk+stress_risk
  total<-cbind(data,smoking_risk,passivesmk_risk,bmi_risk,food_risk,body_risk,stress_risk,common_risk)
  return(total)
  
}

