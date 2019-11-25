risk_function<-function(data){
  names(data)<-tolower(names(data))
  data$cpd<-ifelse(data$cpd==99,0,data$cpd)
  data$smkyrs<-ifelse(data$smkyrs==99,0,data$smkyrs)
  data$baonian<-ifelse(data$smoking==2,(data$cpd*data$smkyrs)/20,
                       ifelse(data$smoking==3,(data$cpd*data$smkyrs)/20,0))
  data$bmi<-data$weight/((data$height/100)^2)
  attach(data)
  #------------------------------------------------------共同因素-----------------------------------------------------
  #家族史函数
  family_sum<-function(x){
    x1<-sort(x,decreasing = TRUE)
    x2<-ifelse(length(x1[x1>2])<=2,x1[1]+x1[2],ifelse(x1[1]==5,10,9))
    return(x2) 
  }  
  #(1)年龄
  #≤ 49 岁（0分）
  #50-59 岁（1分）
  #≥60岁（2分）
  age_risk<-ifelse(age<=49 | is.na(age),0,ifelse(age>59,2,1))
  #(2)吸烟
  #（1）从不吸烟（0分）
  #（2）吸烟<20包年 a)目前已戒烟，且戒烟大于15年（1分）
  #                 b)目前已戒烟，且戒烟不足15年（2分）
  #                 c)目前仍在吸烟（3分）
  
  #（3）吸烟≥20包年 a)目前已戒烟，且戒烟大于15年（4分）
  #                 b)目前已戒烟，且戒烟不足15年（5分）
  #                 c)目前仍在吸烟（6分）
  smoking_risk<-vector()
  smoking_risk[data$smoking==1]<-0
  smoking_risk[data$smoking==2 & is.na(data$baonian)]<-0
  smoking_risk[data$smoking==3 & is.na(data$baonian)]<-0
  smoking_risk[data$smoking==2 & data$baonian<20]<-3
  smoking_risk[data$smoking==2 & data$baonian>=20]<-6
  smoking_risk[data$smoking==3 & data$baonian<20 & data$quitsmkyrs>15 & data$quitsmkyrs<99]<-1
  smoking_risk[data$smoking==3 & data$baonian<20 & data$quitsmkyrs<=15 ]<-2
  smoking_risk[data$smoking==3 & data$baonian>=20 & data$quitsmkyrs>15 & data$quitsmkyrs<99]<-4
  smoking_risk[data$smoking==3 & data$baonian>=20 & data$quitsmkyrs<=15]<-5
  #(3)被动吸烟
  #1、从未暴露过（0分）
  #2、暴露过，但暴露时间<10年（1分）
  #3、暴露过，但暴露时间≥10年（2分）
  passivesmk_risk<-ifelse(is.na(passivesmk) | is.na(psmkyrs),0,ifelse(passivesmk==2 & psmkyrs>=10 & psmkyrs<99,2,1))
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
  food_risk1<-ifelse(is.na(alcohol),0,ifelse(alcohol==2,1,0))
  food_risk2<-ifelse(is.na(tea),0,ifelse(tea==2,-1,0))
  food_risk3<-ifelse(is.na(milk),0,ifelse(milk==2,-1,0))
  food_risk4<-ifelse(is.na(yogurt),0,ifelse(yogurt==2,-1,0))
  food_risk5<-ifelse(is.na(coffee),0,ifelse(coffee==2,-1,0))
  food_risk6<-ifelse(is.na(sodas),0,ifelse(sodas==2,1,0))
  food_risk7<-ifelse(is.na(juice),0,ifelse(juice==2,1,0))
  food_risk8<-ifelse(is.na(teadr),0,ifelse(teadr==2,1,0))
  food_risk9<-ifelse(is.na(veget),0,ifelse(veget==2,-1,0))
  food_risk10<-ifelse(is.na(fruit),0,ifelse(fruit==2,-1,0))
  food_risk11<-ifelse(is.na(grain),0,ifelse(grain==2,-1,0))
  food_risk12<-ifelse(is.na(egg),0,ifelse(egg==2,-1,0))
  food_risk13<-ifelse(is.na(seafd),0,ifelse(seafd==2,-1,0))
  food_risk14<-ifelse(is.na(potato),0,ifelse(potato==2,-1,0))
  food_risk15<-ifelse(is.na(cereal),0,ifelse(cereal==2,-1,0))
  food_risk16<-ifelse(is.na(beans),0,ifelse(beans==2,-1,0))
  food_risk17<-ifelse(is.na(nuts),0,ifelse(nuts==2,-1,0))
  food_risk18<-ifelse(is.na(garlic),0,ifelse(garlic==2,-1,0))
  food_risk19<-ifelse(is.na(fungus),0,ifelse(fungus==2,-1,0))
  food_risk20<-ifelse(is.na(fried),0,ifelse(fried==2,1,0))
  food_risk21<-ifelse(is.na(barbecued),0,ifelse(barbecued==2,1,0))
  food_risk22<-ifelse(is.na(smked),0,ifelse(smked==2,1,0))
  food_risk23<-ifelse(is.na(salted),0,ifelse(salted==2,1,0))
  food_risk24<-ifelse(is.na(sauced),0,ifelse(sauced==2,1,0))
  food_risk25<-ifelse(is.na(dried),0,ifelse(dried==2,1,0))
  food_risk<-food_risk1+food_risk2+food_risk3+food_risk4+food_risk5+food_risk6+food_risk7+food_risk8+food_risk9+food_risk10+
    food_risk11+food_risk12+food_risk13+food_risk14+food_risk15+food_risk16+food_risk17+food_risk18+food_risk19+
    food_risk20+food_risk21+food_risk22+food_risk23+food_risk24+food_risk25
  #(6)身体活动相关因素
  #每周至少3次，且每次至少30分钟的快走、太极拳、广场舞、瑜伽、游泳、跑步、球类、器械、或其他锻炼方式（-1分/个）
  #主要出行方式：步行或自行车，-1分；公交车/班车/地铁、私家车/出租车、电动车/摩托车，1分
  #每天累计坐着、靠着或躺着的时间：少于 3小时，0分；3-6小时，1分；7-12小时，2分；13小时及以上，3分
  #每天累计使用手机时间：少于 3小时，0分；3-6小时，1分；7-12小时，2分；13小时及以上，3分
  body_risk1<-ifelse(is.na(jog),0,ifelse(jog==2,-1,0))
  body_risk2<-ifelse(is.na(taichi),0,ifelse(taichi==2,-1,0))
  body_risk3<-ifelse(is.na(fitdance),0,ifelse(fitdance==2,-1,0))
  body_risk4<-ifelse(is.na(yoga),0,ifelse(yoga==2,-1,0))
  body_risk5<-ifelse(is.na(swim),0,ifelse(swim==2,-1,0))
  body_risk6<-ifelse(is.na(run),0,ifelse(run==2,-1,0))
  body_risk7<-ifelse(is.na(ball),0,ifelse(ball==2,-1,0))
  body_risk8<-ifelse(is.na(apparatus),0,ifelse(apparatus==2,-1,0))
  body_risk9<-ifelse(is.na(othexer),0,ifelse(othexer==2,-1,0))
  body_risk10<-ifelse(is.na(traffic),0,ifelse(traffic<=2,-1,1))
  body_risk11<-ifelse(is.na(sedentaryh),0,sedentaryh-1)
  body_risk12<-ifelse(is.na(cellphoneh),0,cellphoneh-1)
  body_risk<-body_risk1+body_risk2+body_risk3+body_risk4+body_risk5+body_risk6+body_risk7+body_risk8+body_risk9+
    body_risk10+body_risk11+body_risk12
  #(7)重度精神问题并接受治疗≥3个月（1分）
  stress_risk<-ifelse(is.na(stress),0,ifelse(stress==2,1,0))
  #共同因素得分之和
  common_risk<-age_risk+smoking_risk+passivesmk_risk+bmi_risk+food_risk+body_risk
#----------------------------------------------------------肺癌----------------------------------------------------------
  #肺癌
  lung_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==34,ifelse(!is.na(fapersoid) & nchar(fapersoid)>4,5,3),0),0)
  lung_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==34,ifelse(!is.na(mopersoid) & nchar(mopersoid)>4,5,3),0),0)
  lung_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==34,ifelse(!is.na(br1persoid) & nchar(br1persoid)>4,5,3),0),0)
  lung_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==34,ifelse(!is.na(br2persoid) & nchar(br2persoid)>4,5,3),0),0)
  lung_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==34,ifelse(!is.na(si1persoid) & nchar(si1persoid)>4,5,3),0),0)
  lung_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==34,ifelse(!is.na(si2persoid) & nchar(si2persoid)>4,5,3),0),0)
  lung_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==34,ifelse(!is.na(ch1persoid) & nchar(ch1persoid)>4,5,3),0),0)
  lung_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==34,ifelse(!is.na(ch2persoid) & nchar(ch2persoid)>4,5,3),0),0)
  lung_family1<-data.frame(lung_fath,lung_moth,lung_brot1,lung_brot2,lung_sist1,lung_sist2,lung_chil1,lung_chil2)
  lung_family<-apply(lung_family1,1,family_sum)
  #3.曾患或现患：弥漫性肺间质纤维化，肺结核，慢性支气管炎，肺气肿，哮喘支气管扩张，矽肺或尘肺
  disea_1<-ifelse(is.na(disea1),0,ifelse(disea1==2,1,0))
  disea_2<-ifelse(is.na(disea2),0,ifelse(disea2==2,1,0))
  disea_3<-ifelse(is.na(disea3),0,ifelse(disea3==2,1,0))
  disea_4<-ifelse(is.na(disea4),0,ifelse(disea4==2,1,0))
  disea_5<-ifelse(is.na(disea5),0,ifelse(disea5==2,1,0))
  disea_6<-ifelse(is.na(disea6),0,ifelse(disea6==2,1,0))
  lung_disea<-disea_1+disea_2+disea_3+disea_4+disea_5+disea_6
  #4.特殊职业暴露（石棉，镉，镍，砷，氡，X射线等））
  occuexpo_1<-ifelse(!is.na(asbestos),ifelse(asbestos==2,1,0),0)
  occuexpo_2<-ifelse(!is.na(cadmium),ifelse(cadmium==2,1,0),0)
  occuexpo_3<-ifelse(!is.na(nickel),ifelse(nickel==2,1,0),0)
  occuexpo_4<-ifelse(!is.na(arsenic),ifelse(arsenic==2,1,0),0)
  occuexpo_5<-ifelse(!is.na(radon),ifelse(radon==2,1,0),0)
  occuexpo_6<-ifelse(!is.na(xray),ifelse(xray==2,1,0),0)
  lung_occuexpo<-occuexpo_1+occuexpo_2+occuexpo_3+occuexpo_4+occuexpo_5+occuexpo_6
  lung_disea_occuexpo<-ifelse(lung_disea+lung_occuexpo>5,5,lung_disea+lung_occuexpo)
  lung_score<-common_risk+lung_family+lung_disea_occuexpo
  #-----------------------------------------------------2.乳腺癌----------------------------------------------------------------
  #1.一级亲属乳腺癌家族史任意一人曾患过乳腺癌，能提供身份证时5分，不能时3分
  breast_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==47,ifelse(!is.na(fapersoid) & nchar(fapersoid)>4,5,3),0),0)
  breast_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==47,ifelse(!is.na(mopersoid) & nchar(mopersoid)>4,5,3),0),0)
  breast_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==47,ifelse(!is.na(br1persoid) & nchar(br1persoid)>4,5,3),0),0)
  breast_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==47,ifelse(!is.na(br2persoid) & nchar(br2persoid)>4,5,3),0),0)
  breast_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==47,ifelse(!is.na(si1persoid) & nchar(si1persoid)>4,5,3),0),0)
  breast_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==47,ifelse(!is.na(si2persoid) & nchar(si2persoid)>4,5,3),0),0)
  breast_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==47,ifelse(!is.na(ch1persoid) & nchar(ch1persoid)>4,5,3),0),0)
  breast_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==47,ifelse(!is.na(ch2persoid) & nchar(ch2persoid)>4,5,3),0),0)
  breast_family1<-data.frame(breast_fath,breast_moth,breast_brot1,breast_brot2,
                     breast_sist1,breast_sist2,breast_chil1,breast_chil2)
  breast_family<-apply(breast_family1,1,family_sum)
  #2.曾患或现患：乳腺小叶不典型增生，乳腺导管不典型增生，或乳腺小叶原位癌（每个1分）
  disea_25<-ifelse(is.na(disea25),0,ifelse(sex==2 & disea25==2,1,0))
  disea_26<-ifelse(is.na(disea26),0,ifelse(sex==2 & disea26==2,1,0))
  disea_27<-ifelse(is.na(disea27),0,ifelse(sex==2 & disea27==2,1,0))
  breast_disea<-disea_25+disea_26+disea_27
  #3.具有以下两项或更多着（每个1分）
   #(1)初潮年龄<13岁
  #(2)绝经年龄≥55岁 (3)未生育或首次生育年龄>30岁
  #(4)未哺乳或哺乳时间<6个月   (5)口服避孕药3次或以上
  #(6)使用激素替代治疗半年或以上
  #(7)人工流产2次或更多
  breast_risk1<-ifelse(!is.na(agemenarch),ifelse(sex==2 & agemenarch<13,1,0),0)
  breast_risk2<-ifelse(!is.na(agemenopau),ifelse(sex==2 & agemenopau>=55 & menopause==2 & agemenopau<99,1,0),0)
  breast_risk3<-ifelse(!is.na(deliver),ifelse(sex==2 & deliver==1,1,0),0)
  breast_risk4<-ifelse(!is.na(deliver),ifelse(sex==2 & agefirdeli>30 & deliver==2 & agefirdeli<99,1,0),0)
  breast_risk5<-ifelse(sex==2,ifelse(deliver==2,ifelse(breastfeed==1,1,0),1),0)
  breast_risk6<-ifelse(!is.na(breastfeed),ifelse(sex==2 & breastfeed==2 & brstfedmth<6,1,0),0)
  breast_risk7<-ifelse(!is.na(oralcontra),ifelse(sex==2 & oralcontra==2,1,0),0)
  breast_risk8<-ifelse(!is.na(hrt),ifelse(sex==2 & hrt==2,1,0),0)
  breast_risk9<-ifelse(abortion==2 & sex==2,ifelse(induabort>=2 & !is.na(induabort),1,0),0)
  breast_risk<-breast_risk1+breast_risk2+breast_risk3+breast_risk4+breast_risk5+breast_risk6+
    breast_risk7+breast_risk8+breast_risk9
  breast_risk_score<-ifelse(breast_risk+breast_disea>5,5,breast_risk+breast_disea)
  #4.乳腺癌总分
  breast_score<-breast_family+common_risk+breast_risk_score
#---------------------------------------------------------- ##3.肝癌----------------------------------------------------------------
  #1.家族史(能提供身份证5分，不能提供身份证3分)
  liver_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==24,ifelse(!is.na(fapersoid) & nchar(fapersoid)>4,5,3),0),0)
  liver_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==24,ifelse(!is.na(mopersoid) & nchar(mopersoid)>4,5,3),0),0)
  liver_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==24,ifelse(!is.na(br1persoid) & nchar(br1persoid)>4,5,3),0),0)
  liver_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==24,ifelse(!is.na(br2persoid) & nchar(br2persoid)>4,5,3),0),0)
  liver_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==24,ifelse(!is.na(si1persoid) & nchar(si1persoid)>4,5,3),0),0)
  liver_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==24,ifelse(!is.na(si2persoid) & nchar(si2persoid)>4,5,3),0),0)
  liver_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==24,ifelse(!is.na(ch1persoid) & nchar(ch1persoid)>4,5,3),0),0)
  liver_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==24,ifelse(!is.na(ch2persoid) & nchar(ch2persoid)>4,5,3),0),0)
  liver_family1<-data.frame(liver_fath,liver_moth,liver_brot1,liver_brot2,
                    liver_sist1,liver_sist2,liver_chil1,liver_chil2)
  liver_family<-apply(liver_family1,1,family_sum)
  #2.疾病史,曾患肝硬化、慢性乙型肝炎、慢性丙型肝炎（每个1分）
  disea_10<-ifelse(is.na(disea10),0,ifelse(disea10==2,1,0))
  disea_11<-ifelse(is.na(disea11),0,ifelse(disea11==2,1,0))
  disea_12<-ifelse(is.na(disea12),0,ifelse(disea12==2,1,0))
  liver_disea<-disea_10+disea_11+disea_12
  #3.具有以下两项或更多者
  #（4）特殊职业暴露（氯乙烯）
  #（5）脂肪肝、血吸虫病感染史（1个1分）
  liver_risk1<-ifelse(is.na(chloroethy),0,ifelse(chloroethy==2,1,0))
  liver_risk2<-ifelse(is.na(disea9),0,ifelse(disea9==2,1,0))
  liver_risk3<-ifelse(is.na(disea13),0,ifelse(disea13==2,1,0))
  liver_risk<-liver_risk1+liver_risk2+liver_risk3
  liver_disea_risk<-ifelse(liver_disea+liver_risk>5,5,liver_disea+liver_risk)
  #5.肝癌总分
  liver_score<-liver_family+liver_disea_risk+common_risk
  ##4.胃癌
  #1.家族史（有身份证5分，无身份证3分，上限15分）
  gastric_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==16,ifelse(!is.na(fapersoid) & nchar(fapersoid)>4,5,3),0),0)
  gastric_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==16,ifelse(!is.na(mopersoid) & nchar(mopersoid)>4,5,3),0),0)
  gastric_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==16,ifelse(!is.na(br1persoid) & nchar(br1persoid)>4,5,3),0),0)
  gastric_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==16,ifelse(!is.na(br2persoid) & nchar(br2persoid)>4,5,3),0),0)
  gastric_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==16,ifelse(!is.na(si1persoid) & nchar(si1persoid)>4,5,3),0),0)
  gastric_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==16,ifelse(!is.na(si2persoid) & nchar(si2persoid)>4,5,3),0),0)
  gastric_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==16,ifelse(!is.na(ch1persoid) & nchar(ch1persoid)>4,5,3),0),0)
  gastric_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==16,ifelse(!is.na(ch2persoid) & nchar(ch2persoid)>4,5,3),0),0)
  gastric_family1<-data.frame(gastric_fath,gastric_moth,gastric_brot1,gastric_brot2,
                      gastric_sist1,gastric_sist2,gastric_chil1,gastric_chil2)
  gastric_family<-apply(gastric_family1,1,family_sum)
  #2.疾病史，曾患或现患：胃溃疡、幽门螺旋杆菌、胃粘膜异性或不典型增生、残胃、肠上皮化生
  #萎缩性胃炎、胃息肉、食管或胃上皮内流变、十二指肠溃疡、Barrett食管、EB病毒感染史（每个1分）
  disea_14<-ifelse(is.na(disea14),0,ifelse(disea14==2,1,0))
  disea_15<-ifelse(is.na(disea15),0,ifelse(disea15==2,1,0))
  disea_16<-ifelse(is.na(disea16),0,ifelse(disea16==2,1,0))
  disea_17<-ifelse(is.na(disea17),0,ifelse(disea17==2,1,0))
  disea_18<-ifelse(is.na(disea18),0,ifelse(disea18==2,1,0))
  disea_19<-ifelse(is.na(disea19),0,ifelse(disea19==2,1,0))
  disea_20<-ifelse(is.na(disea20),0,ifelse(disea20==2,1,0))
  disea_21<-ifelse(is.na(disea21),0,ifelse(disea21==2,1,0))
  disea_22<-ifelse(is.na(disea22),0,ifelse(disea22==2,1,0))
  disea_23<-ifelse(is.na(disea23),0,ifelse(disea23==2,1,0))
  disea_24<-ifelse(is.na(disea24),0,ifelse(disea24==2,1,0))
  gastric_disea1<-disea_14+disea_15+disea_16+disea_17+disea_18+disea_19+disea_20+disea_21+disea_22+disea_23+disea_24
  gastric_disea<-ifelse(gastric_disea1>5,5,gastric_disea1)
  #胃癌总分
  gastric_score<-gastric_family+gastric_disea+common_risk
  ###合并
  common_risk_table<-data.frame(age_risk,smoking_risk,passivesmk_risk,bmi_risk,food_risk,body_risk,stress_risk,common_risk)
  lung_risk_table<-data.frame(lung_family,lung_disea,lung_occuexpo,lung_disea_occuexpo,lung_score)
  breast_risk_table<-data.frame(sex,breast_family,breast_disea,breast_risk,breast_risk_score,breast_score)
  breast_risk_table$breast_family[sex==1]<-NA
  breast_risk_table$breast_risk[sex==1]<-NA
  breast_risk_table$breast_disea[sex==1]<-NA
  breast_risk_table$breast_risk_score[sex==1]<-NA
  breast_risk_table$breast_score[sex==1]<-NA
  liver_risk_table<-data.frame(liver_family,liver_disea,liver_risk,liver_disea_risk,liver_score)
  gastric_risk_table<-data.frame(gastric_family,gastric_disea,gastric_score)
  total<-cbind(data,common_risk,lung_risk_table,breast_risk_table,liver_risk_table,gastric_risk_table)
  return(total)
}
percent_value<-function(x){
  p1<-round(quantile(x,0.01,na.rm=TRUE),2)
  p2.5<-round(quantile(x,0.025,na.rm=TRUE),2)
  p5<-round(quantile(x,0.05,na.rm=TRUE),2)
  p10<-round(quantile(x,0.1,na.rm=TRUE),2)
  p25<-round(quantile(x,0.25,na.rm=TRUE),2)
  p50<-round(quantile(x,0.5,na.rm=TRUE),2)
  p75<-round(quantile(x,0.75,na.rm=TRUE),2)
  p90<-round(quantile(x,0.9,na.rm=TRUE),2)
  p95<-round(quantile(x,0.95,na.rm=TRUE),2)
  p97<-round(quantile(x,0.97,na.rm=TRUE),2)
  p97.5<-round(quantile(x,0.975,na.rm=TRUE),2)
  p98<-round(quantile(x,0.98,na.rm=TRUE),2)
  p99<-round(quantile(x,0.99,na.rm=TRUE),2)
  table<-c(p1,p2.5,p5,p10,p25,p50,p75,p90,p95,p97,p97.5,p98,p99)
}
