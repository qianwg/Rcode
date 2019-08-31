library(shiny)
library(DT)
library(tidyverse)
library(rio)
library(data.table)
library(shinydashboard)
library(reshape2)
library(plyr)
options(shiny.maxRequestSize=70*1024^2)
shinyServer(function(input, output) {
    #反应式1
    dataset<-reactive({
        inFile1 <- input$file
        if (is.null(inFile1)) return(NULL)
        data<-rio::import(inFile1$datapath) 
    })
    #街道的命名
    area.function<-function(data){
       #示范区项目+早诊早治（试点街道）
        data$area[data$id<=31033000 & data$id>=31030001]<-'尖山街'
        data$area[data$id<=31063000 & data$id>=31060001]<-'下瓦房街'
        data$area[data$id<=31083000 & data$id>=31080001]<-'友谊路街'
        data$area[data$id<=31093000 & data$id>=31090001]<-'桃园街'
        data$area[data$id<=31073000 & data$id>=31070001]<-'越秀街'
        data$area[data$id<=31113000 & data$id>=31110001]<-'陈塘庄街'
        data$area[data$id<=33033000 & data$id>=33030001]<-'南营门街'
        data$area[data$id<=33043000 & data$id>=33040001]<-'新兴街'
        data$area[data$id<=34043000 & data$id>=34040001]<-'万兴街(东南)'
        data$area[data$id<=34053000 & data$id>=34050001]<-'水上公园街'
        data$area[data$id<=35033000 & data$id>=35030001]<-'二号桥街'
        data$area[data$id<=35053000 & data$id>=35050001]<-'大王庄街'
        data$area[data$id<=32023000 & data$id>=32020001]<-'别山镇'
        data$area[data$id<=32043000 & data$id>=32040001]<-'马伸桥镇'
        #早诊早治项目
        #河西区
        data$area[data$id>=31010001 & data$id<=31013000]<-"东海街"
        data$area[data$id>=31020001 & data$id<=31023000]<-'挂甲寺街'
        data$area[data$id>=31040001 & data$id<=31043000]<-"马场街"
        data$area[data$id>=31050001 & data$id<=31053000]<-"天塔街"
        data$area[data$id>=31100001 & data$id<=31103000]<-"柳林街"
        data$area[data$id>=31120001 & data$id<=31123000]<-"大营门街"
        data$qu[data$id>=31010001 & data$id<=31123000]<-'河西区'
        ###和平区
        data$area[data$id>=33010001 & data$id<=33013000]<-"小白楼街"
        data$area[data$id>=33020001 & data$id<=33023000]<-"南市街"
        data$area[data$id>=33050001 & data$id<=33053000]<-"五大道街"
        data$area[data$id>=33060001 & data$id<=33063000]<-"劝业场街"
        data$qu[data$id>=33010001 &data$id<=33063000]<-'和平区'
        ###南开区
        data$area[data$id>=34010001 & data$id<=34013000]<-"华苑街"
        data$area[data$id>=34020001 & data$id<=34023000]<-"王顶堤街"
        data$area[data$id>=34030001 & data$id<=34033000]<-"体育中心街"
        data$area[data$id>=34060001 & data$id<=34063000]<-"万兴街（三潭)"
        data$qu[data$id>=34010001 & data$id<=34063000 ]<-'南开区'
        ###河北区
        data$area[data$id>=36010001 & data$id<=36013000]<-"王串场街"
        data$area[data$id>=36020001 & data$id<=36023000]<-"鸿顺里街"
        data$area[data$id>=36030001 & data$id<=36033000]<-"月牙河街"
        data$area[data$id>=36040001 & data$id<=36043000]<-'建昌道街'
        data$area[data$id>=36050001 & data$id<=36053000]<-"铁东路街"
        data$area[data$id>=36060001 & data$id<=36063000]<-'宁园街'
        
        data$qu[data$id>=36010001 & data$id<=36053000]<-'河北区'
        ###河东区
        data$area[data$id>=35010001 & data$id<=35013000]<-"上杭路街"
        data$area[data$id>=35020001 & data$id<=35023000]<-"鲁山道街"
        data$area[data$id>=35040001 & data$id<=35043000]<-"唐家口街"
        data$area[data$id>=35060001 & data$id<=35063000]<-"富民路街"
        data$qu[data$id>=35010001 & data$id<=35063000]<-'河东区'
        ### 红桥区
        data$area[data$id>=37010001 & data$id<=37013000]<-"芥园道街"
        data$area[data$id>=37020001 & data$id<=37023000]<-"邵公庄街"
        data$area[data$id>=37030001 & data$id<=37033000]<-"和苑街"
        data$area[data$id>=37040001 & data$id<=37043000]<-"铃铛阁街"
        data$area[data$id>=37050001 & data$id<=37053000]<-"双环坉街"
        data$area[data$id>=37060001 & data$id<=37063000]<-"丁字沽街"
        data$qu[data$id>=37010001 & data$id<=37063000]<-'红桥区'
        ###蓟州区
        data$area[data$id>=32010001 & data$id<=32013000]<-"官庄镇"
        data$area[data$id>=32030001 & data$id<=32033000]<-"下营镇"
        data$area[data$id>=32050001 & data$id<=32053000]<-"桑梓镇"
        data$area[data$id>=32060001 & data$id<=32063000]<-"东二营镇"
        data$qu[data$id>=32010001 & data$id<=32063000]<-"蓟州区"
        return(data)
    }
    dataset2<-reactive({
        dataset2<-area.function(dataset())
    })
      
    
    
    #高危人群筛查程序  
    risk_function<-function(data){
        names(data)<-tolower(names(data))
        data$cpd<-ifelse(data$cpd==99,0,data$cpd)
        data$smkyrs<-ifelse(data$smkyrs==99,0,data$smkyrs)
        data$baonian<-ifelse(data$smoking==2,(data$cpd*data$smkyrs)/20,
                             ifelse(data$smoking==3,(data$cpd*data$smkyrs)/20,0))
        data$bmi<-data$weight/((data$height/100)^2)
        attach(data)
        lung_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==34,ifelse(!is.na(fapersoid) & fapersoid!=9 & fapersoid!=99,5,3),0),0)
        lung_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==34,ifelse(!is.na(mopersoid) & mopersoid!=9 & mopersoid!=99,5,3),0),0)
        lung_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==34,ifelse(!is.na(br1persoid) & br1persoid!=9 & br1persoid!=99,5,3),0),0)
        lung_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==34,ifelse(!is.na(br2persoid) & br2persoid!=9 & br2persoid!=99,5,3),0),0)
        lung_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==34,ifelse(!is.na(si1persoid) & si1persoid!=9 & si1persoid!=99,5,3),0),0)
        lung_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==34,ifelse(!is.na(si2persoid) & si2persoid!=9 & si2persoid!=99,5,3),0),0)
        lung_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==34,ifelse(!is.na(ch1persoid) & ch1persoid!=9 & ch1persoid!=99,5,3),0),0)
        lung_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==34,ifelse(!is.na(ch2persoid) & ch2persoid!=9 & ch2persoid!=99,5,3),0),0)
        lung_family1<-(lung_fath+lung_moth+lung_brot1+lung_brot2+lung_sist1+lung_sist2+lung_chil1+lung_chil2)
        lung_family<-ifelse(lung_family1>=15,15,lung_family1)
        #2.年龄大于55岁（1分）
        lung_age<-ifelse(age>=55,1,0)
        #3.曾患或现患：弥漫性肺间质纤维化，肺结核，慢性支气管炎，肺气肿，哮喘支气管扩张，矽肺或尘肺
        #（1个1分，2个2分，≥3个3分）
        disea_1<-ifelse(disea1==2,1,0)
        disea_2<-ifelse(disea2==2,1,0)
        disea_3<-ifelse(disea3==2,1,0)
        disea_4<-ifelse(disea4==2,1,0)
        disea_5<-ifelse(disea5==2,1,0)
        disea_6<-ifelse(disea6==2,1,0)
        lung_disea<-disea_1+disea_2+disea_3+disea_4+disea_5+disea_6
        lung_disea_score<-ifelse(lung_disea>=3,3,lung_disea)
        #4.特殊职业暴露（石棉，镉，镍，砷，氡，X射线等）（1个1分，≥2个2分）
        occuexpo_1<-ifelse(!is.na(asbestos),ifelse(asbestos==2,1,0),0)
        occuexpo_2<-ifelse(!is.na(cadmium),ifelse(cadmium==2,1,0),0)
        occuexpo_3<-ifelse(!is.na(nickel),ifelse(nickel==2,1,0),0)
        occuexpo_4<-ifelse(!is.na(arsenic),ifelse(arsenic==2,1,0),0)
        occuexpo_5<-ifelse(!is.na(radon),ifelse(radon==2,1,0),0)
        occuexpo_6<-ifelse(!is.na(xray),ifelse(xray==2,1,0),0)
        lung_occuexpo<-occuexpo_1+occuexpo_2+occuexpo_3+occuexpo_4+occuexpo_5+occuexpo_6
        lung_occuexpo_score<-ifelse(lung_occuexpo>=2,2,lung_occuexpo)
        #5.被动吸烟(每周≥1天，每天≥15分钟)≥10年（1分）
        lung_passivesmk<-ifelse(passivesmk==2 & psmkyrs>=10 & psmkyrs<99,1,0)
        #6.重度精神问题并接受治疗3个月（1分）
        lung_stress<-ifelse(stress==2,1,0)
        #7.吸烟状况
        #（1）从不吸烟（0分）
        #（2）吸烟<20包年 a)目前已戒烟，且戒烟大于15年（1分）
        #                 b)目前已戒烟，且戒烟不足15年（2分）
        #                 c)目前仍在吸烟（3分）
        
        #（3）吸烟≥20包年 a)目前已戒烟，且戒烟大于15年（3分）
        #                 b)目前已戒烟，且戒烟不足15年（4分）
        #                 c)目前仍在吸烟（5分）
        lung_smoking_score<-vector()
        lung_smoking_score[data$smoking==1]<-0
        lung_smoking_score[data$smoking==2 & data$baonian<20]<-3
        lung_smoking_score[data$smoking==2 & data$baonian>=20]<-5
        lung_smoking_score[data$smoking==3 & data$baonian<20 & data$quitsmkyrs>15 & data$quitsmkyrs<99]<-1
        lung_smoking_score[data$smoking==3 & data$baonian<20 & data$quitsmkyrs<=15 ]<-2
        lung_smoking_score[data$smoking==3 & data$baonian>=20 & data$quitsmkyrs>15 & data$quitsmkyrs<99]<-3
        lung_smoking_score[data$smoking==3 & data$baonian>=20 & data$quitsmkyrs<=15]<-4
        lung_score<-lung_family+lung_age+lung_stress+lung_disea_score+lung_occuexpo_score+lung_passivesmk+lung_smoking_score
        ##2.乳腺癌
        #1.一级亲属乳腺癌家族史任意一人曾患过乳腺癌，能提供身份证时5分，不能时3分
        breast_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==47,ifelse(!is.na(fapersoid) & fapersoid!=9 & fapersoid!=99,5,3),0),0)
        breast_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==47,ifelse(!is.na(mopersoid) & mopersoid!=9 & mopersoid!=99,5,3),0),0)
        breast_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==47,ifelse(!is.na(br1persoid) & br1persoid!=9 & br1persoid!=99,5,3),0),0)
        breast_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==47,ifelse(!is.na(br2persoid) & br2persoid!=9 & br2persoid!=99,5,3),0),0)
        breast_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==47,ifelse(!is.na(si1persoid) & si1persoid!=9 & si1persoid!=99,5,3),0),0)
        breast_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==47,ifelse(!is.na(si2persoid) & si2persoid!=9 & si2persoid!=99,5,3),0),0)
        breast_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==47,ifelse(!is.na(ch1persoid) & ch1persoid!=9 & ch1persoid!=99,5,3),0),0)
        breast_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==47,ifelse(!is.na(ch2persoid) & ch2persoid!=9 & ch2persoid!=99,5,3),0),0)
        breast_family1<-(breast_fath+breast_moth+breast_brot1+breast_brot2+
                             breast_sist1+breast_sist2+breast_chil1+breast_chil2)
        breast_family<-ifelse(breast_family1>=15,15,breast_family1)
        #2.曾患或现患：乳腺小叶不典型增生，乳腺导管不典型增生，或乳腺小叶原位癌（每个1分）
        disea_25<-ifelse(sex==2 & disea25==2,1,0)
        disea_26<-ifelse(sex==2 & disea26==2,1,0)
        disea_27<-ifelse(sex==2 & disea27==2,1,0)
        breast_disea<-disea_25+disea_26+disea_27
        #3.具有以下两项或更多着（0-1个0分，≥2个每个1分）
        #(1)年龄≥50岁     (2)初潮年龄<13岁
        #(3)绝经年龄≥55岁 (4)未生育或首次生育年龄>30岁
        #(5)未哺乳或哺乳时间<6个月   (6)口服避孕药3次或以上
        #(7)使用激素替代治疗半年或以上
        #(8)肥胖(BMI≥28kg/m^2)
        #(9)人工流产2次或更多
        #(10)重度精神问题并接受治疗≥3个月(18年无)
        breast_risk1<-ifelse(sex==2 & age>=50,1,0)
        breast_risk2<-ifelse(!is.na(agemenarch),ifelse(sex==2 & agemenarch<13,1,0),0)
        breast_risk3<-ifelse(!is.na(agemenopau),ifelse(sex==2 & agemenopau>=55 & menopause==2 & agemenopau<99,1,0),0)
        breast_risk4<-ifelse(!is.na(deliver),ifelse(sex==2 & deliver==1,1,0),0)
        breast_risk5<-ifelse(!is.na(deliver),ifelse(sex==2 & agefirdeli>30 & deliver==2 & agefirdeli<99,1,0),0)
        breast_risk6<-ifelse(sex==2,ifelse(deliver==2,ifelse(breastfeed==1,1,0),1),0)
        breast_risk7<-ifelse(!is.na(breastfeed),ifelse(sex==2 & breastfeed==2 & brstfedmth<6,1,0),0)
        breast_risk8<-ifelse(!is.na(oralcontra),ifelse(sex==2 & oralcontra==2,1,0),0)
        breast_risk9<-ifelse(!is.na(hrt),ifelse(sex==2 & hrt==2,1,0),0)
        breast_risk10<-ifelse(abortion==2 & sex==2,ifelse(induabort>=2 & !is.na(induabort),1,0),0)
        breast_risk11<-ifelse(!is.na(bmi),ifelse(sex==2 & bmi>=28,1,0),0)
        breast_risk12<-ifelse(sex==2 & stress==2,1,0)
        breast_risk<-breast_risk1+breast_risk2+breast_risk3+breast_risk4+breast_risk5+breast_risk6+
            breast_risk7+breast_risk8+breast_risk9+breast_risk10+breast_risk11+breast_risk12
        breast_risk_score<-ifelse(breast_risk>=2,breast_risk,0)
        #4.乳腺癌总分
        breast_score<-breast_family+breast_disea+breast_risk_score
        ##3.肝癌
        #1.家族史(能提供身份证5分，不能提供身份证3分)
        liver_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==24,ifelse(!is.na(fapersoid) & fapersoid!=9 & fapersoid!=99,5,3),0),0)
        liver_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==24,ifelse(!is.na(mopersoid) & mopersoid!=9 & mopersoid!=99,5,3),0),0)
        liver_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==24,ifelse(!is.na(br1persoid) & br1persoid!=9 & br1persoid!=99,5,3),0),0)
        liver_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==24,ifelse(!is.na(br2persoid) & br2persoid!=9 & br2persoid!=99,5,3),0),0)
        liver_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==24,ifelse(!is.na(si1persoid) & si1persoid!=9 & si1persoid!=99,5,3),0),0)
        liver_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==24,ifelse(!is.na(si2persoid) & si2persoid!=9 & si2persoid!=99,5,3),0),0)
        liver_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==24,ifelse(!is.na(ch1persoid) & ch1persoid!=9 & ch1persoid!=99,5,3),0),0)
        liver_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==24,ifelse(!is.na(ch2persoid) & ch2persoid!=9 & ch2persoid!=99,5,3),0),0)
        liver_family1<-(liver_fath+liver_moth+liver_brot1+liver_brot2+
                            liver_sist1+liver_sist2+liver_chil1+liver_chil2)
        liver_family<-ifelse(liver_family1>=15,15,liver_family1)
        #2.疾病史,曾患肝硬化、慢性乙型肝炎、慢性丙型肝炎（每个1分）
        disea_10<-ifelse(disea10==2,1,0)
        disea_11<-ifelse(disea11==2,1,0)
        disea_12<-ifelse(disea12==2,1,0)
        liver_disea_score<-disea_10+disea_11+disea_12
        #3.具有以下两项或更多者(0-1个0分，≥2个每个1分，其中第五个最多2分)
        #（1）男性年龄≥40岁   （2）女性年龄≥50岁
        #（3）近1年，每周至少饮酒3次
        #（4）特殊职业暴露（氯乙烯）
        #（5）胆囊息肉(18年没有)、胆结石（18年没有）、脂肪肝、血吸虫病感染史（1个1分，≥2个2分）
        #（6）肥胖（BMI≥28）
        #（7）重度精神问题并接受治疗≥3个月
        liver_risk1<-ifelse(sex==1 & age>=40,1,0)
        liver_risk2<-ifelse(sex==2 & age>=50,1,0)
        liver_risk3<-ifelse(alcohol==2,1,0)
        liver_risk4<-ifelse(chloroethy==2,1,0)
        liver_risk5_1<-ifelse(disea9==2,1,0)
        liver_risk5_2<-ifelse(disea13==2,1,0)
        liver_risk5_3<-ifelse(disea7==2,1,0)
        liver_risk5_4<-ifelse(disea8==2,1,0)
        liver_risk5_1234<-liver_risk5_1+liver_risk5_2+liver_risk5_3+liver_risk5_4
        liver_risk5<-ifelse(liver_risk5_1234>=2,2,liver_risk5_1234)
        liver_risk6<-ifelse(bmi>=28,1,0)
        liver_risk7<-ifelse(stress==2,1,0)
        liver_risk<-liver_risk1+liver_risk2+liver_risk3+liver_risk4+liver_risk5+liver_risk6+liver_risk7
        liver_risk_score<-ifelse(liver_risk>=2,liver_risk,0)
        #4.具有以下三种或更多特殊饮食偏好（1-2个0分，3个1分，≥4个2分）
        #油炸、烧烤、熏制、腌制、酱制食品
        liver_food_1<-ifelse(!is.na(fried),ifelse(fried==2,1,0),0)
        liver_food_2<-ifelse(!is.na(barbecued),ifelse(barbecued==2,1,0),0)
        liver_food_3<-ifelse(!is.na(smked),ifelse(smked==2,1,0),0)
        liver_food_4<-ifelse(!is.na(salted),ifelse(salted==2,1,0),0)
        liver_food_5<-ifelse(!is.na(sauced),ifelse(sauced==2,1,0),0)
        liver_food<-liver_food_1+liver_food_2+liver_food_3+liver_food_4+liver_food_5
        liver_food_score<-sapply(liver_food,function(data){
            if(data<3){
                liver_food_score<-0
            }else if(data==3){
                liver_food_score<-1
            }else{liver_food_score<-2}
            return(liver_food_score)
        })
        #5.肝癌总分
        liver_score<-liver_family+liver_disea_score+liver_risk_score+liver_food_score
        ##4.胃癌
        #1.家族史（有身份证5分，无身份证3分，上限15分）
        gastric_fath<-ifelse(!is.na(catpfath),ifelse(catpfath==16,ifelse(!is.na(fapersoid) & fapersoid!=9 & fapersoid!=99,5,3),0),0)
        gastric_moth<-ifelse(!is.na(catpmoth),ifelse(catpmoth==16,ifelse(!is.na(mopersoid) & mopersoid!=9 & mopersoid!=99,5,3),0),0)
        gastric_brot1<-ifelse(!is.na(catpbrot1),ifelse(catpbrot1==16,ifelse(!is.na(br1persoid) & br1persoid!=9 & br1persoid!=99,5,3),0),0)
        gastric_brot2<-ifelse(!is.na(catpbrot2),ifelse(catpbrot2==16,ifelse(!is.na(br2persoid) & br2persoid!=9 & br2persoid!=99,5,3),0),0)
        gastric_sist1<-ifelse(!is.na(catpsist1),ifelse(catpsist1==16,ifelse(!is.na(si1persoid) & si1persoid!=9 & si1persoid!=99,5,3),0),0)
        gastric_sist2<-ifelse(!is.na(catpsist2),ifelse(catpsist2==16,ifelse(!is.na(si2persoid) & si2persoid!=9 & si2persoid!=99,5,3),0),0)
        gastric_chil1<-ifelse(!is.na(catpchil1),ifelse(catpchil1==16,ifelse(!is.na(ch1persoid) & ch1persoid!=9 & ch1persoid!=99,5,3),0),0)
        gastric_chil2<-ifelse(!is.na(catpchil2),ifelse(catpchil2==16,ifelse(!is.na(ch2persoid) & ch2persoid!=9 & ch2persoid!=99,5,3),0),0)
        gastric_family1<-(gastric_fath+gastric_moth+gastric_brot1+gastric_brot2+
                              gastric_sist1+gastric_sist2+gastric_chil1+gastric_chil2)
        gastric_family<-ifelse(gastric_family1>=15,15,gastric_family1)
        #2.疾病史，曾患或现患：胃溃疡、幽门螺旋杆菌、胃粘膜异性或不典型增生、残胃、肠上皮化生（每个1分）
        disea_18<-ifelse(disea18==2,1,0)
        disea_20<-ifelse(disea20==2,1,0)
        disea_22<-ifelse(disea22==2,1,0)
        disea_24<-ifelse(disea24==2,1,0)
        disea_23<-ifelse(disea23==2,1,0)
        gastric_disea_score<-disea_18+disea_20+disea_22+disea_24+disea_23
        #3.具有以下两项或等多者（0-1个0分，≥2个每个1分，其中第3个最多2分）
        #（1）年龄≥55岁
        #（2）吸烟≥20包年
        #（3）萎缩性胃炎、胃息肉、食管或胃上皮内流变(18年没有)、
        #     十二指肠溃疡(18年没有)、Barrett食管(18年没有)、EB病毒感染史(1个1分，≥2个2分)
        #（4）肥胖（BMI≥28）
        #（5）重度精神问题并接受≥3个月治疗（18年没有）
        gastric_risk1<-ifelse(age>=55,1,0)
        gastric_risk2<-ifelse(!is.na(baonian),ifelse(baonian>=20,1,0),0)
        gastric_risk3_1<-ifelse(disea17==2,1,0)
        gastric_risk3_2<-ifelse(disea19==2,1,0)
        gastric_risk3_3<-ifelse(disea21==2,1,0)
        gastric_risk3_4<-ifelse(disea14==2,1,0)
        gastric_risk3_5<-ifelse(disea15==2,1,0)
        gastric_risk3_6<-ifelse(disea16==2,1,0)
        gastric_risk3_7<-gastric_risk3_1+gastric_risk3_2+gastric_risk3_3+gastric_risk3_4+gastric_risk3_5+gastric_risk3_6
        gastric_risk3<-ifelse(gastric_risk3_7>=2,2,gastric_risk3_7)
        gastric_risk4<-ifelse(bmi>=28,1,0)
        gastric_risk5<-ifelse(stress==2,1,0)
        gastric_risk<-gastric_risk1+gastric_risk2+gastric_risk3+gastric_risk4+gastric_risk5
        gastric_risk_score<-ifelse(gastric_risk>=2,gastric_risk,0)
        #gastric_risk5
        #4.特殊饮食偏好（1-2个0分，3个1分，≥4个2分）
        gastric_food_1<-ifelse(!is.na(fried),ifelse(fried==2,1,0),0)
        gastric_food_2<-ifelse(!is.na(barbecued),ifelse(barbecued==2,1,0),0)
        gastric_food_3<-ifelse(!is.na(smked),ifelse(smked==2,1,0),0)
        gastric_food_4<-ifelse(!is.na(salted),ifelse(salted==2,1,0),0)
        gastric_food_5<-ifelse(!is.na(sauced),ifelse(sauced==2,1,0),0)
        gastric_food<-gastric_food_1+gastric_food_2+gastric_food_3+gastric_food_4+gastric_food_5
        gastric_food_score<-sapply(gastric_food,function(data){
            if(data<3){
                gastric_food_score<-0
            }else if(data==3){
                gastric_food_score<-1
            }else{gastric_food_score<-2}
            return(gastric_food_score)
        })
        #胃癌总分
        gastric_score<-gastric_family+gastric_disea_score+gastric_risk_score+gastric_food_score
        ###合并
        lung_risk_table<-data.frame(lung_family,lung_age,lung_stress,lung_disea_score,lung_occuexpo_score,lung_passivesmk,
                                    lung_smoking_score,lung_score)
        breast_risk_table<-data.frame(sex,breast_family,breast_risk_score,breast_disea,breast_score)
        breast_risk_table$breast_family[sex==1]<-0
        breast_risk_table$breast_risk_score[sex==1]<-0
        breast_risk_table$breast_score[sex==1]<-0
        liver_risk_table<-data.frame(liver_family,liver_disea_score,liver_risk_score,liver_food_score,liver_score)
        gastric_risk_table<-data.frame(gastric_family,gastric_disea_score,gastric_risk_score,gastric_food_score,gastric_score)
        lung<-cbind(lung_risk_table)
        breast<-cbind(breast_risk_table)
        liver<-cbind(liver_risk_table)
        gastric<-cbind(gastric_risk_table)
        total<-cbind(data,lung_risk_table,breast_risk_table,liver_risk_table,gastric_risk_table)
        risk<-list(lung,breast,liver,gastric,total)
        names(risk)<-c('lung','breast','liver','gastric','total')
        return(risk)
    }
    #反应式2：高危人群的得分计算
    risk<-reactive({
        risk_function(dataset2())
    })
    #反应式3：高危人群得分2
    name1<-c('id','name','lung_score','breast_score','liver_score','gastric_score')
    name2<-c('area','id','name','sex','age','persoid','cellphone','telephone','lung_score','breast_score','liver_score','gastric_score')
    risk_sum<-reactive({
        data_risk<-risk_function(dataset2())
        risk<-data_risk$total
        risk_sum<-risk[,which(names(risk) %in% name1)]
    })
    risk_sum2<-reactive({
        risk_sum2<-melt(risk_sum(),id.vars=c('id','name'),variable.name = 'cancer',value.name = 'score')
    })
    
    ###反应式4：推荐项目
    axVar <- function(x, na.rm = TRUE) {
        ## compute `max`
        maxx <- max(x, na.rm = na.rm)
        ## which equal the max
        wmax <- which(x == max(x))
        ## how many equal the max
        nmax <- length(wmax)
        return(nmax)
    }
    ##初步制定肺癌≥8分为高危，乳腺≥6分，肝癌≥5分，胃癌≥5分
    risk_sum3<-reactive({
        data_risk<-risk_function(dataset2())
        risk<-data_risk$total
        risk_sum<-risk[,which(names(risk) %in% name2)]
        risk_sum$sex<-factor(risk_sum$sex,levels = c(1,2),labels = c('男','女'))
        risk_sum$lung<-ifelse(risk_sum$lung_score>7,1,0)
        risk_sum$breast<-ifelse(!is.na(risk_sum$breast_score),ifelse(risk_sum$breast_score>5,1,0),0)
        risk_sum$liver<-ifelse(risk_sum$liver_score>4,1,0)
        risk_sum$gastric<-ifelse(risk_sum$gastric_score>4,1,0)
        risk_sum$total<-risk_sum$lung+risk_sum$breast+risk_sum$liver+risk_sum$gastric
        risk_sum$max<-apply(risk_sum[,c("lung_score","breast_score",'liver_score','gastric_score')],1,function(x)max(x,na.rm = TRUE))
        #risk_sum$item[risk_sum$lung_score>7 & risk_sum$lung_score==risk_sum$max]<-'低剂量螺旋CT'
        #risk_sum$item[risk_sum$breast_score>5 & risk_sum$breast_score==risk_sum$max]<-'乳腺超声及X线'
        #risk_sum$item[risk_sum$breast_score>5 & risk_sum$breast_score==risk_sum$max]<-'乳腺超声及X线'
        #risk_sum$item[risk_sum$liver_score>4 & risk_sum$lung_score<=7 & risk_sum$liver_score>=risk_sum$breast_score & risk_sum$liver_score>risk_sum$gastric_score]<-'肝脏超声'
        #risk_sum$item[risk_sum$liver_score>4 & risk_sum$liver_score==risk_sum$max & risk_sum$liver_score!=risk_sum$lung_score]<-'肝脏超声'
        #risk_sum$item[risk_sum$gastric_score>4 & risk_sum$gastric_score>=risk_sum$breast_score & risk_sum$gastric_score>risk_sum$liver_score & risk_sum$lung_score<=7]<-'胃镜'
        #risk_sum$item[risk_sum$gastric_score>4 & risk_sum$gastric_score==risk_sum$max & risk_sum$gastric_score!=risk_sum$lung_score]<-'胃镜'
        risk_sum$axvar<-apply(risk_sum[,c("lung_score","breast_score","liver_score","gastric_score")],1,axVar)#每一行中有几个最大分
        #1.高危一项的
        risk_sum$item[risk_sum$total==1 & risk_sum$lung==1]<-'低剂量螺旋CT'
        risk_sum$item[risk_sum$total==1 & risk_sum$breast==1]<-'乳腺超声及X线'
        risk_sum$item[risk_sum$total==1 & risk_sum$liver==1]<-'肝脏超声'
        risk_sum$item[risk_sum$total==1 & risk_sum$gastric==1]<-'胃镜'
        #2高危二项的
        #2.1肺癌和乳腺
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$breast==1 & risk_sum$breast_score==risk_sum$lung_score]<-'二次选择'
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$breast==1 & risk_sum$breast_score<risk_sum$lung_score]<-'低剂量螺旋CT'
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$breast==1 & risk_sum$breast_score>risk_sum$lung_score]<-'乳腺超声及X线'
        #2.2肺癌和肝癌
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$liver==1 & risk_sum$liver_score==risk_sum$lung_score]<-'二次选择'
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$liver==1 & risk_sum$liver_score<risk_sum$lung_score]<-'低剂量螺旋CT'
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$liver==1 & risk_sum$liver_score>risk_sum$lung_score]<-'肝脏超声'
        #2.3肺癌和胃癌
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$gastric==1 & risk_sum$gastric_score==risk_sum$lung_score]<-'二次选择'
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$gastric==1 & risk_sum$gastric_score<risk_sum$lung_score]<-'低剂量螺旋CT'
        risk_sum$item[risk_sum$total==2 & risk_sum$lung==1 & risk_sum$gastric==1 & risk_sum$gastric_score>risk_sum$lung_score]<-'胃镜'
        #2.4乳腺和胃癌
        risk_sum$item[risk_sum$total==2 & risk_sum$breast==1 & risk_sum$gastric==1 & risk_sum$gastric_score<risk_sum$breast_score]<-'乳腺超声及X线'
        risk_sum$item[risk_sum$total==2 & risk_sum$breast==1 & risk_sum$gastric==1 & risk_sum$gastric_score==risk_sum$breast_score]<-'二次选择'
        risk_sum$item[risk_sum$total==2 & risk_sum$breast==1 & risk_sum$gastric==1 & risk_sum$gastric_score>risk_sum$breast_score]<-'胃镜'
        #2.5乳腺和肝癌
        risk_sum$item[risk_sum$total==2 & risk_sum$breast==1 & risk_sum$liver==1 & risk_sum$liver_score<risk_sum$breast_score]<-'乳腺超声及X线'
        risk_sum$item[risk_sum$total==2 & risk_sum$breast==1 & risk_sum$liver==1 & risk_sum$liver_score==risk_sum$breast_score]<-'二次选择'
        risk_sum$item[risk_sum$total==2 & risk_sum$breast==1 & risk_sum$liver==1 & risk_sum$liver_score>risk_sum$breast_score]<-'肝脏超声'
        #2.6肝癌和胃癌
        risk_sum$item[risk_sum$total==2 & risk_sum$gastric==1 & risk_sum$liver==1 & risk_sum$liver_score==risk_sum$gastric_score]<-'二次选择'
        risk_sum$item[risk_sum$total==2 & risk_sum$gastric==1 & risk_sum$liver==1 & risk_sum$liver_score>risk_sum$gastric_score]<-'肝脏超声'
        risk_sum$item[risk_sum$total==2 & risk_sum$gastric==1 & risk_sum$liver==1 & risk_sum$liver_score<risk_sum$gastric_score]<-'胃镜'
        #3高危三项的
        risk_sum$item[risk_sum$axvar>1 & risk_sum$total==3]<-'二次选择'
        risk_sum$item[risk_sum$axvar==1 & risk_sum$total==3 & risk_sum$lung_score==risk_sum$max]<-'低剂量螺旋CT'
        risk_sum$item[risk_sum$axvar==1 & risk_sum$total==3 & risk_sum$breast_score==risk_sum$max]<-'乳腺超声及X线'
        risk_sum$item[risk_sum$axvar==1 & risk_sum$total==3 & risk_sum$liver_score==risk_sum$max]<-'肝脏超声'
        risk_sum$item[risk_sum$axvar==1 & risk_sum$total==3 & risk_sum$gastric_score==risk_sum$max]<-'胃镜'
        #risk_sum$item<-ifelse(risk_sum$total>1,ifelse(risk_sum$axvar>1,'二次选择',risk_sum$item),risk_sum$item)
        risk_sum[,c('area','id','name','sex','age','persoid','cellphone','telephone','lung_score','breast_score','liver_score','gastric_score','lung','breast','liver','gastric','total','item')]
    })
    risk_sum2<-reactive({
        risk_sum2<-melt(risk_sum(),id.vars=c('id','name'),variable.name = 'cancer',value.name = 'score')
    })
    
    ##table3：输出上传表格
    output$table3<-renderDT({
        datatable(dataset2(),class="cell-border stripe",caption = '表1：上传原始数据',width = 12,options=list(pageLength=10,autoWidth=TRUE))
    })
    
    #table：输出人群高危得分
    output$table<-renderDT({
        input$goButton
        inFile<-input$file
        if(is.null(inFile))
            return(NULL)
        datatable(risk_sum3(),options=list(),colnames=c('街道','编号','姓名','性别','年龄','身份证号','手机号','座机',
                                                        '肺癌高危得分','乳腺高危得分','肝癌高危得分','胃癌高危得分','低剂量螺旋CT','乳腺超声及X线','肝脏超声','胃镜','合计','推荐项目'),
                  caption="表2:人群高危得分",
                  class="cell-border stripe")
    })
    #table2:输出各癌种得分分布情况
    risk_table<-reactive({
        table(risk_sum2()$cancer,risk_sum2()$score)
    })
    output$table2<-renderPrint({
        inFile<-input$file
        if(is.null(inFile))
            return(NULL)
        risk_table()
    })
    #输出图像
    mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
                   axis.text=element_text(face="bold",size=9),
                   panel.grid.major = element_line(colour=NA),
                   panel.grid.minor = element_blank(),
                   panel.background=element_rect(fill=NA),
                   axis.title.x=element_blank())
    output$histogram2<-renderPlot({
        ggplot(data=risk_sum(),aes_string(x=risk_sum()$lung_score))+geom_bar(color='black',fill='blue')+
            scale_x_continuous(breaks = seq(0,15,1))+mytheme+labs(title='lung score')
        
    })
    
    output$histogram1<-renderPlot({
        ggplot(data=risk_sum(),aes_string(x=risk_sum()$breast_score))+geom_bar(color='black',fill='red')+
            scale_x_continuous(breaks = seq(0,15,1))+mytheme+labs(title='breast score')
    })
    output$histogram3<-renderPlot({
        ggplot(data=risk_sum(),aes_string(x=risk_sum()$liver_score))+geom_bar(color='black',fill='green')+
            scale_x_continuous(breaks = seq(0,15,1))+mytheme+labs(title='liver score')
    })
    output$histogram4<-renderPlot({
        ggplot(data=risk_sum(),aes_string(x=risk_sum()$gastric_score))+geom_bar(color='black',fill='orange')+
            scale_x_continuous(breaks = seq(0,15,1))+mytheme+labs(title='gastric score')
    })
    ##输出累计分布图
    #1.乳腺
    breast_prop<-reactive({
        vbreast<-cumsum(risk_table()[2,])
        pbreast<-vector()
        for(i in 1:length(vbreast)){
            pbreast[i]<-round(vbreast[i]/max(vbreast),2) 
        }  
        prop.table<-data.frame(score=seq(0,length(pbreast)-1,1),prop=pbreast)
    })
    output$histogram1_1<-renderPlot({
        ggplot(data=breast_prop(),aes(x=score,y=prop))+geom_line(color='red')+scale_x_continuous(breaks=seq(0,max(breast_prop()$score),1))+
            geom_text(aes(x=score,y=prop,label=prop))+labs(x='breast score',y='累计百分比')
        
    })
    #2.肺癌
    lung_prop<-reactive({
        vlung<-cumsum(risk_table()[1,])
        plung<-vector()
        for(i in 1:length(vlung)){
            plung[i]<-round(vlung[i]/max(vlung),2) 
        }  
        prop.table<-data.frame(score=seq(0,length(plung)-1,1),prop=plung)
    })
    output$histogram2_1<-renderPlot({
        ggplot(data=lung_prop(),aes(x=score,y=prop))+geom_line(color='blue')+scale_x_continuous(breaks=seq(0,max(lung_prop()$score),1))+
            geom_text(aes(x=score,y=prop,label=prop))+labs(x='lung score',y='累计百分比')
        
    })
    #肝癌
    liver_prop<-reactive({
        vliver<-cumsum(risk_table()[3,])
        pliver<-vector()
        for(i in 1:length(vliver)){
            pliver[i]<-round(vliver[i]/max(vliver),2) 
        }  
        prop.table<-data.frame(score=seq(0,length(pliver)-1,1),prop=pliver)
    })
    output$histogram3_1<-renderPlot({
        ggplot(data=liver_prop(),aes(x=score,y=prop))+geom_line(color='green')+scale_x_continuous(breaks=seq(0,max(liver_prop()$score),1))+
            geom_text(aes(x=score,y=prop,label=prop))+labs(x='liver score',y='累计百分比')
        
    })
    #胃癌
    gastric_prop<-reactive({
        vgastric<-cumsum(risk_table()[4,])
        pgastric<-vector()
        for(i in 1:length(vgastric)){
            pgastric[i]<-round(vgastric[i]/max(vgastric),2) 
        }  
        prop.table<-data.frame(score=seq(0,length(pgastric)-1,1),prop=pgastric)
    })
    output$histogram4_1<-renderPlot({
        ggplot(data=gastric_prop(),aes(x=score,y=prop))+geom_line(color='orange')+scale_x_continuous(breaks=seq(0,max(gastric_prop()$score),1))+
            geom_text(aes(x=score,y=prop,label=prop))+labs(x='gastric score',y='累计百分比')
        
    })
    #输出推荐的检查项目分布
    output$bar<-renderPlot({
        a<-data.frame(table(risk_sum3()$item))
        names(a)<-c('item','freq')
        ggplot(data=a,aes(x=item,y=freq,fill=item))+geom_bar(stat='identity',width=0.6)+ylab('数量')+xlab('种类')+guides(fill=FALSE)+geom_text(aes(label=freq))+theme(axis.text.x=element_text(size=12))
    })
    #检索个人ID结果
    risk2<-reactive({
        risk_one<-risk_function(dataset())$total
        one<-risk_one[which(id==input$id),]
    })
    #个人信息
    base<-reactive({
        
        base<-risk2()[,c('id','name','sex','age')]
        base$sex<-factor(base$sex,levels = c(1,2),labels=c('男','女'))
        names(base)<-c('ID','姓名','性别','年龄')
        base
    })
    output$IDtable<-renderTable(
        base()
    )
    #肺癌得分
    lung<-reactive({
        lung<-risk2()[,c('lung_family','lung_age','lung_stress','lung_disea_score','lung_occuexpo_score','lung_passivesmk','lung_smoking_score','lung_score')]
        names(lung)<-c('肺癌家族史','年龄','精神','肺病史','职业暴露','被动吸烟','吸烟史','肺癌高危得分')
        lung
    })
    output$lungtable<-renderTable(
        lung()
    ) 
    #乳腺得分
    breast<-reactive({
        breast<-risk2()[,c('breast_family','breast_disea','breast_risk_score','breast_score')]
        names(breast)<-c('乳腺癌家族史','乳腺癌疾病史','乳腺癌危险因素','乳腺癌高危得分')
        breast
    })
    output$breasttable<-renderTable(
        breast()
    )
    # 肝癌得分
    liver<-reactive({
        liver<-risk2()[,c('liver_family','liver_disea_score','liver_risk_score','liver_food_score','liver_score')]
        names(liver)<-c('肝癌家族史','肝病史','肝癌危险因素','食物因素','肝癌高危得分')
        liver
    })
    output$livertable<-renderTable(
        liver()
    )
    #胃癌得分
    gastric<-reactive({
        gastric<-risk2()[,c('gastric_family','gastric_disea_score','gastric_risk_score','gastric_food_score','gastric_score')]
        names(gastric)<-c('胃癌家族史','胃病史','胃癌风险因素','食物因素','胃癌高危得分')
        gastric
    })
    output$gastrictable<-renderTable(
        gastric()
    )
    
    
    
    
    #提供高危人群下载项和非高危人群下载项
    output$download1 <- downloadHandler(
        filename = function(){
            paste("人群高危得分表" ,Sys.Date(),".xlsx", sep = "")
        },
        content = function(file) {
            data<-risk()$total
           
            rio::export(data,file)
            
        }
    )
    output$download2 <- downloadHandler(
        filename = function(){
            paste("高危人群项目推荐表" ,Sys.Date(),".xlsx", sep = "")
        },
        content = function(file) {
            data<-risk_sum3()
            
            rio::export(data,file)
            
        }
    )
    
    
})

