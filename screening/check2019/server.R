library(shiny)
library(DT)
require(shinydashboard)
library(ggplot2)
# Define server logic required to draw a histogram
options(shiny.maxRequestSize=70*1024^2,shiny.sanitize.errors=TRUE,endcoding='UTF-8')
shinyServer(function(input, output) {
    #反应式1,读取上传的数据库
    dataset<-reactive({
        inFile1 <- input$file
        if(is.null(inFile1)) return(NULL)
        if(strsplit(inFile1$datapath,'.',fixed=TRUE)[[1]][2]=='xlsx'){
            data<-openxlsx::read.xlsx(inFile1$datapath,detectDates = TRUE)
        }else if(strsplit(inFile1$datapath,'.',fixed=TRUE)[[1]][2]=='sav'){
            data<-haven::read_sav(inFile1$datapath) 
        }else{print('无法支持的数据格式')}
    })
    ##家族史词典
    family<-c('外唇癌','唇癌','舌癌','牙龈癌','口底癌','口腔癌','腮腺癌','大涎腺癌',
              '扁桃体癌','口咽癌','鼻咽癌','下咽癌','咽癌','食管癌','贲门癌','胃癌',
              '十二指肠癌','空肠癌','回肠癌','小肠癌','结肠癌','直肠癌','肛门癌','肝癌',
              '肝内胆管癌','胆囊癌','肝外胆管癌','胆道癌','胰癌','肠道癌','胃肠道癌',
              '鼻窦癌','喉癌','肺癌','纵隔癌','胸膜癌','骨癌','甲状腺','乳癌',
              '白血病','骨髓癌','脾癌','网状内皮系统癌','造血系统癌','皮肤癌','周围神经和自主神经系统癌',
              '腹膜癌','结缔组织、皮下组织和其他软组织癌','乳腺癌','外阴癌','阴道癌','宫颈癌','子宫癌',
              '子宫附件癌','女性生殖道癌','阴茎癌','前列腺癌','睾丸癌','阴囊癌','男性生殖器官癌','肾癌',
              '肾盂癌','膀胱癌','尿道癌','泌尿系统癌','眼癌','脑膜癌','脑癌','中枢神经系统癌',
              '甲状腺癌','内分泌腺癌','淋巴结癌','原发部位未知癌','卵巢癌','消化系统交搭跨越的恶性肿瘤',
              '胸腺癌','子宫内膜癌','子宫体癌','输尿管癌','颈动脉体癌','头、脸和颈部其他部位癌','胸部其他部位癌',
              '腹部其他部位癌','血癌','肠癌','胆癌','胆管癌','淋巴癌','盲肠癌','血管癌','输卵管癌','黑色素瘤','胰腺癌',
              '食道癌','大肠癌','乳腺','胃','鼻癌','肺','肝','鼻咽','喷门癌','胰头癌','胰腺','不详',
              '前列腺','肝胆','肝胆癌','脊膜癌','子宫内膜癌','子宫内膜','其他','喉咽癌','壶腹周围癌','纤维细胞瘤','肉瘤',
              '胆囊炎','脑瘤','再生障碍贫血','多发性骨髓瘤','肝胆管癌','脊髓瘤','脑转癌','腹部细胞癌','胆总管癌',
              '脑瘤','淋巴癌，肾癌','黑色素癌','汗腺癌','脊髓瘤','贲门癌，肠癌','肺转淋巴癌','肺癌、食道癌','大肠癌、胃癌',
              '肺癌、肠癌','肝癌腹膜癌','胃十二指肠癌','胆总管下段癌','肠癌、子宫癌','胃癌、结肠癌','乳癌癌','卵巢输卵管癌',
              '甲状腺肿瘤','胃癌喉癌',
              '上颌窦癌 ','喉癌，肺癌','喉癌肾癌','喷门癌食管癌','壶腹癌','已扩散全身','幽门癌','慢性淋巴瘤',
              '淋巴瘤','粘膜癌', '肝癌，肺癌','肝癌，骨癌','肺小细胞癌','肺癌.','肺癌胃癌肝癌','肺癌 骨癌','肺转脑',
              '肾癌肺癌','胃癌肝癌','胸膜间皮瘤','胸膜间隙癌','脂肪瘤','腹壶癌','腹部癌','膀胱癌、前列','血液癌',
              '血液肿瘤','面部','骨髓瘤','鳞癌','黑素瘤',
               '乳腺癌子宫癌','乳腺癌宫颈癌','乳腺癌，肺癌','乳腺癌 胃癌','乳腺癌转肝癌',
              '宫颈癌、胃癌','淋巴癌、肺癌','直肠癌、肝癌','肺泡癌','肺癌.','胃癌、肠癌','直肠癌、肺癌','淋巴癌，肺癌',
              '肺癌/骨癌','乳腺啊','肝癌乳腺癌','胃癌，肺癌','乳腺癌卵巢癌','肺癌、肾癌','子宫癌肝癌'
    )
    family2<-c('外唇癌'=1,'唇癌'=2,'舌癌'=3,'牙龈癌'=4,'口底癌'=5,'口腔癌'=6,'腮腺癌'=7,'大涎腺癌'=8,
               '扁桃体癌'=9,'口咽癌'=10,'鼻咽癌'=11,'下咽癌'=12,'咽癌'=13,'食管癌'=14,'贲门癌'=16,'胃癌'=16,
               '十二指肠癌'=17,'空肠癌'=18,'回肠癌'=19,'小肠癌'=20,'结肠癌'=21,'直肠癌'=22,'肛门癌'=23,'肝癌'=24,
               '肝内胆管癌'=25,'胆囊癌'=26,'肝外胆管癌'=27,'胆道癌'=28,'胰癌'=29,'肠道癌'=30,'胃肠道癌'=31,
               '鼻窦癌'=32,'喉癌'=33,'肺癌'=34,'纵隔癌'=35,'胸膜癌'=36,'骨癌'=37,
               '白血病'=38,'骨髓癌'=39,'脾癌'=40,'网状内皮系统癌'=41,'造血系统癌'=42,'皮肤癌'=43,'周围神经和自主神经系统癌'=44,
               '腹膜癌'=45,'结缔组织、皮下组织和其他软组织癌'=46,'乳腺癌'=47,'外阴癌'=48,'阴道癌'=49,'宫颈癌'=50,'子宫癌'=51,
               '子宫附件癌'=52,'女性生殖道癌'=53,'阴茎癌'=54,'前列腺癌'=55,'睾丸癌'=56,'阴囊癌'=57,'男性生殖器官癌'=58,'肾癌'=59,
               '肾盂癌'=60,'膀胱癌'=61,'尿道癌'=62,'泌尿系统癌'=63,'眼癌'=64,'脑膜癌'=65,'脑癌'=66,'中枢神经系统癌'=67,
               '甲状腺癌'=68,'内分泌腺癌'=69,'淋巴结癌'=70,'原发部位未知癌'=71,'卵巢癌'=72,'消化系统交搭跨越的恶性肿瘤'=73,
               '胸腺癌'=74,'子宫内膜癌'=75,'子宫体癌'=76,'输尿管癌'=78,'颈动脉体癌'=79,'头、脸和颈部其他部位癌'=80,'胸部其他部位癌'=81,
               '腹部其他部位癌'=82,'血癌'=83,'肠癌'=84,'胆癌'=85,'胆管癌'=86,'淋巴癌'=87,'盲肠癌'=88,'血管癌'=89,'输卵管癌'=90,'黑色素瘤'=91,
               '胰腺癌'=29,'食道癌'=14,'大肠癌'=92,'乳腺'=47,'胃'=16,'鼻癌'=11,'肺'=34,'肝'=24,'鼻咽'=11,'脊膜癌'=65,
               '喷门癌'=16,'胰头癌'=29,'胰腺'=29,'不详'=99,'前列腺'=55,'甲状腺'=68,'乳癌'=47,'肝胆'=24,'子宫内膜癌'=51,
               '肝胆癌'=24,'子宫内膜'=51,'其他'=99,'喉咽癌'=99,'壶腹周围癌'=99,'纤维细胞瘤'=99,'肉瘤'=99,'脑转癌'=99,
               '胆囊炎'=99,'脑瘤'=99,'再生障碍贫血'=99,'多发性骨髓瘤'=99,'肝胆管癌'=24,'脊髓瘤'=99,'腹部细胞癌'=99,'胆总管癌'=99,
               '脑瘤'=99,'黑色素癌'=91,'汗腺癌'=99,'脊髓瘤'=99,'贲门癌，肠癌'=16,'肺转淋巴癌'=34,'肺癌、食道癌'=34,
               '肺癌、肠癌'=34,'大肠癌、胃癌'=24,'肝癌腹膜癌'=24,'胃十二指肠癌'=16,'胆总管下段癌'=99,'肠癌、子宫癌'=75,
               '胃癌、结肠癌'=24,'乳癌癌'=47,'卵巢输卵管癌'=72,'甲状腺肿瘤'=68,'胃癌喉癌'=16,
               '上颌窦癌 '=99,'喉癌，肺癌'=34,'喉癌肾癌'=12,'喷门癌食管癌'=16,'壶腹癌'=99,'已扩散全身'=99,'幽门癌'=16,'慢性淋巴瘤'=99,
               '淋巴瘤'=87,'粘膜癌'=99, '肝癌，肺癌'=34,'肝癌，骨癌'=24,'肺小细胞癌'=34,'肺癌.'=34,'肺癌胃癌肝癌'=34,'肺癌 骨癌'=34,'肺转脑'=34,
               '肾癌肺癌'=34,'胃癌肝癌'=16,'胸膜间皮瘤'=99,'胸膜间隙癌'=99,'脂肪瘤'=99,'腹壶癌'=99,'腹部癌'=99,'膀胱癌、前列'=99,'血液癌'=99,
               '血液肿瘤'=99,'面部'=99,'骨髓瘤'=99,'鳞癌'=99,'黑素瘤'=99,
               '乳腺癌子宫癌'=47,'乳腺癌宫颈癌'=47,'乳腺癌，肺癌'=34,'乳腺癌 胃癌'=16,'乳腺癌转肝癌'=47,
               '宫颈癌、胃癌'=16,'淋巴癌、肺癌'=34,'直肠癌、肝癌'=24,'肺泡癌'=34,'肺癌.'=34,'胃癌、肠癌'=16,'直肠癌、肺癌'=34,
               '淋巴癌，肺癌'=34, '肺癌/骨癌'=34,'乳腺啊'=47,'肝癌乳腺癌'=24,'胃癌，肺癌'=34,'乳腺癌卵巢癌'=47,'肺癌、肾癌'=34,'子宫癌肝癌'=24  
    )
    ##家族史情况
    familytab<-reactive({
        familytab<-dataset()[,c('CATPSelf','CATPFath','CATPMoth','CATPBrot1','CATPBrot2','CATPSist1','CATPSist2','CATPChil1','CATPChil2')]
    })
    family_value<-reactive({
        
        family_value<-apply(familytab(),2,function(x){x<-ifelse(x %in% family,family2[x],x)}) 
        
    })
    #家族史函数
    famfun<-function(data){
      names(data)<-tolower(names(data))
        data[,c('catpfath','catpmoth','catpbrot1','catpbrot2',
                'catpsist1','catpsist2','catpchil1','catpchil2')]<-apply(data[,c('catpfath','catpmoth','catpbrot1','catpbrot2',
                                                                                 'catpsist1','catpsist2','catpchil1','catpchil2')],2,
                                                                                function(x){x<-ifelse(x %in% family,family2[x],99)}) 
        return(data)
    }
    famfun2<-function(data){
      data[,c('CATPFath','CATPMoth','CATPBrot1','CATPBrot2',
              'CATPSist1','CATPSist2','CATPChil1','CATPChil2')]<-apply(data[,c('CATPFath','CATPMoth','CATPBrot1','CATPBrot2','CATPSist1','CATPSist2','CATPChil1','CATPChil2')],2,
                                                                       function(x){x<-ifelse(x %in% family,family2[x],x)})
      return(data)
    }
    ##家族史赋值
    dataset2<-reactive({
        famfun(dataset())
    })
    dataset3<-reactive(
      {famfun2(dataset())}
    )
    #输出赋值前的数据
    output$tablefam<-renderDT({
        datatable(dataset(),class="cell-border stripe",width = 12,options=list(pageLength=5,autoWidth=TRUE))
    })
    #输出赋值后的数据
    output$tablefam2<-renderDT({
        if(input$action){
            datatable(dataset3(),class="cell-border stripe",width = 12,options=list(pageLength=5,autoWidth=TRUE))
        }
    })
    #输出家族史summary
    output$summary_family<-renderPrint({
        if(input$action){
            summary(family_value(),maxsum=99)
            }
    })
    #自建问卷评价函数
    questionare<-function(data){
        #data<-data.frame(apply(data,2,function(x){
        #  x<-ifelse(x==99 | x==9,NA,x)
        #}))
        names(data)<-tolower(names(data))
        data$telephone<-as.character(data$telephone)
        data$cellphone<-as.character(data$cellphone)
        attach(data)
        check1<-ifelse(is.na(id),1,0)
        check2<-ifelse(is.na(name),1,0)
        check3<-ifelse(is.na(sex),1,0)
        check4<-ifelse(is.na(age),1,0)
        check5<-ifelse(is.na(race),1,ifelse(race<=2 & race>=1,0,1))
        check6<-ifelse(is.na(persoid),1,0)
        check7<-ifelse(is.na(cellphone) & is.na(telephone),1,0)
        check8<-ifelse(!is.na(marriag),ifelse(marriag>=1 & marriag<=4,0,1),1)
        check9<-ifelse(!is.na(educati) & educati>=1 & educati<=6,0,1)
        check10<-ifelse(!is.na(employm) & employm>=1 & employm<=4,0,1)
        check11<-ifelse(!is.na(occupat),0,ifelse(employm==4,0,1))
        check12<-ifelse(!is.na(income) & income>=1 & income<=4,0,1)
        check13<-ifelse(!is.na(bloodtp) & bloodtp>=1 & bloodtp<=5,0,1)
        check14<-ifelse(!is.na(insuran) & insuran>=1 & insuran<=7,0,1)
        check15<-ifelse(is.na(address),1,0)
        check16<-ifelse(!is.na(weight) & weight>=30 & weight<=400,0,1)
        check17<-ifelse(!is.na(height),ifelse(height>=30 & height<=250,0,1),1)
        check18<-ifelse(is.na(investigat),1,0)
        check19<-ifelse(is.na(investdate),1,0)
        check20<-ifelse(is.na(recorddate),1,0)
        check_part1_table<-data.frame(check1,check2,check3,check4,check5,check6,check7,check8,check9,
                                      check10,check11,check12,check13,check14,check15,check16,check17,check18,check19,check20)
        check_part1<-rowSums(check_part1_table)
        ##第二部分--基础信息不合格率
        ###河西区
        area<-vector()
        qu<-vector()
        area[id>=31010001 & id<=31013000]<-"东海街"
        area[id>=31040001 & id<=31043000]<-"马场街"
        area[id>=31050001 & id<=31053000]<-"天塔街"
        area[id>=31100001 & id<=31103000]<-"柳林街"
        qu[id>=31010001 & id<=31123000]<-'河西区'
        ###和平区
        area[id>=33010001 & id<=33013000]<-"小白楼街"
        area[id>=33020001 & id<=33023000]<-"南市街"
        area[id>=33050001 & id<=33053000]<-"五大道街"
        area[id>=33060001 & id<=33063000]<-"劝业场街"
        qu[id>=33010001 &id<=33063000]<-'和平区'
        ###南开区
        area[id>=34010001 & id<=34013000]<-"华苑街"
        area[id>=34020001 & id<=34023000]<-"王顶堤街"
        area[id>=34030001 & id<=34033000]<-"体育中心街"
        area[id>=34060001 & id<=34063000]<-"万兴街（三潭)"
        qu[id>=34010001 & id<=34063000 ]<-'南开区'
        ###河北区
        area[id>=36010001 & id<=36013000]<-"王串场街"
        area[id>=36020001 & id<=36023000]<-"鸿顺里街"
        area[id>=36030001 & id<=36033000]<-"月牙河街"
        area[id>=36050001 & id<=36053000]<-"铁东路街"
        qu[id>=36010001 & id<=36053000]<-'河北区'
        ###河东区
        area[id>=35010001 & id<=35013000]<-"上杭路街"
        area[id>=35020001 & id<=35023000]<-"鲁山道街"
        area[id>=35040001 & id<=35043000]<-"唐家口街"
        area[id>=35060001 & id<=35063000]<-"富民路街"
        qu[id>=35010001 & id<=35063000]<-'河东区'
        ### 红桥区
        area[id>=37010001 & id<=37013000]<-"芥园道街"
        area[id>=37020001 & id<=37023000]<-"邵公庄街"
        area[id>=37030001 & id<=37033000]<-"和苑街"
        area[id>=37060001 & id<=37063000]<-"丁字沽街"
        qu[id>=37010001 & id<=37063000]<-'红桥区'
        ###蓟州区
        area[id>=32010001 & id<=32013000]<-"官庄镇"
        area[id>=32030001 & id<=32033000]<-"下营镇"
        area[id>=32050001 & id<=32053000]<-"桑梓镇"
        area[id>=32060001 & id<=32063000]<-"东二营镇"
        qu[id>=32010001 & id<=32063000]<-'蓟州区'
        check21<-ifelse(is.na(area),1,0)
        check22<-ifelse(!is.na(cellphone),ifelse(substr(cellphone,1,1)==1 & nchar(cellphone)==11,0,1),1)
        check23<-ifelse(!is.na(telephone),ifelse(substr(telephone,1,1)>=2 & nchar(telephone)==8,0,1),1)
        check24<-ifelse(!is.na(name),ifelse(nchar(name)>=2 & nchar(name)<=4,0,1),0)
        check25<-ifelse(!is.na(persoid),ifelse(nchar(persoid)==18,0,ifelse(nchar(persoid)==15,0,1)),0)
        check26<-ifelse(check22==1 & check23==1,1,0)
        check_part2_table<-data.frame(check21,check22,check23,check24,check25,check26)
        check_part2<-check21+check24+check25+check26
        ##第三部分--重要信息逻辑错误
        #1.癌症家族史
        cafath_0<-ifelse(is.na(cafath),1,ifelse(cafath==2,2,1))
        camoth_0<-ifelse(is.na(camoth),1,ifelse(camoth==2,2,1))
        cabrot1_0<-ifelse(is.na(cabrot1),1,ifelse(cabrot1==2,2,1))
        cabrot2_0<-ifelse(is.na(cabrot2),1,ifelse(cabrot2==2,2,1))
        casist1_0<-ifelse(is.na(casist1),1,ifelse(casist1==2,2,1))
        casist2_0<-ifelse(is.na(casist2),1,ifelse(casist2==2,2,1))
        cachil1_0<-ifelse(is.na(cachil1),1,ifelse(cachil1==2,2,1))
        cachil2_0<-ifelse(is.na(cachil2),1,ifelse(cachil2==2,2,1))
        cancer_score<-cafath_0+camoth_0+cabrot1_0+cabrot2_0+casist1_0+casist2_0+cachil1_0+cachil2_0
        check27<-ifelse(is.na(cancerfh),1,ifelse(cancerfh==2 & cancer_score<=8,1,0))
        check28<-ifelse(is.na(cancerfh),1,ifelse(cancerfh==1 & cancer_score>8,1,0))
        check2728<-ifelse(check27==1 | check28==1,1,0)#合并家族史方面的错误
        #2.疾病史
        disea_table<-data[,which(names(data) %in% paste0("disea",1:33))]
        dis_table<-as.data.frame(apply(disea_table,2,function(x){
            x<-ifelse(is.na(x),1,ifelse(x==2,2,1))
        }))
        names(dis_table)<-paste0("dis",1:33)
        disea_score<-rowSums(dis_table)
        check29<-ifelse(!is.na(nodisea),ifelse(nodisea==2 & disea_score>33,1,0),0)#疾病史逻辑错误1
        check30<-ifelse(!is.na(nodisea),ifelse(nodisea==1 & disea_score<=33,1,0),0)#疾病史逻辑错误2
        check300<-ifelse(is.na(disea1) |is.na(disea2) | is.na(disea3) |is.na(disea4)|
                             is.na(disea5)|is.na(disea6)|is.na(disea7)|is.na(disea8)|is.na(disea9)|
                             is.na(disea10)|is.na(disea11)|is.na(disea12)|is.na(disea13)|is.na(disea14)|
                             is.na(disea15)|is.na(disea16)|is.na(disea17)|is.na(disea18)|is.na(disea19)|
                             is.na(disea20)|is.na(disea21)|is.na(disea22)|is.na(disea23)|is.na(disea24)|
                             is.na(disea25)|is.na(disea26)|is.na(disea27)|is.na(disea28)|is.na(disea29)|
                             is.na(disea30)|is.na(disea31)|is.na(disea32)|is.na(disea33),1,0 )
        check301<-ifelse(is.na(disea1) |is.na(disea2) | is.na(disea3) |is.na(disea4)|
                             is.na(disea5)|is.na(disea6)|is.na(disea7)|is.na(disea8)|is.na(disea9)|
                             is.na(disea10)|is.na(disea11)|is.na(disea12)|is.na(disea13)|is.na(disea14)|
                             is.na(disea15)|is.na(disea16)|is.na(disea17)|is.na(disea18)|is.na(disea21)|is.na(disea22)|
                             is.na(disea23)|is.na(disea24)| is.na(disea19) | is.na(disea20) |
                             is.na(disea28)|is.na(disea29)|is.na(disea30)|is.na(disea31)|is.na(disea32)|
                             is.na(disea33) ,1,0 )
        check3000<-ifelse(!is.na(sex),ifelse(sex==2 & check300==1,1,0),0)#疾病史有缺失1
        check3010<-ifelse(!is.na(sex),ifelse(sex==1 & check301==1,1,0),0)#疾病史有缺失2
        check2930<-ifelse(check29==1 | check30==1 |check3000==1 | check3010==1,1,0)#整个疾病史有无错误
        #3.吸烟史
        check31<-ifelse(!is.na(smoking),ifelse(smoking==2,ifelse(is.na(cpd) | is.na(smkyrs),1,0),0),0)
        check32<-ifelse(!is.na(smoking),ifelse(smoking==3,ifelse(!is.na(cpd) & !is.na(smkyrs) & !is.na(quitsmkyrs) & quitsmkyrs>0,0,1),0),0)
        check33<-ifelse(is.na(smoking),1,0)
        check3132<-ifelse(check31==1 |check32==1 |check33==1,1,0)#合并吸烟史的错误
        #4.职业暴露史
        asbestos0<-ifelse(is.na(asbestos),1,ifelse(asbestos==2,2,1))
        cadmium0<-ifelse(is.na(cadmium),1,ifelse(cadmium==2,2,1))
        nickel0<-ifelse(is.na(nickel),1,ifelse(nickel==2,2,1))
        arsenic0<-ifelse(is.na(arsenic),1,ifelse(arsenic==2,2,1))
        radon0<-ifelse(is.na(radon),1,ifelse(radon==2,2,1))
        chloroethy0<-ifelse(is.na(chloroethy),1,ifelse(chloroethy==2,2,1))
        xray0<-ifelse(is.na(xray),1,ifelse(xray==2,2,1))
        benzene0<-ifelse(is.na(benzene),1,ifelse(benzene==2,2,1))
        expouse_score<-asbestos0+cadmium0+nickel0+arsenic0+radon0+chloroethy0+xray0+benzene0
        check34<-ifelse(is.na(noccuexp),ifelse(is.na(asbestos) |is.na(cadmium)| is.na(nickel)|
                                                   is.na(arsenic)|is.na(radon)|is.na(chloroethy)|
                                                   is.na(xray)|is.na(benzene),1,0),0)#noccuexp
        check35<-ifelse(!is.na(noccuexp),ifelse(noccuexp==2 & expouse_score>8,1,0),0)
        check36<-ifelse(!is.na(noccuexp),ifelse(noccuexp==1 & expouse_score<=8,1,0),0)
        check3536<-ifelse(check34==1 | check35==1 | check36==1,1,0)#合并职业暴露方面的错误
        #5.女性生理与生育方面
        check37_1<-ifelse(sex==2,ifelse(!is.na(agemenarch) & !is.na(menopause) & !is.na(deliver) & menopause>=1 & menopause<=2 & deliver>=1 & deliver<=2,0,1),0)
        check37_2<-ifelse(sex==2,ifelse(abortion==2,ifelse(induabort>10 | sponabort>10,1,0),0),0)
        check37<-ifelse(check37_1==0 & check37_2==0,0,1)
        check38<-ifelse(sex==1 & !is.na(agemenarch) & !is.na(sex),1,0)
        check39<-ifelse(!is.na(sex) & sex==2 & agemenarch>0 & agemenarch<99 & agemenarch>age & !is.na(agemenarch) & !is.na(age),1,0)
        check40_2<-ifelse(sex==2,ifelse(menopause==2,ifelse(agemenopau>5 & agemenopau<=age,0,1),0),0)
        check40_1<-ifelse(!is.na(menopause) & !is.na(sex) & !is.na(agemenopau) & !is.na(age),ifelse(sex==2 & menopause==2 & agemenopau<99 & agemenopau>age,1,0),0)
        check40<-ifelse(check40_1==0 & check40_2==0,0,1)
        check41<-ifelse(!is.na(menopause) & !is.na(sex) & !is.na(agemenopau) & !is.na(agemenarch),ifelse(sex==2 & menopause==2 & agemenopau<99 & agemenarch<99 & agemenopau<agemenarch,1,0),0)
        check42_1<-ifelse(sex==2 & deliver==2 & agefirdeli<99 & agefirdeli>age & !is.na(sex) &!is.na(deliver) &!is.na(agefirdeli) &!is.na(age),1,0)
        check42_2<-ifelse(sex==2,ifelse(deliver==2,ifelse(deliver<=10 & deliver>=1 & agefirdeli<=age & agefirdeli>agemenarch,0,1),0),0)
        check42<-ifelse(check42_1==1 | check42_2==1,1,0)
        check43_1<-ifelse(sex==2 & deliver==2 & is.na(agefirdeli)  & !is.na(sex) & !is.na(deliver),1,0)
        check43_2<-ifelse(sex==2,ifelse(deliver==2,ifelse(agefirdeli<=age & agefirdeli>agemenarch,0,1),0),0)
        check43<-ifelse(check43_1==0 & check43_2==0,0,1)
        check44<-ifelse(sex==2 & menopause==2 & is.na(agemenopau)& !is.na(sex) &!is.na(menopause),1,0)
        check3745<-ifelse(check37==1 | check38==1 | check39==1 | check40==1 | check41==1 | check42==1 | check43==1 | check44==1,1,0)
        check_part3_table<-data.frame(check27,check28,check29,check30,check3000,check3010,
                                      check31,check32,check33,check34,check35,check36,
                                      check37,check38,check39,check40,check41,check42,
                                      check43,check44,check2728,check2930,check3132,check3536,check3745)
      
        
        check_part3<-check2728+check2930+check3132+check3536+check3745
        ##合并信息
        end<-ifelse(check_part1==0 & check_part2==0 & check_part3==0,0,1)
        check_table<-cbind(qu,area,data,check_part1_table,check_part1,check_part2_table,check_part2,
                           check_part3_table,check_part3,end)
        
        return(check_table)
    }
    
    #反应式2 计算的结果  
    dataset_result<-reactive({
        questionare(dataset2())
    })
    
    #反应式4 单独输出合格问卷
    check_name2<-c('area','qu',paste("check",1:44,sep=""),'check2728','check2930','check3132','check3536',
                   'check3745','check_part1','check_part2',
                   'check_part3','end')
    check_name3<-c(paste("check",1:44,sep=""),'check2728','check2930','check3132','check3536','check3000','check3010',
                   'check3745','check_part1','check_part2',
                   'check_part3','end')
    data_right<-reactive({
        data_right<-dataset_result()[which(dataset_result()$end==0),-which(names(dataset_result()) %in% check_name3)]
        data_right$catpfath<-ifelse(data_right$cafath==1 | is.na(data_right$cafath),NA,data_right$catpfath)
        data_right$catpmoth<-ifelse(data_right$camoth==1 | is.na(data_right$camoth),NA,data_right$catpmoth)
        data_right$catpbrot1<-ifelse(data_right$cabrot1==1 | is.na(data_right$cabrot1),NA,data_right$catpbrot1)
        data_right$catpbrot2<-ifelse(data_right$cabrot2==1 | is.na(data_right$cabrot2),NA,data_right$catpbrot2)
        data_right$catpsist1<-ifelse(data_right$casist1==1 | is.na(data_right$casist1),NA,data_right$catpsist1)
        data_right$catpsist2<-ifelse(data_right$casist2==1 | is.na(data_right$casist2),NA,data_right$catpsist2)
        data_right$catpchil1<-ifelse(data_right$chil1==1 | is.na(data_right$cachil1),NA,data_right$catpchil1)
        data_right$catpchil2<-ifelse(data_right$cachil2==1 | is.na(data_right$cachil2),NA,data_right$catpchil2)
        data_right
    })
    #反应式5 单独输出不合格问卷
    data_wrong<-reactive({
        dataset_result()[which(dataset_result()$end>0),]
       
      
      })
    
    #反应式3 单独输出结果合格中的各部分检查概况
    check_name<-c('id','name','area','check_part1','check_part2','check_part3','end')
    data_check<-reactive({
        dataset_result()[which(dataset_result()$end==0),which(names(dataset_result()) %in% check_name)]
    })
    #反应式 单独输出不合格中的部分检查概况
    data_check2<-reactive({
        dataset_result()[which(dataset_result()$end>0),which(names(dataset_result()) %in% check_name)]
    })
    #反应式6 各街道合格与不合格分布
    area_question<-reactive({
        table_prim<-table(dataset_result()$area,dataset_result()$end)
        table_sum<-colSums(table_prim)
        table<-rbind(table_prim,table_sum)
        failure_rate<-paste(round(table[,2]/(table[,1]+table[,2]),4)*100,"%",sep="")
        table_new<-cbind(table,failure_rate)
        colnames(table_new)<-c('合格','不合格','不合格率') 
        table_new
    })
    ##画图：统计错误类别
   
    #table
    output$table<-renderDT({
        inFile<-input$file
        if(is.null(inFile))
            return(NULL)
        datatable(data_check(),options=list(),class="cell-border stripe",colnames = c('编号','姓名','街道',
                                                                                      '第一部分','第二部分','第三部分','合计'))%>%formatStyle('id',
                                                                                                                                'end',backgroundColor = styleEqual(c(1,0),c('red','gray'))
                                                                                      )
    })
    #table2
    output$table2<-renderDT({
        inFile<-input$file
        if(is.null(inFile))
            return(NULL)
        datatable(data_check2(),options=list(),class="cell-border stripe",colnames = c('街道','编号','姓名',
                                                                                      '第一部分','第二部分','第三部分','合计'))%>%formatStyle('id',
                                                                                                                                'end',backgroundColor = styleEqual(c(1,0),c('red','gray'))
                                                                                      )
    })
    #summary
    output$summary<-renderPrint({
        inFile<-input$file
        if(is.null(inFile))
            return(NULL)
        area_question()
    })
    ###搜索个人
    #   names(check_table)[196:250]<-c('编号','姓名','性别','年龄','种族','身份证','电话','婚姻','文化程度',
    #    '就业','职业','收入',
    #   '血型','社保','地址','体重','身高','调查者','调查日','录入日','第一部分合计',
    #    '街道','手机','座机','姓名逻辑','身份证逻辑','电话逻辑','第二部分合计',
    #    '癌症家族史1','癌症家族史2','疾病史1','疾病史2','疾病史缺失1','疾病史缺失2','吸烟史1','吸烟史2','吸烟史3','职业暴露缺失',
    #    '职业暴露1','职业暴露2','生理与生育缺失','初潮','初潮2','绝经1','绝经2','生育1','生育2',
    #    '绝经3','癌症家族史','疾病史','吸烟史','职业暴露史','女性生理与生育','第三部分合计','总计')
   
     check_one<-reactive({
        check_one2<-dataset_result()[which(id==input$id),]
        names(check_one2)[193:240]<-c('编号','姓名','性别','年龄','种族','身份证','电话','婚姻','文化程度',
                                            '就业','职业','收入','血型','社保','地址','体重','身高','调查者','调查日','录入日','第一部分合计',
                                            '编号范围','手机','座机','姓名逻辑','身份证逻辑','电话逻辑','第二部分合计',
                                            '癌症家族史1','癌症家族史2','疾病史1','疾病史2','疾病史缺失1','疾病史缺失2',
                                            '吸烟史1','吸烟史2','吸烟史3','职业暴露缺失','职业暴露1','职业暴露2','生理与生育缺失',
                                            '初潮','初潮2','绝经1','绝经2','生育1','生育2','绝经3')
                                            
        names(check_one2)[245]<-'第三部分合计'
        check_one2
        })
    output$ID1table<-renderTable(
        check_one()[,c('id','name')]
    )
    output$ID2table<-renderTable(
        check_one()[,193:207]
        #[,which(names(check_one()) %in% c(paste("check",1:15,sep="")))]
    )
    output$ID3table<-renderTable(
        check_one()[,208:213]
        #[,which(names(check_one()) %in% c(paste("check",16:20,sep=""),'check_part1'))]
    )
    output$ID4table<-renderTable(
        check_one()[,214:220]
        #[,which(names(check_one()) %in% c('check_part2',paste("check",21:26,sep="")))]
    )
    
    
    output$ID5table<-renderTable(
        check_one()[221:222]
        #[,c('check27','check28')]
    )
    output$ID6table<-renderTable(
        check_one()[,223:226]
        #[,c('check29','check30','check3000','check3010')]
    )
    output$ID7table<-renderTable(
        check_one()[,227:229]
        #[,c('check31','check32','check33')]
    )
    output$ID8table<-renderTable(
        check_one()[,230:232]
        #[,c('check34','check35','check36')]
        
    )
    output$ID9table<-renderTable(
        check_one()[,which(names(check_one()) %in% c('生理与生育缺失','初潮','初潮2','绝经1','绝经2','生育1','生育2','绝经3','第三部分合计'))]
        #[,which(names(check_one()) %in% c(paste("check",37:44,sep=""),'check_part3'))]
        
    )
    #下载1,：合格问卷
    output$download1 <- downloadHandler(
        filename = function(){
            paste("合格问卷" ,Sys.Date(),".xlsx", sep = "")
        },
        content = function(file) {
            data<-data_right()
            openxlsx::write.xlsx(data,file)
            
        }
    )
    ##下载2：不合格问卷
    output$download2 <- downloadHandler(
        filename = function(){
            paste("不合格问卷" ,Sys.Date(),".xlsx", sep = "")
        },
        content = function(file) {
            data<-data_wrong()
            openxlsx::write.xlsx(data,file)
            
        }
    )
    
    
})

