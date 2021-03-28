rm(list=ls())
###2021-03-18：整理2020年示范区去筛查幽门螺杆和基线数据
#1、读取数据
questionnaire<-import('~/data/2020/data2020/示范区.xlsx')%>%mutate(
  ID=ID20,姓名=NAME,身份证号=ID18,调查员=survey_name,上传时间=upload_time,初筛编号=A0a,社区=qu
)
Hp<-import('~/data/2020/data2020/1_5680.xlsx')%>%mutate(
  初筛编号=ID20,姓名.HP=NAME,身份证号=ID18,社区.HP=JieDao,调查员.HP=survey_name,调查时间.HP=JCDate,C14Value
)
#2、身份证重复问题###############################
#2.2Hp
#Hp%>%group_by(身份证号)%>%dplyr::summarise(n=n())%>%filter(n>1)%>%arrange(身份证号)
#Hp[which(Hp$身份证号 %in% Hp$身份证号[duplicated(Hp$身份证号)]),]%>%arrange(身份证号)
shanchu<-c(31070432,10801467,10800446,10304813,10900707,10801176,
           10801329,10901217,10301291,10303214,21032108,10303714,10303730,
           10900703,10901186,10301283,10900871,10901603,10300805,10901117,
           21081191,10304967,31090447,10301623)
Hp[which(Hp$姓名.HP=="马卫东"),'身份证号']<-"120103195111134517"
Hp[which(Hp$姓名.HP=="张淑琴"),'身份证号']<-"120103195112134543"
Hp2<-Hp%>%filter(!初筛编号 %in% shanchu)
Hp3<-Hp2[which(!duplicated(Hp2$身份证号)),]
#Hp3[which(Hp3$身份证号 %in% Hp3$身份证号[duplicated(Hp3$身份证号)]),]%>%arrange(身份证号)
#2.1问卷
#questionnaire%>%group_by(身份证号)%>%dplyr::summarise(n=n())%>%filter(n>1)%>%arrange(身份证号)
#questionnaire[which(questionnaire$身份证号 %in% questionnaire$身份证号[duplicated(questionnaire$身份证号)]),]%>%arrange(身份证号)

questionnaire2<-questionnaire%>%filter(!初筛编号 %in% shanchu[-which(shanchu %in% c(10304967,10901603,10303714,10300805,10304813))])
shanchu2<-c(41030430,41090448,41030442,41060752,41091028,41030750,41070098,41030427,41030839,41030273,41090215)
questionnaire3<-questionnaire2%>%filter(!ID %in% shanchu2)
#questionnaire3[which(questionnaire3$身份证号 %in% questionnaire3$身份证号[duplicated(questionnaire3$身份证号)]),]%>%arrange(身份证号)
export(questionnaire3,"~/示范区.xlsx")


#3、Hp匹配问卷#####
#3.1 (身份证及初筛编号匹配):Hp中有142人未匹配到问卷
questionnaire3$match.quest<-"问卷匹配"
questionnaire3$match.IID<-"初筛编号匹配"
match_ques_to_Hp<-left_join(Hp3,questionnaire3[,c('身份证号','match.quest')],by="身份证号")
match_ques_to_Hp<-left_join(match_ques_to_Hp,questionnaire3[,c('初筛编号','姓名','身份证号','match.IID')],by="初筛编号")
match_ques_to_Hp$match.quest<-ifelse(is.na(match_ques_to_Hp$match.quest),"未匹配",match_ques_to_Hp$match.quest)
match_ques_to_Hp$match.IID<-ifelse(is.na(match_ques_to_Hp$match.IID),"未匹配",match_ques_to_Hp$match.IID)
#table(match_ques_to_Hp$match.quest)
#table(match_ques_to_Hp$match.IID)
#match_ques_to_Hp%>%filter(match.quest=="未匹配" & match.IID=="初筛编号匹配")
#Hp中身份证错误名单
persoid_old<-match_ques_to_Hp[which(match_ques_to_Hp$match.quest=="未匹配" & match_ques_to_Hp$match.IID=="初筛编号匹配"),'身份证号.x']
persoid_new<-match_ques_to_Hp[which(match_ques_to_Hp$match.quest=="未匹配" & match_ques_to_Hp$match.IID=="初筛编号匹配"),'身份证号.y']
update<-data.frame(身份证号=persoid_old,persoid_new)
#更新错误身份证号
Hp4<-left_join(Hp3,update,by='身份证号')
Hp4$persoid_new<-ifelse(is.na(Hp4$persoid_new),Hp4$身份证号,persoid_new)
names(Hp4)[which(names(Hp4) %in% "身份证号")]<-"persoID.Old"
names(Hp4)[which(names(Hp4) %in% "persoid_new")]<-'身份证号'
#再次删除重复
shanchu3<-c('130302195507301730','120103196206131436','120103194907049722','120103196409175527 ',
            '12010119550602255X')
Hp4<-Hp4%>%filter(!persoID.Old %in% shanchu3)
#再次更新(最终8人无问卷)
Hp4[which(Hp4$persoID.Old=="120101195006120346"),'身份证号']<-'120102196506121620'
Hp4[which(Hp4$persoID.Old=="110101195006130317"),'身份证号']<-'120103196206131436'
Hp4[which(Hp4$persoID.Old=="140602195708011557"),'身份证号']<-'120103196008016111'
Hp4[which(Hp4$persoID.Old=="120101195401160147"),'身份证号']<-'120103196309266421'
Hp4[which(Hp4$persoID.Old=="110101195002170258"),'身份证号']<-'120103195812284534'
Hp4[which(Hp4$persoID.Old=="120103196010194216"),'身份证号']<-'120106196211240097'
Hp4[which(Hp4$persoID.Old=="12020319741030292X"),'身份证号']<-'12010319741030292X'
Hp4[which(Hp4$persoID.Old=="120103195708252917"),'身份证号']<-'120103195708252911'
Hp4[which(Hp4$persoID.Old=="12010119600413153X"),'身份证号']<-'12010119600413453X'
Hp4[which(Hp4$persoID.Old=="120101195506022554"),'身份证号']<-'12010119550602255X'
Hp4[which(Hp4$persoID.Old=="120104195805080024"),'身份证号']<-'12010419580508002X'
Hp4[which(Hp4$persoID.Old=="120106196005136547"),'身份证号']<-'120101195406252024'
#Hp4[which(Hp4$persoID.Old=="120111196103161018"),'身份证号']<-'120111196103161018'
Hp4[which(Hp4$persoID.Old=="120103195208254528"),'身份证号']<-'120103195208254523'
Hp4[which(Hp4$persoID.Old=="120103195208254528"),'身份证号']<-'120103197806030027'
#删除无问卷的
Hp5<-Hp4%>%filter(!初筛编号 %in% c(10700162,21111263,31070967,31090573,31110101,31110150,31110269,31110377))
#match_ques_to_Hp2<-left_join(Hp5,questionnaire3[,c('身份证号','match.quest')],by="身份证号")
#match_ques_to_Hp2$match.quest<-ifelse(is.na(match_ques_to_Hp2$match.quest),"未匹配",match_ques_to_Hp2$match.quest)
#table(match_ques_to_Hp2$match.quest)


#4、问卷匹配hp####
#4.1 (身份证匹配):
Hp5$match.Hp<-"Hp匹配"
match_Hp_to_quest<-left_join(questionnaire3,Hp5[,c('身份证号','match.Hp')],by="身份证号")
match_Hp_to_quest$match.Hp<-ifelse(is.na(match_Hp_to_quest$match.Hp),"未匹配",match_Hp_to_quest$match.Hp)
#table(match_Hp_to_quest$match.Hp)
export(match_Hp_to_quest,'~/.xlsx')

#未匹配到Hp(217人)
questionnaire3_noHp<-questionnaire3[which(questionnaire3$ID %in% match_Hp_to_quest[which(match_Hp_to_quest$match.Hp=="未匹配"),'ID']),]

#补充Hp结果
Hp_imput<-import('~/data/2020/data2020/Hp未上传补充(总结20210318).xlsx')%>%filter(!is.na(备注C14值))%>%
  transmute(
    TIME=NA,stime=NA,etime=NA,survey_name=NA,upload_time=NA,
    F_ID=NA,STATE=NA,IS_UPLOAD_ACCESS=NA,F_PATH=NA,NAME=NA,ID20=NA,ID18=NA,match.Hp=NA,
    JieDao=NA,C14Value=NA,Result=NA,JCDate=NA,persoID.Old=NA,社区.HP=NA,调查员.HP=调查员,调查时间.HP=NA,
    身份证号=身份证号.x,姓名.HP=姓名,初筛编号=初筛编号.x,C14Value=备注C14值)
#summary(Hp_imput$备注C14值)
#根据身份证号进行匹配
#questionnaire3_noHp2<-left_join(questionnaire3_noHp,Hp_imput[,c('姓名','ID','身份证号','初筛编号','备注1(原因)','备注C14值','match.imput2')],by="姓名")
#questionnaire3_noHp2$match.imput<-ifelse(is.na(questionnaire3_noHp2$match.imput2),"未匹配上",questionnaire3_noHp2$match.imput2)
#table(questionnaire3_noHp2$match.imput)
#Hp_imput[-which(Hp_imput$身份证号 %in% questionnaire3_noHp2[which(questionnaire3_noHp2$match.imput=="姓名匹配"),"身份证号.y"]),]
#export(questionnaire3_noHp2,'~/Hp未上传补充.xlsx')
#合并
Hp6<-rbind(Hp5,Hp_imput)
export(Hp6,"~/Hp20210318.xlsx")






