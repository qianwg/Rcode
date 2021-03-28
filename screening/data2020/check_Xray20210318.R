rm(list=ls())
##2021-03-18：检查示范区X线数据
#1、读取数据
questionnaire<-import('~/data/2020/data2020/示范区.xlsx')%>%transmute(
  ID=ID20,姓名=NAME,身份证号=ID18,调查员=survey_name,上传时间=upload_time,初筛编号=A0a,社区=qu
)
#2、身份证重复问题###############################
shanchu<-c(31070432,10801467,10800446,10304813,10900707,10801176,
           10801329,10901217,10301291,10303214,21032108,10303714,10303730,
           10900703,10901186,10301283,10900871,10901603,10300805,10901117,
           21081191,10304967,31090447,10301623)
questionnaire2<-questionnaire%>%filter(!初筛编号 %in% shanchu[-which(shanchu %in% c(10304967,10901603,10303714,10300805,10304813))])
shanchu2<-c(41030430,41090448,41030442,41060752,41091028,41030750,41070098,41030427,41030839,41030273,41090215)
questionnaire3<-questionnaire2%>%filter(!ID %in% shanchu2)
#
Xray<-import('~/data/2020/Xray/Xray.xlsx')%>%transmute(
  ID=ID20,姓名=NAME,身份证号=ID18,调查医师=doctor1,检查时间=InvestDate,社区=sheqcode)

#问卷匹配胸片
#身份证号匹配
Xray$match.persoid<-"身份证匹配"
match_Xray_to_quest<-left_join(questionnaire3,Xray,by="身份证号")
match_Xray_to_quest$match.persoid<-ifelse(is.na(match_Xray_to_quest$match.persoid),"未匹配",match_Xray_to_quest$match.persoid)
table(match_Xray_to_quest$match.persoid)
names(match_Xray_to_quest)[which(names(match_Xray_to_quest) %in% c('ID.x'))]<-"ID"
Xray$match.ID<-"ID匹配"
match_Xray_to_quest2<-left_join(match_Xray_to_quest,Xray[,c('ID','姓名','身份证号','match.ID')],by="ID")
match_Xray_to_quest2$match.ID<-ifelse(is.na(match_Xray_to_quest2$match.ID),"未匹配",match_Xray_to_quest2$match.ID)
table(match_Xray_to_quest2$match.ID)
table(match_Xray_to_quest2$match.ID,match_Xray_to_quest2$match.persoid)
match_Xray_to_quest2[which(match_Xray_to_quest2$match.persoid=="未匹配" & match_Xray_to_quest2$match.ID=="ID匹配"),c('ID','姓名.x',"身份证号.x","ID.y",'姓名.x',"身份证号.y","match.persoid","match.ID")]                         
a<-match_Xray_to_quest2[which(match_Xray_to_quest2$match.persoid=="未匹配" & match_Xray_to_quest2$match.ID=="未匹配"),c('ID','姓名.x',"身份证号.x","ID.y",'姓名.x',"身份证号.y","match.persoid","match.ID")]                         
b<-questionnaire[which(questionnaire$ID %in% a$ID),]
export(b,'~/示范区X线未上传名单.xlsx')
