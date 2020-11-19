rm(list=ls())
source('~/Rcode/statistics/check_idcard.R')
##数据剔除
#6027人
pepsinogen<-import('~/data/示范区2019年--2020-8-10.sav')
#排除无PG数据的:7
summary(pepsinogen$PGI)
#step1:排除身份证无效或缺失的人：0人
pepsinogen$check_id<-check_id(pepsinogen$persoID_check)
summary(pepsinogen$persoID_check)#无缺失
pepsinogen[which(pepsinogen$persoID_check %in% pepsinogen$persoID[duplicated(pepsinogen$persoID)]),
           c('ID_BLAST','persoID_check','name_check','region')]#重复2人
#去除郄玉萍/31030159(删除)/31080045和杨春玲/31060088/31060461(删除)
table(pepsinogen$check_id)#无编码规则错误

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#step2:排除问卷中既往患癌的人：11人
with(pepsinogen,table(CASelf))
#step3:排除既往有胃部切除术：8人
with(pepsinogen,table(Disea24))
#step4:排除初筛时发现的胃癌及癌前病变：37人
gastroscopy<-import('~/data/示范区胃镜结果--2020-8-17.xlsx')
names(gastroscopy)[1]<-'ID_BLAST'
pepsinogen<-left_join(pepsinogen,gastroscopy[,c('ID_BLAST','type')],by='ID_BLAST')
table(pepsinogen$type)#4\6\7\8\9=3+12+9+13+3=40人
#门诊或住院记录匹配到已患癌
with(pepsinogen,table(是否患癌))
pepsinogen[which(pepsinogen$是否患癌==1),c('ID_BLAST','name_check','DOI_check','CA_type','是否患癌','诊断时间','癌症类型')]

###
pepsinogen2<-pepsinogen%>%mutate(type2=ifelse(is.na(type),0,type))%>%filter(ID_BLAST!=31030159,ID_BLAST!=31060461,
                                 !is.na(PGI),CASelf!=2,Disea24!=2,type2!=4,type2!=6,type2!=7,type2!=8,type2!=9,是否患癌!=1,
                                 CA_type=="")

##导出
export(pepsinogen2,'~/data/示范区2019年(去除不合格数据后)--2020-8-18.xlsx')
