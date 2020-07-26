
#读取数据
biomarker2020<-import('~/data/2020肿瘤标志物+Hp(截止到2020-7-22).xlsx')
pepsinogen2019_2020<-import('~/data/match2019-2020.xlsx')
source('~/Rcode/statistics/data_summary.R')









#查看2020年肿标分布
do.call(rbind,apply(biomarker2020[,c('PG1','PG2','PGR','AFP')],2,data_summary2))
table(biomarker2020[,c('C14_pos')])
##
