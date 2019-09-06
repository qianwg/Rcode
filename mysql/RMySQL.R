library(RMySQL)
cnn<-dbConnect(MySQL(),user='root',password='',dbname='screening')#与mysql进行连接
dbListTables(cnn)#查看screening下的数据集
dbListFields(cnn,'biomarker')#查看字段
#解决中文乱码问题
encoding <- if(grepl(pattern = 'utf8|utf-8',x = Sys.getlocale(),ignore.case = T)) 'utf8' else 'latin1'
dbSendQuery(cnn,paste("SET names",encoding))
#读取数据集
baseline<-dbReadTable(cnn,'biomarker')
str(baseline)
#通过sql语句查询数据
dbGetQuery(cnn,"SELECT b.* FROM biomarker AS b WHERE PG1='>200.0'")#查询PG1>200的
dbDisconnect(cnn)#断开连接
rm(list=ls())#清楚environment
