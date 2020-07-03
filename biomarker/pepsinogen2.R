rm(list=ls())
pepsinogen%>%group_by(年龄分组)%>%summarise(n=n(),median=median(PGR,na.rm=TRUE),Q1=quantile(PGR,0.25,na.rm=TRUE),Q3=quantile(PGR,0.75,na.rm=TRUE))
pepsinogen%>%group_by(吸烟,年龄分组)%>%summarise(n=n(),median=median(PGR,na.rm=TRUE),Q1=quantile(PGR,0.25,na.rm=TRUE),Q3=quantile(PGR,0.75,na.rm=TRUE))
pepsinogen%>%group_by(性别,吸烟,年龄分组)%>%summarise(n=n(),median=median(PGR,na.rm=TRUE),Q1=quantile(PGR,0.25,na.rm=TRUE),Q3=quantile(PGR,0.75,na.rm=TRUE))
prop.table(table(pepsinogen$年龄分组,pepsinogen$吸烟))
#PG1
pepsinogen%>%group_by(吸烟,年龄分组)%>%summarise(n=n(),median=median(PG1,na.rm=TRUE),Q1=quantile(PG1,0.25,na.rm=TRUE),Q3=quantile(PG1,0.75,na.rm=TRUE))
pepsinogen%>%group_by(性别,吸烟,年龄分组)%>%summarise(n=n(),median=median(PG1,na.rm=TRUE),Q1=quantile(PG1,0.25,na.rm=TRUE),Q3=quantile(PG1,0.75,na.rm=TRUE))
#BMI 与PG1  
pepsinogen%>%group_by(BMI_group,年龄分组)%>%summarise(n=n(),median=median(PG1,na.rm=TRUE),Q1=quantile(PG1,0.25,na.rm=TRUE),Q3=quantile(PG1,0.75,na.rm=TRUE))
pepsinogen%>%group_by(性别,BMI_group,年龄分组)%>%summarise(n=n(),median=median(PG1,na.rm=TRUE),Q1=quantile(PG1,0.25,na.rm=TRUE),Q3=quantile(PG1,0.75,na.rm=TRUE))

#########3
