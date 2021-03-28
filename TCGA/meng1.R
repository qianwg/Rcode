rm(list=ls())
library(rio)
library(tidyverse)
library(ggpubr)
epic<-import('~/data/meng/MCPCOUNTER.xlsx')
epic2<-data.frame(t(epic))
names(epic2)<-as.vector(unlist(epic2[1,]))
epic3<-epic2[-1,]%>%rownames_to_column("patient")%>%mutate(IID=substring(patient,1,12),
                                                           code=ifelse(as.numeric(as.character(substring(patient,14,15)))<=9,"T",'N'))
sample1<-read.table('~/data/meng/sample1.txt')%>%mutate(group="sample1")
sample2<-read.table('~/data/meng/sample2.txt')%>%mutate(group="sample2")
sample<-rbind(sample1,sample2)%>%mutate(IID=substring(V1,1,12),group=factor(group))
epic4<-left_join(epic3,sample,by='IID')
epic4[,2:9]<-apply(epic4[,2:9],2,as.numeric)
variables<-names(epic4)[2:9]
#variables<-c("B cell_EPIC","Cancer associated fibroblast_EPIC","T cell CD4+_EPIC","T cell CD8+_EPIC","Endothelial cell_EPIC",            
#"Macrophage_EPIC","NK cell_EPIC","uncharacterized cell_EPIC")
epic5<-epic4%>%pivot_longer(cols=variables,names_to="type",values_to="value")%>%mutate(value2=log2(value))
#summary(epic4)
#pdf("~/tu/violin.pdf")
ggviolin(epic5,x="type",y="value2",fill="group",palette = "lancet",#palette改变配色
         x.text.angle= 50,add='boxplot')+
  labs(x="",y="log2(value)")+stat_compare_means(label.y=19,aes(label = ..p.signif..,group = group,),size=5)
#label.y=x,x由坐标轴实际最大值决定，例如这y轴显示最大为20，x可以选择20左右


###导出P值excel
p<-list()
for(i in variables){
    p[[i]]<-round(wilcox.test(epic4[,i]~group,data=epic4)$p.value,4)
}
export(as.data.frame(do.call(rbind,p))%>%rownames_to_column('type'),'~/P_value.xlsx')





