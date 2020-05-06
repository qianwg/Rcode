###2020-5-1
##肝脏超声影像学特征与AFP、CA199、HBsAg的相关性分析
liverBUS<-import('~/data/基层肝脏超声(18-19).sav')
liverAFP<-inner_join(biomarker,liverBUS,by='ID_BLAST')
liverAFP2<-liverAFP%>%transmute(AFP,CA199,HBsAg,AFP_pos=AFP_pos,性别,年龄,BMI,吸烟,
                                肝脏形态=livershap,大小=liversize,包膜=livermemb,占位=ifelse(liverlump<3 & liverlump>0,liverlump,2),占位个数=livelumpno,
                                占位1类型=livelumpN1,占位2类型=livelumpN2,占位3类型=livelumpN2,胆结石=galston,胆囊切除=choletomy,
                                livultrdia
)%>%filter(!is.na(AFP))
liverAFP2$nan<-apply(liverAFP2[,c('占位1类型','占位2类型','占位3类型')],1,function(x)sum(is.na(x)))
#liverAFP2[which(liverAFP2$nan==1),]
#liverAFP2[which(liverAFP2$占位个数==2),c('nan','占位','占位个数','占位1类型','占位2类型','占位3类型')]

##数据处理1
#summary(liverAFP2)
summary(liverAFP$liverlump);table((liverAFP$liverlump))
summary(liverAFP2$占位);table(liverAFP2$占位)
#占位与占位类型与占位个数相关
#summary(liverAFP2$占位);with(liverAFP2,table(占位))
#summary(liverAFP2$占位个数);with(liverAFP2,table(占位个数))
#with(liverAFP2,table(占位个数,占位))
#with(subset(liverAFP2,占位==2),table(is.na(占位),is.na(占位个数)))#有占位而占位个数缺失的有880个
#with(subset(liverAFP2,占位==1),table(is.na(占位),is.na(占位个数)))#无占位而有占位个数的有97个
#with(subset(liverAFP2,is.na(占位)),table(is.na(占位),is.na(占位个数) & nan==3))#占位缺失时占位个数和占位类型的信息
liverAFP2%>%transmute(
  占位1类型,占位2类型,占位3类型,占位=factor(占位,levels = c(1,2),labels=c('占位:无','占位:有')),占位个数=factor(ifelse(占位个数>3,3,占位个数),levels=c(1,2,3),labels=c('占位个数:1','占位个数:2','占位个数:>=3'))
)%>%pivot_longer(cols=c('占位1类型','占位2类型','占位3类型'),names_to='占位类型',values_to ='value' )%>%
  ggplot(aes(x=value,fill=占位类型))+geom_bar(position = 'fill')+facet_grid(占位~占位个数)
#

#无占位而有占位个数的有97个
liverAFP2[which(liverAFP2$占位==1 & !is.na(liverAFP2$占位个数)),c('nan','占位','占位个数','占位1类型','占位2类型','占位3类型')]
#有占位而无占位个数的
liverAFP2[which(liverAFP2$占位==2 & is.na(liverAFP2$占位个数)),c('nan','占位','占位个数','占位1类型','占位2类型','占位3类型')]
#占位缺失
liverAFP2[which(is.na(liverAFP2$占位)),c('占位','占位个数','占位1类型','占位2类型','占位3类型')]
#医生诊断文字描述的文本分析
table(liverAFP2$诊断描述)

#数据处理2

#2.1 无占位而有占位个数的
#占位1，占位类型都缺失,占位个数赋值为无
liverAFP2$占位个数[liverAFP2$占位==1 & is.na(liverAFP2$占位1类型) & is.na(liverAFP2$占位2类型) & is.na(liverAFP2$占位3类型)]<-NA
#占位为1，占位1类型为14，其他占位类型无，占位个数赋值为无
liverAFP2$占位个数[liverAFP2$占位==1 & liverAFP2$占位1类型==14 & is.na(liverAFP2$占位2类型) & is.na(liverAFP2$占位3类型)]<-NA
#占位为1，占位个数为11的，并且只有一个占位类型的，占位个数改为1
liverAFP2$占位个数[liverAFP2$占位==1 & liverAFP2$占位个数==11  & !is.na(liverAFP2$占位1类型) & is.na(liverAFP2$占位2类型) & is.na(liverAFP2$占位3类型)]<-1
#占位为1，根据缺失的占位类型，修改占位个数
liverAFP2$占位个数[liverAFP2$占位==1 & liverAFP2$nan==2]<-1
liverAFP2$占位个数[liverAFP2$占位==1 & liverAFP2$nan==1]<-2
liverAFP2$占位个数[liverAFP2$占位==1 & liverAFP2$nan==0]<-3
#占位为1，占位个数>=1,占位类型至少有一个,占位改为2
liverAFP2$占位[liverAFP2$占位==1 & !is.na(liverAFP2$占位个数)]<-2
#liverAFP2[which(liverAFP2$占位==1 & !is.na(liverAFP2$占位个数)),c('nan','占位','占位个数','占位1类型','占位2类型','占位3类型')]
#2.2 有占位而无占位个数的
#占位为2，根据占位类型的缺失数量，反推占位或占位个数
liverAFP2$占位个数[liverAFP2$占位==2  & liverAFP2$nan==2]<-1
liverAFP2$占位个数[liverAFP2$占位==2  & liverAFP2$nan==1]<-2
liverAFP2$占位个数[liverAFP2$占位==2  & liverAFP2$nan==0]<-3

###占位缺失
#占位缺失，占位个数和占位类型都无，占位改为1
liverAFP2$占位[is.na(liverAFP2$占位) & is.na(liverAFP2$占位个数) & liverAFP2$nan==3]<-1
#占位缺失，占位个数缺失，占位类型至少有一个
liverAFP2$占位个数[is.na(liverAFP2$占位) & liverAFP2$nan==2]<-1
liverAFP2$占位个数[is.na(liverAFP2$占位) & liverAFP2$nan==0]<-3
liverAFP2$占位[is.na(liverAFP2$占位)]<-2
#占位类型异常值
liverAFP2$占位1类型[liverAFP2$占位1类型>15]<-15
###
summary(liverAFP2$占位);table(liverAFP2$占位)
with(liverAFP2,table(占位,nan))#占位中有963人占位类型缺失

###文字诊断分析
source('~/Rcode/biomarker/segment2.R')
word_list<-liverAFP2%>%transmute(占位=占位,诊断=livultrdia)%>%group_by(诊断)%>%summarise(n=n())%>%arrange(desc(n))
head(word_list,20)
liverAFP2%>%transmute(占位=factor(占位,levels = c(1,2),labels=c('占位:否','占位:是')),诊断=livultrdia)%>%group_by(占位,诊断)%>%
  summarise(n=n())%>%arrange(desc(n))%>%filter(n>30)%>%ggplot(aes(诊断,n, fill = 占位)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~占位, scales = "free_y") +
  theme(text=element_text(size=14))+
  coord_flip()
#重新赋值
##word2--pivot_longer转换
word2<-liverAFP2%>%separate(livultrdia,c('占位1','占位2','占位3',"占位4"),sep="([\\.\\ \\；\\，\\、\\？\\。\\/])",remove=FALSE)%>%
  pivot_longer(cols=c('占位1','占位2','占位3','占位4'),names_to = '占位数',values_to = '类型')
word2<-liverBUS_inf(word2)
word2%>%group_by(类型)%>%
  summarise(n=n())%>%filter(!is.na(类型),n>20)%>%ggplot(aes(reorder(类型,n),n)) +
  geom_col(show.legend = FALSE) + mytheme+
  coord_flip()
word2_list<-word2%>%group_by(类型)%>%summarise(n=n())%>%arrange(desc(n))
print(word2_list)
##word3 pivot_wider 再转换
word3<-word2%>%pivot_wider(names_from = 占位数,values_from = 类型)%>%unnest()
#head(word3)
export(word3,'~/data/word3.xlsx')
##