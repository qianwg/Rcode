###2021-03-13:
#@分析早期胃癌患者术后淋巴结转移的临床病理特征及预后分析
library(survival)
library(compareGroups)
#读取数据
seer<-import('~/data/SEER/数据.xlsx')%>%transmute(
  Year_diagnsis=factor(case_when(
    Year_diagnosis>=2004 & Year_diagnosis<=2010 ~ 1,
    Year_diagnosis>2010 & Year_diagnosis<=2015 ~ 2,
    
  ),levels=c(1,2),labels=c('2004-2010','2011-2015')),Age,
  Age_group=factor(ifelse(Age<65,0,1),levels=c(0,1),labels=c('<60','>=60')),
  extension=factor(extension,levels=c(1,2),labels=c('I1a','I1b')),
  N=factor(N,levels=c(1,2,3,4),labels=c('N0','N1','N2','N3')),
  N2=factor(ifelse(N=="N0",0,1),levels=c(0,1),labels=c('Negative','Positive')),
  Grade=factor(Grade,levels=c(1,2),labels=c('I/II','III、IV')),
  Primary_Site=factor(ifelse(Primary_Site==0,3,Primary_Site),levels=c(1,2,3),labels=c('Noncardia','Cardia','Other')),
  Race=factor(Race,levels=c(1,2,3),labels=c('White','Black','Other')),
  Race2=factor(`Race recode (W, B, AI, API)`),Race3=factor(`Origin recode NHIA (Hispanic, Non-Hisp)`),
  Sex=factor(Sex,levels=c(1,2),labels=c('Male','Female')),
  laurn=factor(Histologic,levels=c(1,2,3),labels=c('intestinal','diffuse','other')),
  surgery_type=
    case_when(
      Surg==33 | Surg==51 ~ 1,
      Surg==40 | Surg==41 |Surg==42 | Surg==52 ~ 2,
      ),
  surgery_type=factor(ifelse(is.na(surgery_type),3,surgery_type),levels=c(1,2,3),labels=c('proximal gastrectomy','total gastrectomy','Other')),
  size_group=factor(
   case_when(
     size/10<=3 | size==992 | size==993 ~ 1,
     size/10<=5 & size/10>3 ~2,
     size==994 | size==995 ~ 2,
     size/10>5 ~ 3
 ),levels=c(1,2,3),labels=c('<=3','3.01-5','>5')
 ),
 nodes,nodes_pos=`Regional nodes positive (1988+)`,
 nodes_group=factor(
   case_when(
     nodes<=6 ~ 1,
     between(nodes,7,14) ~ 2,
     nodes>=15 ~ 3
   ),levels = c(1,2,3),labels=c('<=6','7-14','>=15')
 ),
 node_pos_rate=ifelse(nodes>0,nodes_pos/nodes,NA),
 status,time=months,survival=Surv(time,status)
)
summary(seer)
##基线描述
descrTable(survival~Age_group+extension+N+N2+Grade+
          Primary_Site+Race+Race2+Sex+laurn+size_group,ref=c(Race2=4),
          show.ratio = TRUE,show.p.trend=TRUE,data=seer)
#早期胃癌患者术后淋巴结转移的情况分布
descrTable(N2~Year_diagnsis+Age_group+extension+Grade+surgery_type+nodes+nodes_group+
             Primary_Site+Race+Race2+Sex+laurn+size_group,ref=c(Race2=4),show.all = TRUE,
           show.ratio = TRUE,show.p.trend=TRUE,data=seer)
#







