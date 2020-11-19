library(rio)
library(tidyverse)
library(ggplotify)
library(eoffice)
survival<-import('C:/Users/sheng/Desktop/survival.xlsx')
mytheme<-theme(plot.title=element_text(hjust=0.5),
               axis.title=element_text(family="serif",size=8),
               axis.text=element_text(family="serif",size=10),
               axis.text.x  = element_text(family="serif",size=10),
               #panel.grid.major = element_line(colour=NA),
               #panel.grid.major.x = element_line(color='grey'),
               #panel.grid.major.y = element_line(color='grey'),
               panel.background = element_blank(),
               axis.line = element_line(color='grey'),
               #legend.title=element_text(face="bold",size=14),
               legend.title=element_blank(),
               legend.text = element_text(face='bold',size=13),
               legend.position = 'top',
               axis.ticks.y=element_blank(),
               axis.line.y = element_blank(),
               #strip.text.x =element_text(face='blod',color='red')
)
mytheme2<-theme(plot.title=element_text(hjust=0.5),
                axis.title=element_text(family="serif",size=8),
                axis.text=element_text(family="serif",size=10),
                axis.text.x  = element_text(family="serif",size=10),
                #panel.grid.major = element_line(colour=NA),
                panel.grid.major.x = element_line(color='grey'),
                #panel.grid.major.y = element_line(color='grey'),
                panel.background = element_blank(),
                axis.line = element_line(color='grey'),
                #legend.title=element_text(face="bold",size=14),
                legend.title=element_blank(),
                legend.text = element_text(face='bold',size=13),
                legend.position = 'none',
                #strip.text.x =element_text(face='blod',color='red')
)
survival2<-survival%>%ggplot()+geom_bar(aes(x=reorder(cancer_site,percent),y=percent),stat='identity',color='lightblue',fill='lightblue',width = 0.5)+
  geom_errorbar(aes(x=cancer_site,ymin=lower,ymax=upper),size=0.5,width=0.5)+coord_flip()+
  mytheme+labs(x='',y='')+
  scale_x_discrete(labels=c('Pancreas        17823','Liver        66575','Gallbladder        10550','Lung        122870',
                            'Leukaemia        13190','Bone        3430','Brain        10391','Oesophagus        63506',
                            'Stomach        82065','Other thoracic organs        1880','Lymphoma        16903',
                            ' Ovary        8576','All        659732','Melanoma of skin        1305','Nasopharynx        7966',
                            'Oral cavity and pharynx        7627','All others        23970','Testis        578',
                            'Colon–rectum        61736','Larynx        4029',' Cervix        11496','Prostate        11690',
                            'Kidney        15671','Uterus        11531','Bladder        16727','Breast        49176',
                            'Thyroid        18470'
                            
  ))
survival3<-survival%>%ggplot()+geom_bar(aes(x=reorder(cancer_site,percent),y=0),stat='identity',color='lightblue',fill='lightblue',width = 0.5)+
  coord_flip()+mytheme+labs(x='',y='')+scale_y_continuous(limits = c(0,85))+
  scale_x_discrete(labels=c('7.2','12.1','16.4','19.7','25.4','26.5','26.7','30.3',
                            '35.1','36.7','37.2','39.1','40.5','45.1','45.5',
                            '50.4','53.3','55.2','56.9','57.7','59.8','66.4',
                            '69.8','72.8','72.9','82.0',
                            '84.3'
                            
  ))


topptx(survival2, 'C:/Users/sheng/Desktop/survival.pptx')
topptx(survival3, 'C:/Users/sheng/Desktop/survival3.pptx')

###gastric cancer 5-year survival rate 
survival<-import('C:/Users/sheng/Desktop/survival.xlsx')
survival1<-survival%>%ggplot(aes(x=Stage2,y=OS,color=Stage,group=Stage))+geom_point()+geom_line()+
  theme_bw()+theme(legend.position = 'none')+labs(x='',y='5-year Overall survival rate')+
  scale_y_continuous(limits = c(0,100),breaks=seq(0,100,10),labels=c('0','10%','20%',
                                                                     '30%','40%','50%','60%','70%','80%',
                                                                     '90%','100%'))+
  scale_x_discrete(labels=c('IA\n91.5%\n91.5%','IB\n83.6%\n83.3%',
                            'II\n70.6%\n68.9%','IIIA\n53.6%\n49.6%',
                            'IIIB\n34.8%\n32.3%','IIIB\n16.4%\n17.0%'))
topptx(survival1, 'C:/Users/sheng/Desktop/survival1.pptx')


