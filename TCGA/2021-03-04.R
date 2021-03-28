rm(list=ls())
library(readr)
library(rio)
library(tidyverse)
library(survival)
library(survminer)
library(forestmodel)
library(openxlsx)
mytheme<-theme(
  axis.title=element_text(family="serif",size=12,face="bold"),
  axis.text=element_text(family="serif",size=12,face="bold"),
  panel.grid.major = element_line(colour=NA),
  panel.grid.minor = element_blank(),
  panel.background=element_rect(fill=NA),
  axis.line = element_line(color='black'),
  legend.title = element_text(family='serif',size=12),
  legend.text = element_text(family = 'serif',size=10),
  legend.key = element_blank(),
  #legend.background = element_rect(colour = 'black')
  
)
#####前列癌#####
##1、数据读取及处理

end<-import('~/data/TCGA_PRAD_Cancer/rs1192691.xlsx')%>%transmute(
  `_PATIENT`=PATIENT,rs1192691=factor(case_when(
    rs1192691=="GG" ~ 2,
    rs1192691=="GT" ~ 1,
    rs1192691=="TT" ~ 0,
    ),levels=c(0,1,2),labels=c('TT','GT','GG')))
PRAD_survival<-import('~/data/TCGA_PRAD_Cancer/TCGA-PRAD.survival.xlsx')%>%transmute(OS.time=round(OS.time/30,2),`_PATIENT`,OS)

geno_survival<-inner_join(end,PRAD_survival,by='_PATIENT')
#2.1 OS breast
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=geno_survival)
ggsurvplot(kms_T, data = geno_survival,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,xlab = "Time in months",risk.table.height = 0.25,legend.labs =
                    c("TT", 'GG','GT'),surv.median.line = 'hv',
                  ggtheme = mytheme,palette =c("#E7B800","#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Overall Survial')
