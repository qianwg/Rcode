rm(list=ls())
library(readr)
library(rio)
library(tidyverse)
library(survival)
library(survminer)
library(forestmodel)
library(openxlsx)
source('~/Rcode/statistics/HR.R')
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
#####卵巢癌#####
##1、数据读取及处理
geno<-import('~/data/TCGA_OV_Cancer/rs1192691.txt')
geno<-geno%>%mutate(rs1192691=paste(V7,V8,sep=""))%>%
  filter(rs1192691!="00")%>%transmute(IID=V2,rs1192691=ifelse(rs1192691=="TT",1,2),
                                                          )
inf<-import('~/data/TCGA_OV_Cancer/match-TCGA-IID.txt')
inf<-inf%>%select(IID,Tumor.Normal,`_PATIENT`=bcr_patient_barcode)%>%filter(!duplicated(`_PATIENT`))

OV_survival<-import('~/data/TCGA_OV_Cancer/OV_survival.txt')
OV_survival$code<-ifelse(as.numeric(as.character(substring(OV_survival$sample,14,15)))<=9,"T",'N')
OV_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')]<-apply(OV_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')],2,
                                                                     function(x){
                                                                       x<-round(x/30,2)
                                                                     })#天数转月
OV_survival_T<-OV_survival%>%filter(code=='T')%>%filter(!duplicated(`_PATIENT`))#癌组织

geno_inf<-inner_join(geno,inf,by='IID')
geno_survival_T<-inner_join(geno_inf,OV_survival_T,by='_PATIENT')
#2.1 OS breast
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=geno_survival_T)
a<-summary(kms_T)
options(digits = 3)
print(survdiff(Surv(OS.time,OS)~rs1192691,data=geno_survival_T),digits = 4)
xOS.T<-ggsurvplot(kms_T, data = geno_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                 risk.table = T,xlab = "Time in months",risk.table.height = 0.25,legend.labs =
                   c("TT", 'GG+GT'),surv.median.line = 'hv',
                 ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
survdiff(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
DDS.T<-ggsurvplot(kms.dss.T, data = geno_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", 'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
survdiff(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
DFI.T<-ggsurvplot(kms.dfi.T, data = geno_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
survdiff(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
PFI.T<-ggsurvplot(kms.pfi.T, data = geno_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)
#Cox
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691,data=geno_survival_T)
summary(model.OS.T)
forest_model(model.OS.T)

