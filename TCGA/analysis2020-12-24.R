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
#####乳腺癌#####
##1、数据读取及处理
phenotype<-read_tsv('~/data/TCGA_Breast_Cancer/TCGA-BRCA.GDC_phenotype.tsv')
phenotyp2<-phenotype%>%transmute(
  `_PATIENT`=submitter_id,Sex=factor(gender.demographic),Age=age_at_initial_pathologic_diagnosis,
  tumor_stage.diagnoses
)
phenotyp2$stage<-NA
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage i" | phenotyp2$tumor_stage.diagnoses=="stage ia" | phenotyp2$tumor_stage.diagnoses=="stage ib"]<-1
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage ii" | phenotyp2$tumor_stage.diagnoses=="stage iia" | phenotyp2$tumor_stage.diagnoses=="stage iib"]<-2
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage iii" | phenotyp2$tumor_stage.diagnoses=="stage iiia" | phenotyp2$tumor_stage.diagnoses=="stage iiib" | phenotyp2$tumor_stage.diagnoses=="stage iiic"]<-3
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage iv"]<-4
phenotyp2$stage<-factor(phenotyp2$stage,levels=c(1,2,3,4),labels=c('I','II','III','IV'))
phenotyp2<-phenotyp2[which(!duplicated(phenotyp2$`_PATIENT`)),]
###
geno<-import('~/data/TCGA_Breast_Cancer/rs1192691_Breast.xlsx')
geno<-geno%>%filter(rs1192691_genotype!="00")%>%transmute(IID,
                                                          code=ifelse(as.numeric(as.character(substring(IID,14,15)))<=9,"T",'N'),
                                                          a=as.character(substring(IID,16,16)),
                                                          rs1192691=factor(ifelse(rs1192691_genotype=="TT",1,ifelse(rs1192691_genotype=="GT",2,3)),levels=c(1,2,3),labels=c('TT','GT','GG')),
                                                          `_PATIENT`=PATIENT)

geno_T<-geno%>%filter(code=="T")%>%filter(!duplicated(`_PATIENT`))#癌组织
#geon_T[which(duplicated(geon_T$`_PATIENT`)),]
#id<-geon_T[which(duplicated(geon_T$`_PATIENT`)),'_PATIENT']
#geon_T[which(geon_T$`_PATIENT` %in% id),]
BRCA_survival<-import('~/data/TCGA_Breast_Cancer/BRCA_survival.txt')
BRCA_survival$code<-ifelse(as.numeric(as.character(substring(BRCA_survival$sample,14,15)))<=9,"T",'N')
BRCA_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')]<-apply(BRCA_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')],2,
                                                                     function(x){
                                                                       x<-round(x/30,2)
                                                                     })#天数转月
BRCA_survival_T<-BRCA_survival%>%filter(code=='T')%>%filter(!duplicated(`_PATIENT`))#癌组织
#
#id2<-BRCA_survival_T[which(duplicated(BRCA_survival_T$`_PATIENT`)),'_PATIENT']
#BRCA_survival_T[which(BRCA_survival_T$`_PATIENT` %in% id2),]
##
geno_survival_T<-inner_join(BRCA_survival_T,geno_T,by='_PATIENT')
#2.1 OS breast
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(geno_survival_T,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192691,data=subset(geno_survival_T,!is.na(OS.time)))
OS.T<-ggsurvplot(kms_T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
survdiff(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
DDS.T<-ggsurvplot(kms.dss.T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
survdiff(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
DFI.T<-ggsurvplot(kms.dfi.T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
survdiff(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
PFI.T<-ggsurvplot(kms.pfi.T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)
##COX regression
phen_survival_T<-inner_join(geno_survival_T,phenotyp2,by='_PATIENT')
phen_survival_N<-inner_join(geno_survival_N,phenotyp2,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,PFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu4/cox_breast.xlsx')

#####肝癌####
rm(list=ls())
##1、数据读取及处理
phenotyp<-read_tsv('~/data/TCGA/TCGA_Liver_Cancer/TCGA-LIHC.GDC_phenotype.tsv')
phenotyp2<-phenotyp%>%transmute(
  `_PATIENT`=submitter_id,Sex=factor(gender.demographic),Age=age_at_initial_pathologic_diagnosis,
  tumor_stage.diagnoses
)
phenotyp2$stage<-NA
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage i" | phenotyp2$tumor_stage.diagnoses=="stage ia" | phenotyp2$tumor_stage.diagnoses=="stage ib"]<-1
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage ii" | phenotyp2$tumor_stage.diagnoses=="stage iia" | phenotyp2$tumor_stage.diagnoses=="stage iib"]<-2
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage iii" | phenotyp2$tumor_stage.diagnoses=="stage iiia" | phenotyp2$tumor_stage.diagnoses=="stage iiib" | phenotyp2$tumor_stage.diagnoses=="stage iiic"]<-3
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage iv" | phenotyp2$tumor_stage.diagnoses=="stage iva" | phenotyp2$tumor_stage.diagnoses=="stage ivb"]<-4
phenotyp2$stage<-factor(phenotyp2$stage,levels=c(1,2,3,4),labels=c('I','II','III','IV'))
phenotyp2<-phenotyp2[which(!duplicated(phenotyp2$`_PATIENT`)),]
#apply(phenotyp2,2,function(x)sum(is.na(x)))
#summary(phenoty2)
###
geno<-import('~/data/TCGA/TCGA_Liver_Cancer/rs1192691_Liver.xlsx')
geno<-geno%>%filter(rs1192691_genotype!="00")%>%transmute(IID,
                                                          code=ifelse(as.numeric(as.character(substring(IID,14,15)))<=9,"T",'N'),
                                                          a=as.character(substring(IID,16,16)),
                                                          rs1192691=factor(ifelse(rs1192691_genotype=="TT",1,ifelse(rs1192691_genotype=="GT",2,3)),levels=c(1,2,3),labels=c('TT','GT','GG')),
                                                          `_PATIENT`=PATIENT)
geno_T<-geno%>%filter(code=="T")%>%filter(!duplicated(`_PATIENT`))#癌组织
#
LIHC_survival<-import('~/data/TCGA/TCGA_Liver_Cancer/LIHC_survival.txt')
LIHC_survival$code<-ifelse(as.numeric(as.character(substring(LIHC_survival$sample,14,15)))<=9,"T",'N')
LIHC_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')]<-apply(LIHC_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')],2,
                                                                     function(x){
                                                                       x<-round(x/30,2)
                                                                     })#天数转月
LIHC_survival_T<-LIHC_survival%>%filter(code=='T')%>%filter(!duplicated(`_PATIENT`))#癌组织
#head(BRCA_survival)
#geno[which(duplicated(geno$`_PATIENT`)),]
#id<-BRCA_survival[which(duplicated(BRCA_survival5$`_PATIENT`)),2]
#BRCA_survival[which(BRCA_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
geno_survival_T<-inner_join(LIHC_survival_T,geno_T,by='_PATIENT')%>%
  mutate(rs1192691=factor(case_when(
    rs1192691=="TT" ~ 1,
    rs1192691=="GT" ~ 2,
    rs1192691=="GG" ~ 2,
    
  ),levels=c(1,2),labels=c('TT','GT+GG')))#%>%filter(OS.time>0)
#head(geno_survival)
#geno_survival[which(geno_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
##2、K-M curve  breast cancer
#table(geno_survival$rs1192658_genotype)
#summary(geno_survival[,c('OS.time','OS','DSS.time','DSS','DFI.time','DFI','PFI.time','PFI')])

#2.1 OS liver
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(geno_survival_T,!is.na(OS.time)))
#survdiff(Surv(OS.time,OS)~rs1192691,data=subset(geno_survival_T,!is.na(OS.time)))
OS.T<-ggsurvplot(kms_T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT", "GT+GG"),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
#survdiff(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
DDS.T<-ggsurvplot(kms.dss.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT+GG"),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
#survdiff(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
DFI.T<-ggsurvplot(kms.dfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT+GG"),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
#survdiff(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
PFI.T<-ggsurvplot(kms.pfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT+GG"),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)

##COX regression
phen_survival_T<-inner_join(geno_survival_T,phenotyp2,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691+Age+stage,data=subset(phen_survival_T,rs1192691!="00"))
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,PFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu4/cox.Liver.xlsx')

#####肺癌####

rm(list=ls())
##1、数据读取及处理
phenotyp.LUAD<-read_tsv('~/data/TCGA/TCGA_Lung_Cancer/TCGA-LUAD.GDC_phenotype.tsv')
phenotyp.LUSC<-read_tsv('~/data/TCGA/TCGA_Lung_Cancer/TCGA-LUSC.GDC_phenotype.tsv')
#class(phenotype)
#dim(phenotype)
#names(phenotype)
phenotyp2.LUAD<-phenotyp.LUAD%>%transmute(
  `_PATIENT`=submitter_id,Sex=factor(gender.demographic),Age=age_at_initial_pathologic_diagnosis,
  tumor_stage.diagnoses
)
phenotyp2.LUSC<-phenotyp.LUSC%>%transmute(
  `_PATIENT`=submitter_id,Sex=factor(gender.demographic),Age=age_at_initial_pathologic_diagnosis,
  tumor_stage.diagnoses
)
phenotyp2<-rbind(phenotyp2.LUAD,phenotyp2.LUSC)
#table(phenotyp2$tumor_stage.diagnoses)
phenotyp2$stage<-NA
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage i" | phenotyp2$tumor_stage.diagnoses=="stage ia" | phenotyp2$tumor_stage.diagnoses=="stage ib"]<-1
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage ii" | phenotyp2$tumor_stage.diagnoses=="stage iia" | phenotyp2$tumor_stage.diagnoses=="stage iib"]<-2
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage iii" | phenotyp2$tumor_stage.diagnoses=="stage iiia" | phenotyp2$tumor_stage.diagnoses=="stage iiib" | phenotyp2$tumor_stage.diagnoses=="stage iiic"]<-3
phenotyp2$stage[phenotyp2$tumor_stage.diagnoses=="stage iv" | phenotyp2$tumor_stage.diagnoses=="stage iva" | phenotyp2$tumor_stage.diagnoses=="stage ivb"]<-4
phenotyp2$stage<-factor(phenotyp2$stage,levels=c(1,2,3,4),labels=c('I','II','III','IV'))
phenotyp2<-phenotyp2[which(!duplicated(phenotyp2$`_PATIENT`)),]
#apply(phenotyp2,2,function(x)sum(is.na(x)))
#summary(phenoty2)
###
geno<-import('~/data/TCGA/TCGA_Lung_Cancer/rs1192691_LUNG.xlsx')
geno<-geno%>%filter(rs1192691_genotype!="00")%>%transmute(IID,
                                                          code=ifelse(as.numeric(as.character(substring(IID,14,15)))<=9,"T",'N'),
                                                          a=as.character(substring(IID,16,16)),
                                                          rs1192691=factor(ifelse(rs1192691_genotype=="TT",1,ifelse(rs1192691_genotype=="GT",2,3)),levels=c(1,2,3),labels=c('TT','GT','GG')),
                                                          `_PATIENT`=PATIENT)
geno_T<-geno%>%filter(code=="T")%>%filter(!duplicated(`_PATIENT`))#癌组织
##
LUNG_survival<-import('~/data/TCGA/TCGA_Lung_Cancer/LUNG_survival.txt')
LUNG_survival$code<-ifelse(as.numeric(as.character(substring(LUNG_survival$sample,14,15)))<=9,"T",'N')
LUNG_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')]<-apply(LUNG_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')],2,
                                                                     function(x){
                                                                       x<-round(x/30,2)
                                                                     })#天数转月
LUNG_survival_T<-LUNG_survival%>%filter(code=='T')%>%filter(!duplicated(`_PATIENT`))#癌组织
#head(BRCA_survival)
#geno[which(duplicated(geno$`_PATIENT`)),]
#id<-BRCA_survival[which(duplicated(BRCA_survival5$`_PATIENT`)),2]
#BRCA_survival[which(BRCA_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
geno_survival_T<-inner_join(LUNG_survival_T,geno_T,by='_PATIENT')
#head(geno_survival)
#geno_survival[which(geno_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
##2、K-M curve  breast cancer
#table(geno_survival$rs1192658_genotype)
#summary(geno_survival[,c('OS.time','OS','DSS.time','DSS','DFI.time','DFI','PFI.time','PFI')])

#2.1 OS liver
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(geno_survival_T,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192691,data=subset(geno_survival_T,!is.na(OS.time)))
OS.T<-ggsurvplot(kms_T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
survdiff(Surv(DSS.time,DSS)~rs1192691,data=geno_survival_T)
DDS.T<-ggsurvplot(kms.dss.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
survdiff(Surv(DFI.time,DFI)~rs1192691,data=geno_survival_T)
DFI.T<-ggsurvplot(kms.dfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
survdiff(Surv(PFI.time,PFI)~rs1192691,data=geno_survival_T)
PFI.T<-ggsurvplot(kms.pfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)

##COX regression
phen_survival_T<-inner_join(geno_survival_T,phenotyp2,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691+Age+stage,data=subset(phen_survival_T,rs1192658!="00"))
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu4/cox.LUNG.xlsx')


#####肺鳞癌LUAD####
phenotyp2.LUAD<-phenotyp.LUAD%>%transmute(
  `_PATIENT`=submitter_id,Sex=factor(gender.demographic),Age=age_at_initial_pathologic_diagnosis,
  tumor_stage.diagnoses
)
#table(phenotyp2$tumor_stage.diagnoses)
phenotyp2.LUAD$stage<-NA
phenotyp2.LUAD$stage[phenotyp2.LUAD$tumor_stage.diagnoses=="stage i" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage ia" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage ib"]<-1
phenotyp2.LUAD$stage[phenotyp2.LUAD$tumor_stage.diagnoses=="stage ii" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage iia" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage iib"]<-2
phenotyp2.LUAD$stage[phenotyp2.LUAD$tumor_stage.diagnoses=="stage iii" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage iiia" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage iiib" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage iiic"]<-3
phenotyp2.LUAD$stage[phenotyp2.LUAD$tumor_stage.diagnoses=="stage iv" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage iva" | phenotyp2.LUAD$tumor_stage.diagnoses=="stage ivb"]<-4
phenotyp2.LUAD$stage<-factor(phenotyp2.LUAD$stage,levels=c(1,2,3,4),labels=c('I','II','III','IV'))
phenotyp2.LUAD<-phenotyp2.LUAD[which(!duplicated(phenotyp2.LUAD$`_PATIENT`)),]
#apply(phenotyp2,2,function(x)sum(is.na(x)))
#summary(phenoty2)
###
phen_survival_T<-inner_join(geno_survival_T,phenotyp2.LUAD,by='_PATIENT')%>%
  mutate(rs1192691=factor(case_when(
    rs1192691=="TT" ~ 1,
    rs1192691=="GT" ~ 1,
    rs1192691=="GG" ~ 2,
    
  ),levels=c(1,2),labels=c('TT+GT','GG')))#%>%filter(OS.time>0)
##plot--KM
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=phen_survival_T)
OS.T<-ggsurvplot(kms_T, data =phen_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT+GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192691,data=phen_survival_T)
#survdiff(Surv(DSS.time,DSS)~rs1192691,data=phen_survival_T)
DDS.T<-ggsurvplot(kms.dss.T, data = phen_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT+GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192691,data=phen_survival_T)
#survdiff(Surv(DFI.time,DFI)~rs1192691,data=phen_survival_T)
DFI.T<-ggsurvplot(kms.dfi.T, data =phen_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT+GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192691,data=phen_survival_T)
#survdiff(Surv(PFI.time,PFI)~rs1192691,data=phen_survival_T)
PFI.T<-ggsurvplot(kms.pfi.T, data =phen_survival_T,pval = T, break.x.by = 30,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT+GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)




#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691+Age+stage,data=subset(phen_survival_T,rs1192691!="00"))
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192691','Age','stage'),data=subset(phen_survival_T,rs1192691!="GT"))
coxph1<-coxph(Surv(OS.time,OS)~rs1192691,data=subset(phen_survival_T,rs1192691!="GT"))
print(coxph1,digits=4)
print(survdiff(Surv(OS.time,OS)~rs1192691,data=subset(phen_survival_T,rs1192691!="GT")),digits=4)
summary(coxph1)$logtest  #Likelihood ratio test 的统计量、p 值等
summary(coxph1)$waldtest  #Wald test 的统计量、p 值等
summary(coxph1)$sctest  #Score (logrank) test 的统计量、p 值等

#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu4/cox.LUNG.LUAD.xlsx')

###腺癌
phenotyp2.LUSC<-phenotyp.LUSC%>%transmute(
  `_PATIENT`=submitter_id,Sex=factor(gender.demographic),Age=age_at_initial_pathologic_diagnosis,
  tumor_stage.diagnoses
)
#table(phenotyp2$tumor_stage.diagnoses)
phenotyp2.LUSC$stage<-NA
phenotyp2.LUSC$stage[phenotyp2.LUSC$tumor_stage.diagnoses=="stage i" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage ia" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage ib"]<-1
phenotyp2.LUSC$stage[phenotyp2.LUSC$tumor_stage.diagnoses=="stage ii" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage iia" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage iib"]<-2
phenotyp2.LUSC$stage[phenotyp2.LUSC$tumor_stage.diagnoses=="stage iii" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage iiia" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage iiib" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage iiic"]<-3
phenotyp2.LUSC$stage[phenotyp2.LUSC$tumor_stage.diagnoses=="stage iv" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage iva" | phenotyp2.LUSC$tumor_stage.diagnoses=="stage ivb"]<-4
phenotyp2.LUSC$stage<-factor(phenotyp2.LUSC$stage,levels=c(1,2,3,4),labels=c('I','II','III','IV'))
phenotyp2.LUSC<-phenotyp2.LUSC[which(!duplicated(phenotyp2.LUSC$`_PATIENT`)),]
#apply(phenotyp2,2,function(x)sum(is.na(x)))
#summary(phenoty2)
###
phen_survival_T<-inner_join(geno_survival_T,phenotyp2.LUSC,by='_PATIENT')
##plot--KM
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(phen_survival_T,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192691,data=subset(phen_survival_T,!is.na(OS.time)))
OS.T<-ggsurvplot(kms_T, data = phen_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192691,data=phen_survival_T)
survdiff(Surv(DSS.time,DSS)~rs1192691,data=phen_survival_T)
DDS.T<-ggsurvplot(kms.dss.T, data = phen_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192691,data=phen_survival_T)
survdiff(Surv(DFI.time,DFI)~rs1192691,data=phen_survival_T)
DFI.T<-ggsurvplot(kms.dfi.T, data = phen_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192691,data=phen_survival_T)
survdiff(Surv(PFI.time,PFI)~rs1192691,data=phen_survival_T)
PFI.T<-ggsurvplot(kms.pfi.T, data = phen_survival_T,pval = T, break.x.by = 20,legend.title="rs1192691",    
                  risk.table = T,legend.labs =
                    c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)

#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691+Age+stage,data=subset(phen_survival_T,rs1192691!="00"))
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,DFI)',x=c('rs1192691','Age','stage'),data=phen_survival_T)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu4/cox.LUNG.LUSC.xlsx')




