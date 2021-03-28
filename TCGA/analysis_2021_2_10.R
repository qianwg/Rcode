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
#####AML#####
##1、数据读取及处理
gene_AML<-import('~/data/TCGA_AML_Cancer/rs1192691_AML.xlsx')%>%transmute(
  `_PATIENT`=PATIENT,rs1192691=factor(case_when(
    rs1192691_genotype=="TT" ~ 1,
    rs1192691_genotype=="GT" ~ 2, 
    rs1192691_genotype=="GG" ~ 3,
     ),levels=c(1,2,3),labels=c('TT','GT','GG')))
survival_AML<-read_tsv('~/data/TCGA_AML_Cancer/TCGA-LAML.survival.tsv')%>%mutate(OS.time=round(OS.time/30,2),
                      code=ifelse(as.numeric(as.character(substring(sample,14,15)))<=9,"T",'N'))%>%
                      filter(code=='T')%>%filter(!duplicated(`_PATIENT`))
merge_AML<-inner_join(gene_AML,survival_AML,by='_PATIENT')
#2.1 OS AML
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(merge_AML,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192691,data=subset(merge_AML,!is.na(OS.time)))
OS.T<-ggsurvplot(kms_T, data = merge_AML,pval = T, break.x.by = 10,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT", "GT","GG"),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800","#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
#2

kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(merge_AML,!is.na(OS.time)))
ggsurvplot(kms_T, data = merge_AML,pval = T, break.x.by = 10,legend.title="rs1192691",    
           risk.table = T,legend.labs =
             c("TT", "GT",'GG'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall Survial')

##COX regression
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691+Age+stage,data=phen_survival_T)
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192691','Age','stage'),data=phen_survival_T)

#####Stomach#####
##1、数据读取及处理
gene_Stomach<-import('~/data/TCGA_stomach_cancer/rs1192691_stomach.xlsx')%>%transmute(
  `_PATIENT`=PATIENT,rs1192691=factor(case_when(
    rs1192691_genotype=="TT" ~ 1,
    rs1192691_genotype=="GT" ~ 2,
    rs1192691_genotype=="GG" ~ 3,
  ),levels=c(1,2,3),labels=c('TT','GT','GG')))
survival_Stomach<-read_tsv('~/data/TCGA_stomach_cancer/TCGA-STAD.survival.tsv')%>%mutate(OS.time=round(OS.time/30,2),
                                                                                 code=ifelse(as.numeric(as.character(substring(sample,14,15)))<=9,"T",'N'))%>%
  filter(code=='T')%>%filter(!duplicated(`_PATIENT`))
merge_Stomach<-inner_join(gene_Stomach,survival_Stomach,by='_PATIENT')
#2.1 OS AML
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(merge_Stomach,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192691,data=subset(merge_Stomach,!is.na(OS.time)))
OS.T<-ggsurvplot(kms_T, data = merge_Stomach,pval = T, break.x.by = 10,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT", "GT","GG"),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')

##COX regression
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691,data=merge_Stomach)
forest_model(model.OS.T)

#####Colorectal#####
##1、数据读取及处理
gene_colorectal<-import('~/data/TCGA_colorectal_cancer/rs1192691_colorectal.xlsx')%>%transmute(
  `_PATIENT`=PATIENT,rs1192691=factor(case_when(
    rs1192691_genotype=="TT" ~ 1,
    rs1192691_genotype=="GT" ~ 2,
    rs1192691_genotype=="GG" ~ 3,
  ),levels=c(1,2,3),labels=c('TT','GT','GG')))
survival_colorectal1<-import('~/data/TCGA_colorectal_cancer/COADREAD_survival.txt')%>%mutate(OS.time=round(OS.time/30,2),
                                                                                         code=ifelse(as.numeric(as.character(substring(sample,14,15)))<=9,"T",'N'))%>%filter(code=='T')%>%filter(!duplicated(`_PATIENT`))
survival_colorectal2<-import('~/data/TCGA_colorectal_cancer/COAD_survival.txt')%>%mutate(OS.time=round(OS.time/30,2),
                                                                                         code=ifelse(as.numeric(as.character(substring(sample,14,15)))<=9,"T",'N'))%>%filter(code=='T')%>%filter(!duplicated(`_PATIENT`))
survival_colorectal3<-import('~/data/TCGA_colorectal_cancer/READ_survival.txt')%>%mutate(OS.time=round(OS.time/30,2),
                                                                                         code=ifelse(as.numeric(as.character(substring(sample,14,15)))<=9,"T",'N'))%>%filter(code=='T')%>%filter(!duplicated(`_PATIENT`))
survival_colorectal<-rbind(survival_colorectal1,survival_colorectal2,survival_colorectal3)%>%filter(!duplicated(`_PATIENT`))

merge_colorectal<-inner_join(gene_colorectal,survival_colorectal,by='_PATIENT')
#2.1 OS colorectal
kms_T<-survfit(Surv(OS.time,OS)~rs1192691,data=subset(merge_colorectal,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192691,data=subset(merge_colorectal,!is.na(OS.time)))
OS.T<-ggsurvplot(kms_T, data = merge_colorectal,pval = T, break.x.by = 10,legend.title="rs1192691",    
                 risk.table = T,legend.labs =
                   c("TT",'GT' ,"GG"),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800","#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
##COX regression
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192691,data=merge_colorectal)
forest_model(model.OS.T)
