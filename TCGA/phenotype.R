rm(list=ls())
library(readr)
library(rio)
library(tidyverse)
library(survival)
library(survminer)
library(forestmodel)
library(openxlsx)
source('~/Rcode/statistics/HR.R')
#################<<<<<<<<<<<<<<<<<<<<<<<<BREAST CANCER>>>>>>>>>>>>>>>>>>>>###############
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
##1、数据读取及处理
phenotype<-read_tsv('~/data/TCGA_Breast_Cancer/TCGA-BRCA.GDC_phenotype.tsv')
#class(phenotype)
#dim(phenotype)
#names(phenotype)
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
#summary(phenotyp2)
###
geno<-import('~/data/TCGA_Breast_Cancer/BRCA_geno_rs1192658.txt')
geno<-geno%>%filter(rs1192658_genotype!="00")%>%transmute(rs1192658=factor(rs1192658_genotype),`_PATIENT`)
BRCA_survival<-import('~/data/TCGA_Breast_Cancer/BRCA_survival.txt')
BRCA_survival$code<-ifelse(as.numeric(as.character(substring(BRCA_survival$sample,14,15)))<=9,"T",'N')
BRCA_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')]<-apply(BRCA_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')],2,
                                                                     function(x){
                                                                       x<-round(x/30,2)
                                                                     })#天数转月
BRCA_survival_T<-BRCA_survival[which(BRCA_survival$code=='T'),]#癌组织
BRCA_survival_N<-BRCA_survival[which(BRCA_survival$code=='N'),]#正常组织
#head(BRCA_survival)
#geno[which(duplicated(geno$`_PATIENT`)),]
#id<-BRCA_survival[which(duplicated(BRCA_survival5$`_PATIENT`)),2]
#BRCA_survival[which(BRCA_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
geno_survival_T<-inner_join(BRCA_survival_T,geno,by='_PATIENT')
geno_survival_N<-inner_join(BRCA_survival_N,geno,by='_PATIENT')
#head(geno_survival)
#geno_survival[which(geno_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
##2、K-M curve  breast cancer
#table(geno_survival$rs1192658_genotype)
#summary(geno_survival[,c('OS.time','OS','DSS.time','DSS','DFI.time','DFI','PFI.time','PFI')])

#2.1 OS breast
kms_T<-survfit(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_T,!is.na(OS.time)))
kms_N<-survfit(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_N,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_T,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_N,!is.na(OS.time)))
#pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
#plot(kms_T,lty="solid",col=c("red","green",'blue'),
#     xlab="Survival time in months",ylab="Survival probabilities",
#     main="",sep="")
#legend("topright",c("CC","TC",'TT'),lty="solid",col=c("red","green",'blue'))
OS.T<-ggsurvplot(kms_T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
             ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
             tables.theme=clean_theme(),title='Overall Survial')
OS.N<-ggsurvplot(kms_N, data = geno_survival_N,pval = T, break.x.by = 30,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Overall Survial')

#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_T)
kms.dss.N<-survfit(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_N)
survdiff(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_T)
survdiff(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_N)
DDS.T<-ggsurvplot(kms.dss.T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Disease-specific survival')
DDS.N<-ggsurvplot(kms.dss.N, data = geno_survival_N,pval = T, break.x.by = 30,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Disease-specific survival')

#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_T)
kms.dfi.N<-survfit(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_N)
survdiff(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_T)
survdiff(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_N)
DFI.T<-ggsurvplot(kms.dfi.T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Disease-free interval')
DFI.N<-ggsurvplot(kms.dfi.N, data = geno_survival_N,pval = T, break.x.by = 30,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Disease-free interval')

#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_T)
kms.pfi.N<-survfit(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_N)
survdiff(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_T)
survdiff(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_N)
PFI.T<-ggsurvplot(kms.pfi.T, data = geno_survival_T,pval = T, break.x.by = 60,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Progression-free interval')
PFI.N<-ggsurvplot(kms.pfi.N, data = geno_survival_N,pval = T, break.x.by = 30,legend.title="rs1192658",    
           risk.table = T,legend.labs =
             c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
           ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
           tables.theme=clean_theme(),title='Progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)
##COX regression
phen_survival_T<-inner_join(geno_survival_T,phenotyp2,by='_PATIENT')
phen_survival_N<-inner_join(geno_survival_N,phenotyp2,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=phen_survival_T)
model.OS.N<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.OS.N)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_T)
model.DSS.N<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DSS.N)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_T)
model.DFI.N<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DFI.N)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_T)
model.PFI.N<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,PFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.PFI.N)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu3/cox.xlsx')
#################<<<<<<<<<<<<<<<<<<<<<<<<LIVER CANCER>>>>>>>>>>>>>>>>>>>>###############
rm(list=ls())
##1、数据读取及处理
phenotyp<-read_tsv('~/data/TCGA_Liver_Cancer/TCGA-LIHC.GDC_phenotype.tsv')
#class(phenotype)
#dim(phenotype)
#names(phenotype)
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
geno<-read.table('~/data/TCGA_Liver_Cancer/LIHC_geno_rs1192658.txt',header=T,row.names = NULL)
geno<-geno%>%filter(rs1192658_genotype!="00")%>%transmute(IID,rs1192658=factor(rs1192658_genotype),`_PATIENT`=X_PATIENT)
LIHC_survival<-import('~/data/TCGA_Liver_Cancer/LIHC_survival.txt')
LIHC_survival$code<-ifelse(as.numeric(as.character(substring(LIHC_survival$sample,14,15)))<=9,"T",'N')
LIHC_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')]<-apply(LIHC_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')],2,
                                                                     function(x){
                                                                       x<-round(x/30,2)
                                                                     })#天数转月
LIHC_survival_T<-LIHC_survival[which(LIHC_survival$code=='T'),]#癌组织
LIHC_survival_N<-LIHC_survival[which(LIHC_survival$code=='N'),]#正常组织
#head(BRCA_survival)
#geno[which(duplicated(geno$`_PATIENT`)),]
#id<-BRCA_survival[which(duplicated(BRCA_survival5$`_PATIENT`)),2]
#BRCA_survival[which(BRCA_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
geno_survival_T<-inner_join(LIHC_survival_T,geno,by='_PATIENT')
geno_survival_N<-inner_join(LIHC_survival_N,geno,by='_PATIENT')
#head(geno_survival)
#geno_survival[which(geno_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
##2、K-M curve  breast cancer
#table(geno_survival$rs1192658_genotype)
#summary(geno_survival[,c('OS.time','OS','DSS.time','DSS','DFI.time','DFI','PFI.time','PFI')])

#2.1 OS liver
kms_T<-survfit(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_T,!is.na(OS.time)))
kms_N<-survfit(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_N,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_T,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_N,!is.na(OS.time)))
#pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
#plot(kms_T,lty="solid",col=c("red","green",'blue'),
#     xlab="Survival time in months",ylab="Survival probabilities",
#     main="",sep="")
#legend("topright",c("CC","TC",'TT'),lty="solid",col=c("red","green",'blue'))
OS.T<-ggsurvplot(kms_T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                 risk.table = T,legend.labs =
                   c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
OS.N<-ggsurvplot(kms_N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                 risk.table = T,legend.labs =
                   c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')

#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_T)
kms.dss.N<-survfit(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_N)
survdiff(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_T)
survdiff(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_N)
DDS.T<-ggsurvplot(kms.dss.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
DDS.N<-ggsurvplot(kms.dss.N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')

#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_T)
kms.dfi.N<-survfit(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_N)
survdiff(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_T)
survdiff(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_N)
DFI.T<-ggsurvplot(kms.dfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
DFI.N<-ggsurvplot(kms.dfi.N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')

#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_T)
kms.pfi.N<-survfit(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_N)
survdiff(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_T)
survdiff(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_N)
PFI.T<-ggsurvplot(kms.pfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
PFI.N<-ggsurvplot(kms.pfi.N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)

##COX regression
phen_survival_T<-inner_join(geno_survival_T,phenotyp2,by='_PATIENT')
phen_survival_N<-inner_join(geno_survival_N,phenotyp2,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=subset(phen_survival_T,rs1192658!="00"))
model.OS.N<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.OS.N)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_T)
model.DSS.N<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DSS.N)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_T)
model.DFI.N<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DFI.N)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_T)
model.PFI.N<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,PFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.PFI.N)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu3/cox.Liver.xlsx')
#################<<<<<<<<<<<<<<<<<<<<<<<<LUNG CANCER>>>>>>>>>>>>>>>>>>>>###############
rm(list=ls())
##1、数据读取及处理
phenotyp.LUAD<-read_tsv('~/data/TCGA_Lung_Cancer/TCGA-LUAD.GDC_phenotype.tsv')
phenotyp.LUSC<-read_tsv('~/data/TCGA_Lung_Cancer/TCGA-LUSC.GDC_phenotype.tsv')
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
geno<-read.table('~/data/TCGA_Lung_Cancer/LUNG_geno_rs1192658.txt',header=T,row.names = NULL)
geno<-geno%>%filter(rs1192658_genotype!="00")%>%transmute(IID,rs1192658=factor(rs1192658_genotype),`_PATIENT`=X_PATIENT)
LUNG_survival<-import('~/data/TCGA_Lung_Cancer/LUNG_survival.txt')
LUNG_survival$code<-ifelse(as.numeric(as.character(substring(LUNG_survival$sample,14,15)))<=9,"T",'N')
LUNG_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')]<-apply(LUNG_survival[,c('OS.time','DSS.time','DFI.time','PFI.time')],2,
                                                                     function(x){
                                                                       x<-round(x/30,2)
                                                                     })#天数转月
LUNG_survival_T<-LUNG_survival[which(LUNG_survival$code=='T'),]#癌组织
LUNG_survival_N<-LUNG_survival[which(LUNG_survival$code=='N'),]#正常组织
#head(BRCA_survival)
#geno[which(duplicated(geno$`_PATIENT`)),]
#id<-BRCA_survival[which(duplicated(BRCA_survival5$`_PATIENT`)),2]
#BRCA_survival[which(BRCA_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
geno_survival_T<-inner_join(LUNG_survival_T,geno,by='_PATIENT')
geno_survival_N<-inner_join(LUNG_survival_N,geno,by='_PATIENT')
#head(geno_survival)
#geno_survival[which(geno_survival$`_PATIENT` %in% id),]%>%arrange(`_PATIENT`)
##2、K-M curve  breast cancer
#table(geno_survival$rs1192658_genotype)
#summary(geno_survival[,c('OS.time','OS','DSS.time','DSS','DFI.time','DFI','PFI.time','PFI')])

#2.1 OS liver
kms_T<-survfit(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_T,!is.na(OS.time)))
kms_N<-survfit(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_N,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_T,!is.na(OS.time)))
survdiff(Surv(OS.time,OS)~rs1192658,data=subset(geno_survival_N,!is.na(OS.time)))
#pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
#plot(kms_T,lty="solid",col=c("red","green",'blue'),
#     xlab="Survival time in months",ylab="Survival probabilities",
#     main="",sep="")
#legend("topright",c("CC","TC",'TT'),lty="solid",col=c("red","green",'blue'))
OS.T<-ggsurvplot(kms_T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                 risk.table = T,legend.labs =
                   c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')
OS.N<-ggsurvplot(kms_N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                 risk.table = T,legend.labs =
                   c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
                 ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
                 tables.theme=clean_theme(),title='Overall Survial')

#2.2 DSS time
kms.dss.T<-survfit(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_T)
kms.dss.N<-survfit(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_N)
survdiff(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_T)
survdiff(Surv(DSS.time,DSS)~rs1192658,data=geno_survival_N)
DDS.T<-ggsurvplot(kms.dss.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')
DDS.N<-ggsurvplot(kms.dss.N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-specific survival')

#2.3 DFI time
kms.dfi.T<-survfit(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_T)
kms.dfi.N<-survfit(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_N)
survdiff(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_T)
survdiff(Surv(DFI.time,DFI)~rs1192658,data=geno_survival_N)
DFI.T<-ggsurvplot(kms.dfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')
DFI.N<-ggsurvplot(kms.dfi.N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='Disease-free interval')

#2.4 PFI.time
kms.pfi.T<-survfit(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_T)
kms.pfi.N<-survfit(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_N)
survdiff(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_T)
survdiff(Surv(PFI.time,PFI)~rs1192658,data=geno_survival_N)
PFI.T<-ggsurvplot(kms.pfi.T, data = geno_survival_T,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC",'TT'),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF",'#FF6600'),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
PFI.N<-ggsurvplot(kms.pfi.N, data = geno_survival_N,pval = T, break.x.by = 20,legend.title="rs1192658",    
                  risk.table = T,legend.labs =
                    c("CC", "TC"),xlab = "Time in months",risk.table.height = 0.25,
                  ggtheme = mytheme,palette =c("#E7B800", "#2E9FDF"),risk.table.title='No.at Risk',
                  tables.theme=clean_theme(),title='progression-free interval')
###合并
arrange_ggsurvplots(list(OS.T,DDS.T,DFI.T,PFI.T),nrow=2,ncol=2,risk.table.height=0.3)

##COX regression
phen_survival_T<-inner_join(geno_survival_T,phenotyp2,by='_PATIENT')
phen_survival_N<-inner_join(geno_survival_N,phenotyp2,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=subset(phen_survival_T,rs1192658!="00"))
model.OS.N<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.OS.N)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_T)
model.DSS.N<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DSS.N)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_T)
model.DFI.N<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DFI.N)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_T)
model.PFI.N<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.PFI.N)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu3/cox.LUNG.xlsx')


######肺鳞癌LUAD
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
phen_survival_T<-inner_join(geno_survival_T,phenotyp2.LUAD,by='_PATIENT')
phen_survival_N<-inner_join(geno_survival_N,phenotyp2.LUAD,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=subset(phen_survival_T,rs1192658!="00"))
model.OS.N<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.OS.N)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_T)
model.DSS.N<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DSS.N)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_T)
model.DFI.N<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DFI.N)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_T)
model.PFI.N<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.PFI.N)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu3/cox.LUNG.xlsx')

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
phen_survival_N<-inner_join(geno_survival_N,phenotyp2.LUSC,by='_PATIENT')
#OS
model.OS.T<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=subset(phen_survival_T,rs1192658!="00"))
model.OS.N<-coxph(Surv(OS.time,OS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.OS.T)
cox.OS.T<-cox(y='Surv(OS.time,OS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.OS.N)
#DSS
model.DSS.T<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_T)
model.DSS.N<-coxph(Surv(DSS.time,DSS)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DSS.T)
cox.DSS.T<-cox(y='Surv(DSS.time,DSS)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DSS.N)
#DFI
model.DFI.T<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_T)
model.DFI.N<-coxph(Surv(DFI.time,DFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.DFI.T)
cox.DFI.T<-cox(y='Surv(DFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.DFI.N)
#PFI
model.PFI.T<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_T)
model.PFI.N<-coxph(Surv(PFI.time,PFI)~rs1192658+Age+stage,data=phen_survival_N)
forest_model(model.PFI.T)
cox.PFI.T<-cox(y='Surv(PFI.time,DFI)',x=c('rs1192658','Age','stage'),data=phen_survival_T)
forest_model(model.PFI.N)
#导出
write.xlsx(list(OS=cox.OS.T,DSS=cox.DSS.T,DFI=cox.DFI.T,PFI=cox.PFI.T),file='~/tu3/cox.LUNG.xlsx')


######2020-12-24分析









