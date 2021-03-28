rm(list=ls())
install.packages("devtools")
setwd("D:/早诊早治/衍生项目/肿瘤基因多效性/R package for MR")
devtools::install_local('./MRInstruments.zip')
devtools::install_local('./MRPRESSO.zip')
devtools::install_local('./TwoSampleMR.zip')
library(TwoSampleMR)
library(MRPRESSO)

# 1. 获取暴露数据
agemena <-extract_instruments(outcomes='ieu-a-1095',
                              clump=TRUE,
                              r2=0.001,
                              kb=10000,
                              access_token = NULL)
dim(agemena)


# 2. 获取结局信息
brca <- extract_outcome_data(
  snps=agemena$SNP,
  outcomes='ieu-a-1126',
  proxies = FALSE,
  maf_threshold = 0.01,
  access_token = NULL
)

dim(brca)

# 3. 将IV的效应等位基因对齐
mydata <- harmonise_data(
  exposure_dat=agemena,
  outcome_dat=brca,
  action= 2
)
table(mydata$mr_keep)

# 4. 计算并解读MR结果
res <- mr(mydata)
res


# 5. 异质性分析
het <- mr_heterogeneity(mydata)
het
mr(mydata,method_list=c('mr_ivw_mre')) 

# 6. 多效性检验 （Pleiotropy test）
pleio <- mr_pleiotropy_test(mydata)
pleio
pleio2 <- mr_presso(BetaOutcome = "beta.outcome", 
                    BetaExposure = "beta.exposure", 
                    SdOutcome = "se.outcome", 
                    SdExposure = "se.exposure", 
                    OUTLIERtest = TRUE, 
                    DISTORTIONtest = TRUE, 
                    data = mydata, 
                    NbDistribution = 1000,  
                    SignifThreshold = 0.05)
pleio2


# 7. 逐项剔除检验 （Leave-one-out sensitivity test）
single <- mr_leaveoneout(mydata)
mr_leaveoneout_plot(single)

# 8. 绘制散点图
mr_scatter_plot(res,mydata)

# 9. 绘制森林图
res_single <- mr_singlesnp(mydata)
mr_forest_plot(res_single)

# 10. 绘制漏斗图
mr_funnel_plot(res_single)

# 11.绘制初潮年龄与ER+及ER-乳腺癌的1对多MR森林图
brcasub <- extract_outcome_data(
  snps=agemena$SNP,
  outcomes=c('ieu-a-1127','ieu-a-1128'),
  proxies = FALSE,
  maf_threshold = 0.01,
  access_token = NULL
)

mydata2 <- harmonise_data(
  exposure_dat=agemena,
  outcome_dat=brcasub,
  action= 2
)

res<-mr(mydata2)

# res<-subset_on_method(res)
res<-sort_1_to_many(res,b="b",group="exposure",sort_action=3,priority="Inverse variance weighted",trait_m="method") #this sorts results by decreasing effect size (largest effect at top of the plot)
res$weight<-1/res$se

min(exp(res$b-1.96*res$se)) #identify value for 'lo' in forest_plot_1_to_many
max(exp(res$b+1.96*res$se)) #identify value for 'up' in forest_plot_1_to_many

res$pval<-formatC(res$pval, format = "e", digits = 2)

forest_plot_1_to_many(res,b="b",se="se",
                      exponentiate=T,ao_slc=F,lo=0.3,up=2.5,
                      TraitM="outcome", by="method",
                      trans="log2",
                      xlab="OR for breast cancer per 1 year increase in age at menarche (95% CI)",
                      weight="weight",subheading_size=11,
                      col1_title="Risk factor",
                      col1_width=2.5,
                      col_text_size=4,
                      addcols=c("nsnp","pval"),
                      addcol_widths=c(1.0,1.0),
                      addcol_titles=c("No. SNPs","P-val"))

# 12. 绘制多种危险因素与乳腺癌及不同分子分型的多对多MR森林图
brcafactor <-extract_instruments(outcomes=c('ieu-a-1095', 'ukb-b-17422', 
                                            'ukb-b-12405', 'ukb-b-1209',
                                            'ukb-b-13414', 'ukb-b-9509'),
                                 clump=TRUE,
                                 r2=0.001,
                                 kb=10000,
                                 access_token = NULL)

brca2 <- extract_outcome_data(
  snps=brcafactor$SNP,
  outcomes=c('ieu-a-1126','ieu-a-1127','ieu-a-1128'),
  proxies = FALSE,
  maf_threshold = 0.01,
  access_token = NULL
)

mydata2 <- harmonise_data(
  exposure_dat=brcafactor,
  outcome_dat=brca2,
  action= 2
)

res<-mr(mydata2,method_list=c("mr_egger_regression","mr_ivw"))

res$weight<-1/res$se

min(exp(res$b-1.96*res$se)) 
max(exp(res$b+1.96*res$se)) 

res$pval<-formatC(res$pval, format = "e", digits = 2)
res$rf<-res$exposure

forest_plot_1_to_many(res,b="b",se="se",
                      exponentiate=T,ao_slc=F,lo=0.3,up=2.5,
                      TraitM="outcome", by="method",
                      trans="log2",
                      xlab="OR for breast cancer per 1 unit increase in risk factor (95% CI)",
                      weight="weight",subheading_size=11,
                      col1_title="Risk factor",
                      col1_width=2.5,
                      col_text_size=4,
                      addcols=c("rf","nsnp","pval"),
                      addcol_widths=c(4.0,1.0,1.0),
                      addcol_titles=c("Risk factor","No. SNPs","P-val"))

# 13. MR.RAPS: Many weak instruments analysis
res <- mr(mydata2, method_list = c("mr_raps"))
res

res$weight<-1/res$se

min(exp(res$b-1.96*res$se)) 
max(exp(res$b+1.96*res$se)) 

res$pval<-formatC(res$pval, format = "e", digits = 2)
res$rf<-res$exposure

forest_plot_1_to_many(res,b="b",se="se",
                      exponentiate=T,ao_slc=F,lo=0.3,up=2.5,
                      TraitM="outcome", by="method",
                      trans="log2",
                      xlab="OR for breast cancer per 1 unit increase in risk factor (95% CI)",
                      weight="weight",subheading_size=11,
                      col1_title="Risk factor",
                      col1_width=2.5,
                      col_text_size=4,
                      addcols=c("rf","nsnp","pval"),
                      addcol_widths=c(4.0,1.0,1.0),
                      addcol_titles=c("Risk factor","No. SNPs","P-val"))

# 14. MR Steiger directionality test
out <- directionality_test(mydata2)
out

# 15. Multivariable MR
exposure.mv <-mv_extract_exposures(c('ieu-a-1095', 'ukb-b-17422'))

outcome1.mv <- extract_outcome_data(exposure.mv$SNP, c('ieu-a-1126'))
outcome2.mv <- extract_outcome_data(exposure.mv$SNP, c('ieu-a-1127'))
outcome3.mv <- extract_outcome_data(exposure.mv$SNP, c('ieu-a-1128'))

mvdat1 <- mv_harmonise_data(exposure.mv, outcome1.mv)
mvdat2 <- mv_harmonise_data(exposure.mv, outcome2.mv)
mvdat3 <- mv_harmonise_data(exposure.mv, outcome3.mv)

res1 <- mv_multiple(mvdat1, plots=TRUE)
res2 <- mv_multiple(mvdat2, plots=TRUE)
res3 <- mv_multiple(mvdat3, plots=TRUE)

# 16. MR estimates when instruments are correlated
mydata3 <- dat_to_MRInput(mydata)
MendelianRandomization::mr_ivw(mydata3[[1]])

mydata3 <- dat_to_MRInput(mydata, get_correlation=TRUE)
MendelianRandomization::mr_ivw(mydata3[[1]], correl=TRUE)
