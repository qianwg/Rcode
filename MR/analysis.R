rm(list=ls())
library(TwoSampleMR)
library(MRPRESSO)
mytheme<-theme(plot.title=element_text(hjust=0.5,face="bold"),
               axis.title=element_text(family="serif",size=14,face="bold"),
               axis.text=element_text(family="serif",size=14,face="bold"),
               panel.grid.major = element_line(colour=NA),
               panel.grid.minor = element_blank(),
               panel.background=element_rect(fill=NA),
               axis.line = element_line(color='black'),
               legend.title = element_text(family='serif',size=15,face='bold'),
               legend.text = element_text(family = 'serif',size=15,face='bold'),
               legend.position = 'top'
)
# 1. 获取暴露数据
agemena <-extract_instruments(outcomes='ebi-a-GCST006363',
                              clump=TRUE,
                              p1=5e-6,
                              r2=0.001,
                              kb=10000,
                              access_token = NULL)
dim(agemena)


# 2. 获取结局信息
brca<-extract_outcome_data(
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
res%>%mutate(LCL=round(exp(b-se*1.96),2),
                 OR=round(exp(b),2),
                 UCL=round(exp(b+se*1.96),2),
                  CI=paste0(OR,'(',LCL,'-',UCL,')'))
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

########EB病毒

