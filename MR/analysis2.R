rm(list=ls())

library(TwoSampleMR)
library(MRPRESSO)

# 1. 获取暴露数据
blood <-extract_instruments(outcomes='ieu-a-273',
                              clump=TRUE,
                              #p1=5e-6,
                              r2=0.001,
                              kb=10000,
                              access_token = NULL)

# 2. 获取结局信息
cancer <- extract_outcome_data(
  snps=blood$SNP,
  outcomes='ukb-b-1392',
  proxies = FALSE,
  maf_threshold = 0.01,
  access_token = NULL
)


# 3. 将IV的效应等位基因对齐
mydata <- harmonise_data(
  exposure_dat=blood,
  outcome_dat=cancer,
  action= 2
)

# 4. 计算并解读MR结果
res <- mr(mydata)
res

