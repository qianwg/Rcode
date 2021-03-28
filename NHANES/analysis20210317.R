rm(list=ls())
#20210317：NHANES：性别、激素、抑郁、癌症、炎症
library(foreign)
library(plyr)
#合并文件
filenames <- list.files(path="~/data/NHANES/analysis20210317", full.names=TRUE)
import.list <- llply(filenames, read.xport)
data <- Reduce(function(x, y) merge(x, y, all=T, 
                                    by=c("SEQN")), import.list, accumulate=F)
rm(import.list,filenames)
####建立数据库
data2<-data%>%transmute(
  #Demographic Variables and sample weights
  ID=SEQN,Gender=RIAGENDR,Age=RIDAGEYR,Race=RIDRETH3,Marital=DMDMARTL,
  #Body Measures
  Weight=BMXWT, Height=BMXHT,BMI=BMXBMI,
  #Laboratory Data
  HDL_C=LBDHDD,# (mg/dL)
  HDL_C2=LBDHDDSI,#(mmol/L)
  LDL_C=LBDLDL,# (mg/dL)
  LDL_C2=LBDLDLSI,# (mmol/L)
  HS_CRP=LBXHSCRP,#(mg/L)
  ##alcohol
  drinke=ALQ111,
  cancer=MCQ220,cancer1st=MCQ230a,
  #depression
  DPQ010,DPQ020,DPQ030,DPQ040,DPQ050,DPQ060,DPQ070,DPQ080,DPQ090,DPQ100
)
