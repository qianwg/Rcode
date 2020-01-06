rm(list=ls())
library(rio)
library(tidyverse)
biomarker<-import('~/data/biomarker/Biomarker--2017+18+19.sav')
names(biomarker)
source('~/Rcode/biomarker/marker_pos.R')
biomarker%>%transmute(CEA_pos=CEA_pos,AFP_pos=AFP_pos,CA199_pos=CA199_pos,
                      CA125_pos=CA125_pos,CA153_pos=CA153_pos,PG_pos=PG_pos,HBsAg_pos=HBsAg_pos)%>%
  pivot_longer(cols=c('CEA_pos','AFP_pos','CA199_pos',
                      'CA125_pos','CA153_pos','PG_pos','HBsAg_pos'),names_to = 'marker',values_to = 'value')%>%
  group_by(marker,value)%>%summarise(n=n())
export(biomarker,'~/肿瘤标志物阳性判定.xlsx')










