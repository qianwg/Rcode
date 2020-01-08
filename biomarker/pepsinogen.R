rm(list=ls())
library(rio)
library(openxlsx)
library(tidyverse)
#全部做过PG的人群
biomarker<-read.xlsx('~/data/biomarker/Biomarker+baseline(2017+18+19).xlsx',detectDates = TRUE)
biomarker<-biomarker%>%filter(!is.na(PGI))
#做过胃镜有病理结果的
gastroscopy<-read.xlsx('~/data/biomarker/PG_gastric.xlsx',sheet=3)
gastroscopy%>%group_by(病理结果2)%>%summarise(mean=mean(PG1))
