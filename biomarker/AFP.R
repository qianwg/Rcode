rm(list=ls())
library(rio)
library(openxlsx)
library(ggpubr)
library(ggstatsplot)
library(table1)
library(tidyverse)
library(fmsb)
library(stargazer)
biomarker<-read.xlsx('~/data/biomarker/Biomarker+baseline(2017+18+19).xlsx',detectDates=TRUE)
source('~/Rcode/statistics/derout.R')
#age
biomarker$age_group[biomarker$age>=40 & biomarker$age<45]<-1
biomarker$age_group[biomarker$age>=45 & biomarker$age<50]<-2
biomarker$age_group[biomarker$age>=50 & biomarker$age<55]<-3
biomarker$age_group[biomarker$age>=55 & biomarker$age<60]<-4
biomarker$age_group[biomarker$age>=60 & biomarker$age<65]<-5
biomarker$age_group[biomarker$age>=65 & biomarker$age<70]<-6
biomarker$age_group[biomarker$age>=70]<-7
biomarker$age_group2[biomarker$age>=40 & biomarker$age<50]<-1
biomarker$age_group2[biomarker$age>=50 & biomarker$age<60]<-2
biomarker$age_group2[biomarker$age>=60 & biomarker$age<70]<-3
biomarker$age_group2[biomarker$age>=70]<-4
#baonian
biomarker$baonian<-(biomarker$cpd*biomarker$smkyrs)/20
#--1包年缺失值查看
summary(biomarker[which(biomarker$smoking>1),'baonian'])
#--2baonian极端值查看
outlier_func(x=biomarker$baonian,id=biomarker$ID_BLAST)
#--检查或剔除极端值或缺失值
#---BMI
biomarker$bmi<-with(biomarker,weight/((height/100)^2))
#中国人标准
biomarker$bmi_group<-with(biomarker,case_when(
  bmi<18.5 ~ 1,
  between(bmi,18.5,23.9) ~ 2,
  between(bmi,24,27.9) ~ 3,
  bmi>=28 ~ 4
))
#亚洲人标准
biomarker$bmi_group3<-with(biomarker,case_when(
  bmi<=22.9 ~ 1,
  between(bmi,23,27.4) ~2,
  bmi>=27.5 ~3
))
biomarker$bmi_group2<-with(biomarker,ifelse(bmi<25,1,2))
#AFP
outlier_func(biomarker$AFP,biomarker$ID_BLAST)
biomarker2<-biomarker%>%filter(!is.na(AFP))#无缺失值
biomarker3<-biomarker2%>%filter(AFP<=quantile(AFP,0.75)+IQR(AFP))#剔除极端值
#---------------------------------------------------------------------------------------
#年龄
biomarker3%>%transmute(AFP=log(AFP),age=age,age_group=age_group)%>%
  ggscatterstats(                                            # dataframe from which variables are taken
    x = age,                                                  # predictor/independent variable 
    y = AFP,                                                  # dependent variable
    xlab = "age",                 # label for the x-axis
    ylab = "log(AFP)",                                     # label for the y-axis
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             # show marginal distribution 
    marginal.type = "density",                                   # type of plot for marginal distribution
    centrality.para = "mean",                                    # centrality parameter to be plotted
    margins = "both",                                            # marginal distribution on both axes
    xfill = "#CC79A7",                                           # fill for marginals on the x-axis
    yfill = "#009E73",                                           # fill for marginals on the y-axis
    xalpha = 0.5,                                                # transparency for the x-axis marginals
    yalpha = 0.75,                                               # transparency for the y-axis marginals
    xsize = 1,                                                   # size for the x-axis marginals
    ysize = 1,                                                   # size for the y-axis marginals
    type = "pearson",                                            # type of linear association
    title = "Relationship between AFP and age",
    messages = FALSE
  )
#吸烟
biomarker3%>%transmute(CA125=log(AFP),smoking=factor(smoking,labels=c('Never','Current','Former')))%>%
  ggbetweenstats(
    x = smoking,
    y =AFP,
    ylab='log(CA1215)',
    xlab='smoking',
    nboot = 10,
    messages = FALSE,
    pairwise.comparisons = TRUE, # display results from pairwise comparisons
    pairwise.display = "significant", # display only significant pairwise comparisons
    pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between AFP and smoking",
  )   
#BMI
biomarker3%>%transmute(CA125=log(CA125),bmi=bmi,bmi_group=bmi_group)%>%
  ggscatterstats(                                            # dataframe from which variables are taken
    x = bmi,                                                  # predictor/independent variable 
    y = CA125,                                                  # dependent variable
    xlab = "BMI",                 # label for the x-axis
    ylab = "log(CA125)",                                     # label for the y-axis
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             # show marginal distribution 
    marginal.type = "density",                                   # type of plot for marginal distribution
    centrality.para = "mean",                                    # centrality parameter to be plotted
    margins = "both",                                            # marginal distribution on both axes
    xfill = "#CC79A7",                                           # fill for marginals on the x-axis
    yfill = "#009E73",                                           # fill for marginals on the y-axis
    xalpha = 0.5,                                                # transparency for the x-axis marginals
    yalpha = 0.75,                                               # transparency for the y-axis marginals
    xsize = 1,                                                   # size for the x-axis marginals
    ysize = 1,                                                   # size for the y-axis marginals
    type = "pearson",                                            # type of linear association
    title = "Relationship between CA125 and BMI",
    messages = FALSE
  )
#饮酒
biomarker3%>%transmute(CA125=log(CA125),alcohol=factor(alcohol,labels=c('NO','YES')))%>%
  ggbetweenstats(
    x = alcohol,
    y =CA125,
    ylab='log(CA1215)',
    xlab='alcohol',
    nboot = 10,
    messages = FALSE,
    pairwise.comparisons = TRUE, # display results from pairwise comparisons
    pairwise.display = "significant", # display only significant pairwise comparisons
    pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA125 and alcohol",
  )   

#疾病史(糖尿病，高血压，高血脂，冠心病，偏头疼)(偏头疼有意义)
biomarker3%>%transmute(CA125=log(CA125),
                       Diabetes=factor(Disea28,labels=c('NO','YES')),
                       Hypertension=factor(Disea29,labels=c('NO','YES')),
                       Hyperlipidemia=factor(Disea30,labels=c('NO','YES')),
                       Coronarry=factor(Disea31,labels=c('NO','YES')),
                       Migraine=factor(Disea33,labels=c('NO','YES')))%>%
  pivot_longer(cols=c('Diabetes','Hypertension','Hyperlipidemia','Coronarry','Migraine'),names_to='disea',values_to = 'levels')%>%
  grouped_ggbetweenstats(
    x = levels,
    y = CA125,
    grouping.var = disea,
    xlab = "levels",
    ylab = "log(CA125)",
    k = 2,
    nboot = 10,
    partial = FALSE, # partial omega or omega?
    pairwise.comparisons = TRUE, # display results from pairwise comparisons
    pairwise.display = "significant", # display only significant pairwise comparisons
    pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    ggstatsplot.layer = FALSE,
    messages = FALSE,
    # arguments relevant for ggstatsplot::combine_plots
    title.text = "The relationship between CA125 and Disease",
    nrow = 3,
  )






























