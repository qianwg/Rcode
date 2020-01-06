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
#age-
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
#CA125(除)
outlier_func(biomarker$CA125,biomarker$ID_BLAST)
biomarker2<-biomarker%>%filter(!is.na(CA125),sex==2)#无缺失值
biomarker3<-biomarker2%>%filter(CA125<=quantile(CA125,0.75)+IQR(CA125))#剔除极端值
#---------------------------------------------------------------------------------------
#年龄
biomarker3%>%transmute(CA125=log(CA125),age=age,age_group=age_group)%>%
  ggscatterstats(                                            # dataframe from which variables are taken
    x = age,                                                  # predictor/independent variable 
    y = CA125,                                                  # dependent variable
    xlab = "age",                 # label for the x-axis
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
    title = "Relationship between CA125 and age",
    messages = FALSE
  )
#吸烟
biomarker3%>%transmute(CA125=log(CA125),smoking=factor(smoking,labels=c('Never','Current','Former')))%>%
  ggbetweenstats(
    x = smoking,
    y =CA125 ,
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
    title = "Relationship between CA125 and smoking",
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
  
#绝经状态
p_menopause<-biomarker3%>%transmute(CA125=log(CA125),menopause=factor(menopause,labels=c('NO','YES')))%>%
  ggbetweenstats(
    x = menopause,
    y =CA125,
    ylab='log(CA1215)',
    xlab='menopause',
    nboot = 10,
    messages = FALSE,
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA125 and menopause",
  )   
                       
#------------------------绝经年龄(有意义)
p_agemenopau<-biomarker3%>%transmute(CA125=log(CA125),agemenopau=agemenopau)%>%
  ggscatterstats(                                            
    x = agemenopau,                                                  
    y = CA125,                                                  
    xlab = "age of menopause",                 
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
    title = "Relationship between CA125 and age of menopause",
    messages = FALSE
  )
#绝经状态+绝经年龄
combine_plots(
  p_menopause, p_agemenopau,
  nrow = 2,
  labels = c("(a)", "(b)"),
  title.text = "",
  caption.text = "",
  title.size = 14,
  caption.size = 12
)
#异常观测值
library(car)
row.names(biomarker3)<-biomarker3$ID_BLAST
fit_agemenopau<-lm(log(CA125)~agemenopau,data=biomarker3)
summary(fit_agemenopau)
#1离群点
outlierTest(fit_agemenopau)
#2强影响点
cutoff<-4/(6099-length(fit_agemenopau$coefficients)-2)
plot(fit_agemenopau,which=4,cook.levels=cutoff)
biomarker%>%filter(ID_BLAST==21090532 |ID_BLAST==31030853 | ID_BLAST==31070991)%>%select(ID_BLAST,CA125,agemenopau)%>%
  transmute(ID=ID_BLAST,CA125=log(CA125),agemenopau=agemenopau)
#
biomarker3%>%transmute(CA125=log(CA125),agemenopau=agemenopau)%>%filter(agemenopau>=40)%>%
ggscatter(x='agemenopau',y='CA125',add='reg.line')+stat_cor(method='spearman')
#----------------------初潮年龄
#分布
biomarker3%>%select(agemenarch)%>%
gghistogram(x='agemenarch',binwidth = 1)
#极端值
biomarker3%>%select(agemenarch)%>%
gghistogram(x='agemenarch',binwidth = 1)+coord_cartesian(ylim=c(0,50))
outlier_func(biomarker3$agemenarch,biomarker3$ID_BLAST)
fit_agemenarch<-lm(log(CA125)~agemenarch,data=biomarker3)
summary(fit_agemenarch)
#散点图
biomarker3%>%transmute(agemenarch=agemenarch,CA125=log(CA125))%>%
  ggscatterstats(                                            
    x = agemenarch,                                                  
    y = CA125,                                                  
    xlab = "age of menarche",                 
    ylab = "log(CA125)",                                     
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             
    marginal.type = "density",                                   
    centrality.para = "mean",                                   
    margins = "both",                                           
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                           
    xalpha = 0.5,                                                
    yalpha = 0.75,                                              
    xsize = 1,                                                   
    ysize = 1,                                                   
    type = "pearson",                                            
    title = "Relationship between CA125 and age of menarche",
    messages = FALSE
  )
#绝经年龄-初潮年龄
biomarker3%>%transmute(difference=agemenopau-agemenarch,CA125=log(CA125))%>%
  ggscatterstats(                                            
    x = difference,                                                  
    y = CA125,                                                  
    xlab = "age of difference",                 
    ylab = "log(CA125)",                                     
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             
    marginal.type = "density",                                   
    centrality.para = "mean",                                   
    margins = "both",                                           
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                           
    xalpha = 0.5,                                                
    yalpha = 0.75,                                              
    xsize = 1,                                                   
    ysize = 1,                                                   
    type = "pearson",                                            
    title = "Relationship between CA125 and age of difference",
    messages = FALSE
  )
#手术史
biomarker3%>%transmute(Sterlization=factor(sterilizat,labels=c('NO','YES')),
                       hysterectomy=factor(hysterecto,labels=c('NO','YES')),
                       ovariectomy=factor(ovariectom,labels=c('NO','YES')),CA125=log(CA125))%>%
  pivot_longer(cols=c('Sterlization','hysterectomy','ovariectomy'),names_to = 'surgery',values_to = 'levels')%>%
  grouped_ggbetweenstats(
    x = levels,
    y = CA125,
    grouping.var = surgery,
    xlab = "levels",
    ylab = "log(CA125)",
    k = 2,
    nboot = 10,
    partial = FALSE, # partial omega or omega?
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    ggstatsplot.layer = FALSE,
    messages = FALSE,
    # arguments relevant for ggstatsplot::combine_plots
    title.text = "The relationship between CA125 and surgery",
    nrow = 3,
  )
#--------------------------------------------------------------------------------
#绝经状态与手术方式





























