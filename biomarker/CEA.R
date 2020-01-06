biomarker2%>%select(bmi_group3,sex,CEA)%>%transmute(
  bmi=factor(bmi_group3,labels=c('Lower','Normal','Obesity')),
  sex=factor(sex,labels=c('man','woman')),
  CEA=log(CEA))%>%filter(!is.na(bmi))%>%
  grouped_ggbetweenstats(
    x = bmi,
    y =CEA ,
    ylab='log(CEA)',
    xlab='BMI',
    k=2,
    nboot = 10,
    grouping.var = sex,
    effsize.type = "unbiased", 
    messages = FALSE,
    pairwise.comparisons = TRUE, # display results from pairwise comparisons
    pairwise.display = "significant", # display only significant pairwise comparisons
    pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco"
  ) 
library(effects)
biomarker2$CEA.log<-log(biomarker2$CEA)
biomarker2%>%filter(!is.na(bmi_group3))%>%group_by(bmi_group3)%>%summarise(mean=mean(CEA.log,na.rm = TRUE),sd=(sd(CEA.log,na.rm = TRUE))/sqrt(length(CEA.log)))
biomarker2$bmi_group3<-factor(biomarker2$bmi_group3)
fit<-aov(log(CEA)~bmi_group3,data=biomarker2)
summary(fit)
data.frame(effect('bmi_group3',fit))
#