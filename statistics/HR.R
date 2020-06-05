library(survival)
logit<-function(status,time,x,data){
  #单因素 
  result1<-data.frame()
  for(i in x){
    formula_uni<-as.formula(paste(Surv(time,status),'~', i))
    modelit_uni<-coxph(formula_uni,data)
    tableit_uni<-data.frame(summary(modelit_uni)$coef)
    tableit_uni$HR<-tableit_uni$exp.coef.
    tableit_uni$LCL <- exp(tableit_uni$coef - tableit_uni$se.coef. * 1.96 )
    tableit_uni$UCL <- exp(tableit_uni$coef + tableit_uni$se.coef. * 1.96 )
    tableit_uni$`p-value` <- round(tableit_uni$Pr...z..,4)
    tableit_uni$`HR(95%CI)`<-paste0(round(tableit_uni$HR,2),'(',round(tableit_uni$LCL,2),'-',round(tableit_uni$UCL,2),')')
    result1 <- rbind(result1,tableit_uni[,c(10,9)])
  }
  result1<-cbind(variable=row.names(result1), result1)
  row.names(result1)=NULL
  #多因素
  formula<- as.formula(paste(Surv(time,status),'~', paste(x, collapse= "+")))
  modelit<-coxph(formula,data)
  tableit<-data.frame(summary(modelit)$coef)
  tableit$HR<-exp(tableit$exp.coef.)
  tableit$LCL <- exp(tableit$coef - tableit$se.coef. * 1.96 )
  tableit$UCL <- exp(tableit$coef + tableit$se.coef. * 1.96 )
  tableit$`p-value(adjusted)` <- round(tableit$Pr...z..,4)
  tableit$`aHR(95%CI)`<-paste0(round(tableit$HR,2),'(',round(tableit$LCL,2),'-',round(tableit$UCL,2),')')
  result2 <- tableit[-1,c(10,9)]
  result2<-cbind(variable=row.names(result2), result2)
  row.names(result2)=NULL
  #合并
  result<-merge(result1,result2,by='variable',all.x=FALSE,all.y=FALSE)
  #result<-kable(result, digits = 4, align = rep('c',4)) %>%
  #  kable_styling(bootstrap_options = "striped", full_width = F)
  return(result)
  
  
}