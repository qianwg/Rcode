library(kableExtra)
linear<-function(y,x,data){
  #单因素 
  result1<-data.frame()
  for(i in x){
    formula_uni<-as.formula(paste(y,'~', i))
    modelit_uni<-lm(formula_uni,data)
    tableit_uni<-data.frame(summary(modelit_uni)$coef)
    tableit_uni$`p-value` <- round(tableit_uni$Pr...t..,4)
    tableit_uni<-tableit_uni%>%rownames_to_column('variable')
    result1 <- rbind(result1,tableit_uni[which(str_starts(tableit_uni[,'variable'],i)),c(1,2,6)])
  }
  #多因素
  #formula<- as.formula(paste(y,'~', paste(x, collapse= "+")))
  #modelit<-glm(formula,data,family = 'binomial')
  #tableit<-data.frame(summary(modelit)$coef)
  #tableit$OR<-exp(tableit$Estimate)
  #tableit$LCL <- exp(tableit$Estimate - tableit$Std..Error * 1.96 )
  #tableit$UCL <- exp(tableit$Estimate + tableit$Std..Error * 1.96 )
  #tableit$`p-value(adjusted)` <- round(tableit$Pr...z..,4)
  #tableit$`aOR(95%CI)`<-paste0(round(tableit$OR,2),'(',round(tableit$LCL,2),'-',round(tableit$UCL,2),')')
  #result2 <- tableit[-1,c(9,8)]
  #result2<-cbind(variable=row.names(result2), result2)
  #row.names(result2)=NULL
  #合并
  #result<-merge(result1,result2,by='variable',all.x=FALSE,all.y=FALSE)
  #result<-kable(result, digits = 4, align = rep('c',4)) %>%
  #  kable_styling(bootstrap_options = "striped", full_width = F)
  return(result1)
  
  
}

linear2<-function(y,x,data){
  #单因素 
  result1<-data.frame()
  for(i in x){
    formula_uni<-as.formula(paste(y,'~', i,'+',"性别+吸烟1+饮酒+BMI_group+年龄分组+Hp_pos2"))
    modelit_uni<-lm(formula_uni,data)
    tableit_uni<-data.frame(summary(modelit_uni)$coef)
    tableit_uni$`p-value` <- round(tableit_uni$Pr...t..,4)
    tableit_uni<-tableit_uni%>%rownames_to_column('variable')
    result1 <- rbind(result1,tableit_uni[which(str_starts(tableit_uni[,'variable'],i)),c(1,2,6)])
  }
  #result1<-cbind(variable=row.names(result1), result1)
  #row.names(result1)=NULL
  #多因素
  #formula<- as.formula(paste(y,'~', paste(x, collapse= "+")))
  #modelit<-glm(formula,data,family = 'binomial')
  #tableit<-data.frame(summary(modelit)$coef)
  #tableit$OR<-exp(tableit$Estimate)
  #tableit$LCL <- exp(tableit$Estimate - tableit$Std..Error * 1.96 )
  #tableit$UCL <- exp(tableit$Estimate + tableit$Std..Error * 1.96 )
  #tableit$`p-value(adjusted)` <- round(tableit$Pr...z..,4)
  #tableit$`aOR(95%CI)`<-paste0(round(tableit$OR,2),'(',round(tableit$LCL,2),'-',round(tableit$UCL,2),')')
  #result2 <- tableit[-1,c(9,8)]
  #result2<-cbind(variable=row.names(result2), result2)
  #row.names(result2)=NULL
  #合并
  #result<-merge(result1,result2,by='variable',all.x=FALSE,all.y=FALSE)
  #result<-kable(result, digits = 4, align = rep('c',4)) %>%
  #  kable_styling(bootstrap_options = "striped", full_width = F)
  return(result1)
  
  
}

