Detect_Outliers <- function(x)
{
  ## Check if data is numeric in nature
  if(!(class(x) %in% c("numeric","integer")))
  {
    stop("Data provided must be of integer\numeric type")
  }
  
  ## Calculate lower limit
  lower.limit <- as.numeric(quantile(x,na.rm=T)[2] - IQR(x,na.rm=T)*1.5)
  
  ## Calculate upper limit
  upper.limit <- as.numeric(quantile(x,na.rm=T)[4] + IQR(x,na.rm=T)*1.5)
  
  ## Retrive index of elements which are outliers
  lower.index <- which(x < lower.limit)
  upper.index <- which(x > upper.limit)
  
  ## to plot before and after boxplots together
  par(mfrow = c(1, 2))
  
  y<- x
  
  ## Set outliers to NA
  y[c(lower.index,upper.index)] <- NA
  
  ## boxplot with outliers
  boxplot(x,xlab = "With Outliers")
  
  ## boxplot without outliers
  boxplot(y,xlab = "Without Outliers")
  
  ## print results
  cat(" Lower Limit ",lower.limit ,"\n", "Upper Limit", upper.limit ,"\n",
      "Lower range outliers  ",x[lower.index] ,"\n", "Upper range outlers", x[upper.index])
}
##function 2
is_outlier <- function(x) {
  return(x < quantile(x, 0.25,na.rm=TRUE) - 1.5 * IQR(x,na.rm=TRUE) | x > quantile(x, 0.75,na.rm=TRUE) + 1.5 * IQR(x,na.rm=TRUE))
}

outlier_func<-function(x,id){
  data2<-data.frame(x=x,id=id)
  outlier<-data2%>%transmute(
    x=x,
    x_group=case_when(
      x<quantile(x,0.25,na.rm = T) ~ 1,
      x>quantile(x,0.75,na.rm=T) ~ 2
    ),
    id=id)%>%
    transmute(x=x,x_group=x_group,is_outlier=ifelse(is_outlier(x),x,as.numeric(NA)),id=id)%>%
    transmute(x=x,x_group=x_group,is_outlier=is_outlier,id=ifelse(is.na(is_outlier),NA,id))
  plot<-outlier%>%filter(!is.na(x_group))%>%group_by(x_group)%>%summarise(na_n=sum(!is.na(is_outlier)))
  table<-outlier%>%filter(!is.na(x_group))%>%ggboxplot(x='x_group',y='x',fill='x_group',palette = 'jco')+geom_text(aes(label=id),nudge_y = 0.05,na.rm = T)
  return(list(plot,table))
  
}
outlier_func(x=biomarker$bmi,id=biomarker$ID_BLAST)
