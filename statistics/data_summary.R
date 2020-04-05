data_summary<-function(x){
  NAN<-sum(is.na(x))
  x2<-x[!is.na(x)]
  n<-length(x2)
  m<-mean(x2)
  v<-var(x2)
  s<-sd(x2)
  me<-median(x2)
  cv<-100*s/m
  css<-sum((x2-m)^2)
  uss<-sum(x2^2)
  R<-max(x2)-min(x2)
  R1<-quantile(x2,3/4)-quantile(x2,1/4)
  sm<-s/sqrt(n)
  g1<-n/((n-1)*(n-2))*sum((x2-m)^3)/s^3
  g2<-((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x2-m)^4/s^4)-(3*(n-1)^2)/((n-2)*(n-3)))
       data.frame(N=n,Missing=NAN,Mean=m,Median=me,Var=v,set_dev=s,
                  std_mean=sm,CV=cv,CSS=css,USS=uss,R=R,R1=R1,Skewness=g1,
                  Kurtosis=g2,row.names = 1)
       
}
data_summary2<-function(x){
  NAN<-sum(is.na(x))
  x2<-x[!is.na(x)]
  n<-length(x2)
  m<-mean(x2)
  v<-var(x2)
  s<-sd(x2)
  me<-median(x2)
  #cv<-100*s/m
  #css<-sum((x2-m)^2)
  #uss<-sum(x2^2)
  #R<-max(x2)-min(x2)
  #R1<-quantile(x2,3/4)-quantile(x2,1/4)
  sm<-s/sqrt(n)
  g1<-n/((n-1)*(n-2))*sum((x2-m)^3)/s^3
  g2<-((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x2-m)^4/s^4)-(3*(n-1)^2)/((n-2)*(n-3)))
  data.frame(N=n,Missing=NAN,Mean=m,Median=me,std_mean=sm,Skewness=g1,
             Kurtosis=g2,row.names = 1)
  
}