#判断字字符串中是否有特定的字符
ifdif<-function(x,y){
  result<-vector()
  a<-substring(x,1:nchar(x),1:nchar(x))
  for(i in 1:length(y))
  {
    result[i]<-ifelse(all(a %in% substring(y[i],1:nchar(y[i]))),"TRUE","FALSE")
  }
  return(result)
}
