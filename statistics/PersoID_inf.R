#出生日期处理函数
afunc<-function(id){
  if(nchar(id)==15)
    paste('19',substr(id,7,12),sep="")
  else
    paste(substr(id,7,10),"-",substr(id,11,12),"-",substr(id,13,14),sep="")
}

#性别处理函数
bfunc<-function(id){
  if(nchar(id)==15)
    ifelse(as.numeric(substr(id,15,15)) %% 2 ==1,'M','F')
  else
    ifelse(as.numeric(substr(id,17,17)) %% 2 ==1,'M','F')
}
