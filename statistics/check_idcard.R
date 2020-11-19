check_id=function(id){
  id=as.character(id)
  w=c(7,9,10,5,8,4,2,1,6,3,7,9,10,5,8,4,2)
  v=c(1,0,"X",9,8,7,6,5,4,3,2)
  for (i in 1:length(id)) {
    if(nchar(id[i])==16){
      return(1)
    }else if(nchar(id[i])==18){
      sum=0;a=0;c=0
      for (j in 1:17) {
        sum=sum+as.numeric(substr(id[i],j,j))*w[j]
      }
      a=sum%%11
      c=v[a+1]
      if(c==substr(id[i],18,18)){
        return(0)#print(sprintf("第%d个身份证号是真的",i))
      }else{
        return(1)#print(sprintf("第%d个身份证号是假的，伪造身份证，你被逮捕了！",i))
        #print(id[i])
        }
    }
    else{
      return(1)#print(sprintf("第%d个身份证号是假的，伪造身份证，你被逮捕了！",i))
      #print(id[i])
    }
  }
}
#check_id('370214199505056514')
