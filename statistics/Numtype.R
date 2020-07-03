#转换所有
col_names <- names(mydata)
mydata[,col_names] <- lapply(mydata[,col_names] , factor)
#转换指定变量
names <- c('Credit' ,'Balance')
mydata[,names] <- lapply(mydata[,names] , factor)
str(mydata)
#将所有数字型转换为因子
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], as.factor)