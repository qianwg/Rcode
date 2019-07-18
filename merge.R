###csv格式文件
files = list.files("C:/Users/Administrator/Desktop/gentyping",pattern="*.csv")
myfiles = do.call(rbind, lapply(paste("C:/Users/Administrator/Desktop/gentyping/",
                                      path,sep="") , function(x) read.csv(x,sep=",",header=T,stringsAsFactors = FALSE)))
###excel格式文件
library(openxlsx)
files = list.files("C:/Users/Administrator/Desktop/result",pattern="*.xlsx")
myfiles = do.call(rbind, lapply(paste("C:/Users/Administrator/Desktop/result/",files,sep=""),function(x) read.xlsx(x)))
write.xlsx(myfiles,'C:/Users/Administrator/Desktop/合并文件.xlsx')
###一般
library(rio)
files = list.files("C:/Users/Administrator/Desktop/result",pattern="*.xlsx")
myfiles = do.call(rbind, lapply(paste("C:/Users/Administrator/Desktop/result/",files,sep=""),function(x) import(x)))
export(myfiles,'C:/Users/Administrator/Desktop/合并文件.xlsx')
