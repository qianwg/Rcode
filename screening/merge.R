library(rio)
files = list.files("C:/Users/Administrator/Desktop/result",pattern="*.xlsx")
myfiles = do.call(rbind, lapply(paste("C:/Users/Administrator/Desktop/result/",files,sep=""),function(x) import(x)))
export(myfiles,'C:/Users/Administrator/Desktop/合并文件.xlsx')
