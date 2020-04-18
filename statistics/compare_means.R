##
p<-list()
for(i in variable){
  formula_uni<-as.formula(paste('PG1','~', i))
  if(length(table(pepsinogen[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=pepsinogen)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=pepsinogen)$p.value,4)
  }
}
result_p<-do.call(rbind,p)
result_p<-cbind(variable=row.names(result_p), result_p)
row.names(result_p)=NULL
result_p
