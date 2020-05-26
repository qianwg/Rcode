rm(list=ls())
library(ggstatsplot)
library(car)
#读取数据
source('~/Rcode/biomarker/data.R')
biomarker2<-biomarker%>%filter(!is.na(CA153))
biomarker3<-biomarker2%>%filter(CA153<=quantile(CA153,0.75)+IQR(CA153) & CA153>=quantile(CA153,0.25)-IQR(CA153))
#---------------------------------------------------------------------------------------
#年龄
biomarker2%>%transmute(CA153=log(CA153),age=age,age_group=age_group)%>%
  ggscatterstats(                                           
    x = age,                                                   
    y = CA153, nboot = 5,                                                
    xlab = "age",                 
    ylab = "log(CA153)",                                    
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             
    marginal.type = "density",                                  
    centrality.para = "mean",                                    
    margins = "both",                                            
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                           
    xalpha = 0.5,                                               
    yalpha = 0.75,                                               
    xsize = 1,                                                   
    ysize = 1,                                                   
    type = "pearson",                                            
    title = "Relationship between CA153 and age",
    messages = FALSE
  )
#吸烟
plot.smoking1<-biomarker2%>%transmute(CA153=log(CA153),smoking=factor(smoking,labels=c('Never','Current','Former')))%>%
  ggbetweenstats(
    x = smoking,
    y =CA153 ,
    ylab='log(CA153)',
    xlab='smoking',
    nboot = 10,
    messages = FALSE,bf.message = FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA153 and smoking(Drop the missing records)",
  )   
plot.smoking2<-biomarker3%>%transmute(CA153=log(CA153),smoking=factor(smoking,labels=c('Never','Current','Former')))%>%
  ggbetweenstats(
    x = smoking,
    y =CA153 ,
    ylab='log(CA153)',
    xlab='smoking',
    nboot = 10,
    messages = FALSE,bf.message = FALSE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr", 
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA153 and smoking(Drop the outlier records)",
  )   
combine_plots(
  plot.smoking1, plot.smoking2,
  nrow = 1,
  labels = c("(a)", "(b)"),
  title.text = "",
  caption.text = "",
  title.size = 14,
  caption.size = 12
)

#BMI
biomarker3%>%transmute(CA153=log(CA153),bmi=bmi,bmi_group=bmi_group)%>%
  ggscatterstats(                                           
    x = bmi,                                                  
    y = CA153,                                                  
    xlab = "BMI",                 
    ylab = "log(CA153)",                                    
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             
    marginal.type = "density",                                  
    centrality.para = "mean",                                   
    margins = "both",                                            
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                         
    xalpha = 0.5,                                                
    yalpha = 0.75,                                               
    xsize = 1,                                                 
    ysize = 1,                                                 
    type = "pearson",                                            
    title = "Relationship between CA153 and BMI",
    messages = FALSE,nboot=5
  )
plot.bmi1<-biomarker3%>%transmute(CA153=log(CA153),bmi_group=factor(bmi_group,labels = c('Lower','Normal','overweight','besity')))%>%
  ggbetweenstats(
    x = bmi_group,
    y =CA153,
    ylab='log(CA153)',
    xlab='BMI',
    nboot = 10,
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, # display results from pairwise comparisons
    pairwise.display = "significant", # display only significant pairwise comparisons
    pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA153 and smoking(Drop the outlier records)"
    #title = "Relationship between CA153 and smoking(Drop the missing records)",
    
  )  
plot.bmi2<-biomarker2%>%transmute(CA153=log(CA153),bmi_group=factor(bmi_group,labels = c('Lower','Normal','overweight','besity')))%>%
  ggbetweenstats(
    x = bmi_group,
    y =CA153,
    ylab='log(CA153)',
    xlab='BMI',
    nboot = 10,
    messages = FALSE,bf.message=FALSE,
    pairwise.comparisons = TRUE, # display results from pairwise comparisons
    pairwise.display = "significant", # display only significant pairwise comparisons
    pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    #title = "Relationship between CA153 and smoking(Drop the outlier records)",
    title = "Relationship between CA153 and smoking(Drop the missing records)",
    
  )   
combine_plots(
  plot.bmi2,plot.bmi1,
  nrow = 1,
  labels = c("(a)", "(b)"),
  title.text = " ",
  caption.text = " ",
  title.size = 14,
  caption.size = 12
)

#饮酒
plot.alcohol1<-biomarker2%>%transmute(CA153=log(CA153),alcohol=factor(alcohol,labels=c('NO','YES')))%>%
  ggbetweenstats(
    x = alcohol,
    y =CA153,
    ylab='log(CA153)',
    xlab='alcohol',
    nboot = 10,
    messages = FALSE,
    #pairwise.comparisons = TRUE, # display results from pairwise comparisons
    #pairwise.display = "significant", # display only significant pairwise comparisons
    #pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    #p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA153 and alcohol(Drop the missing records)",
  )
plot.alcohol2<-biomarker3%>%transmute(CA153=log(CA153),alcohol=factor(alcohol,labels=c('NO','YES')))%>%
  ggbetweenstats(
    x = alcohol,
    y =CA153,
    ylab='log(CA153)',
    xlab='alcohol',
    nboot = 10,
    messages = FALSE,
    #pairwise.comparisons = TRUE, # display results from pairwise comparisons
    #pairwise.display = "significant", # display only significant pairwise comparisons
    #pairwise.annotation = "p.value", # annotate the pairwise comparisons using p-values
    #p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA153 and alcohol(Drop the outlier records)",
  ) 
combine_plots(
  plot.alcohol1,plot.alcohol2,
  nrow = 1,
  labels = c("(a)", "(b)"),
  title.text = " ",
  caption.text = " ",
  title.size = 14,
  caption.size = 12
)

#疾病史(糖尿病，高血压，高血脂，冠心病，偏头疼)(偏头疼有意义)
biomarker3%>%transmute(CA153=log(CA153),
                       Diabetes=factor(Disea28,labels=c('NO','YES')),
                       Hypertension=factor(Disea29,labels=c('NO','YES')),
                       Hyperlipidemia=factor(Disea30,labels=c('NO','YES')),
                       Coronarry=factor(Disea31,labels=c('NO','YES')),
                       Migraine=factor(Disea33,labels=c('NO','YES')))%>%
  pivot_longer(cols=c('Diabetes','Hypertension','Hyperlipidemia','Coronarry','Migraine'),names_to='disea',values_to = 'levels')%>%
  grouped_ggbetweenstats(
    x = levels,
    y = CA153,
    grouping.var = disea,
    xlab = "levels",
    ylab = "log(CA153)",
    k = 2,
    nboot = 10,
    partial = FALSE, 
    pairwise.comparisons = TRUE, 
    pairwise.display = "significant", 
    pairwise.annotation = "p.value", 
    p.adjust.method = "fdr",
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    ggstatsplot.layer = FALSE,
    messages = FALSE,
    #title.text = "The relationship between CA153 and Disease(Drop the missing records)",
    title.text = "The relationship between CA153 and Disease(Drop the outlier records)",
    nrow = 3,
  )

#绝经状态
p_menopause1<-biomarker3%>%transmute(CA153=log(CA153),menopause=factor(menopause,labels=c('NO','YES')))%>%
  ggbetweenstats(
    x = menopause,
    y =CA153,
    ylab='log(CA1215)',
    xlab='menopause',
    nboot = 10,
    messages = FALSE,
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA153 and menopause(Drop the outlier records)",
  ) 
p_menopause2<-biomarker2%>%transmute(CA153=log(CA153),menopause=factor(menopause,labels=c('NO','YES')))%>%
  ggbetweenstats(
    x = menopause,
    y =CA153,
    ylab='log(CA1215)',
    xlab='menopause',
    nboot = 10,
    messages = FALSE,
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    title = "Relationship between CA153 and menopause(Drop the missing records)",
  ) 
combine_plots(
  p_menopause2,p_menopause1,
  nrow = 1,
  labels = c("(a)", "(b)"),
  title.text = " ",
  caption.text = " ",
  title.size = 14,
  caption.size = 12
)

#------------------------绝经年龄(有意义)
p_agemenopau1<-biomarker3%>%transmute(CA153=log(CA153),agemenopau=agemenopau)%>%
  ggscatterstats(                                            
    x = agemenopau,                                                  
    y = CA153,                                                  
    xlab = "age of menopause",                 
    ylab = "log(CA153)",                                     
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                            
    marginal.type = "density",                                   
    centrality.para = "mean",                                    
    margins = "both",                                           
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                           
    xalpha = 0.5,                                                
    yalpha = 0.75,                                              
    xsize = 1,                                                   
    ysize = 1,                                                   
    type = "pearson",nboot=5,                                            
    title = "Relationship between CA153 and age of menopause(Drop the outlier records)",
    messages = FALSE
  )
p_agemenopau2<-biomarker2%>%transmute(CA153=log(CA153),agemenopau=agemenopau)%>%
  ggscatterstats(                                            
    x = agemenopau,                                                  
    y = CA153,                                                  
    xlab = "age of menopause",                 
    ylab = "log(CA153)",                                     
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                            
    marginal.type = "density",                                   
    centrality.para = "mean",                                    
    margins = "both",                                           
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                           
    xalpha = 0.5,                                                
    yalpha = 0.75,                                              
    xsize = 1,                                                   
    ysize = 1,                                                   
    type = "pearson",nboot=5,                                         
    title = "Relationship between CA153 and age of menopause(Drop the missing records)",
    messages = FALSE,bf.message = FALSE
  )
combine_plots(
  p_agemenopau1,p_agemenopau2,
  nrow = 1,
  labels = c("(a)", "(b)"),
  title.text = " ",
  caption.text = " ",
  title.size = 14,
  caption.size = 12
)

#绝经状态+绝经年龄
combine_plots(
  p_menopause, p_agemenopau,
  nrow = 2,
  labels = c("(a)", "(b)"),
  title.text = "",
  caption.text = "",
  title.size = 14,
  caption.size = 12
)
#异常观测值
library(car)
row.names(biomarker3)<-biomarker3$ID_BLAST
fit_agemenopau<-lm(log(CA153)~agemenopau,data=biomarker3)
summary(fit_agemenopau)
#1离群点
outlierTest(fit_agemenopau)
#2强影响点
cutoff<-4/(6099-length(fit_agemenopau$coefficients)-2)
plot(fit_agemenopau,which=4,cook.levels=cutoff)
biomarker%>%filter(ID_BLAST==21090532 |ID_BLAST==31030853 | ID_BLAST==31070991)%>%select(ID_BLAST,CA153,agemenopau)%>%
  transmute(ID=ID_BLAST,CA153=log(CA153),agemenopau=agemenopau)
#
biomarker3%>%transmute(CA153=log(CA153),agemenopau=agemenopau)%>%filter(agemenopau>=40)%>%
  ggscatter(x='agemenopau',y='CA153',add='reg.line')+stat_cor(method='spearman')
#----------------------初潮年龄
#分布
biomarker3%>%select(agemenarch)%>%
  gghistogram(x='agemenarch',binwidth = 1)
#极端值
biomarker3%>%select(agemenarch)%>%
  gghistogram(x='agemenarch',binwidth = 1)+coord_cartesian(ylim=c(0,50))
outlier_func(biomarker3$agemenarch,biomarker3$ID_BLAST)
fit_agemenarch<-lm(log(CA153)~agemenarch,data=biomarker3)
summary(fit_agemenarch)
#散点图
biomarker3%>%transmute(agemenarch=agemenarch,CA153=log(CA153))%>%
  ggscatterstats(                                            
    x = agemenarch,                                                  
    y = CA153,                                                  
    xlab = "age of menarche",                 
    ylab = "log(CA153)",                                     
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             
    marginal.type = "density",                                   
    centrality.para = "mean",                                   
    margins = "both",                                           
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                           
    xalpha = 0.5,                                                
    yalpha = 0.75,                                              
    xsize = 1,                                                   
    ysize = 1,                                                   
    type = "pearson",                                            
    title = "Relationship between CA153 and age of menarche(Drop the outlier records)",
    messages = FALSE
  )
#绝经年龄-初潮年龄
biomarker3%>%transmute(difference=agemenopau-agemenarch,CA153=log(CA153))%>%
  ggscatterstats(                                            
    x = difference,                                                  
    y = CA153,                                                  
    xlab = "age of difference",                 
    ylab = "log(CA153)",                                     
    point.alpha = 0.7,
    point.size = 4,
    point.color = "grey50",
    marginal = TRUE,                                             
    marginal.type = "density",                                   
    centrality.para = "mean",                                   
    margins = "both",                                           
    xfill = "#CC79A7",                                           
    yfill = "#009E73",                                           
    xalpha = 0.5,                                                
    yalpha = 0.75,                                              
    xsize = 1,                                                   
    ysize = 1,nboot=5,                                                 
    type = "pearson",                                            
    title = "Relationship between CA153 and age of difference(Drop the outlier records)",
    messages = FALSE
  )
#手术史
biomarker3%>%transmute(Sterlization=factor(sterilizat,labels=c('NO','YES')),
                       hysterectomy=factor(hysterecto,labels=c('NO','YES')),
                       ovariectomy=factor(ovariectom,labels=c('NO','YES')),CA153=log(CA153))%>%
  pivot_longer(cols=c('Sterlization','hysterectomy','ovariectomy'),names_to = 'surgery',values_to = 'levels')%>%
  grouped_ggbetweenstats(
    x = levels,
    y = CA153,
    grouping.var = surgery,
    xlab = "levels",
    ylab = "log(CA153)",
    k = 2,
    nboot = 10,
    partial = FALSE, # partial omega or omega?
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "default_jco",
    ggstatsplot.layer = FALSE,
    messages = FALSE,bf.message = FALSE,
    title.text = "The relationship between CA153 and surgery(Drop the outlier records)",
    nrow = 3,
  )
#2020-4-19：CA153的分析

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
source('~/Rcode/statistics/OR.R')
get_coe <- function(the_fit,the_lamb){
  Coefficients <- coef(the_fit, s = the_lamb)
  Active.Index <- which(Coefficients != 0)
  Active.Coefficients <- Coefficients[Active.Index]
  re <- data.frame(rownames(Coefficients)[Active.Index],Active.Coefficients)
  re <- data.table('var_names'=rownames(Coefficients)[Active.Index],
                   'coef'=Active.Coefficients)
  re$expcoef <- exp(re$coef)
  return(re[order(expcoef)])
}
#连续性指标
source('~/Rcode/biomarker/data_CA153.R')
variables<-c("癌症家族史","乳腺癌家族史",      
             "年龄","就业状况","BMI",              
             "偏咸","腌制","饮酒","喝茶",             
             "酸奶","吸烟" ,"被动吸烟", "婚姻",             
             "教育", "血型","蔬菜","水果",             
             "谷类","鸡蛋","杂粮","豆类",              
             "坚果", "菌类","油炸","烧烤",              
             "熏制","静态时间","手机使用时间","乳腺小叶不典型增生",
             "乳腺导管不典型增生","乳腺重度不典型增生","乳腺纤维瘤","糖尿病",           
             "高血压","高血脂","冠心病","中风" ,             
             "初潮年龄","绝经年龄","口服避孕药","激素治疗",          
             "人工流产次数",'绝育手术','子宫摘除术','卵巢摘除术')
#单因素分析(均值比较)
means_CA153<-CA153%>%pivot_longer(cols=variables,names_to='variable',values_to = 'level')%>%
  group_by(variable,level)%>%summarise(n=n(),median=median(CA153),Q1=quantile(CA153,0.25),Q3=quantile(CA153,0.75))
print(means_CA153,n=107)
p<-list()
for(i in variables){
  formula_uni<-as.formula(paste('CA153','~', i))
  if(length(table(CA153[,i]))==2){
    p[[i]]<-round(wilcox.test(formula_uni,data=CA153)$p.value,4)
  }
  else{
    p[[i]]<-round(kruskal.test(formula_uni,data=CA153)$p.value,4)
  }
}
do.call(rbind,p)
#多因素回归分析
summary(glm(log(CA153)~.,data=CA153[,c('CA153',variables)]))
#CA153作为二分类变量(CA153>=25)
#频率分布
table1(~癌症家族史+乳腺癌家族史+年龄+家庭收入+就业状况+BMI+偏咸+腌制+饮酒+喝茶+
         酸奶+吸烟+被动吸烟+婚姻+教育+血型+蔬菜+水果+谷类+鸡蛋+杂粮+豆类+坚果+
         +菌类+油炸+烧烤+熏制+静态时间+手机使用时间+乳腺小叶不典型增生+乳腺导管不典型增生+
         乳腺重度不典型增生+乳腺纤维瘤+糖尿病+高血压+高血脂+冠心病+中风+初潮年龄+
         绝经年龄+口服避孕药+激素治疗+人工流产次数+绝育手术+子宫摘除术+卵巢摘除术 | CA153_pos, data=CA153,render.categorical=my.render.cat)
p2<-list()
for(i in variables){
  y<-CA153[[i]]
  p2[[i]]<-round(chisq.test(table(y,CA153$CA153_pos))$p.value,3)
}
do.call(rbind,p2)
#多因素分析
#多重共线性检验
vif_data<-vif(glm(CA153_pos~.,family='binomial',data=CA153[,c('CA153_pos',variables)]))
vif_data
sort(vif_data,decreasing = TRUE)
#logistic回归
logit(y='CA153_pos',x=c("癌症家族史","乳腺癌家族史",      
                        "年龄","家庭收入","就业状况","BMI",              
                        "偏咸","腌制","饮酒","喝茶",             
                        "酸奶","吸烟" ,"被动吸烟", "婚姻",             
                        "教育", "血型","蔬菜","水果",             
                        "谷类","鸡蛋","杂粮","豆类",              
                        "坚果", "菌类","油炸","烧烤",              
                        "熏制","静态时间","手机使用时间","乳腺小叶不典型增生",
                        "乳腺导管不典型增生","乳腺重度不典型增生","乳腺纤维瘤","糖尿病",           
                        "高血压","高血脂","冠心病","中风" ,             
                        "初潮年龄","绝经年龄","口服避孕药","激素治疗",          
                        "人工流产次数",'绝育手术','子宫摘除术','卵巢摘除术'),data=CA153)
#lasso-logistic回归
CA153_2<-na.omit(CA153[,c('CA153_pos',variables)])
library(glmnet)
x<-model.matrix(CA153_pos~.,CA153_2,contrasts.arg = lapply(CA153_2[ ,sapply(CA153_2, is.factor)], contrasts, contrasts = FALSE ))
x<-x[,-1]
y<-CA153_2[,c('CA153_pos')]
lasso <- glmnet(x,y, family = "binomial", alpha = 1)
plot(lasso, xvar = "lambda", label = TRUE)
cv_output<-cv.glmnet(x,y,alpha=1,family='binomial')#lambda
plot(cv_output)
coef(cv_output,s='lambda.min')#系数
get_coe(cv_output,cv_output$lambda.min)
#group-lasso logistic
group<-c("癌症家族史","癌症家族史","乳腺癌家族史","乳腺癌家族史","年龄","年龄",                 
         "年龄","就业状况","就业状况","就业状况","就业状况","BMI",                   
         "BMI","BMI","BMI","偏咸","偏咸","腌制",                     
         "腌制","饮酒","饮酒","喝茶","喝茶","酸奶", "酸奶","吸烟","吸烟",          
         "吸烟","被动吸烟","被动吸烟","被动吸烟","婚姻","婚姻",                  
         "婚姻","教育","教育","教育","血型","血型",                     
         "血型","血型","血型", "蔬菜","蔬菜","水果","水果","谷类","谷类",                     
         "鸡蛋","鸡蛋","杂粮", "杂粮","豆类","豆类","坚果","坚果","菌类",                     
         "菌类","油炸","油炸","烧烤","烧烤","熏制","熏制", "静态时间","静态时间",                 
         "静态时间","静态时间","手机使用时间","手机使用时间","手机使用时间","手机使用时间",  
         "乳腺小叶不典型增生","乳腺小叶不典型增生","乳腺导管不典型增生",      
         "乳腺导管不典型增生","乳腺重度不典型增生","乳腺重度不典型增生",      
         "乳腺纤维瘤","乳腺纤维瘤","糖尿病","糖尿病","高血压","高血压",                  
         "高血脂","高血脂","冠心病","冠心病","中风","中风",                    
         "初潮年龄","初潮年龄","绝经年龄","绝经年龄","绝经年龄","口服避孕药",              
         "口服避孕药","激素治疗","激素治疗", "人工流产次数","人工流产次数","绝育手术","绝育手术",
         "子宫摘除术","子宫摘除术","卵巢摘除术","卵巢摘除术")
fit <- grpreg(x, y, group, penalty="grLasso", family="binomial")
plot(fit)
cvfit<- cv.grpreg(x, y, group, penalty="grLasso",family='binomial')
plot(cvfit)
as.matrix(coef(cvfit,lambda=cvfit$lambda.min))
#对于乳腺癌的诊断效果(诊断效果很差的)
roc(CA153$CA_breast, CA153$CA153,col="red",legacy.axes="TRUE",ci="TRUE",print.ci=TRUE)
plot.roc(CA153$CA_breast,CA153$CA153,direction='<',add=F,legacy.axes=T,las=1,col="red", print.auc=T,print.thres=T)




