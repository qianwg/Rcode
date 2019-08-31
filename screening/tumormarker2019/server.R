library(shiny)
library(DT)
library(tidyverse)
library(rio)
library(data.table)
library(shinydashboard)
library(reshape2)
options(shiny.maxRequestSize=70*1024^2)
shinyServer(function(input, output) {
    ##读取数据
    biomark<-reactive({
        inFile1 <- input$file
        if (is.null(inFile1)) return(NULL)
        data<-rio::import(inFile1$datapath) 
    })
   
    ## 肿瘤标志物高危高危界定
    #1高危标准
    risk<-function(x){
        x2<-factor(x,levels = c(0,1,2,3,4,5),labels=c('正常','超出截值1-2倍','超出截值2-3倍','超出截值3-4倍','超出截值4倍-200','超过200'))
        return(x2)
    }
   
    biomark2<-reactive({
       biomark<-biomark()
        biomark2<-within(biomark,{
            CEA.risk<-vector()
            AFP.risk<-vector()
            CA199.risk<-vector()
            CA153.risk<-vector()
            CA125.risk<-vector()
            #CEA
            CEA.risk[CEA<=5]<-0
            CEA.risk[CEA>5 & CEA<=10]<-1
            CEA.risk[CEA>10 & CEA<=15]<-2
            CEA.risk[CEA>15 & CEA<=20]<-3
            CEA.risk[CEA>20 & CEA<200]<-4
            CEA.risk[CEA>=200]<-5
            #AFP
            AFP.risk[AFP<=7]<-0
            AFP.risk[AFP>7 & AFP<=14]<-1
            AFP.risk[AFP>14 & AFP<=21]<-2
            AFP.risk[AFP>21 & AFP<=28]<-3
            AFP.risk[AFP>28 & AFP<200]<-4
            AFP.risk[AFP>=200]<-5
            
            #CA199
            CA199.risk[CA199<=27]<-0
            CA199.risk[CA199>27 & CA199<=27*2]<-1
            CA199.risk[CA199>27*2 & CA199<=27*3]<-2
            CA199.risk[CA199>27*3 & CA199<=27*4]<-3
            CA199.risk[CA199>27*4 & CA199<200]<-4
            CA199.risk[CA199>=200]<-4
            
            #CA153
            CA153.risk[CA153<=30]<-0
            CA153.risk[CA153>30 & CA153<=60]<-1
            CA153.risk[CA153>60 & CA153<=90]<-2
            CA153.risk[CA153>90 & CA153<=120]<-3
            CA153.risk[CA153>120 & CA153<200]<-4
            CA153.risk[CA153>=200]<-5
            #CA125
            CA125.risk[CA125<=35]<-0
            CA125.risk[CA125>35 & CA125<=70]<-1
            CA125.risk[CA125>70 & CA125<=105]<-2
            CA125.risk[CA125>105 & CA125<=140]<-3
            CA125.risk[CA125>140 & CA125<200]<-4
            CA125.risk[CA125>=200]<-4
            
        
            
        })
        biomark2[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')]<-apply(biomark2[,c('CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')],2,risk)
          biomark2
        
})
   
    
    #上传原始数据
    output$table3<-renderDT({
        datatable(biomark(),class="cell-border stripe",caption = '表1：上传原始数据',width = 12,options=list(pageLength=10,autoWidth=TRUE))
    })
    #上传高危情况
    table.risk<-reactive({
       
        biomark2()[,c('id','name','CEA.risk','AFP.risk','CA199.risk','CA153.risk','CA125.risk')]
    })
    output$table.risk<-renderDT({
        datatable(table.risk(),class="cell-border stripe",caption = '表1：高危情况',width = 12,options=list(pageLength=10,autoWidth=TRUE),colnames=c('编号','姓名','CEA','AFP','CA199','CA153','CA125'))
    })
    #输出高危分布情况
    
    
   
 
    #output$select.biomark<-renderTable({
     #   table(biomark2()$CEA.risk)
     #})
    output$select.biomark<-renderTable({
        table(biomark2()[,input$biomark])
    })
    
    #输出图片
    biomark3<-reactive({
        biomark3<-subset(biomark2(),input$biomark!=0)
        biomark3
    })
    output$barplot<-renderPlot({
       
        aa1<-data.frame(table(biomark3()[,input$biomark]))
        bb1<-table(biomark3()[,input$biomark])
        ee1<-barplot(bb1,ylim=c(0,max(aa1[,2]+10)))
        text(ee1,aa1[,2]+10,labels=aa1[,2],pos=1,cex=1.5)
    })

    })
   
    
    
    
    
    
