library(shiny)
library(DT)
require(shinydashboard)
shinyUI(dashboardPage(
    dashboardHeader(title="问卷质量评价"),
    dashboardSidebar(sidebarMenu(
        fileInput("file","选择上传文件(支持xlsx/sav)"),
        actionButton('action','家族史赋值'),
        menuItem("家族史赋值结果",tabName='family',icon=icon('seedling')),
        menuItem("结果",tabName = "result",icon=icon("adjust")),
        numericInput(inputId = "id",
                     "ID",21010001,21010001,22063000),
        menuItem("搜素ID结果",tabName = "resultID",icon=icon("th")),
        menuItem(text = "高危人群筛选", icon = icon("refresh"), href = "http://39.106.31.215:3838/risk2019/")
        
        
        
    )
    
    ),
    dashboardBody(tabItems(
        tabItem(tabName = 'family',
                fluidRow(
                    box(title = "家族史赋值前",div(style='overflow-x:scroll',DT::DTOutput("tablefam"),width = 12),width = 12,solidHeader = TRUE,status='info'
                    )),
                fluidRow(
                    box(title = "家族史赋值后",div(style='overflow-x:scroll',DT::DTOutput("tablefam2"),width = 12),width = 12,solidHeader = TRUE,status='primary'
                    )
                ),
                fluidRow(
                    box(title = "家族史赋值情况",
                        div(style='width:800px',verbatimTextOutput("summary_family"),placeholder=TRUE),width = 12,solidHeader = TRUE,status='success'))),
        tabItem(tabName = "result",
                fluidRow(
                    box(title='合格问卷',div(style='overflow-x:scroll',DT::DTOutput("table"),width = 12),width = 12,solidHeader = TRUE,status='success')),
                fluidRow(
                    box(title='不合格问卷',div(style='overflow-x:scroll',DT::DTOutput("table2"),width = 12),width = 12,solidHeader = TRUE,status='danger')),
                fluidRow( box(title = "各街道合格情况",
                              verbatimTextOutput("summary"),width = 6,solidHeader = TRUE,status='success'),
                          box(title="下载内容",downloadButton("download1", "下载合格问卷"),
                              downloadButton("download2", "下载不合格问卷"),solidHeader = TRUE,status='info'))
        ),
       
        tabItem(tabName = "resultID",
                fluidRow(box(title='个人信息',tableOutput("ID1table"),width=4,solidHeader = TRUE,status='danger'),
                         box(title='第一部分(基础信息完整)',tableOutput("ID2table"),width=12,solidHeader = TRUE,status='info'),
                         box(tableOutput("ID3table"),width=12,solidHeader = TRUE,status='info'),
                         box(title='第二部分(基础信息不合格)',tableOutput("ID4table"),width=12,solidHeader = TRUE,status='success'),
                         box(title='第三部分重要信息逻辑错误（癌症史）',tableOutput("ID5table"),width=4,solidHeader = TRUE,status='primary'),
                         box(title='第三部分重要信息逻辑错误（疾病史）',tableOutput("ID6table"),width=4,solidHeader = TRUE,status='primary'),
                         box(title='第三部分重要信息逻辑错误（吸烟史）',tableOutput("ID7table"),width=4,solidHeader = TRUE,status='primary'),
                         box(title='第三部分重要信息逻辑错误（职业暴露）',tableOutput("ID8table"),width=4,solidHeader = TRUE,status='primary'),
                         box(title='第三部分重要信息逻辑错误（女性月经与生育史）',tableOutput("ID9table"),width=8,solidHeader = TRUE,status='primary')
                         
                ))
        
    )
    
    
    
    )
    
    
)
)



