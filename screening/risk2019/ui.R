library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title="高危人群筛选"),
    dashboardSidebar(sidebarMenu(
        fileInput("file","选择上传文件"),
        menuItem("原始数据",tabName = "primary",icon=icon("adjust")),
        menuItem("高危得分", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("绘图", tabName = "someplots", icon = icon("images")),
        numericInput(inputId = "id",
                     "ID",21010001,21010001,22063000),
        menuItem('搜素ID结果',tabName = 'searchID',icon=icon('search')),
        
        menuItem(text = "问卷质量评估", icon = icon("refresh"), href = "http://39.106.31.215:3838/check2019/")
        
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "primary",
                fluidRow(box(div(style='overflow-x:scroll',DT::DTOutput("table3"),width = 12),width = 12)
                )
        ),
        tabItem(tabName = "dashboard",
                
                fluidRow(box(div(style='overflow-x:scroll',DT::DTOutput("table")),width = 12),width = 12
                ),
                fluidRow(box(verbatimTextOutput("table2"),width = 8),
                         box(downloadButton("download1", "下载人群高危得分表"),width=4),
                         box(downloadButton("download2", "下载高危人群项目推荐表"),width=4)
                         
                ),
                fluidRow(box(plotOutput('bar')))
                
        ),
        
       
        tabItem(tabName = "someplots",
                fluidRow(
                    box(plotOutput("histogram1")),box(plotOutput("histogram1_1")),
                    box(plotOutput("histogram2")),box(plotOutput("histogram2_1")),
                    box(plotOutput("histogram3")),box(plotOutput("histogram3_1")),
                    box(plotOutput("histogram4")),box(plotOutput("histogram4_1"))
                )),
        tabItem(tabName = 'searchID',
                fluidRow(
                    box(title='个人信息',tableOutput("IDtable"),width=4,solidHeader = TRUE,status='danger'),
                    box(title='肺癌高危',tableOutput("lungtable"),width=12,solidHeader = TRUE,status='info'),
                    box(title='乳腺高危',tableOutput("breasttable"),width=12,solidHeader = TRUE,status='success'),
                    box(title='肝癌高危',tableOutput("livertable"),width=12,solidHeader = TRUE,status='primary'),
                    box(title='胃癌高危',tableOutput("gastrictable"),width=12,solidHeader = TRUE,status='warning')
                    
                ))
        
    )
    )
)
)

