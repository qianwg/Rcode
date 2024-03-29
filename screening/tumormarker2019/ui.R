library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title="高危人群筛选"),
    dashboardSidebar(sidebarMenu(
        fileInput("file","选择上传文件"),
        menuItem("原始数据",tabName = "primary",icon=icon("adjust")),
        menuItem("高危情况", tabName = "dashboard", icon = icon("dashboard")),
        
        numericInput(inputId = "id",
                     "ID",21010001,21010001,22063000),
        menuItem('搜素ID结果',tabName = 'searchID',icon=icon('search')),
        selectInput("biomark", 
                    label = "肿瘤标志物",
                    choices = c("CEA"="CEA.risk", 
                                "AFP"="AFP.risk",
                                "CA199"="CA199.risk", 
                                "CA153"="CA153.risk",
                                "CA125"="CA125.risk")
                    ),
    
        menuItem(text = "问卷质量评估", icon = icon("refresh"), href = "http://39.106.31.215:3838/check2019/"),
        menuItem(text = "问卷高危评估", icon = icon("refresh"), href = "http://39.106.31.215:3838/risk2019/")
        
        
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "primary",
                fluidRow(box(div(style='overflow-x:scroll',DT::DTOutput("table3")),width = 12)
                )
        ),
        tabItem(tabName = "dashboard",
                
                fluidRow(box(div(style='overflow-x:scroll',DT::DTOutput("table.risk"),width = 12),width = 12
                )),
                fluidRow(#box(tableOutput("select.biomark")),
                         #box(downloadButton("download", "下载人群高危情况表"),width=4,status = "info",
                        #     collapsible = T, collapsed = F),
                  tabBox(
                    side = "left", height = "500px",
                    selected = "Tab1",
                    tabPanel("标志物分布情况", tableOutput("select.biomark")),
                    tabPanel("标志物分布图", plotOutput('barplot'))
                  ),
                  box(downloadButton("download", "下载人群高危情况表"),width=4,status = "info",
                           collapsible = T, collapsed = F)
                        
                      
                        
                         )
               # ,
              #  fluidRow(
               #          box(
            #             box(downloadButton("download2", "下载高危人群项目推荐表"),width=4)
           #              1
                #)
         #       fluidRow(box(plotOutput('bar')))
         #       
        #),
        
        
       # tabItem(tabName = "someplots",
    #            fluidRow(
     #               box(plotOutput("histogram1")),box(plotOutput("histogram1_1")),
      #              box(plotOutput("histogram2")),box(plotOutput("histogram2_1")),
       #             box(plotOutput("histogram3")),box(plotOutput("histogram3_1")),
    #                box(plotOutput("histogram4")),box(plotOutput("histogram4_1"))
    #            )),
     #   tabItem(tabName = 'searchID',
       #         fluidRow(
        #            box(title='个人信息',tableOutput("IDtable"),width=4,solidHeader = TRUE,status='danger'),
        #            box(title='肺癌高危',tableOutput("lungtable"),width=12,solidHeader = TRUE,status='info'),
        #            box(title='乳腺高危',tableOutput("breasttable"),width=12,solidHeader = TRUE,status='success'),
        #            box(title='肝癌高危',tableOutput("livertable"),width=12,solidHeader = TRUE,status='primary'),
        #            box(title='胃癌高危',tableOutput("gastrictable"),width=12,solidHeader = TRUE,status='warning')
                    
        #        ))
        
    )
    )
)
)
)


