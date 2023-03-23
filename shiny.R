require(shiny)
require(shinythemes)
require(shinydashboard)
require(shinyjqui)
require(ggplot2)
require(flexdashboard)
require(shinyjs)
runExample("01_hello") # a histogram
runExample("02_text") # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # Shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer
 

#1 滑动条
shinyApp(
  ui <- fluidPage(titlePanel(title = "我的第一个shiny.APP",windowTitle = "浏览器标题"),
                  sidebarLayout(sidebarPanel(sliderInput(inputId = "滑块",label = "观察值的数目",min = 1,max = 20,value = 10)),
                                mainPanel(plotOutput(outputId = "plot"))),
  ),
  server <- function(input,output){
    output$plot <- renderPlot({
      dist <- rnorm(input$滑块)
      hist(x = dist,main = "我是直方图")
    })
  }
)


#2 选择框和数字框
ui <- fluidPage(titlePanel(title = "我的第二个shiny.app"),
                sidebarPanel(selectInput(inputId = "原始数据框", #选择框
                                         label = "原始数据",
                                         choices = c("rock","pressure","cars")),
                             numericInput(inputId = "观察数",  #数字框
                                          label = "观察数",
                                          min = 5,
                                          max = 15,
                                          value = 10)),
                mainPanel(verbatimTextOutput("summary"),
                          tableOutput("view")
                          )
                )

require(datasets)
server <- function(input,output){
  datasetInput <- reactive(switch(EXPR = input$原始数据框,   #switch:切换选项；reactive:选项发生改变时执行切换结果,相对于isolate（需要按钮执行）
                                   "rock" = rock,
                                   "pressure" = pressure,
                                   "cars" = cars))
  output$summary <- renderPrint(summary(datasetInput()))
  output$view <- renderTable(head(datasetInput(),
                                  n = input$观察数))
}

shinyApp(ui = ui,server = server)

#3 文本输入框
ui <- fluidPage(titlePanel(title = "我的第三个shiny.app"),
                sidebarPanel(textInput(inputId = "总结框",    #文本输入框
                                       label = "总结框命名：",
                                       value = "请输入："),
                             selectInput(inputId = "原始数据框",
                                         label = "原始数据",
                                         choices = c("rock","pressure","cars")),
                             numericInput(inputId = "观察数",
                                          label = "观察数",
                                          value = 10,
                                          min = 5,
                                          max = 15)),
                mainPanel(h3(textOutput("name")),
                          verbatimTextOutput("summary"),
                          tableOutput("view"))
                )

require(datasets)
server <- function(input,output){
  datasetInput <- reactive(switch(EXPR = input$原始数据框,   #switch:切换并且指定选项；reactive:选项发生改变时执行切换结果
                                  "rock" = rock,
                                  "pressure" = pressure,
                                  "cars" = cars))
  output$name <- renderText(input$总结框)
  output$summary <- renderPrint(summary(datasetInput()))
  output$view <- renderTable(head(x = datasetInput(),
                      n = input$观察数))
}

shinyApp(ui = ui,server = server)

#4 勾选框
ui <- fluidPage(titlePanel(title = "我的第四个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(selectInput(inputId = "变量",
                                        label = "请选择变量",
                                        choices = c("cyl","am","gear"),
                                        multiple = F),  #多选
                            checkboxInput(inputId = "异常值",  #勾选框（箱线图是否显示异常值）
                                          label = "√ 表示显示",
                                          value = F)),
                mainPanel(h3(textOutput("标题")),
                         plotOutput("箱线图")
                         )
                 )

require(datasets)
mtcars$am <- factor(mtcars$am,labels = c("Automatic","Manual"))    

server <- function(input,output){
  formulaText <- reactive(paste("mpg ~",input$变量))
  output$标题 <- renderText(formulaText())
  output$箱线图 <- renderPlot({boxplot(as.formula(formulaText()),data = mtcars,outline = input$异常值)})
} #outline:异常值显示；as.formula:转换表达式
                
shinyApp(ui = ui,server = server)               
                
#5 滑动条
ui <- fluidPage(titlePanel(title = "我的第五个shiny.app",windowTitle = "浏览器标题"),
                sidebarLayout(sidebarPanel(sliderInput(inputId = "a",label = "a:",min = 0,max = 1000,value = 500),
                             sliderInput(inputId = "b",label = "b:",min = 0,max = 1,value = 0.5,step = 0.1),
                             sliderInput(inputId = "c",label = "c:",min = 1,max = 1000,value = c(200,500)),
                             sliderInput(inputId = "d",label = "d:",min = 0,max = 10000,value = 0,step = 2500,pre = "$",sep = ",",animate= T),   #pre:放在值前面的 字符串;animate:播放按钮;sep:千分符号
                             sliderInput(inputId = "e",label = "e:",min = 1,max = 2000,value = 1,step = 100,animate = animationOptions(loop = T))), #loop:循环播放
                              mainPanel(tableOutput("values")),
                              position = "left",  #侧边栏位置
                              fluid = T  #适当变换位置
                              ))

server <- function(input,output){
  sliderValues <- reactive({
    data.frame(name = c("a","b","c","d","e"),
               value = as.character(c(input$a,input$b,paste(input$c, collapse=' '),input$d,input$e)),stringsAsFactors = F)
  }
                           )
  output$values <- renderTable(sliderValues())
}
                
shinyApp(ui = ui,server = server)              
              
#6 选项卡和选项列表
ui <- fluidPage(titlePanel(title = "我的第六个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(radioButtons(inputId = "选项卡",label = "请选择",choices = c("rnorm","runif","rlnorm","rexp")), #选项卡
                             sliderInput(inputId = "数值",label = "选择值：",min = 1,max = 1000,value = 500),
                             br()), #加大垂直间距
                mainPanel(tabsetPanel(type = "tabs", #tabsetPanel:创建选项列表
                                      tabPanel(title = "plot",plotOutput("plot")), 
                                      tabPanel(title = "summary",verbatimTextOutput("summary")),
                                      tabPanel(title = "table",tableOutput("table")))
                          )
                )
                
server <- function(input,output){
  data <- reactive({计算 <- switch(EXPR = input$选项卡,
                          "rnorm" = rnorm,
                          "runif" = runif,
                          "rlnorm" = rlnorm,
                          "rexp" = rexp,
                          rnorm)
                    计算(input$数值)})
  
  output$plot <- renderPlot(hist(data(),border = "blue")) 
  output$summary <- renderPrint(summary(data()))
  output$table <- renderTable(data.frame(data()))
}

shinyApp(ui = ui,server = server)

#7 帮助文本和提交按钮
ui <- fluidPage(titlePanel(title = "我的第七个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(selectInput(inputId = "选择",label = "请选择",choices = c("rock","pressure","cars")),
                             numericInput(inputId = "数值",label = "请输入0~15的数值：",value = 10,min = 0,max = 15),
                             helpText("这是一个帮助,但现在没有帮助!"), #帮助文本
                             submitButton(text = "提交")), #提交按钮
                mainPanel(h4("Summary"),  #按顺序出现，很重要
                          verbatimTextOutput("summary"),
                          h4("Table"),
                          tableOutput("view")
                          )
                )

server <- function(input,output){
  data <- reactive(switch(EXPR = input$选择,
                          "rock" = rock,
                          "pressure" = pressure,
                          "cars" = cars)
                   )
  output$summary <- renderPrint(summary(data()))
  output$view <- renderTable(head(x = data(),n = input$数值))
}

shinyApp(ui = ui,server = server)

#8 上传文件(默认不超过5M,server前可以改)
ui <- fluidPage(titlePanel(title = "我的第八个shiny.app",windowTitle = "浏览器标题"),
                sidebarLayout(sidebarPanel(fileInput(inputId = "路径",label = "请选择文件：",multiple = F,accept = c(".csv",".xlsx"),buttonLabel = "浏览",placeholder = "无文件选中"), #上传文件
                                           helpText("这是一个帮助,但现在没有帮助!"),
                                           tags$hr(), #水平线
                                           checkboxInput(inputId = "表头",label = "表头",value = T), #"√ 表示显示表达"
                                           tags$hr(),
                                           radioButtons(inputId = "间符",label = "间隔符号",choices = c(",",";","\t"),selected = ","), # \t:tab键
                                           tags$hr(),
                                           radioButtons(inputId = "head",label = "head",choices = c("head","all"))),
                              mainPanel(tableOutput("文件内容"))
                                            ))
options(shiny.maxRequestSize = 30*1024^2) #上传大小改为30M

server <- function(input,output){
  output$文件内容 <- renderTable({
    req(input$路径) #req:在继续计算或操作之前，请确保值可用。如果给定值中有任何值不是真的，则通过引发“静默”异常来停止操作。
    文件 <- input$路径
    df <- read.table(文件$datapath, #超级要注意 $datapath：选中文件的路径
                   header = input$表头,
                   sep = input$间符
                   )
  
    ifelse(input$head == T,return(df()),return(head(df())))
    
  })
}

shinyApp(ui = ui,server = server)

#9 下载文件
ui <- fluidPage(titlePanel(title = "我的第九个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(selectInput(inputId = "选择",label = "请选择",choices = c("rock","pressure","cars"),selected = "cars"),
                             downloadButton(outputId = "下载",label = "下载"),  #下载文件
                             tags$hr(),
                             sliderInput(inputId = "num",label = "显示数",min = 1,max = 50,value = 10,step = 1,animate = T)),
                mainPanel(tableOutput("table")))

require(datasets)

server <- function(input,output){
  data <- reactive(switch(EXPR = input$选择,
                          "rock" = rock,
                          "pressure" = pressure,
                          "cars" = cars))
  output$table <- renderTable(head(x = data(),n = input$num))
  output$下载 <- downloadHandler(filename = function(){paste0(input$选择,".csv")},
                                 content = function(file){write.csv(data,file)})
} #downloadHandler;filename:默认文件名,不能直接paste;cotent:往文件里写内容

shinyApp(ui = ui,server = server)

#10 时间（刷新间隔）
ui <- fluidPage(titlePanel(title = "我的第十个shiny.app",windowTitle = "浏览器标题"),
                h1(textOutput("时间"))
                )

server <- function(input,output,session){ #session:客户端数据
  output$时间 <- renderText({
    invalidateLater(1000,session) #刷新间隔(对象将在间隔过后失效(并重新执行))
    paste0("当前时间：",Sys.time())}
  )
}

shinyApp(ui = ui,server = server)

#11-1 控制动态显示(以输入为条件)
ui <- fluidPage(titlePanel(title = "我的第十一个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(checkboxInput(inputId = "线性",label = "是否选择模拟线性：",value = F),
                             conditionalPanel(condition = "input.线性 == true",  #控制组件(用.而不是$,必须用小写true or false)
                                              selectInput(inputId = "选择",label = "请选择模拟线性：",choices = c("lm","glm","gam","loess","rlm"),selected = "glm")
                                              )
                              )
                )

shinyApp(ui = ui,server = server)

#11-2 控制动态显示(以输出为条件)
ui <- fluidPage(titlePanel(title = "我的第十二个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(selectInput(inputId = "数据",label = "请选择数据：",choices = c("rock","pressure","cars"),selected = "cars"),
                             conditionalPanel(condition = "output.nrows > 20",
                                              checkboxInput(inputId = "head",label = "显示全部",value = F)
                                              )
                             ),
                mainPanel(verbatimTextOutput("nrows"),
                          tableOutput("数据框"))
                )

require(datasets)

server <- function(input,output){
  data <- reactive(switch(EXPR = input$数据,
                          "rock" = rock,
                          "pressure" = pressure,
                          "cars" = cars)
                   )
  output$nrows <- reactive(nrow(data()))
  output$数据框 <- renderTable({
    ifelse(input$head == T,return(data()),return(head(data(),20)))
  })
}

shinyApp(ui = ui,server = server)

#12 客户端数据(clientData) session像python里的self,谁访问就是谁
ui <- fluidPage(titlePanel(title = "我的第十三个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(h3("shiny端数据:"),
                             tags$hr(),
                             sliderInput(inputId = "数目",label = "选择数目：",min = 10,max = 1000,value = 10,step = 10,round = T,animate = T)),
                mainPanel(h3("客户端数据:"),
                          verbatimTextOutput("clientData"),
                          plotOutput("plot"),
                          )
                )

server <- function(input,output,session){
  data <- session$clientData
  output$clientData <- renderText({
    value <- lapply(names(data), function(name){
      paste(name,data[[name]],sep = "=")
    })
  paste(value,collapse = "\n")
  })

  output$plot <- renderPlot(hist(x = rnorm(input$数目),main = "直方图生成"))}

shinyApp(ui = ui,server = server)

#13 非常规图输出(其他图象输出设备)
ui <- fluidPage(titlePanel(title = "我的第十三个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(sliderInput(inputId = "数目",label = "选择数目：",min = 10,max = 1000,value = 10,step = 10,round = T,animate = T)),
                mainPanel(imageOutput("image"))  #图像输出，优于plotOutput,作图多样化
                )

server <- function(input,output,session){
  output$image <- renderImage({
    outfile <- tempfile(pattern = "我的图_",fileext = ".png",tmpdir = "C:\\学习\\R\\shiny") #tempfile:返回可用作临时文件名称(包括路径)的字符串矢量;pattern:提供名称开头部分的非空字符向量;tmpdir:路径名
    png(filename = outfile,width = 400,height = 300)  #shiny端图片设置（png图形输出设备）
    hist(x = rnorm(input$数目),main = "renderImaget")
    dev.off()
    list(src = outfile, #浏览器上图片的设置(必须返回网页上img列表）  
         contentType = "image/png",
         width = 400,
         height = 400,
         alt = "说明文本")
  },deleteFile = T )    #和上个大括号同级

}
    
shinyApp(ui = ui,server = server)

#13-2 非常规图输出(已做好的图+动态出图)
ui <- fluidPage(titlePanel(title = "我的第十四个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(fileInput(inputId = "浏览",label = "请选择文件",multiple = T,accept = ".jpg",buttonLabel = "浏览")),
                #mainPanel(imageOutput("image"))
                mainPanel(tableOutput("path"),
                          imageOutput("image"))
                )

server <- function(input,output,session){
  output$path <- renderTable({
    file <- input$浏览
  })
  output$image <- renderImage({
    width <- session$clientData$output_image_width
    height <- session$clientData$output_image_height
    pixelratio <- session$clientData$pixelratio #分辨率
    
    file <- input$浏览   #各输出独立，变量不共用
    filename <- normalizePath(path = file$datapath,winslash = "/",mustWork = T) #矫正路径分隔符；path错误不用管，系统问题
    list(src = filename,
         contentType = "image/jpg",
         width = width*pixelratio,
         height = height*pixelratio,
         res = 72*pixelratio,
         alt = "本地的图")},
    deleteFile = F)
}

shinyApp(ui = ui,server = server)

#14-3 动态输出非常规图(依据客户端窗口)
ui <- fluidPage(titlePanel(title = "我的第十五个shiny.app",windowTitle = "浏览器标题"),
                sidebarPanel(sliderInput(inputId = "数目",label = "选择数目：",min = 10,max = 1000,value = 10,step = 10,round = T,animate = T)),
                mainPanel(imageOutput("image"))  #图像输出，优于plotOutput,作图多样化
)

server <- function(input,output,session){
  output$image <- renderImage({
    width <- session$clientData$output_image_width
    height <- session$clientData$output_image_height
    pixelratio <- session$clientData$pixelratio #分辨率
    
    outfile <- tempfile(pattern = "我的图_",fileext = ".png",tmpdir = "C:\\学习\\R\\shiny") #tempfile:返回可用作临时文件名称(包括路径)的字符串矢量;pattern:提供名称开头部分的非空字符向量;tmpdir:路径名
    png(filename = outfile,width = width*pixelratio,height = height*pixelratio,res = 72*pixelratio)  #shiny端图片设置（png图形输出设备）
    hist(x = rnorm(input$数目),main = "renderImaget")
    dev.off()
    list(src = outfile, #浏览器上图片的设置(必须返回网页上img列表）  
         contentType = "image/png",
         width = width,
         height = height,
         alt = "说明文本")
  },deleteFile = T )    #和上个大括号同级
  
}

shinyApp(ui = ui,server = server)

#15 reactive和isolate的区别(用submitButton可以解决，但是会控制全部组件,且不能共存）) ;生成数据的函数并不需要每次都进行运算，所以通过isolate()进行隔离，从而减少依赖和运算量
ui <- fluidPage(titlePanel( "我的第十六个shiny.app",windowTitle = "浏览器标题"),
                hr(),
                sidebarPanel(sliderInput(inputId = "数值",label = "请选择数值或点击播放按钮：",min = 5,max = 1000,value = 5,step = 50,round = T,animate = T),
                             actionButton(inputId = "单独提交",label = "提交"),
                             hr(),
                             textInput(inputId = "文字",label = "请输入文字：")),

                mainPanel(h3("比较："),
                          hr(),
                          textOutput("文字"),
                          hr(),
                          plotOutput("plot"))
                )

server <- function(input,output,session){
  output$文字 <- renderText(input$文字)
  output$plot <- renderPlot({
    input$单独提交
    isolate(hist(rnorm(input$数值),main = "我是延迟的图")
                            )})
}

shinyApp(ui = ui,server = server)

#16 日期和日期区间
ui <- fluidPage(titlePanel( "我的第十七个shiny.app",windowTitle = "浏览器标题"),
                hr(),
                sidebarLayout(sidebarPanel(h3("请选择日期"),
                                          dateInput(inputId = "日期",label = "请选择日期：",value = Sys.Date(),language = "zh-CN",autoclose = F), #startview:第一次单击输入对象时显示的日期范围;weekstart:星期几；format:默认"yyyy-mm-dd"
                                          dateRangeInput(inputId = "日期范围",label = "请选择日期范围：",start = Sys.Date() - 2,end = Sys.Date() + 2,min = Sys.Date() - 10, max = Sys.Date() + 10,language = "zh-CN",autoclose = F,separator = " to ")), 
                                      
                 mainPanel(verbatimTextOutput("date"),
                           verbatimTextOutput("dateRange"),)
                                           
                ))
                
server <- function(input,output){
  output$date <- renderPrint(input$日期)
  output$dateRange <- renderPrint(input$日期范围)
}                

shinyApp(ui = ui,server = server)                
                
#17 主题+一体化程序
shinyApp(
  ui <- fluidPage(titlePanel("我的第十八个shiny.app",windowTitle = "浏览器标题"),
                  hr(),
                  checkboxInput(inputId = "显示",label = "显示主题选项",value = F),
                  conditionalPanel(condition = "input.显示 == true",
                                   themeSelector()), #选择主题
                  ),
  
  server <- function(input,output){}
)


#18 shinydashboard
#icon:refresh(刷新图标)；user（用户图标）；th（九宫格）；bar-chart-o（图表） 网站（https://icons.getbootstrap.com/）
#badgeStatus(status）primary (Blue)；success (Green)；info (Blue)；warning (Orange)；danger (Red)
require(shinydashboard)
require(datasets)
require(ggplot2)
require(shiny)
shinyApp(
  ui <- dashboardPage(header = dashboardHeader(title = "我是shiny标题",titleWidth = 250,
                                               dropdownMenu(type = "notifications",badgeStatus = "info",icon = icon("book"),headerText = "我是下拉标题",notificationItem(text = "查看帮助",icon = icon("file"),status = "success",href = "www.hao123.com"))), #多加菜单多加notificationItem
                      sidebar = dashboardSidebar(sidebarUserPanel(name = "王春虎",subtitle = a(icon("circle",class="text-success"))),
                                                 sidebarSearchForm(textId = "搜索",buttonId = "搜索",label = "请搜索：",icon = icon("search")),
                                                 sidebarMenu(id = "我是shiny标题",
                                                             menuItem(text = "First",icon = icon("th"),badgeLabel = "我是First's简介",badgeColor = "blue",tabName = "First"),
                                                             menuItem(text = "Second",icon = icon("bar-chart-o"),badgeLabel = "我是Second's简介",badgeColor = "blue",tabName = "Second"))),
                      body = dashboardBody(tabItems(
                        tabItem(tabName = "First",fluidPage(
                          box(title = "我是大box",width = 600,background = "maroon",
                              box(title = "我是图",height = 310,plotOutput("plot",height = 250)),
                              box(title = "我是滑动条",height = 200,sliderInput(inputId = "滑动条",label = "我是滑动条",min = 0,max = 100,value = 50,round = T,animate = T))))),
                        tabItem(tabName = "Second",fluidPage(
                          box(title = "我是大box",width = 600,background = "navy",
                              box(title = "我是选择框",height = 310,selectInput(inputId = "选择",label = "请选择：",choices = c("rock","pressure","cars"))),
                              tableOutput("数据框"))
                        ))
                                                   )),
                      title = "我是浏览器标题",
                      skin = "green"
    
  ),
  
  server <- function(input,output){
    output$plot <- renderPlot(ggplot(data.frame(x = rnorm(input$滑动条),y = rnorm(input$滑动条)),aes(x,y))+
                                geom_line())
    
    data <- reactive(switch(EXPR = input$选择,
                            "rock" = rock,
                            "pressure" = pressure,
                            "cars" = cars))
    output$数据框 <- renderTable(head(data()))
  }
)

#19  jQuery创建可伸缩扩展的页面元素(移动缩放可以嵌套，但各自不能包含多对象)(shinyqui包）=适用于dashboard + 动态ui（控件直接写到renderUI里，输出件另起，相当于下移一层）
require(shinyjqui)
require(shinythemes)
shinyApp(
  ui <- fluidPage(titlePanel(title = h4("jQuery创建可伸缩扩展的页面元素"),windowTitle = "浏览器标题"),
                  fluidPage(title = "",themeSelector(),
                            sidebarPanel(jqui_draggable(checkboxInput(inputId = "勾选框",label = "我是勾选框,我可以动",value = F)),
                                         hr(),
                                         jqui_draggable(selectInput(inputId = "选择框",label = "我是选择框框,我也可以动",choices = c(1,2,3))),
                                         hr(),
                                         jqui_resizable(selectInput(inputId = "选择框",label = "我是选择框框,我也可以缩放",choices = c(1,2,3))),
                                         jqui_draggable(sliderInput(inputId = "a",label = "a:",min = 0,max = 1000,value = 500))
                                          ),
                            mainPanel(jqui_resizable(jqui_draggable(plotOutput("plot"))),
                                      jqui_draggable(uiOutput("ui")),
                                      uiOutput("ui2"),
                                      jqui_draggable(uiOutput("ui3")),
                                      jqui_draggable(uiOutput("ui4")))
                            )),
  server <- function(input,output){
    output$plot <- renderPlot(hist(rnorm(input$a),main = "我会动"))
    output$ui <- renderUI(
      sliderInput(inputId = "b",label = "我是动态ui，而且我会动",min = 0,max = 1000,value = 500)
      )
    
    output$ui2 <- renderUI(plotOutput("plot1"))
    output$plot1 <- renderPlot(hist(rnorm(input$b),main = "我不会动"))
    
    output$ui3 <- renderUI(gaugeOutput("plot2")) #gaugeOutput:仪表盘（flexdashboard）
    output$plot2 <- renderGauge(gauge(value = 97,min = 0,max = 100,gaugeSectors(
      danger = c(0, 39), warning = c(40, 79), success = c(80, 100)
    )))
    
    output$ui4 <- renderUI(gaugeOutput("plot3"))
    output$plot3 <- renderGauge(gauge(value = 3,min = 0,max = 100,gaugeSectors(
      danger = c(0, 39), warning = c(40, 79), success = c(80, 100)
    )))
    
  }
)

#19-2 orderInput
shinyApp(
  ui <- fluidPage(
    orderInput('source', 'Source', items = 0:9, #items:month.app
               as_source = TRUE, connect = 'dest'), #as_source:可被拖曳；connect：拖曳到哪
    orderInput('dest', 'Dest', items = NULL, placeholder = '请拖到这来'),
    verbatimTextOutput('order')
  ),
  server <- function(input, output) {
    output$order <- renderPrint({ print(input$dest_order) })
  }
)                

#20 进度条
if(interactive()){
  options(device.ask.defaul = F)
}

shinyApp(
  ui <- fluidPage(plotOutput("plot"),
                  plotOutput("plot1")),
  
  server <- function(input,output){
    output$plot <- renderPlot({
      withProgress(value = 0,message = "正在执行...",detail = "(可能需要一杯咖啡的时间)",
                   {
                     for (i in 1:20) {
                       incProgress(amount = 1/20) #按比例。(setProgress:value(按固定值))
                       Sys.sleep(1)
                     }
                   })
      }
    )
    
  }
)

#21 页面控件隐藏/显示（toggle(可再次点击返回,需要先隐藏））/hide,show更好
require(shinyjs)
shinyApp(
  ui <- fluidPage(titlePanel(title = "我是标题",windowTitle = "我是浏览器标题"),
                  useShinyjs(), #此函数必须从UI中调用，才能使所有其他shinyjs函数正常工作。
                  sidebarPanel(actionButton(inputId = "显示",label = "显示",icon = icon("book")),
                               actionButton(inputId = "隐藏",label = "隐藏",icon = icon("file"))),
                  mainPanel(uiOutput("ui1"),
                            uiOutput("ui2"))
                  ),
  server <- function(input,output){
    output$ui1 <- renderUI(
      fluidPage(h3("Rstudio"))
    )
    output$ui2 <- renderUI(
      fluidPage(h3("Rscript"))
    )
    observeEvent(eventExpr = input$显示,handlerExpr = toggle("ui1"))
    observeEvent(eventExpr = input$隐藏,handlerExpr = toggle("ui2"))
  }
    
)

#22 翻页（toggleState:启用或禁用）(没成功)
require(shinyjs)
require(shinyjqui)
require(stringr)
require(magrittr)

shinyApp(
  ui <- fluidPage(
    useShinyjs(), #此函数必须从UI中调用，才能使所有其他shinyjs函数正常工作。
    
    uiOutput("ui1"),
    uiOutput("ui2"),
    uiOutput("ui3"),
    uiOutput("ui4"),
    uiOutput("ui5"),
    verbatimTextOutput("now_page"),
    br(),
    actionButton("pre", "前一页"),
    actionButton("post", "后一页")
  ),
  
  server <- function(input, output) {
    i <- 0
    NUM_PAGES <- 5
    
    now_page <- reactiveValues(page = 1)
    next_page <- function(direction) {
      now_page$page <- now_page$page + direction
    }
    

    
    output$ui1 <- renderUI(h3("我是第一页"))
    output$ui2 <- renderUI(h3("我是第二页"))
    output$ui3 <- renderUI(h3("我是第三页"))
    output$ui4 <- renderUI(h3("我是第四页"))
    output$ui5 <- renderUI(h3("我是第五页"))
    
    observe({  #从给定表达式创建观察者。
      toggleState(id = "pre", condition = now_page$page > 1)  #toggleState:启用或禁用
      toggleState(id = "post", condition = now_page$page < NUM_PAGES)
    })
    
    all <- c("ui1","ui2","ui3","ui4","ui5")
    yin <- c("ui2","ui3","ui4","ui5")
    sapply(yin, hide) #先隐藏2：5页
    
    observeEvent(eventExpr = input$pre,handlerExpr = {
      next_page(-1)
      i = i - 1
      yu <- str_remove(string = all,pattern = all[i]) %>% data.frame() %>% .[-i,]
      sapply(yu, hide) #yu（其余页）
      show(all[i])

    })
    observeEvent(eventExpr = input$post,handlerExpr = {
      next_page(+1)
      i = i + 1
      yu <- str_remove(string = all,pattern = all[i]) %>% data.frame() %>% .[-i,]
      sapply(yu, hide) #yu（其余页）
      show(all[i])
    })
    
  }
)

#23.pth$name/size/type/datapath
shinyApp(
  ui = fluidPage(title = "123",themeSelector(),
                 sidebarPanel(fileInput(inputId = "id_input",label = "请选择文件",accept = "txt",buttonLabel = "浏览",placeholder = "无文件选中")),
                 mainPanel(tableOutput("id_output"))
  )
  ,
  server = function(input,output){
    output$id_output <- renderTable({
      req(input$id_input)
      pth <- input$id_input
      a <- pth
    })
  }
)

#24 本地建文件夹
shinyApp(
  ui = fluidPage(
    titlePanel(title = "试建文件夹",windowTitle = "浏览器标题"),
    sidebarPanel(fileInput(inputId = "setWd",label = "请选择文件夹",buttonLabel = "浏览",placeholder = "无文件选中"),
                 textInput(inputId = "newFolder",label = "请写出新建文件夹路径",value = "/home/huhu/shiyong"),
                 actionButton(inputId = "new",label = "start")
    ),
    mainPanel(verbatimTextOutput("wd"),
              verbatimTextOutput("wd2"))
  ),
  server = function(input,output){
    output$wd <- renderPrint(
      observeEvent(input$new,
                   {setwd(input$newFolder)
                     dir.create("试用成功")})
    )
    output$wd2 <- renderPrint({
      pth_all <- input$setWd$datapath
      pth <- str_split_fixed(string = pth_all,pattern = "/0.",n = 2) %>% data.table() %>% .[,1] %>% .$V1
      observeEvent(input$new,
                   {setwd(pth)
                     dir.create("试用成功2")})
    }
    )
  }
)


                