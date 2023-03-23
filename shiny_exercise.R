bao <- c("shiny","openxlsx","data.table","shinyjqui","flexdashboard","shinyjs","shinydashboard") #shinyjqui：模块移动缩放;flexdashboard：包括仪表盘;shinyjs:隐藏显示
sapply(bao, require,character.on = T)

# #进度条
# if(interactive()){
#   options(device.ask.defaul = F)
# }

#shiny
shinyApp(
  ui <- fluidPage(titlePanel(title = "我的练习shiny.APP",windowTitle = "浏览器标题"),
                  sidebarPanel(jqui_draggable(sliderInput(inputId = "slide",label = "1.观察值的数目:（我可以动）",min = 0,max = 100,value = 10,animate = T)), #滑块/模块移动
                               tags$hr(), #水平线
                               jqui_resizable(helpText("我是帮助文件，然并卵......。但是我可以缩放")),  #模块缩放
                               tags$hr(),
                               selectInput(inputId = "sel",label = "2.请选择:",choices = c("rock","pressure","cars"),selected = "cars"), #选择框
                               checkboxInput(inputId = "cb",label = "以频率做图",value = F), #勾选框
                               tags$hr(),
                               numericInput(inputId = "num",label = "3.请选择:",value = 6,min = 1,max = 12), #数字框
                               tags$hr(),
                               textInput(inputId = "text",label = "4.请输入总结框命名：",value = "1.请命名："), #文本输入框
                               tags$hr(),
                               sliderInput(inputId = "slide_a",label = "5.滑动条测试：",min = 0,max = 1,value = 0.5,step = 0.1),
                               sliderInput(inputId = "slide_b",label = "",min = 1,max = 1000,value = c(200,500),animate = T,step = 100),
                               sliderInput(inputId = "slide_c",label = "",min = 0,max = 1000,pre = "$",value = 0,step = 100,animate = animationOptions(loop = T,pauseButton = "停止")), #animate:播放按钮
                               tags$hr(),
                               radioButtons(inputId = "radio",label = "6.请选择：",choices = c("rnorm","runif","rlnorm","rexp")), #选项卡
                               #submitButton(text = "提交") #按钮（会管理全局）
                               actionButton(inputId = "button",label = "提交"), #与isolate联用
                               tags$hr(),
                               fileInput(inputId = "file_input",label = "7.上传文件：",accept = c("csv","txt","xlsx"),buttonLabel = "浏览",placeholder = "无文件选中..."), #上传文件
                               textInput(inputId = "file_type",label = "请输入文件格式",placeholder = "例如：xlsx,txt等"),
                               checkboxInput(inputId = "file_input_chk",label = "表头处理",value = F),
                               sliderInput(inputId = "file_input_slide",label = "显示行数",min = 3,max = 20,value = 6),
                               conditionalPanel(condition = "input.file_type != ''", #控制组件(用.而不是$,必须用小写true or false)
                                                downloadButton(outputId = "download",label = "点击下载") #下载文件
                               ),
                               tags$hr(),
                               dateInput(inputId = "date",label = "8.请选择日期：",value = Sys.time(),language = "zh-CN",autoclose = F), #日期
                               tags$hr(),
                               dateRangeInput(inputId = "dateR",label = "9.请选择时间段：",start = ,end = ,separator = " to ",language = "zh-CN",autoclose = F),
                               tags$hr(),
                               orderInput(inputId = "oeder",label = "10.请拖曳到location",items = c("a","b","c","d","e"),as_source = T,connect = "loca"), #order
                               orderInput(inputId = "loca",label = "location",items = NULL,placeholder = "请拖到这里来"), #使用 input$loca_order
                               tags$hr(),
                               actionButton(inputId = "show",label = "11.显示隐藏")
                               
                               ),
                               
                  mainPanel(h3(textOutput("time")), #文本输出
                            tags$hr(),
                            textOutput(outputId = "summary_name"),
                            jqui_resizable(plotOutput(outputId = "plot")), #图片输出
                            tags$hr(),
                            h4("2.我是summary"),
                            verbatimTextOutput(outputId = "verbat"), #逐字打印输出
                            tags$hr(),
                            h4("3.我是表格"),
                            tableOutput(outputId = "table"),  #表格输出
                            tags$hr(),
                            h4("5.我是滑块表格"),
                            tableOutput(outputId = "slide"),
                            tags$hr(),
                            h4("6.我是tabpanel"),
                            tabsetPanel(type = "tabs",
                                        tabPanel(title = "plot",jqui_resizable(plotOutput("plot_tab"))),
                                        tabPanel(title = "summary",verbatimTextOutput("sum_tab")),
                                        tabPanel(title = "table",tableOutput("tab_tab"))),
                            tags$hr(),
                            h4("7.上传文件"),
                            tableOutput("file_tab"),
                            tags$hr(),
                            h4("8.客户端数据"),
                            verbatimTextOutput("clientdata"), #客户端数据
                            tags$hr(),
                            # h4("9.上传做好的图"),
                            # imageOutput("imaged"), #本地做好的图
                            tags$hr(),
                            h4("10.日期选择"),
                            verbatimTextOutput("date"),
                            verbatimTextOutput("dateR"),
                            tags$hr(),
                            h4("11.ui输出"),
                            jqui_draggable(uiOutput("ui1")),
                            tags$hr(),
                            h4("12.ui输出仪表盘"),
                            jqui_draggable(uiOutput("ui2")),
                            h4("13.order"),
                            verbatimTextOutput("order"),
                            # h4("14.进度条"),
                            # plotOutput("progress")
                            # h4("14.显示隐藏"),
                            # uiOutput("ui3")
                            
                            ),
                  ),
  server <- function(input,output,session){ #session:客户端数据
    #画图
    dist <- reactive({cal <- switch(EXPR = input$radio,
                            "rnorm" = rnorm,
                            "runif" = runif,
                            "rlnorm" = rlnorm,
                            "rexp" = rexp)
                      cal(input$slide)
      })
    output$plot <- renderPlot({
      hist(x = dist(),main = "我是直方图",freq = input$cb)
    })
    require(datasets)
    dataset <- reactive(switch(EXPR = input$sel, #switch:切换选项；reactive:选项发生改变时执行切换结果,相对于isolate（需要按钮执行）
                               "rock" = rock,
                               "pressure" = pressure,
                               "cars" = cars))
    #表格
    output$table <- renderTable(head(dataset(),n = input$num))
    #逐字打印
    output$verbat <- renderPrint(expr = summary(dataset()))
    output$summary_name <- renderText(expr = input$text)
    #输出文本
    slide_num <- reactive({
      data.frame(name = c("a","b","c"),
                 value = as.character(c(input$slide_a,paste(input$slide_b,collapse = " "),input$slide_c)),stringsAsFactors = F)
    })
    output$slide <- renderTable(slide_num())
    #action不他图哦你
    output$plot_tab <- renderPlot({
      input$button
      isolate(hist(x = dist(),main = "我是直方图"))
    })
    output$sum_tab <- renderPrint({
      input$button
      isolate(dist())})
    output$tab_tab <- renderTable({
      input$button
      data.frame(dist())})
    #数据上传
    options(shiny.maxRequestSize = 30*1024^2) #上传大小改为30Ms
    output$file_tab <- renderTable({
      req(input$file_input)
      pth <- input$file_input
      if(input$file_type == "xlsx"){
        data_file <<- head(read.xlsx(pth$datapath,colNames = input$file_input_chk),n = input$file_input_slide) #超级要注意 $datapath：选中文件的路径
      }else if(input$file_type == "txt"){
        data_file <<- head(fread(pth$datapath,header = input$file_input_chk),n = input$file_input_slide)
      }else{
        data_file <<- head(read.csv(pth$datapath,header = input$file_input_chk),n = input$file_input_slide)
      }
    })
    #数据下载
    output$download <- downloadHandler(filename = function(){paste0(input$file_input)},
                                       content = function(file){
                                         if(input$file_type == "xlsx"){
                                           write.xlsx(data_file,file)
                                         }else if(input$file_type == "txt"){
                                           fwrite(data_file,file)
                                         }else{
                                           write.csv(data_file,file)
                                         }
                                       })
    #系统时间
    output$time <- renderText({
      invalidateLater(1000,session) #刷新间隔(对象将在间隔过后失效(并重新执行))
      paste0("当前时间：",Sys.time())
    })
    #客户端数据
    data_client <- session$clientData
    output$clientdata <- renderText({
      value <- lapply(names(data_client), function(name){
        paste(name,data_client[[name]],sep = "=")
      })
      paste(value,collapse = "\n")
    })
    # output$imaged <- renderImage({  ####会导致程序出bug
    #   width <- session$clientData$output_image_width
    #   height <- session$clientData$output_image_height
    #   pixelratio <- session$clientData$pixelratio #分辨率
    #   file <- input$file_input
    #   list(src = file$datapath,
    #        contentType = "image/jpg",
    #        width = width*pixelratio,
    #        height = height*pixelratio,
    #        res = 72*pixelratio,
    #        alt = "本地的图")
    # })
    #日期
    output$date <- renderPrint(input$date)
    output$dateR <- renderPrint({input$dateR})
    #ui写控件
    output$ui1 <- renderUI({  #可以写控件
      sliderInput(inputId = "b",label = "我是动态ui，而且我会动",min = 0,max = 100,value = 10,animate = animationOptions(loop = T,pauseButton = "暂停"))
    })
    #ui没必要
    output$ui2 <- renderUI(gaugeOutput("yibiao"))
    #仪表盘
    output$yibiao <- renderGauge(gauge(value = input$b,min = 0,max = 100,sectors = gaugeSectors(danger = c(0,39),warning = c(40,79),success = c(80,100))))
    #order
    output$order <- renderPrint(input$loca_order)  
    # #进度条
    # output$progress <- renderPlot({
    #   withProgress(value = 0,message = "正在执行...",detail = "可能需要三分之一苹果的时间。",
    #                {
    #                  for (i in 1:60) {
    #                    incProgress(amount = 1/60)
    #                    Sys.sleep(1)
    #                  }
    #                })
    # })
    # output$ui3 <- renderUI(gaugeOutput("show_out"))
    # output$show_out <- renderGauge(gauge(value = input$b,min = 0,max = 100,sectors = gaugeSectors(danger = c(0,39),warning = c(40,79),success = c(80,100))))
    # observeEvent(eventExpr = input$show,handlerExpr = toggle("ui3")) #显示隐藏    #observeEvent :按钮事件

  }
)

#shinydashboard
shinyApp(ui = dashboardPage(title = "我是浏览器标题",
                            skin = "green",
                            header = dashboardHeader(title = "我是shiny标题", #标题及下拉菜单
                                                     dropdownMenu(type = "notifications", #下拉菜单
                                                                  icon = icon("book"),
                                                                  headerText = "我是下拉标题",
                                                                  notificationItem(text = "百度查询",
                                                                                   icon = icon("file"),
                                                                                   status = "success",
                                                                                   href = "www.hao123.com"),
                                                                  notificationItem(text = "微生信网址",
                                                                                   icon = icon("file"),
                                                                                   status = "info",
                                                                                   href = "http://www.bioinformatics.com.cn/")
                                                                  )
                                                     ),
                            sidebar = dashboardSidebar(sidebarUserPanel(name = "王春虎",
                                                                        subtitle = a(icon(name = "circle",
                                                                                          class = "text-success"))),
                                                       # sidebarSearchForm(textId = "搜索",
                                                       #                   buttonId = "ser",
                                                       #                   label = "请搜索",
                                                       #                   icon = icon("search")),
                                                       sidebarMenu(id = "我是侧边任务栏",
                                                                   menuItem(text = "第一栏",
                                                                            icon = icon("th"),
                                                                            badgeLabel = "我是简介",
                                                                            badgeColor = "blue",
                                                                            tabName = "first"),
                                                                   menuItem(text = "第二栏",
                                                                            icon = icon("refresh"),
                                                                            badgeLabel = "我是简介",
                                                                            badgeColor = "blue",
                                                                            tabName = "second"),
                                                                   menuItem(text = "第三栏",
                                                                            icon = icon("bar-chart-o"),
                                                                            badgeLabel = "我是简介",
                                                                            badgeColor = "blue",
                                                                            tabName = "third"))
                                                       
                                                       ),
                            body = dashboardBody(tabItems(tabItem(tabName = "first",
                                                                  fluidPage(box(title = "我是大box",
                                                                                background = "maroon",
                                                                                width = 600,
                                                                                box(title ="我是滑动条",
                                                                                    height = 200,
                                                                                    sliderInput(inputId = "slide",
                                                                                                label = "观察值的数目",
                                                                                                min = 1,
                                                                                                max = 20,
                                                                                                value = 10,
                                                                                                animate = animationOptions(loop = T,
                                                                                                                           pauseButton = "停止"))
                                                                                ),
                                                                                box(title = "我是图",
                                                                                    height =  310,
                                                                                    plotOutput("plot")))
                                                                            )),
                                                          tabItem(tabName = "second",
                                                                  fluidPage(box(title = "我是大box",width = 600,background = "maroon",
                                                                                box(title = "我是选择框",
                                                                                    selectInput(inputId = "data", #选择框
                                                                                                label = "原始数据",
                                                                                                choices = c("rock","pressure","cars"))
                                                                                ),
                                                                                tableOutput("table"))
                                                                            )),
                                                          tabItem(tabName = "third",
                                                                  fluidPage(box(title = "我是大box",width = 600,background = "maroon",
                                                                                box(title = "我是数字框",
                                                                                numericInput(inputId = "num",  #数字框
                                                                                             label = "观察数",
                                                                                             min = 5,
                                                                                             max = 15,
                                                                                             value = 10)
                                                                                ),
                                                                                textOutput("text"))
                                                                            ))
                                                          )
                                                 )
                            ),
         
         server = function(input,output){
           output$plot <- renderPlot(ggplot(data.frame(x = rnorm(input$slide),
                                                       y = rnorm(input$slide)),
                                            aes(x,y))+
                                       geom_line())
           
           data <- reactive(switch(EXPR = input$data,
                                   "rock" = rock,
                                   "pressure" = pressure,
                                   "cars" = cars))
           output$table <- renderTable(head(data()))
           
           output$text <- renderText(input$num)

})


