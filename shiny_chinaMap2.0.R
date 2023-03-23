bao <- c("shiny","shinydashboard","shinyjqui","ggplot2","data.table","stringr","dplyr" ,"maptools","magrittr","readxl","ggthemes","plyr","openxlsx")
sapply(X = bao,FUN = library,character.on = T)

data <- NULL
china_mapdata <- NULL
last <- NULL #最后的总数居
part <- NULL
全国 <- c("黑龙江省","内蒙古自治区","新疆维吾尔自治区","吉林省","辽宁省","甘肃省","河北省","北京市","山西省","天津市","陕西省","宁夏回族自治区","青海省","山东省","西藏自治区","河南省","江苏省","安徽省","四川省","湖北省","重庆市","上海市","浙江省","湖南省","江西省","云南省","贵州省","福建省","广西壮族自治区","台湾省","广东省","香港特别行政区","海南省" )
#######################################################################################
#上面的代码一般执行一次即可
#######################################################################################
shinyApp(
  ui <- dashboardPage(title = "中国地图",skin = "green",
                      
                      header = dashboardHeader(title = h4("中国地图(请最大化使用)"),titleWidth = 230,
                                               dropdownMenu(type = "notifications",badgeStatus = "success",icon = icon("table"),headerText = "查看软件帮助",
                                                            notificationItem(text = "shiny基础教程",icon = icon("book"),status = "success",href = "http://yanping.me/shiny-tutorial/#hello-shiny"),
                                                            notificationItem(text = "shiny前后端优化",icon = icon("book"),status = "success",href = "https://mp.weixin.qq.com/s/alx68b4mnjnEW1R_jYB5QA"))),
                      
                      sidebar = dashboardSidebar(width = 230,sidebarMenu(menuItem(text = "地图数据",icon = icon("bar-chart-o"),badgeLabel = "地图数据",badgeColor = "teal",tabName = "map"),
                                                                         menuItem(text = "映射数据",icon = icon("bar-chart-o"),badgeLabel = "写入地图数据",badgeColor = "green",tabName = "data"),
                                                                         menuItem(text = "整合的数据",icon = icon("bar-chart-o"),badgeLabel = "地图+映射",badgeColor = "yellow",tabName = "合"),
                                                                         menuItem(text = "纯色中国地图",icon = icon("map"),badgeLabel = "白色",badgeColor = "black",tabName = "纯"),
                                                                         menuItem(text = "填充中国地图",icon = icon("map"),badgeLabel = "彩色",badgeColor = "red",tabName = "彩"))),
                      
                      body = dashboardBody(tabItems(tabItem(tabName = "map",fluidPage(
                        box(title = "地图数据",width = 600,
                            box(fileInput(inputId = "file",label = "上传文件",placeholder = "无文件选中",buttonLabel = "浏览",accept = "xlsx")),
                            #box(textInput(inputId = "path_map",label = "请填写路径:",placeholder = "C:/学习/key/中国地图文件/china_mapdata.xlsx"),helpText(paste0("[路径斜杠会自动校正，所以请乱来!!!只支持xls和xlsx,其他格式请转换后再来，不接受反驳！！！]")),hr(),actionButton(inputId = "提交_map",label = "提交",icon = icon("search")),h5("请给5-20s时间（看心情）")),
                            box(sliderInput(inputId = "sheet_map",label = "选择sheet并点击提交",min = 1,max = 20,value = 1),hr(),h5("由于地图文件接近10万行，所以只显示前30行意思意思。"),hr(),tableOutput("map"))
                        )))
                                                    ,tabItem(tabName = "data",fluidPage(
                        box(title = "映射数据",width = 600,
                            box(fileInput(inputId = "file_ys",label = "上传文件",placeholder = "无文件选中",buttonLabel = "浏览",accept = "xlsx")),
                            # box(textInput(inputId = "path",label = "请填写路径:",placeholder = "C:/学习/key/中国地图文件/population.xlsx"),helpText(paste0("[路径斜杠会自动校正，所以请乱来!!!只支持xls和xlsx,其他格式请转换后再来，也也也不接受反驳！！！]")),hr(),actionButton(inputId = "提交",label = "提交",icon = icon("search")),checkboxInput(inputId = "all",label = "显示全部",value = F)),
                            box(sliderInput(inputId = "sheet",label = "选择sheet并点击提交",min = 1,max = 20,value = 1),hr(),tableOutput("data"))
                            ))),
                                                    tabItem(tabName = "合",fluidPage(box(title = "整合数据",width = 600,
                                                                                        box(helpText("请拖曳你想要的城市从全国到局部去,生成局部地图...（很好玩的）"),hr(),
                                                                                            orderInput(inputId = "全国",label = "全国",items = 全国,as_source = T,connect = "局部"),hr(),
                                                                                            orderInput(inputId = "局部",label = "局部",items = NULL,placeholder = "请拖到这来"),hr(),helpText("【重新选择】请按上面的刷新键")),
                                                                                        box(actionButton(inputId = "整合",label = "整合"),h5("这次显示前50行，够意思了吧。还有，数据导入一次即可，除非手贱清除环境栏（切记）"),hr(),tableOutput("合"))))),
                                                    tabItem(tabName = "纯",fluidPage(box(background = "light-blue",plotOutput("纯")),box(background = "light-blue",plotOutput("纯分"),actionButton(inputId = "纯分",label = "生图")))),
                                                    tabItem(tabName = "彩",fluidPage(box(background = "teal",plotOutput("彩")),box(background = "teal",plotOutput("彩分"),actionButton(inputId = "纯分",label = "生图"))))
                        ))),
  
  server <- function(input,output){
    #导入映射数据
    output$data <- renderTable({
      req(input$file_ys)
      pth <- input$file_ys
      data_ys <<- read.xlsx(pth$datapath,colNames = T) %>% data.table()
    })
    # output$data <- renderTable({
    #   input$提交
    #   isolate({
    #     req(input$path)
    #     path <- normalizePath(path = input$path,winslash = "/",mustWork = T)
    #     data <<- read_excel(path = path,sheet = input$sheet)
    #     ifelse(input$all == T,return(data),return(head(data)))
    #     })
    # })
    
    #导入地图数据
    output$map <- renderTable({
      req(input$file)
      pth <- input$file
      data <<- read.xlsx(pth$datapath,colNames = T) %>% data.table()
      head(data,n = 30)
    })
    # output$map <- renderTable({
    #   input$提交_map
    #   isolate({
    #     req(input$path_map)
    #     path <- normalizePath(path = input$path_map,winslash = "/",mustWork = T)
    #     china_mapdata <<- read_excel(path = path,sheet = input$sheet_map)
    #     head(china_mapdata,n = 30)
    #     })
    # })
    
    #整合数据
    output$合 <- renderTable({
      input$整合
      last <<- data_ys[data,on = "省市"] #合并地图数据和映射数据
      head(last,50)
    })
    
    #纯全地图
    output$纯 <- renderPlot({ 
      ggplot(data = last,aes(x = long,y = lat,group = group)) +
        geom_polygon(fill = "white") +
        geom_path(color = "grey40") +
        labs(x = "经度",y = "纬度",title = "中国地图") +
        theme(panel.background = NULL) +
        coord_quickmap() #coord_map("polyconic") 更加精确
    })
    
    #纯分地图
    output$纯分 <- renderPlot({
      part <- last[省市 %in% input$局部]
      input$纯分
      ggplot(data = part,aes(x = long,y = lat,group = group)) +
        geom_polygon(fill = "white") +
        geom_path(color = "grey40") + 
        labs(x = "经度",y = "纬度",title = "中国部分地区地图") +
        theme(panel.background = NULL) +
        coord_quickmap()
    })
    
    #彩分地图
    output$彩分 <- renderPlot({
      part <- last[省市 %in% input$局部]
      input$彩分
      ggplot(data = part,aes(x = long,y = lat,fill = 人口,group = group)) +
        geom_polygon() +
        geom_path(color = "grey40") + 
        # theme_tech(theme="twitter") + 
        # scale_fill_tech(theme="twitter") +
        labs(x = "经度",y = "纬度",title = "中国部分地区人口分布") +
        scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging",
                                     na.value = "grey50", guide = "colourbar") + # low="blue",mid="white",high="red"
        #scale_fill_economist()+
        theme(panel.background = NULL) +
        coord_quickmap() #投影
    })
    
    #彩全地图
    output$彩 <- renderPlot({
      ggplot(data = last,aes(x = long,y = lat,fill = 人口,group = group)) +
        geom_polygon() +
        geom_path(color = "grey40") + 
        # theme_tech(theme="twitter") + 
        # scale_fill_tech(theme="twitter") +
        labs(x = "经度",y = "纬度",title = "中国人口分布") + #,subtitle="使用推特的配色"
        scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging",
                             na.value = "grey50", guide = "colourbar") + #guide = "colourbar":连续；"legend":离散
        #scale_fill_economist()+
        theme(panel.background = NULL) +
        coord_quickmap()
    })
  }
)       


#1，加映射数据选择
#2，配色选择
#3，地图上标label