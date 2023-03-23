bao <- c("shiny","openxlsx","data.table","shinydashboard","stringr","Rcrawler")
sapply(bao, require,character.on = T)
options(shiny.maxRequestSize = 30*1024^2) #上传大小改为30M

shinyApp(
  ui <-dashboardPage(title = "SpringTiger生信小工具",skin = "blue",
                     header = dashboardHeader(title = "SpringTiger生信小工具",titleWidth = 310,
                                              dropdownMenu(type = "notifications",badgeStatus = "success",icon = icon("bar-chart-o"),headerText = "查看帮助",
                                                           notificationItem(text = "shinyApp官网",
                                                                            icon = icon("file"),
                                                                            status = "danger",
                                                                            href = "https://www.shinyapps.io/"))), 
                     sidebar = dashboardSidebar(width = 310,
                                                sidebarUserPanel(name = "王春虎",
                                                                 subtitle = a(icon(name = "circle",
                                                                                   class = "text-success"))),
                                                sidebarMenu(id = "任务栏",
                                                            menuItem(text = "1.根据基因号搜索注释",
                                                                     icon = icon("file"),
                                                                     badgeLabel = "基于data.table",
                                                                     badgeColor = "green",
                                                                     tabName = "id"),
                                                            menuItem(text = "2.cottonFGD注释下载",
                                                                     icon = icon("th"),
                                                                     badgeLabel = "基于爬虫",
                                                                     badgeColor = "green",
                                                                     tabName = "rcrawler"))
                     ),
                     body = dashboardBody(tabItems(tabItem(tabName = "id",
                                                           fluidPage(titlePanel(title = "1.根据基因号搜索注释"),
                                                                     sidebarPanel(fileInput(inputId = "gene_id_input",label = "1.请选择基因id文件",accept = "xlsx",buttonLabel = "浏览",placeholder = "无文件选中..."),
                                                                                  h6("[列名必须是'id']"),
                                                                                  sliderInput(inputId = "gene_id_slide",label = "sheet_num",min = 1,max = 20,value = 1),
                                                                                  textInput(inputId = "gene_id_nrow",label = "请输入显示行数：",value = 6),
                                                                                  tags$hr(),
                                                                                  fileInput(inputId = "zhushi2_input",label = "2.请选择基因注释文件",accept = "xlsx",buttonLabel = "浏览",placeholder = "无文件选中..."),
                                                                                  h6("[列名必须是'id']"),
                                                                                  sliderInput(inputId = "zhushi2_slide",label = "sheet_num",min = 1,max = 20,value = 1),
                                                                                  h6("[由于文件过大，只显示前30行，且不支持变动]"),
                                                                                  tags$hr(),
                                                                                  downloadButton(outputId = "zhushi2_dw",label = "点击下载注释")),
                                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                                           tabPanel(title = "gene_id",tableOutput("gene_id_output")),
                                                                                           tabPanel(title = "注释",tableOutput("zhushi2_output"))
                                                                     ))
                                                           )),
                                                   tabItem(tabName = "rcrawler",
                                                           fluidPage(titlePanel(title = "2.cottonFGD注释下载"),
                                                                     sidebarPanel(fileInput(inputId = "rcrawler_input",label = "1.请选择基因id文件",accept = "xlsx",buttonLabel = "浏览",placeholder = "无文件选中..."),
                                                                                  h6("[列名必须是'id']"),
                                                                                  sliderInput(inputId = "rcrawler_slide",label = "sheet_num",min = 1,max = 20,value = 1),
                                                                                  textInput(inputId = "rcrawler_nrow",label = "请输入显示行数：",value = 6),
                                                                                  helpText("点击右边[fgd注释]开始爬虫，注释出现表示结束，即可下载到本地"),
                                                                                  tags$hr(),
                                                                                  downloadButton(outputId = "rcrawler_dw",label = "点击下载注释")),
                                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                                           tabPanel(title = "gene_fgd_id",tableOutput("rcrawler_output")),
                                                                                           tabPanel(title = "fdg注释",tableOutput("rcrawler_fgd_output"))
                                                                     ))
                                                           ))
                                                   
                     )
                     )
  ),
  
  server <- function(input,output){
    output$gene_id_output <- renderTable({
      req(input$gene_id_input)
      pth <- input$gene_id_input
      aim_id <<- read.xlsx(pth$datapath,colNames = T,sheet = input$gene_id_slide) %>% data.table()
      head(aim_id,as.numeric(input$gene_id_nrow))
    })
    
    output$zhushi2_output <- renderTable({
      req(input$zhushi2_input)
      pth <- input$zhushi2_input
      data <- read.xlsx(pth$datapath,colNames = T,sheet = input$zhushi2_slide) %>% data.table()
      data_l <<- data[aim_id,on = "id"]
      head(data_l,30)
    })
    
    output$zhushi2_dw <- downloadHandler(
      filename = function(){paste0("sel_",input$zhushi2_input)},
      content = function(file){write.xlsx(data_l,file)}
    )
    
    output$rcrawler_output <- renderTable({
      req(input$rcrawler_input)
      pth <- input$rcrawler_input
      rcrawler_id <<- read.xlsx(pth$datapath,colNames = T,sheet = input$rcrawler_slide) %>% data.table()
      head(rcrawler_id,as.numeric(input$rcrawler_nrow))
    })
    
    output$rcrawler_fgd_output <- renderTable({
      i = 1
      dan <- NULL
      dan_h <- NULL
      all <- NULL
      for (i in 1:nrow(rcrawler_id)) {
        url <- paste0("http://www.cottonfgd.org/profiles/gene/",rcrawler_id[i],"/")
        ContentScraper(Url= url,
                       CssPatterns = c("td"),
                       encod = "UTF-8",
                       ManyPerPattern = TRUE) %>% data.frame() -> dan
        dan <- data.table(dan)[1:5]
        dan_h <- t(dan) %>% data.table()
        all <- rbind(all,dan_h,use.names = F)
      }
      rcrawler <<- all
    })
    
    output$rcrawler_dw <- downloadHandler(
      filename = function(){paste0("fgd_",input$rcrawler_input)},
      content = function(file){write.xlsx(rcrawler,file)}
    )
    
  }
  
)