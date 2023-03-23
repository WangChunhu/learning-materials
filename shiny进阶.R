#install.packages("shiny")
require(shiny)
shinyApp(
  ui <- fluidPage(
    titlePanel("My Shiny App"),
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1(strong(em("First level title",style = "color:blue;text-align:center;border:1px solid #000"))),
        h2("Second level title"),
        h3("Third level title"),
        h4("Fourth level title"),
        h5("Fifth level title"),
        h6("Sixth level title"),
        p("p creates a paragraph of text."),
        p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-size:20px;color:blue;text-align:right;border:2px solid green;text-decoration:underline;background-color:red"),
        strong("strong() makes bold text."),
        em("em() creates italicized (i.e, emphasized) text."),
        br(),
        code("require(ggplot)"),
        div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
        code("require(shiny)"),
        br(),
        code("shiny(ui,server)"),
        br(),
        a("https://www.jianshu.com/p/00ae46257244"),
        br(),
        pre("require(shiny)\nshinyApp(ui,server)\n\t主代码"),
        br(),
        img(src = "I:\\棉花\\活动经费\\微信图片_20210629153507.jpg",width="100%"),
        p("span does the same thing as div, but it works with",
          span("groups of words", style = "color:blue"),
          "that appear inside a paragraph."),
        div(class='divclass',
            h1('这是主标题'),
            h2('这是副标题'),
            p('这是内容段落'),
            tags$li('列表项目1'),
            tags$li('列表项目2'),
            tags$script(src='幻灯片播放脚本.js')
        ),
        hr(),
        tags$head(
          tags$title('页面标题'),
          tags$style(
            rel='stylesheet',
            'h1 {color: red; text-align:center;}',
            '.divclass {width: 600px; float:center; margin: 0 auto;}'
          )
        )
      )
    )
  ),
  
  server <- function(input,output){
    
  }
)



#style
style = "font-family: 'times'; font-size:20px;color:blue;text-align:center;border:1px solid #0000FF;text-decoration:underline;background-color:red"
font-family: 'times' #字体
font-size:20px #字体大小
color:blue #字体颜色
text-align:center #段落格式,right,justify(两端对齐)
border:1px solid #0000FF #边框:width style color,可以直接用颜色名green
text-decoration: #划线
  none : 　无装饰
  blink : 　闪烁
  underline : 　下划线
  line-through : 　贯穿线
  overline : 　上划线
background-color:red #背景色
overflow-x: scroll #滚动条
text-indent:2em #段首空两格
#格式
# p 	    <p>	      A paragraph of text
# h1	    <h1>	    A first level header
# h2    	<h2>	    A second level header
# h3    	<h3>	    A third level header
# h4	    <h4>    	A fourth level header
# h5	    <h5>    	A fifth level header
# h6    	<h6>    	A sixth level header
# a	      <a>	      A hyper link #超链接，网址
# br	    <br>	    A line break (e.g. a blank line) #换行
# hr      <hr>      水平线
# div	    <div>	    A division of text with a uniform style
# span  	<span>  	An in-line division of text with a uniform style
# pre	    <pre>	    Text ‘as is’ in a fixed width font #文本放在一个框中
# code  	<code>  	A formatted block of code
# img	    <img>	    An image #图片记得放在shiny文件同一目录的www文件夹中
# strong/b	<strong>	Bold text #加粗
# em    	<em>	    Italicized text #斜体
# HTML	 	          Directly passes a character string as HTML code

#ui模板
ui <- dashboardPage(title = "调fa序列(SpringTiger)",skin = "blue",
                    header = dashboardHeader(title = shinyDashboardLogo(theme = "blue_gradient", #badgeText的主题
                                                                        boldText = "调fa序列",
                                                                        mainText = "SpringTiger",
                                                                        badgeText = "v1.1"),
                                             titleWidth = 280),
                    sidebar = dashboardSidebar(width = 310,
                                               sidebarUserPanel(name = "王春虎",
                                                                subtitle = a(icon(name = "circle",class = "text-success"))),
                                               sidebarMenu(id = "侧边栏",
                                                           menuItem(text = "输入基因id调用序列",
                                                                    icon = icon("facebook"),
                                                                    tabName = "id2seq")
                                                           # menuSubItem(text = ,
                                                           #             tabName = ,
                                                           #             icon = icon("table")))
                                               )
                    ),
                    body = dashboardBody(shinyDashboardThemes(theme = "onenote"),
                                         tabItems(tabItem(tabName = "id2seq",
                                                          fluidPage(titlePanel(title = "输入基因id调用序列"),
                                                                    sidebarPanel(h3(helpText("1.3.1 基因序列多行变一行工具")),),
                                                                    #mainPanel(tabsetPanel(type = "tabs"))
                                                          )
                                         )
                                         
                                         )
                    ))

#页面布局
shinyApp(
  ui <- fluidPage(
    fluidRow(column(2),column(8,titlePanel(title = strong(h2("增强火山图，要不要试一下？")))),column(2)),
    fluidRow(column(2),column(8,h4("作者：王春虎\t|\t时间：2021-7-20")),column(2)),
    fluidRow(column(2),column(8,strong(h2("前言",style = "background-color:lightblue"))),column(2)),
    fluidRow(column(2),column(8,p(h4("这是页面布局练习的介绍，请多多指教！最近道听途",span("说EnhancedVolcano",style = "background-color:lightgrey"),"绘制火山图的方便性，所以本人就根据其说明文档进行操作。但在操作过程中发现，其",span("shape",style = "background-color:lightgrey"),"功能并没有在",span("help",style = "background-color:lightgrey"),"文档中找到，经过搜索在",span("github",style = "background-color:lightgrey"),"上看到了以下的答复 。。。（说明整个文档功能并没有完全开发，需进行选择）",style = "text-indent:2em;text-align:justify"))),column(2)),
    fluidRow(column(2),column(8,strong(h2("1 Introduction",style = "background-color:lightblue"))),column(2)),
    fluidRow(column(2),column(8,p(h4(span("火山图",style = "color:blue"),"是可视化差异表达分析结果的有效方法。这次更新的",span("EnhancedVolcano",style = "color:red"),"目的就是两个（1）使转录本基因名称的显示更加的合理化，避免出现",span(strong("相互重叠"),style = "color:red"),"的现象；（2）允许用户通过颜色，形状和阴影参数配置在同一绘图空间中识",span(strong("别多达3种"),style = "color:red"),"不同类型的属性。",style = "text-indent:2em;text-align:justify"))),column(2)),
    fluidRow(column(2),column(8,strong(h2("2 Installation",style = "background-color:lightblue"))),column(2)),
    fluidRow(column(2),column(8,strong(h3("2.1 1. 下载安装包",style = "background-color:lightblue"))),column(2)),
    fluidRow(column(2),column(8,pre("# if (!requireNamespace('BiocManager', quietly = TRUE))\n#\tinstall.packages('BiocManager')\n# BiocManager::install('EnhancedVolcano')\nif (!requireNamespace('devtools', quietly = TRUE))\n\tinstall.packages('devtools')\ndevtools::install_github('kevinblighe/EnhancedVolcano')")),column(2)),
    fluidRow(column(2),column(8,strong(h3("2.2 2. 加载R包",style = "background-color:lightblue"))),column(2)),
    fluidRow(column(2),column(8,pre("library(EnhancedVolcano)")),column(2)),
    fluidRow(column(2),column(8,strong(h2("3 开始",style = "background-color:lightblue"))),column(2)),
    fluidRow(column(2),column(8,
                              p(h4("作者使用该流程： RNA-seq workflow: gene-level exploratory analysis and differential expression。具体来说，我们将加载",span("airway",style = "background-color:lightgrey"),"数据，其中不同的气道平滑肌细胞用地塞米松治疗。",style = "text-indent:2em;text-align:justify")),
                              pre("data('airway')\n# %<>%复合赋值操作符， 功能与 %>% 基本是一样的，但多了一项额外的操作，就是把结果写到左侧对象。\n# 对dex列进行relevel，再把revel后的结果赋值到airway$dex。\nairway$dex %<>% relevel('untrt')"),
                              p(h4("使用DESeq2进行差异表达，以创建两组结果(",span("DESeq2差异基因分析和批次效应移除",style = "color:blue"),")：",style = "text-indent:2em;text-align:justify")),
                              pre("library('DESeq2')\n\ndds <- DESeqDataSet(airway, design = ~ cell + dex)\ndds <- DESeq(dds, betaPrior=FALSE)\n# compare trt & untrt\n\tres1 <- results(dds,\n\tcontrast = c('dex','trt','untrt'))\n# shrink log2 fold change\nres1 <- lfcShrink(dds,\n\tcontrast = c('dex','trt','untrt'), res=res1)\n# compare different cells\nres2 <- results(dds,\n\tcontrast = c('cell', 'N061011', 'N61311'))\nres2 <- lfcShrink(dds,\n\tcontrast = c('cell', 'N061011', 'N61311'), res=res2)"),
                              img(src = "shiny.png",width="100%"),
                              br(),
                              a(href = "https://www.jianshu.com/p/00ae46257244","shiny网页教程")
                              ),column(2)),
    fluidRow(column(2),column(8,strong(h2("4 控件演示",style = "background-color:lightblue"))),column(2)),
    fluidRow(column(2),
             column(3,sliderInput(inputId = "bins",label = "请滑动选择一个数字:",min = 1,max = 20,value = 10),
                    numericInput(inputId = "num",label = "这个数字框没用:",value = 50,min = 0,max = 100),
                    textInput(inputId = "text",label = h4("这个文本框没用:",style = "color:red"),placeholder = "请输入主标题"),
                    selectInput(inputId = "sel",label = h4("这个选择框没用:"),choices = c("王","春","虎"),selected = "王",multiple = T),
                    style = "color:blue;border:1px solid #0000FF"
                      ),
             column(4,mainPanel(plotOutput("distPlot"),width = "100%")),
             column(1,br(),br(),br(),br(),br(),br(),br(),downloadButton(outputId = "下载",label = "下载图片",style = "color:blue;background-color:lightgreen;border:1px solid #0000FF")),
             column(2)),
    
  ),
  server <- function(input,output){
    output$distPlot <- renderPlot({
      x    <- faithful$waiting
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "#75AADB", border = "white",
           xlab = "Waiting time to next eruption (in mins)",
           main = "Histogram of waiting times")
    })
  }
)
