bao <- c("shiny","openxlsx","data.table","shinydashboard","stringr","Rcrawler")
sapply(bao, require,character.on = T)
options(shiny.maxRequestSize = 100*1024^2) #上传大小改为100M

shinyApp(
  ui <-dashboardPage(title = "生信小工具(SpringTiger)",skin = "blue",
                     header = dashboardHeader(title = "生信小工具(SpringTiger)",titleWidth = 310,
                                              dropdownMenu(type = ,badgeStatus = "success",icon = icon("question-circle"),headerText = "查看帮助",
                                                           notificationItem(text = "shinyApp官网",icon = icon("globe"),status = "danger",href = "https://www.shinyapps.io/"),
                                                           notificationItem(text = "基因家族鉴定分析操作手册",icon = icon("globe"),status = "danger",href = "https://www.omicsclass.com/article/525 "),
                                                           notificationItem(text = "Ensembl地址(下载dna,cds,pep,gff3)",icon = icon("globe"),status = "danger",href = "http://plants.ensembl.org/index.html"),
                                                           notificationItem(text = "下载Pfam模型",icon = icon("globe"),status = "danger",href = "http://pfam.xfam.org/"),
                                                           notificationItem(text = "图标查看网址",icon = icon("globe"),status = "danger",href = "https://icons.getbootstrap.com/?"),
                                                           notificationItem(text = "百度翻译",icon = icon("globe"),status = "danger",href = "https://fanyi.baidu.com/?aldtype=16047#auto/zh")
                                                           )),
                     sidebar = dashboardSidebar(width = 310,
                                                sidebarUserPanel(name = "王春虎",
                                                                 subtitle = a(icon(name = "circle",class = "text-success"))),
                                                sidebarMenu(id = "任务栏",
                                                            menuItem(text = "1.根据基因号搜索注释",icon = icon("table"),badgeLabel = "基于data.table",badgeColor = "green",tabName = "id"),
                                                            menuItem(text = "2.cottonFGD注释下载",icon = icon("table"),badgeLabel = "基于爬虫",badgeColor = "green",tabName = "rcrawler"),
                                                            menuItem(text = "3.基因家族分析",icon = icon("facebook"),tabName = "family",
                                                                     menuSubItem(text = "3.1 基因家族分析流程",tabName = "fluid",icon = icon("arrow-down")),
                                                                     menuSubItem(text = "3.2 软件管理软件--conda",tabName = "conda",icon = icon("box")),
                                                                     menuSubItem(text = "3.3 家族小工具",tabName = "tools",icon = icon("tools")),
                                                                     menuSubItem(text = "3.4 一次hmmer(pham模型)",tabName = "hmmer",icon = icon("search")),
                                                                     menuSubItem(text = "3.5 提取结构域序列",tabName = "seq_domian",icon = icon("battery-half"))
                                                                     ),
                                                            menuItem(text = "4.查找差异基因",icon = icon("search"),badgeLabel = "转录组数据",badgeColor = "green",tabName = "seq_gene")
                                                            )
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
                                                                                           tabPanel(title = "gene_id",icon = icon("table"),tableOutput("gene_id_output")),
                                                                                           tabPanel(title = "注释",icon = icon("table"),tableOutput("zhushi2_output"))
                                                                                           ))
                                                                     )),
                                                   tabItem(tabName = "rcrawler",
                                                           fluidPage(titlePanel(title = "2.cottonFGD注释下载"),
                                                                     sidebarPanel(fileInput(inputId = "rcrawler_input",label = "1.请选择基因id文件",accept = "xlsx",buttonLabel = "浏览",placeholder = "无文件选中..."),
                                                                                  h6("[列名必须是'id']"),
                                                                                  sliderInput(inputId = "rcrawler_slide",label = "sheet_num",min = 1,max = 20,value = 1),
                                                                                  textInput(inputId = "rcrawler_nrow",label = "请输入显示行数：",value = 6),
                                                                                  tags$hr(),
                                                                                  downloadButton(outputId = "rcrawler_dw",label = "点击下载注释")),
                                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                                           tabPanel(title = "gene_fgd_id",icon = icon("table"),tableOutput("rcrawler_output")),
                                                                                           tabPanel(title = "fdg注释",icon = icon("table"),tableOutput("rcrawler_fgd_output"))
                                                                     ))
                                                                     )),
                                                   tabItem(tabName = "seq_gene",
                                                           fluidPage(titlePanel(title = "4.差异基因提取"),
                                                                     sidebarPanel(fileInput(inputId = "seq_gene_input",label = "1.请选择总基因文件",accept = "xlsx",buttonLabel = "浏览",placeholder = "无文件选中..."),
                                                                                  sliderInput(inputId = "seq_gene_slide",label = "sheet_num",min = 1,max = 20,value = 1),
                                                                                  h6("[由于文件过大，只显示前30行，且不支持变动]"),
                                                                                  tags$hr(),
                                                                                  downloadButton(outputId = "seq_gene_up_dw",label = "点击下载up"),
                                                                                  downloadButton(outputId = "seq_gene_down_dw",label = "点击下载down")
                                                                                  ),
                                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                                           tabPanel(title = "up",icon = icon("arrow-up"),tableOutput("seq_gene_up_output")
                                                                                                    ),
                                                                                           tabPanel(title = "down",icon = icon("arrow-down"),tableOutput("seq_gene_down_output")
                                                                                                    )
                                                                     ))
                                                           )),
                                                   tabItem(tabName = "fluid",
                                                           fluidPage(sidebarPanel(width = 20,
                                                                                  titlePanel(title = "3.1.1 基因家族分析流程"),
                                                                                  tags$hr(),
                                                                                  h4("最后再写"),
                                                                                  tags$hr(),
                                                           )
                                                           )),
                                                   tabItem(tabName = "conda",
                                                           fluidPage(sidebarPanel(width = 20,
                                                                                  titlePanel(title = "3.2.1 conda下载，安装及使用"),
                                                                                  tags$hr(),
                                                                                  h4("1.下载：wget -c https://mirrors.tuna.tsinghua.edu.cn/anaconda/miniconda/Miniconda2-latest-Linux-x86_64.sh"),
                                                                                  h4("2.安装：bash Miniconda2-latest-Linux-x86_64.sh"),
                                                                                  h4("3.source ~/.bashrc"),
                                                                                  h4("4.配置镜像："),
                                                                                  h4("conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/free"),
                                                                                  h4("conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/conda-forge"),
                                                                                  h4("conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/bioconda"),
                                                                                  h4("conda config --set show_channel_urls yes"),
                                                                                  h4("source ~/.condarc"),
                                                                                  h4("5.使用："),
                                                                                  h4("conda create -n *_home  #创建小环境"),
                                                                                  h4("conda activate *_home  / conda deactivate  #进入或退出"),
                                                                                  h4("conda env remove --name *_home  #移除小环境"),
                                                                                  h4("conda install PACKAGENAME==VERSION  #安装相应版本的软件"),
                                                                                  tags$hr(),
                                                                               )
                                                                     )),
                                                   tabItem(tabName = "tools",
                                                           fluidPage(sidebarPanel(h3(helpText("3.3.1 基因序列多行变一行工具")),
                                                                                  tags$hr(),
                                                                                  fileInput(inputId = "duo21_input",label = "3.3.1.1 请选择序列多行式文件(需要几分钟，请耐心等待)",accept = "txt",placeholder = "无文件选中",buttonLabel = "浏览"),
                                                                                  h6("[由于文件过大，只显示前30行，且不支持变动]"),
                                                                                  downloadButton(outputId = "duo21_output_dw",label = "点击下载一行序列文件"),
                                                                                  tags$hr(),
                                                                                  h3(helpText("3.3.2 文件横竖向调整工具")),
                                                                                  fileInput(inputId = "v_input",label = "3.3.2.1 竖变横",accept = "txt",placeholder = "无文件选中",buttonLabel = "浏览"),
                                                                                  h6("[由于文件过大，只显示前30行，且不支持变动]"),
                                                                                  downloadButton(outputId = "v_output_dw",label = "点击下载横向序列文件"),
                                                                                  tags$hr(),
                                                                                  fileInput(inputId = "h_input",label = "3.3.2.2 横变竖(需要几分钟，请耐心等待)",accept = "txt",placeholder = "无文件选中",buttonLabel = "浏览"),
                                                                                  h6("[由于文件过大，只显示前30行，且不支持变动]"),
                                                                                  downloadButton(outputId = "h_output_dw",label = "点击下载竖向序列文件"),
                                                                                  tags$hr(),
                                                                                  h3(helpText("3.3.3 去掉基因号列多余信息")),
                                                                                  fileInput(inputId = "gene_idduo_input",label = "3.3.3.1 整理横向pro全长序列文件基因号",accept = "txt",placeholder = "无文件选中",buttonLabel = "浏览"),
                                                                                  h6("[由于文件过大，只显示前30行，且不支持变动]"),
                                                                                  downloadButton(outputId = "gene_idduo_output_dw",label = "点击下载清洁版pro全长序列文件"),
                                                                                  ),
                                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                                           tabPanel(title = "一行序列",icon = icon("table"),tableOutput("duo21_output")),
                                                                                           tabPanel(title = "横向文件",icon = icon("table"),tableOutput("v_output")),
                                                                                           tabPanel(title = "竖向文件",icon = icon("table"),tableOutput("h_output")),
                                                                                           tabPanel(title = "清洁版pro全长横向文件",icon = icon("table"),tableOutput("gene_idduo_output"))
                                                                                           ))
                                                           )),
                                                   tabItem(tabName = "hmmer",
                                                           fluidPage(sidebarPanel(width = 20,
                                                                                  titlePanel(title = "3.4 pham模型下载及使用"),
                                                                                  tags$hr(),
                                                                                  h3("3.4.1 下载"),
                                                                                  h4("进入：http://pfam.xfam.org/"),
                                                                                  h4("1.KEYWORD SEARCH  #输入基因家族名称"),
                                                                                  h4("2.ACCESSION"),
                                                                                  h4("3.c.curation & model"),
                                                                                  h4("d.download"),
                                                                                  tags$hr(),
                                                                                  h3("3.4.2 使用（linux + conda）"),
                                                                                  h4("1.conda create -n hmmer_home / conda install hmmer"),
                                                                                  h4("2.例子：hmmsearch --domtblout ./Gh/GH_GRAS_hmm_out.txt --cut_tc MEF2_binding.hmm ../../Gossypium_hirsutum_v1.1.pep.txt"),
                                                                                  h4("3.得到*.hmm_out.txt文件"),
                                                                                  h4("4.'of'指有几个结构域;'#'指在第几个结构域;'E'值越小,可靠性越大(筛选);第1个'from to'指pfam模型的序列段，第2个'from to'指匹配数据库到的序列段")
                                                                               )
                                                                     )),
                                                   tabItem(tabName = "seq_domian",
                                                           fluidPage(sidebarPanel(h3(helpText("3.5 结构域序列提取(蛋白质序列)")),
                                                                                  tags$hr(),
                                                                                  h5("3.3.3.1 由pham模型得到基因家族基因"),
                                                                                  fileInput(inputId = "domain_sub_input",label = "请选择*.hmm_out.txt文件",accept = "xlsx",placeholder = "无文件选中",buttonLabel = "浏览"),
                                                                                  downloadButton(outputId = "domain_from_to_dw",label = "点击下载结构域起止位点文件"),
                                                                                  tags$hr(),
                                                                                  h5("3.3.3.2 获得家族基因蛋白质结构域序列"),
                                                                                  fileInput(inputId = "pro_all_input",label = "请选择横向蛋白质全长文件",accept = "xlsx",placeholder = "无文件选中",buttonLabel = "浏览"),
                                                                                  downloadButton(outputId = "pro_all_dw",label = "点击下载家族全长序列文件"),
                                                                                  h4(),
                                                                                  downloadButton(outputId = "pro_domain_dw",label = "点击下载家族结构域序列文件"),
                                                                               ),
                                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                                           tabPanel(title = "结构域基因id + from to",icon = icon("table"),tableOutput("domain_from_to_output")),
                                                                                           tabPanel(title = "蛋白质全长序列",icon = icon("table"),tableOutput("pro_all_output")),
                                                                                           tabPanel(title = "蛋白质结构域序列",icon = icon("table"),tableOutput("pro_domain_output"))
                                                                                           ))
                                                                     ))
                                                   
                     )
                     )
  ),
  server <- function(input,output){
    ################################################################################
    #server主程序
    ################################################################################
    #1.根据基因号搜索注释
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
    #2.cottonFGD注释下载
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
      rcrawler <<- all %>% setnames(c("V1","V2","V3","V4","V5"),c("id","gene_name","注释","Swiss-Prot","NAU-trembl"))
    })
    
    output$rcrawler_dw <- downloadHandler(
      filename = function(){paste0("fgd_",input$rcrawler_input)},
      content = function(file){write.xlsx(rcrawler,file)}
    )
    
    #4.查找差异基因
    output$seq_gene_up_output <- renderTable({
      req(input$seq_gene_input)
      pth <- input$seq_gene_input
      data_seq_gene <<- read.xlsx(pth$datapath,colNames = T,sheet = input$seq_gene_slide) %>% data.table()
      up <<- data_seq_gene[as.numeric(pval) < 0.05 & as.numeric(log2FoldChange) > 1]
      head(up,30)
    })
    
    output$seq_gene_down_output <- renderTable({
      down <<- data_seq_gene[as.numeric(pval) < 0.05 & as.numeric(log2FoldChange) < -1]
      head(down,30)
    })
    
    output$seq_gene_up_dw <- downloadHandler(
      filename = function(){paste0("up_",input$seq_gene_input)},
      content = function(file){write.xlsx(up,file)}
    )
    
    output$seq_gene_down_dw <- downloadHandler(
      filename = function(){paste0("down_",input$seq_gene_input)},
      content = function(file){write.xlsx(down,file)}
    )
    
    #5.序列多行转一行
    output$duo21_output <- renderTable({
      req(input$duo21_input)
      pth <- input$duo21_input
      data_duo21 <<- fread(pth$datapath,header = F) %>% duo2v()
      head(data_duo21,30)
    })
    
    output$duo21_output_dw <- downloadHandler(
      filename = function(){paste0("一行_",input$duo21_input)},
      content = function(file){fwrite(data_duo21,file,col.names = F)}
    )
    #6.v2h
    output$v_output <- renderTable({
      req(input$v_input)
      pth <- input$v_input
      data_v <<- fread(pth$datapath,header = F) %>% v2h()
      head(data_v,30)
    })
    
    output$v_output_dw <- downloadHandler(
      filename = function(){paste0("h_",input$v_input)},
      content = function(file){fwrite(data_v,file,col.names = F)}
    )
    
    #7.h2v
    output$h_output <- renderTable({
      req(input$h_input)
      pth <- input$h_input
      data_h <<- fread(pth$datapath,header = F) %>% h2v()
      head(data_h,30)
    })
    
    output$h_output_dw <- downloadHandler(
      filename = function(){paste0("v_",input$h_input)},
      content = function(file){fwrite(data_h,file,col.names = F)}
    )
    
    #8.家族基因筛选
    output$domain_from_to_output <- renderTable({
      req(input$domain_sub_input)
      pth <<- input$domain_sub_input
      hmmer_out <- fread(pth$datapath,fill = T) %>% .[!1:3] %>% setnames(c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23"),c("target_name","accession","tlen","query_name","accession","qlen","E_value","score","bias","yu","of","c_Evalue","i_Evalue","score","bias","from1","to1","from2","to2","from3","to3","acc","description_of_target"))
      #筛选GRAS的E_value < 0.001 & length >= 40
      hmmer_out_sel_1 <- hmmer_out[as.numeric(E_value) < 0.001]  %>% .[yu == 1]  %>% .[,c(1,10,18,19)] %>% .[,target_name := paste0(">",target_name)] %>% .[,length := as.numeric(to2) - as.numeric(from2)] %>% .[length >=40]
      hmmer_out_sel_2 <- hmmer_out[as.numeric(E_value) < 0.001]  %>% .[yu == 2]  %>% .[,c(1,10,18,19)] %>% .[,target_name := paste0(">",target_name)] %>% .[,length := as.numeric(to2) - as.numeric(from2)] %>% .[length >=40]
      hmmer_out_sel <<- rbind(hmmer_out_sel_1,hmmer_out_sel_2) %>% setorder("target_name")
    })
    
    output$domain_from_to_dw <- downloadHandler(
      filename = function(){paste0("from_to_",input$domain_sub_input)},
      content = function(file){fwrite(hmmer_out_sel,file)}
    )
    #9.提取pro全长序列
    output$pro_all_output <- renderTable({
      req(input$pro_all_input)
      pth <- input$pro_all_input
      pro <- fread(pth$datapath,header = F)
      pro_seq_all <<- sel_first_translate_pro(hmm = hmmer_out_sel,pro = pro)
      pro_all <<- pro_seq_all[[1]]
    })
    
    #10.提取结构域序列
    output$pro_domain_output <- renderTable({
      pro_domain <<- pro_seq_all[[2]]
    })

    #11.下载pro全长序列
    output$pro_all_dw <- downloadHandler(
      filename = function(){paste0("h_pro_all_",input$pro_all_input)},
      content = function(file){fwrite(pro_all,file)}
    )
    #11.下载结构域序列
    output$pro_domain_dw <- downloadHandler(
      filename = function(){paste0("h_pro_domain_",input$pro_all_input)},
      content = function(file){fwrite(pro_domain,file)}
    )
    #13.去掉基因号列多余信息
    output$gene_idduo_output <- renderTable({
      req(input$gene_idduo_input)
      pth <- input$gene_idduo_input
      data_yuan <- fread(pth$datapath,header = F)
      gene_idduo_data <<- cbind(spl_sel_num(data_yuan[,1]," ",1),data_yuan) %>% .[,!2]
      head(gene_idduo_data,30)
    })
    output$gene_idduo_output_dw <- downloadHandler(
      filename = function(){paste0("clear_gene_id_",input$gene_idduo_input)},
      content = function(file){fwrite(gene_idduo_data,file)}
    )


  }

)




  


















