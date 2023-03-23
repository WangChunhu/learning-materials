library(data.table)
library(magrittr)
library(stringr)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(BiocManager)

#上传大小改为3G
#options(shiny.maxRequestSize = 3*1024^3,repos = BiocInstaller::biocinstallRepos()) 
options(shiny.maxRequestSize = 3*1024^3,repos = install(site_repository = character())) 
#1.图片中文显示问题
# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
    system('locate wqy-zenhei.ttc') != 0 &&
    !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    curl::curl_download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)


if (.Platform$OS.type == "windows") {
  if (!grepl("Chinese", Sys.getlocale())) {
    warning(
      "You probably want Chinese locale on Windows for this app",
      "to render correctly. See ",
      "https://github.com/rstudio/shiny/issues/1053#issuecomment-167011937"
    )
  }
} 

#4.常量
style <- "height:800px; overflow-y: scroll;overflow-x: scroll;" #滚动条设置 column()
