#install.packages('rvest')
require(rvest)
require(magrittr)
require(openxlsx)
require(data.table)

#江苏省农科院资讯
url <- 'http://www.jaas.ac.cn/'
web <- url %>% read_html() 
web %>% html_nodes(css = ".newName") %>% html_text() %>% data.frame() -> list_name
web %>% html_nodes(css = ".newDate") %>% html_text() %>% data.frame() -> list_date

result <- cbind(list_name,list_date)
write.xlsx(x = result,file = "C:/学习/找工作/江苏省农科院/江苏省农科院资讯.xlsx")



#爬取表格数据
url = 'http://sn.cma.gov.cn/xwmk/tqkb/202007/t20200727_1913241.html'
url %>% read_html() %>% html_table() %>%  data.frame() -> ta


#豆瓣电影 Top 250
i = 1
num <- seq(0,250,25)
movie <- NULL
name <- NULL
jieshao <- NULL
tip <- NULL
for (i in 1:10) {
  web <- paste0("https://movie.douban.com/top250?start=",num[i],"&filter=")
  web <- url %>% read_html() 
  web %>% html_nodes(css = "tle:nth-child(1)") %>% html_text() %>% data.frame() -> movie_name
  web %>% html_nodes(css = "p:nth-child(1)") %>% html_text() %>% data.table() %>% .[2:26]-> movie_jieshao
  web %>% html_nodes(css = ".inq") %>% html_text() %>% data.frame() -> movie_tip
  name <- rbind(name,movie_name)
  jieshao <- rbind(jieshao,movie_jieshao)
  tip <- rbind(tip,movie_tip)
  movie <- cbind(name,jieshao,tip)
}
write.xlsx(x = movie,file = "C:/娱乐/豆瓣电影Top250.xlsx")



#install.packages("Rcrawler")

require(Rcrawler)
Getencoding(url)
#优酷
url <-"https://movie.youku.com/?spm=a2hcb.12675304.m_6913.d_3"
Rcrawler(Website = url,
         no_cores = 10,
         MaxDepth = 0,
         RequestsDelay = 1,
         ExtractCSSPat = "h3",
         Encod = "UTF-8",
         ManyPerPattern = T,
         DIR = "C:/学习/R/爬虫")


# #qq邮箱登录
# setwd("C:/soft-anzhuang/phantomjs211/phantomjs-2.1.1-windows/bin")
# 
# 
# br <-run_browser()
# 
# LS<-LoginSession(Browser = br,
#                  LoginURL = "https://www.zhihu.com/signin?next=%2F",
#                  LoginCredentials = c('oqbfpherq_rm','an98_krrms68'),
#                  cssLoginCredentials =c('#uinArea', '#pwdArea'),
#                  cssLoginButton='.password-login')
# 
# 
#  stop_browser(browser = br)


#科学网1月内精选博文
 i = 1
 dan <- NULL
 all <- NULL
 for (i in 1:8) {
   url <- paste0("http://blog.sciencenet.cn/blog.php?mod=recommend&op=3&ord=0&page=",i)
   ContentScraper(Url= url,
                  CssPatterns = c("td:nth-child(2) a","td~ td+ td a","tr+ tr td:nth-child(4)","tr+ tr td:nth-child(5)","tr+ tr td:nth-child(6)"), 
                  encod = "GBK",
                  ManyPerPattern = TRUE) %>% data.frame() -> dan
   names(dan) <- c("博文","博主","点击量","评论/条","日期")
   all <- rbind(all,dan)
 }
 
 
 
#动漫
i = 1
dan <- NULL
all <- NULL

for (i in 1:115) {
  url <- paste0("http://www.acglala.net/sort?area=0&year=0&zhonglei=0&status=2&jqlx=0&orderby=1&totalresult=2749&pageno=",i)
  ContentScraper(Url= url,
                 CssPatterns = c(".li_adj5 a",".dhlb_xq li:nth-child(4)"), 
                 encod = "UTF-8",
                 ManyPerPattern = TRUE) %>% data.frame() -> dan
  names(dan) <- c("动漫名","简介")
  all <- rbind(all,dan)
  print(paste0("第",i,"页已爬取完成,共115页,","还剩",115-i,"页"))
}



#知乎图片及文字(右键图片--检查（成功）)

require(downloader)
require(rvest)
require(magrittr)

url <- 'https://www.zhihu.com/question/19647535'

##图片
link <- read_html(url) %>% html_nodes("div.RichContent-inner img") %>% html_attr("src")
link_clear <- grep(pattern = "https",x = link,value = T)

i = 1
for (i in 1:length(link_clear)) {
  download(link_clear[i],paste0("C:/学习/R/爬虫/图片/",i,".jpg"),mode = "wb")
  print(paste0("第",i,"张图片已爬完，","剩余",length(link_clear)-i,"张"))
}

##文字
wenzi <- read_html(url) %>% html_nodes(".CopyrightRichText-richText")  %>% html_text()

##或
ContentScraper(Url= url,
               CssPatterns = c(".CopyrightRichText-richText"), 
               encod = "UTF-8",
               ManyPerPattern = TRUE)

#动态网页(百度图片（成功）)
#install.packages("jsonlite")
require(jsonlite)

num <- seq(0,120,30)

i = 1
for (i in 1:5) {
  url <- paste0("https://image.baidu.com/search/acjson?tn=resultjson_com&ipn=rj&ct=201326592&is=0%2C0&fp=detail&cl=2&lm=-1&ie=utf-8&oe=utf-8&adpicid=0&lpn=0&st=-1&word=%E7%81%AB%E5%BD%B1&z=0&ic=0&hd=undefined&latest=undefined&copyright=undefined&s=undefined&se=&tab=0&width=&height=&face=undefined&istype=2&qc=&nc=&fr=&simics=&srctype=&bdtype=0&rpstart=0&rpnum=0&cs=290943762%2C467711970&catename=&force=undefined&cardserver=&tabname=&pn=",num[i])
  j = 1
  for (j in 1:30) {
    all_image_link <- fromJSON(url)
    all_image_link <- all_image_link[["data"]][["thumbURL"]]
    download(all_image_link[j],paste0("C:/学习/R/爬虫/图片/",(i-1)*30 + j,".jpg"),mode = "wb")
    print(paste0("已完成",((i-1)*30 + j),"/150"))
    Sys.sleep(2)
  }
}


#rvest_网页搜索(长大教务处)（成功）

require(Rcrawler)
Getencoding(url)

url <- "http://jwc.yangtzeu.edu.cn/"

session <- html_session(url)

form <- html_form(session)[[1]] # forms 中的第一个列表是我们的目标列表
form
filled_form <- set_values(form, showkeycode = "教学动态")
session2 <- submit_form(session, filled_form)
session$url # 查看初始 session 的 url
session2$url # 查看提交表单后，返回的新会话 session2 的 url

##图片下载地址
desired_page <- session2 %>% read_html 
img_link <- desired_page %>% html_nodes("table img") %>% html_attr("src") 
##图片名称
img_name <- desired_page %>% html_nodes("div.pl2 > a") %>% html_text(trim=TRUE)
##表格
session2 %>% read_html() %>% html_table()
##教学动态
i = 1
dan <- NULL
all <- NULL

for (i in 1:100) {
  url <- paste0(session2$url,"&searchScope=0&currentnum=",i,"&newskeycode2=5pWZ5a2m5Yqo5oCB")
  ContentScraper(Url= url,
                 CssPatterns = c(".title",".acWHSet , h3"), 
                 encod = "UTF-8",
                 ManyPerPattern = TRUE) %>% data.frame() -> dan
  names(dan) <- c("教学动态","简介")
  all <- rbind(all,dan)
  print(paste0("第",i,"页已爬取完成,共100页,","还剩",100-i,"页"))
}


#动态网页
require(httr)

cookie <-  "EDUWEBDEVICE=f66cc74e61984e8ab183c9230707915d; hb_MA-BFF5-63705950A31C_source=www.sogou.com; __utmz=129633230.1595818315.1.1.utmcsr=sogou.com|utmccn=(referral)|utmcmd=referral|utmcct=/link; EDU-YKT-MODULE_GLOBAL_PRIVACY_DIALOG=true; NTES_YD_PASSPORT=Lvw_JHcbT4z0ZhGdK9DgdZ_b383ED0AWmC4M68vUeICOd3GbdnHABwGK9nERpfoxtY76Tj7566GfRmbMW7mdKttlD_YtmPcz8XoHdeZlkBjmpBD5b0nscIDzT6tOJc1is5LWaa607RSyCwrGlcsv5SgYT3VvCWCjpQuT3YR13AO9DoEbwPFWf3ji5LyxtvtCgLFvW.VNJY6Iio1D2V1eUnxjK; P_INFO=17792914570|1595818501|0|study|00&99|null&null&null#CN&null#10#0#0|&0|null|17792914570; 1441987298=1441987298; NTESSTUDYSI=d54c2905437f4f69a9c30e89d423a15c"

headers <- c("accept" = "application/json",
             "content-type" = "application/json;charset=UTF-8",
             "user-agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.25 Safari/537.36 Core/1.70.3741.400 QQBrowser/10.5.3863.400",
             "referer" = "https://study.163.com/category/480000003131009?mid=2081378",
             "edu-script-token" = "d54c2905437f4f69a9c30e89d423a15c",
             "cookie" = cookie
             )

payload <- list("pageIndex" = 1,
                "pageSize" = 50,
                "relativeOffset" = 0,
                "frontCategoryId" = "480000003120007")

url <- "https://study.163.com/p/search/studycourse.json"  #Request Header里的 origin+path

result <- POST(url = url,body = payload,encode = "json",add_headers(.headers = headers))


#百度学术(成功)
i = 1
dan <- NULL
all <- NULL
for (i in 0:15) {
  url <- paste0("https://xueshu.baidu.com/s?wd=%E6%B0%B4%E7%A8%BB&rsv_bp=",i)
  ContentScraper(Url= url,
                 CssPatterns = c(".c_font a",".c_abstract"), 
                 encod = "UTF-8",
                 ManyPerPattern = TRUE) %>% data.frame() -> dan
  names(dan) <- c("论文名","介绍")
  all <- rbind(all,dan)
  print(paste0("已完成",i+1,"/16"))
}
write.xlsx(x = all,file = "C:/学习/r/爬虫/百度学术.xlsx")


#淘宝（成功）
##文字
i = 1
dan <- NULL
all <- NULL
for (i in 1:100) {
  url <- paste0("https://re.taobao.com/search_ou?spm=a231k.13731936.21333857.202.6f652e63vKsW0K&prepvid=300_11.1.226.95_160547_1596443884044&extra=&keyword=%E5%BC%B9%E7%B0%A7%E5%BA%8A%E5%9E%AB&catid=122910003&refpid=mm_15891853_2192459_8654707&_input_charset=utf8&clk1=2f343d40eee709c12269a24ab19cdb25&page=",i)
  ContentScraper(Url= url,
                 CssPatterns = c("#J_waterfallWrapper .title","#J_waterfallWrapper strong",".payNum"), 
                 encod = "UTF-8",
                 ManyPerPattern = TRUE)  %>% data.frame() -> dan
  
  names(dan) <- c("标题","价格","付款数")
  all <- rbind(all,dan)
  print(paste0("已完成",i,"/100页"))
  Sys.sleep(1)
}

write.xlsx(x = all,file = "C:/学习/r/爬虫/淘宝.xlsx")

##图片
url <- "https://re.taobao.com/search?spm=a230r.1.1989828.58.76a5442blrv40m&keyword=%C4%D0%D7%B0&frontcatid=&isinner=1&refpid=420435_1006"
link <- read_html(url) %>% html_nodes("div.searchResult-items") %>% html_nodes("img") %>% html_attr("data-ks-lazyload")
link_clear <- grep(pattern = "https",x = link,value = T)

i = 1
for (i in 1:length(link_clear)) {
  download(link_clear[i],paste0("C:/学习/R/爬虫/图片/",i,".jpg"),mode = "wb")
  print(paste0("第",i,"张图片已爬完，","剩余",length(link_clear)-i,"张"))
}



#京东联想笔记本(成功)
require(Rcrawler)
require(magrittr)
require(openxlsx)

i = 1 
dan <- NULL 
all <- NULL
for (i in 1:47) {
  url <- paste0("https://search.jd.com/Search?keyword=%E5%AE%8F%E7%A2%81%E7%AC%94%E8%AE%B0%E6%9C%AC%E7%94%B5%E8%84%91&qrst=1&wq=%E5%AE%8F%E7%A2%81%E7%AC%94%E8%AE%B0%E6%9C%AC%E7%94%B5%E8%84%91&ev=exbrand_%E5%AE%8F%E7%A2%81%EF%BC%88acer%EF%BC%89%5E&page=",i*2-1)
  ContentScraper(Url = url,
                 CssPatterns = c(".p-name-type-2 em","#J_goodsList .p-price"),
                 ManyPerPattern = T,
                 encod = "utf-8") %>% data.frame() -> dan
  names(dan) <- c("介绍","价格")
  all <- rbind(all,dan)
  print(paste0("已完成",i,"/47"))
  Sys.sleep(1)
}
write.xlsx(x = all,file = "C:/学习/r/爬虫/京东_宏碁.xlsx")