#爬取文本数据
library(rvest)
library(openxlsx)
#中国研究生招生信息网
hu <- "http://news.baidu.com/"   #中国研究生招生信息网
a <- url(hu,"rb")
hu1 <- read_html(a) %>% html_nodes('//*[contains(concat( " ", @class, " " ), concat( " ", "bold-item", " " ))]//a') %>% html_text()
hu2 <- read_html(hu) %>% html_nodes("div.content-l li a") %>% html_text() #文本
hu3 <- read_html(hu) %>% html_nodes(".nLink") %>% html_attrs()#链接

for (i in 1:length(hu3))    #循环提取第二项
{
  hu4 <- hu3[[i]][2]
}

site <- rep("http://yz.chsi.com.cn",40)   #缺少的前缀
website <- paste(site,hu4,sep="")  #需指定sep="",否则会默认两字符串间有一个空格
hu5 <- data.frame(x=hu1,y=hu2,z=website)
names(hu5) <- c("时间","标题","链接")
 write.xlsx(x = hu5,file = "H:/R学习/练习/中国研究生招生信息网.xlsx",sheetName="中国研究生招生信息网",append=TRUE)

#爬取表格数据
require(XML) 
  hu6 <- "http://yjsc.zzuli.edu.cn/2019/0702/c2878a200815/page.htm"            #encoding = 'gbk'
  witch <- readHTMLTable(hu6)
  table <- readHTMLTable(hu6,which = 7) #分析网页中总的表格/[[num]]代表哪一个
  write.xlsx(x = hu7,file ="H:/R学习/练习/表格.xlsx",append=TRUE )

require(RCurl)  
require(XML)
  myheader<-c(
    
    "User-Agent"="Mozilla/5.0 (Linux; U; Android 1.0; en-us; dream) AppleWebKit/525.10 (KHTML, like Gecko) Version/3.0.4 Mobile Safari/523.12.2 ",
    
    "Accept-Language"="en-us",
    
    "Connection"="keep-alive",
    
    "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")#伪装header，防止不能爬取  
  temp<-getURL("http://news.baidu.com/",httpheader=myheader,encoding="UTF-8")#获取链接  #index_1.html   页码
  #temp <- iconv(temp,"GB2312","UTF-8" )  #转码
  k=htmlParse(temp,encoding="UTF-8")#解析
  youhui=sapply(getNodeSet(k,'//*[(@id = "body")] | //*[(@id = "headline-tabs")]//*[contains(concat( " ", @class, " " ), concat( " ", "clearfix", " " ))]'),xmlValue)
  #其中sapply函数则是为了将xmlvalue类型的数据集合转化为R语言中常见的向量形式

 # 加强版 
  UserAgent=c(
    "Mozilla/5.0 (Windows NT 6.1; rv:2.0.1) Gecko/20100101 Firefox/4.0.1",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.16 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; Intel Mac OS X 10.6; rv:7.0.1) Gecko/20100101 Firefox/7.0.1",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36 OPR/18.0.1284.68",
    "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)",
    "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0)",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:2.0.1) Gecko/20100101 Firefox/4.0.1",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:7.0.1) Gecko/20100101 Firefox/7.0.1",
    "Opera/9.80 (Macintosh; Intel Mac OS X 10.9.1) Presto/2.12.388 Version/12.16",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36 OPR/18.0.1284.68",
    "Mozilla/5.0 (iPad; CPU OS 7_0 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) CriOS/30.0.1599.12 Mobile/11A465 Safari/8536.25",
    "Mozilla/5.0 (iPad; CPU OS 8_0 like Mac OS X) AppleWebKit/600.1.3 (KHTML, like Gecko) Version/8.0 Mobile/12A4345d Safari/600.1.4",
    "Mozilla/5.0 (iPad; CPU OS 7_0_2 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11A501 Safari/9537.53",
    "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_8; en-us) AppleWebKit/534.50 (KHTML, like Gecko) Version/5.1 Safari/534.50",    
    "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-us) AppleWebKit/534.50 (KHTML, like Gecko) Version/5.1 Safari/534.50",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0"
  )
  n=length(UserAgent)
  myheader<-c(
    "User-Agent"=UserAgent[sample(n-1,1)],#随机选择
    "Accept-Language"="en-us",
    "Connection"="keep-alive",
    "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")
  
  urllist=0
  page=1:3
  urllist[page]=paste("http://tejia.aili.com/",page,".html",sep='')
  for(url in urllist){
    temp<-getURL(url,encoding="utf-8",httpheader=myheader)#
    k=htmlParse(temp,asText=T,encoding="utf-8")
    review=sapply(getNodeSet(k,'//*[contains(concat( " ", @class, " " ), concat( " ", "indx_li", " " ))]//span'),xmlValue)
    review[page]
    cat(url,"\n")
    write.table(review,"/Users/song/Desktop/yohui.txt",quote = FALSE,row.names = TRUE,
                col.names = FALSE,append=T)
    Sys.sleep(5)#睡眠5秒
  }
  
  
#重开
require(rvest)
require(magrittr)
  site_1<-"https://www.zhipin.com/job_detail/?query=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&scity=100010000&industry=&position="
  web_1<-read_html(x=site_1)
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    