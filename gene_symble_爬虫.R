require(Rcrawler)
require(magrittr)
require(data.table)
require(stringr)
require(openxlsx)
#Getencoding(url)

data <- fread("/media/huhu/学习/生信/基因家族分析/Gossypium_hirsutum_v1.1.pep.txt",header = F)
name_num <- grep(pattern = ">",x = data$V1)
gene_id <- data[name_num] %$% str_replace_all(string = V1,pattern = ">","") %>% data.table()
sym_num <- grep(pattern = "ca",x = gene_id$.)
gene_ID <- gene_id[!sym_num]
 
i = 1
dan_sw_sw <- NULL
dan_sw_h <- NULL
all <- NULL
for (i in 1:nrow(gene_id)) {
  url <- paste0("http://www.cottonfgd.org/profiles/gene/",gene_id[i],"/")
  ContentScraper(Url= url,
                 CssPatterns = c("td"), 
                 encod = "UTF-8",
                 ManyPerPattern = TRUE) %>% data.frame() -> dan_sw
  dan_sw <- data.table(dan_sw)[1:5]
  dan_sw_h <- t(dan_sw) %>% data.table()
  all <- rbind(all,dan_sw_h,use.names = F)
  print(paste0("已完成",i,"/",nrow(gene_id)))
}
setnames(all,c("V1","V2","V3","V4","V5"),c("gene_id","gene_name","注释","Swiss-Prot","NAU-trembl"))
write.xlsx(x = all,file = "/media/huhu/学习/生信/转录组学分析/gene_注释/gene_注释.xlsx")

#
id <- fread("D:/物种信息/拟南芥/PRO/id.txt",header = F)
id <- unique(id$V1)
url <- paste0("https://www.arabidopsis.org/servlets/Search?type=general&search_action=detail&method=1&show_obsolete=F&name=",id,"&sub_type=gene&SEARCH_EXACT=4&SEARCH_CONTAINS=1")
at.des <- function(url){
  dan.symbol <-   ContentScraper(Url= url,
                                 CssPatterns = c(".result-other-names"), 
                                 encod = "UTF-8",
                                 ManyPerPattern = TRUE) %>% data.frame()
  dan.description <-   ContentScraper(Url= url,
                                      CssPatterns = c(".result-description"), 
                                      encod = "UTF-8",
                                      ManyPerPattern = TRUE) %>% data.frame()
  dan <- cbind(dan.symbol,dan.description)
  names(dan) <- c("symbol","description")
  dan <- data.table(dan) %>% .[1]
  return(dan)
}

i <- 1
all <- NULL
for (i in 1:length(url)) {
  dan.sd <- at.des(url = url[i])
  dan <- cbind(data.table(id = id[i]),dan.sd)
  all <- rbind(all,dan)
  print(paste0(i," / ",length(url)))
}
