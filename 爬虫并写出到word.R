require(Rcrawler)
require(magrittr)
require(data.table)
require(stringr)
require(openxlsx)

at.des <- function(page.num){
  url <- paste0("https://www.mianfeiwendang.com/doc/c6b75e443e1e99ee1da32f1e/",page.num)
  dan1 <-   ContentScraper(Url= url,
                           CssPatterns = c("#contents p"), 
                           encod = "UTF-8",
                           ManyPerPattern = TRUE) %>% data.frame()
  return(dan1)
}


page.num <- 1:18
i <- 1
all <- NULL
for (i in 1:length(page.num)) {
  dan <- at.des(page.num[i]) %>% data.table()
  names(dan) <- "a"
  all <- rbind(all,dan)
  print(paste0(i," / ",18))
}

# install.packages("officer")
# 输出word
require(officer)
doc <- read_docx()
doc <- body_add(x = doc,value = all)
print(x = doc,target = "D:/以前/长江大学/考博材料/作物栽培学考博材料/作物栽培学总论-长江大学.docx")
