setwd("D:/学习/key")
dir()

require(data.table)
require(magrittr)

data <- read.csv("china_stations.csv") %>% data.table() %>% setnames("Station", "区县拼音名")
jw <- fread("D:/学习/模型/经纬度/全国县级经纬度新.txt")
jw[,级别 := stringr::str_sub(string = 区县,start = -1L,end = -1L)]
unique(jw$级别)
jw[,区县名 := stringr::str_sub(string = 区县,start = 1L,end = -2L)]
jw[,区县拼音名 := pinyin::py(char = 区县名, sep = "", dic = pinyin::pydic(dic = c("pinyin2"))) %>% stringr::str_remove_all("\\d{1,}")] %>% .[,区县拼音名 := stringr::str_to_title(区县拼音名)]
result <- data[jw[,c(1:3,8)], on = "区县拼音名"] %>% .[省份 %in% c("江苏省", "浙江省", "安徽省", "湖北省", "湖南省", "重庆市", "四川省", "云南省", "贵州省", "江西省", "河南省")]

result1 <- jw[result, on = c("省份", "地市", "区县")] %>% .[,!c(9,11,12)] %>% na.omit()
unique(result1$省份)

openxlsx::write.xlsx(result1, "中国局部地区站点.xlsx")
openxlsx::write.xlsx(jw, "D:/学习/模型/经纬度/全国各县经纬度--详细.xlsx")
