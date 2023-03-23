require(data.table)
require(magrittr)
require(stringr)

# 目前只支持xlsx格式
remove_DF.Blank <- function(data.path, sheet, trim = T, squish = T){
  data <- openxlsx::read.xlsx(data.path, sheet = sheet) %>% data.table()
  data <- data[,lapply(.SD, str_trim), .SDcols = c(names(data))] %>% # 去除两边的空白字符
    .[,lapply(.SD, str_squish), .SDcols = c(names(data))] # 去除字符串内部多余的空白字符
}

# 测试
# data <- remove_DF.Blank(data.path = "D:/学习/课题/2016-2020年稻茬小麦地区统计年鉴/四川省/四川省2016-2020年各区县市主要粮食作物产量及面积.xlsx",sheet = 10)


