yesno <- function(路径名,期刊名){
  require(openxlsx)
  all <- read.xlsx(路径名)
  ifelse(期刊名 %in% all[,1],paste0(期刊名,"__是科技核心"),paste0(期刊名,"__不是科技核心"))
}

#调试
yesno("C:/学习/2019/毕业/科技核心2018.xlsx",c("湖北农业科学","中国农业科学","123"))
#中文函数名可以用，但是不出match，所以英文函数名好用，中文参数名可用

