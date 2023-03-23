open_all_xlsx <- function(lujing,mingzi){  #默认值用 =
  require(openxlsx)
  
  name <- dir(path = lujing,pattern = ".xlsx")
  if (length(name) == 0) {
    print("当前路径下没有excel文件，请查证后再继续操作")
  }
  
  all <- NULL
  dan <- NULL
  
  i = 1
  for (i in 1:length(name)) {
    dan <- read.xlsx(paste0(lujing,"/",name[i]))
    all <- rbind(all,dan)
  }
  
  write.xlsx(x = all,file = paste0(lujing,"/",mingzi,".xlsx"))
  print("合并写出成功！")
}

#调试
#open_all_xlsx(lujing = "F:/r习",mingzi = "first1")
