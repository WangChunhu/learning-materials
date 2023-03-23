excel2csv <- function(input,output){
  require(readxl)
  data <- read_excel(input,col_names = T)
  write.csv(data,output,row.names = F)
}
#excel2csv(input = "/media/huhu/学习/生信/linux命令/shell/1.xlsx",output = "/media/huhu/学习/生信/linux命令/shell/3.csv")

excel2csv <- function(input,output){require(readxl) data <- read_excel(input,col_names = T) write.csv(data,output,row.names = F)}