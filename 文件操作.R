rm(list = ls()) #清除环境中的对象
path <- "F:/R/练习/4"
setwd(path)

dir(path) #列出路径下文件，同list.files()
list.dirs(path) #列出此级及以下路径
dir.create('5')  #新建路径
getwd() #当前工作路径
setwd("5") #设置当前工作路径

file.copy(from = "F:/R/练习/4/all.xlsx",to = "all.xlsx",overwrite = T) #复制文件
file.copy(from = "F:/R/练习/4",to = "F:/R/练习/5",overwrite = T,recursive = T) #复制目录，recursive = T
file.remove("1.xlsx") #只能删除文件
unlink("all.xlsx") #删除文件或目录
unlink("4",recursive = T) ##删除目录，recursive = T

file.rename(from = "6",to = "4") #重命名
file.exists(path) #参数paths可以是vector，即可以批量查看文件（目录）是否存在
