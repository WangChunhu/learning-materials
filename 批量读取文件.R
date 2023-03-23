library(openxlsx)

#要求：文件内容格式差不多

#1、对于文件名有规律的情况
hu <- data.frame()    # 初始化数据框，用于后面的数据合并
for (i in 1:4) {
 path <- paste("H:/R学习/练习/再练习","/","练习",i,".xlsx",sep="")  
 hu <- rbind(hu,read.xlsx(path))
}

#2、对于文件名没有规律的情况
filenames <- dir(path = "H:/R学习/练习/再练习1")
hu <- data.frame()    # 初始化数据框，用于后面的数据合并
 for (i in filenames) {
 path <- paste("H:/R学习/练习/再练习1","/",i,sep="")  
 hu <- rbind(hu,read.xlsx(path))
 }

#3、对于文件名没有规律的情况，并且只读取某做后缀的文件
filenames <- dir(path = "H:/R学习/练习/再练习1")   #生成该目录下的文件名
filenames1 <- grep(pattern =".xlsx",filenames,value = TRUE)   #通过正则，获取所有xlsx结尾的文件名(在字符向量的每个元素中搜索与参数模式匹配的内容),value = TRUE(返回本身，否则返回索引)
hu <- data.frame()
for (i in filenames1) {
  path <- paste("H:/R学习/练习/再练习1","/",i,sep="")
  hu <- rbind(hu,read.xlsx(path))
}






