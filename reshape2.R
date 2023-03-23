library("reshape2")
library(data.table)
library(openxlsx)
a <-rep(LETTERS[1:2],each=3)
b <-rep(c(1,2),times=3)
c <-c(4,6,5,9,8,5)
d <-rep(letters[6:11])
e <-data.frame(a,b,c,d)
head(e)


#acast(返回一个数组，~左侧的作为行名，~右侧的作为列名)
#dcast(返回一个数据框)
hu1 <- acast(data = e,
             formula = a~b,
             value.var = "c",
             fun.aggregate = sum)

names(airquality) <- tolower(names(airquality))   #把变量名变成小写
melt.air <- melt(airquality,
                 id.vars = c("month","day"),
                 variable.name = "index",
                 na.rm = TRUE)
summary(melt.air)
hu2 <- acast(data = melt.air,formula = month~index,fun.aggregate = mean)
hu3 <- dcast(data = melt.air,formula = month~index,fun.aggregate = mean)
hu4 <- acast(data = melt.air,formula = day~month~index)   #三维数组

names(ChickWeight) <- tolower(names(ChickWeight))
melt.chick <- melt(ChickWeight,
                   id.vars=2:4,
                   variable.name = "index",
                   na.rm=TRUE)
summary(melt.chick)
hu5 <- acast(melt.chick,chick+diet~time)  #两个分类变量的组合，组合以后的每个行名都是唯一的

#margins参数
hu6 <- acast(melt.chick,diet~time,fun.aggregate = mean,margins = TRUE)#(margins参数用于为所有的行或者列计算聚合值,也可以指定行或列（margins = diet）)

#subset参数（用于选择满足某个条件的记录，使用subset参数需要导入plyr）
library(plyr)
hu7 <- dcast(melt.chick,time~diet,fun.aggregate = mean,margins = TRUE,subset = .(diet %in%c(2,3)))

#定义(批量读取文件)函数
batchread <- function(path,sep=","){
  filepath <- paste(path,"/",dir(path),sep = "")
  allfiles <- lapply(filepath,function(x){   #lapply返回一个与X长度相同的列表，其中的每个元素都是对X的相应元素应用Fun的结果。
  read.xlsx(x,sep = sep)  
  })
  return(allfiles)
}
hu10 <- batchread("H:/R学习/练习")





