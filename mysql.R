#install.packages('RMySQL')
require(RMySQL)
#连接数据库
sql <- dbConnect(drv = MySQL(),
                 user="root",
                 password="P17792914570p",
                 host="localhost",
                 port=3306,
                 dbname="huhu")

#连接概述
summary(sql)
#列出连接下所有表
dbListTables(sql)

i <- data(iris)
# #创建表不能写出，直接用dbWriteTable
# data <- data.frame(x = 1:10,y = 11:20)
# dbCreateTable(sql, "iris", iris)

#写出表
dbWriteTable(conn = sql,name = "t1",value = iris,overwrite = T)

#读入表
##先设置编码（windows）
##dbSendQuery(conn = sql,statement = 'set names gbk') 
t5 <- dbReadTable(conn = sql,name = "t5")
t5[1:3,]

#删除表
dbRemoveTable(sql,"t4")

#关闭连接
dbDisconnect(sql)
