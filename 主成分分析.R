library(psych)
library(openxlsx)
library(data.table)
library(ggplot2)
library(stringr)

hu <- read.xlsx("H:/R学习/练习/练习.xlsx")
lu <- read.xlsx("H:/综合/陆健/工作簿1.xlsx")
hu1 = hu[,2:5]
fa.parallel(x = hu1,fa="pc")   #判断主成分数目
hu2 <- principal(hu1,nfactors = 2,rotate = "varimax",scores = T)   
hu2$scores
hu3 <- data.table(pc$scores)

lu1 <- setDT(lu)
ggplot(data = hu3,aes(RC1,RC2))+
  geom_point()

hu3 <- data.frame(hu2)
hu4 <- hu3[5]
hu5 <- data.table(str_split(hu4,","))
hu6 <- str_sub(hu5,start = 3L,end = -2L)





