rm(list = ls())   #清除之前的变量
library(lattice)
library(openxlsx)
hu <- read.xlsx("H:/R学习/练习/练习.xlsx",colNames=TRUE)
attach(hu)
hu1 <- densityplot(~IR_H|Cultivar,main="风",sub="火",xlab="Cultivar",ylab="IR_H.Destiny",data=singer)
plot(hu1)
update(hu1,col="darkgreen",pch=16,cex=.8,lwd=2)
hu2 <- equal.count(IR_E,number=6,overlap=.1)   #等分连续型变量
xyplot(IR_H~GY_H|IR_E,data=quakes)   #不同IR_H程度的IR_H和GY_H的散点图
