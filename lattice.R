rm(list = ls())   #���֮ǰ�ı���
library(lattice)
library(openxlsx)
hu <- read.xlsx("H:/Rѧϰ/��ϰ/��ϰ.xlsx",colNames=TRUE)
attach(hu)
hu1 <- densityplot(~IR_H|Cultivar,main="��",sub="��",xlab="Cultivar",ylab="IR_H.Destiny",data=singer)
plot(hu1)
update(hu1,col="darkgreen",pch=16,cex=.8,lwd=2)
hu2 <- equal.count(IR_E,number=6,overlap=.1)   #�ȷ������ͱ���
xyplot(IR_H~GY_H|IR_E,data=quakes)   #��ͬIR_H�̶ȵ�IR_H��GY_H��ɢ��ͼ