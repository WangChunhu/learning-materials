library("reshape2")
library(data.table)
library(openxlsx)
a <-rep(LETTERS[1:2],each=3)
b <-rep(c(1,2),times=3)
c <-c(4,6,5,9,8,5)
d <-rep(letters[6:11])
e <-data.frame(a,b,c,d)
head(e)


#acast(����һ�����飬~������Ϊ������~�Ҳ����Ϊ����)
#dcast(����һ�����ݿ�)
hu1 <- acast(data = e,
             formula = a~b,
             value.var = "c",
             fun.aggregate = sum)

names(airquality) <- tolower(names(airquality))   #�ѱ��������Сд
melt.air <- melt(airquality,
                 id.vars = c("month","day"),
                 variable.name = "index",
                 na.rm = TRUE)
summary(melt.air)
hu2 <- acast(data = melt.air,formula = month~index,fun.aggregate = mean)
hu3 <- dcast(data = melt.air,formula = month~index,fun.aggregate = mean)
hu4 <- acast(data = melt.air,formula = day~month~index)   #��ά����

names(ChickWeight) <- tolower(names(ChickWeight))
melt.chick <- melt(ChickWeight,
                   id.vars=2:4,
                   variable.name = "index",
                   na.rm=TRUE)
summary(melt.chick)
hu5 <- acast(melt.chick,chick+diet~time)  #���������������ϣ�����Ժ��ÿ����������Ψһ��

#margins����
hu6 <- acast(melt.chick,diet~time,fun.aggregate = mean,margins = TRUE)#(margins��������Ϊ���е��л����м���ۺ�ֵ,Ҳ����ָ���л��У�margins = diet��)

#subset����������ѡ������ĳ�������ļ�¼��ʹ��subset������Ҫ����plyr��
library(plyr)
hu7 <- dcast(melt.chick,time~diet,fun.aggregate = mean,margins = TRUE,subset = .(diet %in%c(2,3)))

#����(������ȡ�ļ�)����
batchread <- function(path,sep=","){
  filepath <- paste(path,"/",dir(path),sep = "")
  allfiles <- lapply(filepath,function(x){   #lapply����һ����X������ͬ���б������е�ÿ��Ԫ�ض��Ƕ�X����ӦԪ��Ӧ��Fun�Ľ����
  read.xlsx(x,sep = sep)  
  })
  return(allfiles)
}
hu10 <- batchread("H:/Rѧϰ/��ϰ")




