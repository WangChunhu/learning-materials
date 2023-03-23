read.csv(file.choose())
#单因素
#读取数据
hu <- read.table("H:\\R学习\\练习.txt",header=T) 

#将hu数据框中的type转换为因子
hu$type <- as.factor(hu$type)

#方差
hu.an<- aov(lm(day~type,data = hu))

summary(hu.an)
boxplot(day~type,data=hu)
                 