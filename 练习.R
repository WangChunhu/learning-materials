library(openxlsx)
library(data.table)
library(ggplot2)
library(tidyr)

hu <-read.xlsx("H:/R学习/练习/练习.xlsx")
hu1 <- setDT(hu)
hu2 <- copy(hu1)
nrow(hu2)
names(hu2)
head(hu2)
hu2[.N]   #最后一行
hu3 <- hu2[,1:3]
hu4 <- hu3[IR_H>350 & GY_H>8]
hu5 <- hu4[Cultivar %in% c("Y-Lliangyou 900","Shanyou63","Liangyoupeijiu")]
summary(hu5)
hu6 <- hu5[,.(mean_IRH=mean(IR_H,na.rm = TRUE),mean_GYH=mean(GY_H,na.rm = TRUE)),by=.(Cultivar)]
hu7 <- hu5[,.(sd_IRH=sd(IR_H,na.rm = TRUE),sd_GYH=sd(GY_H,na.rm = TRUE)),by=.(Cultivar)]
hu8 <- setDT(cbind(hu6,hu7))
hu9 <- hu8[,!4]


#hu6 <- data.table(hu5[,sapply(.SD,function(x) c(mean(x),sd(x))),.SDcols=c("IR_H","GY_H"),by=Cultivar])   #多变量多函数运算
#hu7 <- data.table(t(hu6))   #列变行

ggplot(hu9)+
  geom_point(aes(Cultivar,mean_IRH,size=mean_IRH,color=mean_IRH))+
  geom_point(aes(Cultivar,mean_GYH,size=mean_GYH,color=mean_GYH))+
  theme(axis.text.x = element_text(angle = -45,hjust = 0,family = "serif",color = "darkred",size=rel(1.1)))+   #坐标轴文本倾斜
  scale_size(guide = "none")+   #去掉特定图例
  scale_color_continuous(name="size")+
  #geom_errorbar(aes(ymin=mean_IRH-sd_IRH,ymax=mean_IRH+sd_IRH), width=.2, position=position_dodge(.9))
  labs(y="IR_H")


hu9[,"sg" := c("","*","**")]
hu9[,mean_IRH := round(mean_IRH,digits = 1)]
label <- unite(hu9,"msg",mean_IRH,sg,sep = "")
ggplot(hu9,aes(x = Cultivar ,y = mean_IRH,fill=Cultivar,alpha=.3))+
  geom_col(width = .3)+
  geom_errorbar(aes(ymin=mean_IRH-sd_IRH,ymax=mean_IRH+sd_IRH),width=.1)+
  theme(axis.text.x = element_text(angle = -45))+
  scale_alpha(guide="none")+
  geom_text(aes(y = mean_IRH+1.5*sd_IRH,label=label$msg),size=3.5,fontface="bold",position = position_dodge(.9))+   #添加平均值+显著性
  labs(y="IR_H")

  
 


