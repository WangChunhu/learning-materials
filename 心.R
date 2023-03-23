heart<-data.frame()
for(i in 1:4000)
{
  x=i*0.0005-1
  y=(x^2)^(1/3)+(1-x^2)^(1/2)
  heart<-rbind(heart,c(x,y))
  y=(x^2)^(1/3)-(1-x^2)^(1/2)
  heart<-rbind(heart,c(x,y))
}
colnames(heart)<-c('x','y')
plot(heart$y~heart$x,main='HEART',col='red')


require(ggplot2)
ggplot(data = heart)+
  geom_point(aes(x,y),color = "#FF8B8B")+
  geom_point(aes(x+0.3,y),color = "#FF0000")+
  geom_line(aes(c,d),color = "#FF0000")+
  theme_classic()



