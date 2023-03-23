 library(openxlsx)
 library(ggplot2)
 #????ͼ
hu= read.xlsx("H:/Rѧϰ/��ϰ/��ϰ.xlsx",colNames=TRUE,sheet = 1)
 #data1$ʱ??=ordered(data1$ʱ??,levels=c("CK","5d","10d","15d","20d","25d","30d","35d"))
attach(hu)
head(hu)
p <- ggplot(hu,aes(Cultivar,GY_H))+
  geom_bar(stat = "identity",width = .3)
set.seed(1234)
x <- sample(c(1,2,4,5,7,8),size = 1000,replace = TRUE,prob = c(0.1,0.2,0.2,0.2,0.2,0.1))
hu1 <- data.frame(x)
g <- ggplot(hu1,aes(x=factor(x)))+ geom_bar(fill="steelblue",colour="darkred",width =.5)
x <- rep(1:5,each=3)
y <- rep(c("A","B","C"),times=5)
set.seed(1234)
z <- round(runif(min = 10,max = 20,n=15))
hu2 <- data.frame(x=x,y=y,z=z)
col <- c("darkred","skyblue","purple")
ggplot(hu2,aes(x=factor(x),y=z,fill=y))+ 
  geom_bar(stat="identity",position = "stack",color="black",width = .9)+   #?ٷֱ???"fill";???????ξ???position=position_dodge(0.7)
  #ͼ?????Ļ?+guides(fill=guide_legend(reverse = TRUE))
  scale_fill_manual(values = col)+
  labs(x="x",y="y",title = "picture",subtitle = "z",caption = "picture+x+y+z")+
  geom_text(aes(label=z),size=4,color="black",vjust=2.5,hjust=.5,position = position_stack(0.9))   #???ݱ?ǩ??vjust??????ǩ????λ?ã?1Ϊ?ֽ??ߣ?Խ????1????ǩԽ??????ͼ?Ͻ??·?????֮??Խ??????ͼ???Ͻ??Ϸ???hjust=0.5????ǩˮƽ???з???;????ˮƽ?????Ĵ?????ͼ??????ͨ??position_dodge()????��??????ǩλ??;ͼ??λ??????ǩλ??һ??

hu3 <- ggplot(hu,aes(GY_H),color="black")+
  stat_bin(aes(fill=..count..,color=-1*..ndensity..),binwidth = 1)+
  geom_bar(width = 5,position = "dodge")   #??????reorder(x,z)
hu3
set.seed(12)
x <- 1980+1:35
y <- round(100*rnorm(35))
hu4 <- data.frame(x=x,y=y)
hu4 <- transform(hu4,judge=ifelse(y>0,"YES","NO"))   # ?ж?y?Ƿ?Ϊ??ֵ
  ggplot(hu4,aes(x,y,fill=judge))+
    geom_bar(stat = "identity",position="identity",color="black",width = .9)+
    theme(legend.position = "")+   #????ͼ??
    scale_x_continuous(expand = c(0,0))+   #??????��չ
    scale_size(guide = "none")+   #ɾ???ض?ͼ??
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  #ɾ??????
    xlab("Year")+
    scale_fill_manual(values = c("darkred","skyblue"))+
      theme_classic()   #ֻҪxy??
#????ͼ
  require(ggplot2)
  require(lubridate)  #???????ڵİ?
  year <- year(seq(from=as.Date("2006-01-01"),to=as.Date("2015-01-01"),by="year"))
  weight <- c(23,35,43,57,60,62,63,66,61,62)    
  hu5 <- data.frame(x=year,y=weight)  
  ggplot(hu5,aes(factor(year),weight,group=1))+  #?????????ͱ?��??????ʹ??aes(group=1)??ȷ??ggplot()֪????Щ???ݵ?????ͬһ??????
    geom_line()+
    xlab("Year")
  
  type <- c("A","B","C","D","E")
  quanlity <- c(1,2,2.1,1.5,1.7)    
  hu6 <- data.frame(x=type,y=quanlity)  
  ggplot(hu6,aes(x,y,group=1))+
    geom_line()+
    geom_point()  #?ӵ?
  #??????
  rm(list = ls())
  year <- rep(1990:2025,times=3)
  type <- rep(c("C","D","E"),each=36)  
  value <- c(IR_E,GY_H,GY_E)  
  hu7 <- data.frame(year=year,type=type,value=value)  
  ggplot(hu7,aes(x=year,y=value,color=type,linetype=type,shape=type)) +
    geom_line()+
    geom_point()+
   #+ scale_linetype_manual(values = c(1,2)) #?Զ???????????
    
     +scale_color_manual(values = c("steelblue","darkred")) #?Զ?????ɫ
  
     + scale_shape_manual(values = c(21,23)) #?Զ???????״
  
     + scale_fill_manual(values = c('red','black')) #?Զ???????????ɫ

  #?ѻ?????ͼ
  rm(list = ls())
  year <- rep(1990:2025,times=3)  
  type <- rep(c("A","B","C"),each=36)
  value <- c(GY_E,GY_H,GY_H)
  hu8 <- data.frame(year=year,type=type,value=value)  
  ggplot(hu8,aes(x=year,y=value,fill=type))+
    geom_area(alpha=.6,position = "fill")+   #?ɶѻ?ͼ
    expand_limits()+   #��չͼ??
    labs(title = "?ཬ?ڶ??ڸ???",fill="HU")+   #fillΪͼ??????
    theme(plot.title = element_text(hjust = 0.5,color="darkred",size = rel(1.1)))
    theme_classic()+
    geom_line(color="black",size=1,position = "fill",alpha=.6) + #???ߣ??ٷֱȶѻ???fill
    #guides(fill="none")   #?Ƴ?ͼ??
    #scale_fill_discrete(name="hu",labels=c("a","b","c"))+  #?޸?ͼ?????⼰??ǩ
    #guides(fill=guide_legend(rev=TRUE))   #??תͼ????ǩ
    theme(legend.title = element_text(face="italic",family = "Times",color = "red",size=10))+   #?޸?ͼ????????ʽ
    #theme(legend.text = element_text(lineheight = 0.8),legend.key.height =unit(1,"cm") )  ## ????ͼ??˵???ĸ߶Ȳ???С???еļ???
    #theme(legend.text=element_text(xx))   # ?޸?ͼ????ǩ??????
      Aebaidu$ʱ?? <- factor(x = Aebaidu$ʱ??,levels = c("ck","5-8d","8-11d","11-14d","14-17d","17-20d"))   #?޸ķ?????ǩ??˳??  
#ɢ??ͼ   #????scale_size_area()????ʹ???ݵ????????????ڱ?��ֵ(??????ע??)
  ggplot(hu,aes(GY_E,IR_H))+geom_point(size=10,alpha=.5,color="red",shape=IR_E)+
    facet_grid(Cultivar~.)
    facet_wrap()
  
  rm(list = ls())
   hu_labels <- data.frame(Cultivar=c("Liangyoupeijiu","Shanyou63","Y-Lliangyou 900"),label=c("??","??","??"))
   hu1_labels <- function(wang){
     hu1 <- lm(IR_H~GY_E,data = wang)
     formula <- sprintf("y == %.2f +%.2f*x",round(coef(hu1)[1],2),round(coef(hu1)[2],2))   #???ذ????ı??ͱ?��ֵ???ַ???��
     r <- cor(wang$GY_E,wang$IR_H)
     r2 <- sprintf("R^2 == %.2f",r^2)
     data.frame(formula=formula,r2=r2,stringAsFactors=FALSE)}
   library(plyr)
   labels <- ddply(hu,"Cultivar",hu1_labels)
  ggplot(hu,aes(GY_E,IR_H))+  #,color=GY_E,size=GY_E
    facet_grid(.~Cultivar)+
    geom_point(shape=16)+
    #scale_color_gradient(low = "lightblue",high = "darkblue")+
    #scale_size_area(max_size = 10)+   #???Ĵ?С????��ֵ?仯
    geom_text(x=6, y=400, aes(label=label), data=hu_labels,color="red",size=10)+
    geom_smooth(method = lm,se=FALSE)+
    geom_text(x=4.5,y=240,aes(label=formula),data =labels,parse = TRUE,hjust=0)+
    geom_text(x=5,y=430,aes(label=r2),data =labels,parse=TRUE,hjust=0)

#?ص?????   
require(ggplot2)
require(hexbin)
  set.seed(1234) 
  x <- rnorm(10000)  
  y <- rnorm(10000,0,2)  
  hu10 <- data.frame(x=x,y=y)  
  ggplot(hu10,aes(x,y))
  #1??͸????  geom_point(alpha=.1)
  #2:????????  stat_bin2d(bins = 50,binwidth=c(0.2,0.3))+
  #  scale_fill_gradient(low = "skyblue",high = "darkred",limits=c(0,100),breaks=c(0,25,50,100))   
  #3????????  stat_bin_hex(bins = 50,binwidth = c(0.2,0.3))+
  #  scale_fill_gradient(low = "skyblue",high = "darkred",limits=c(0,100),breaks=c(0,25,50,100))
  #??λ?ܶȹ��Ʋ??ӵȸ???  geom_point(alpha=.1)+
  # stat_density2d()
  #??С???ܶȳɱ???    stat_density2d(geom="point",aes(size=..density..),contour=FALSE )+
  #  scale_size_area()
  #??Ƭͼ   stat_density2d(geom="tile",aes(fill=..density..),contour=FALSE )
 
#ɢ??ͼ+?߼ʵ?̺ 
  ggplot(hu,aes(IR_H,GY_E))+
    geom_point()+
    geom_rug(position="jitter",size=.1)
 position = position_jitter()  
#??????????
  ggplot(hu,aes(IR_H,GY_E))+
    geom_point(alpha=.5)+
    stat_smooth(method = lm,level = 0.99,color="black")   #????????+99%????????
    #  stat_smooth(method = lm,se=FALSE)   #?????????Ҳ???????????
library(MASS)  
b <- biopsy   
b$classn[b$class=="benign"] <- 0
b$classn[b$class=="malignant"] <- 1
b
#??????
ggplot(b,aes(V1,classn))+
  geom_point(position = position_jitter(width = .3,height = .06),alpha=.4,shape=21,size=1.5)+
  geom_smooth(method = glm,method.args=list(family="binomial")) #S??ģ??

#????????
ggplot(hu,aes(GY_H,IR_H,color=IR_E))+
  geom_point()+
  #scale_color_brewer(palette="Set1") +
  geom_smooth(method = lm) 

#ɢ??ͼ????
  #????ɢ??ͼ????(????����)
pairs(~IR_H+GY_E+GY_H,data=hu,main="Basic Scatter Plot Matrix")
mtcars
  #?߼?ɢ??ͼ????1??car????scatterplotMatrix()??????
library(car)
scatterplotMatrix(~ mpg + disp + drat + wt|cyl, data=mtcars, spread=FALSE, diagonal="boxplot", main="Scatter Plot Matrix via car Package")  #spread=FALSEѡ????ʾ??????չʾ??ɢ?ȺͶԳ???Ϣ??ֱ?ߡ?
 #?߼?ɢ??ͼ????2??gclus????cpairs()??????
 cor(hu[c("IR_H","GY_E","GY_H")])  #?鿴??????
 library(gclus)
 mydata <- mtcars[c(1,3,5,6)]
 mydata.corr <- abs(cor(mydata))   #????ֵ
 mycolors <- dmat.color(mydata.corr)
 myorder <- order.single(mydata.corr)
 cpairs(data = mydata,order = myorder,panel.colors = mycolors,gap=.5,main="Variables Ordered and Colored by Correlation")

 #3Dɢ??ͼ(?ع???)
 library(scatterplot3d)
 attach(mtcars) 
s3d <- scatterplot3d(wt,disp,mpg,main = "Basic 3D Scatter Plot",pch=16,highlight.3d = TRUE,type = "h")
fit <- lm(mpg~wt+disp)
s3d$plane3d(fit)
detach(mtcars)
 #????ʽ??άɢ??ͼ1
library(rgl)
attach(mtcars) 
plot3d(wt,disp,mpg,col = "red",size=5) 
detach(mtcars) 

#ֱ??ͼ
ggplot(hu,aes(IR_H))+
  geom_histogram(bins = 10,fill="gray",color="black",binwidth = 20)+#??ֱ??ͼ
  facet_grid(Cultivar~.)   #????ֱ??ͼ????????��??

#Ƶ????????+?????ı????߻???ͷ????????Ӱ??(???????ı???ɢ??ͼ??)
  rm(list = ls())
  library(grid)
  ggplot(hu,aes(IR_H))+
    geom_freqpoly(bins=10)+
    labs(x="??",y="??",title="????",caption="??")+
    facet_grid(Cultivar~.)+   #  ~.(ǰ?Ϻ???)
    annotate("text",x=3,y=4,label="line1",family="serif",color="darkred",size=5)+   #(family="serif/sans/ mono/ symbol")#?????ı????????壨???ߡ??޳??ߡ??ȿ���?????????ȣ?
    annotate("text",x =3,y=-Inf,label="line2",vjust=-1)+
    annotate("text",x=10,y=2.5,parse=TRUE,size=4,label=" y==frac(1, sqrt(2*pi)) * e^{-x^2/2}") +   #?ӹ?ʽ???????ӣ?parse=TRUE??
    geom_hline(yintercept=4.5,color="darkred")+     #????
    geom_vline(xintercept=308,color="darkred") +
    geom_abline(intercept =0,slope =0.02 )+
    annotate("segment",x = 250,xend = 280,y = 3,yend = 4,color="blue",size=0.5,arrow=arrow(length=unit(.2, "cm")))+   #??ͷ
    annotate("segment",x = 380,xend = 420,y = 1,yend = 1,arrow=arrow(ends = "both",angle = 90,length = unit(.1,"cm")))+   #????I???߶?
    annotate("rect",xmin = 285,xmax = 330,ymin =0,ymax = 4,alpha=.1,fill="skyblue",color="darkred")  #????
    

  
  #???ܶ?????
  ggplot(hu,aes(IR_H,..density..))+
    geom_density(fill="skyblue",alpha=.2)+
    facet_grid(Cultivar~.)+
    xlim(100,550)
    

#????ͼ
 ggplot(hu,aes(Cultivar,IR_H,fill=Cultivar)) +
   geom_boxplot(outlier.size = 1.5,outlier.shape = 21)+   #??????Ⱥ??
   geom_jitter(shape=21)+
   stat_summary(fun.y = "mean",geom="point",shape=23,fill="white")#??ֵ????ֵ???бȽ?
   
#С????ͼ
 ggplot(hu,aes(Cultivar,GY_H,fill=Cultivar)) +
   geom_violin(alpha=.5,width=.9)+
   geom_jitter(shape=21)

#??ɫͼ?͵ȸ?ͼ
 x=10*(1:nrow(volcano))
 y=10*(1:ncol(volcano))
image(x,y,volcano,col = terrain.colors(100),axes=FALSE) 
contour(x,y,volcano,levels = seq(90,200,by=5),add = TRUE,col="peru")
box()

#?????꣨õ??ͼ??
ggplot(hu,aes(Cultivar,IR_H,fill=Cultivar))+
  geom_bar(stat = "identity",alpha=.7)+  
  coord_polar()

#????ͼ
library(plyr)
library(dplyr)  
library(ggmap)  
library(maps)
library(ggplot2)  
library(gganimate)  
library(openxlsx)
hu11 <- read.xlsx("H:/Rѧϰ/��ϰ/��ϰ.xlsx",colNames=TRUE,sheet = 1)
names(hu11)  
world <- map_data("world")

##??????????
#??תx,y??
q <- ggplot(hu,aes(Cultivar,IR_H,color=IR_E,group=IR_E))+
  geom_point()+
  geom_smooth(method = lm,se=FALSE)+
  scale_color_gradient(low = "skyblue",high = "darkred",limits=c(1,2),breaks=c(1,2))+
  coord_flip(xlim =  NULL,ylim =  NULL,expand = TRUE,clip = "on")#+#??תx,y?ᣨxΪ��????��??
  #scale_x_discrete(limits=rev(c("Y-Lliangyou 900","Shanyou63","Liangyoupeijiu")))   #rev(????)??????rev????????,?????޸??????ͱ?��˳??

#??????+ͼ??
rm(list=ls())
ggplot(hu,aes(GY_E,IR_H,color=IR_E,group=IR_E))+
  geom_point()+
  geom_smooth(method = lm,se=FALSE)+
  scale_color_gradient(low = "skyblue",high = "darkred",limits=c(1,2),breaks=c(1,2))+
  #xlim(0,10)+   #???˲ü?
  #ylim(200,400)
  scale_y_continuous(limits = c(200,450),breaks = c(200,250,300,350,400,450))+ #Ҳ???˲ü����??ҿ????޸?X,Y???ϵ?ֵ(breaks=seq(200,450,50))
  #coord_cartesian(xlim = c(3,13),ylim = c(200,500))#+   #?????????任?Ŵ?????С??????
  #scale_x_reverse()   #??תһ??��?????????ᣨy??
  #coord_fixed(ratio = 1.5)+   #????x????y???????ű???
  #theme(axis.text.x=element_blank())   # ?Ƴ??̶ȱ?ǩ
  #theme(axis.title.x = element_blank())   #?Ƴ?????????ǩ
  #theme(axis.ticks.x = element_blank())   # ?Ƴ??̶???
  #scale_x_continuous(breaks = NULL)   # ͬʱ?Ƴ?��???ͱ?��?Ŀ̶??ߡ??̶ȱ?ǩ????????
  theme(axis.text.x = element_text(angle = -45,hjust = 0,family = "serif",color = "darkred",size=rel(2)))+   #?޸Ŀ̶ȱ?ǩ??????
  labs(title="?ཬ?ڶ??ڸ???ʵ??",subtitle="???ϵ?",caption="??????",x="????ʱ??/3d",y="?????¶?/??" )+ theme(plot.title = element_text(hjust = 0.5,colour = "blue",size = rel(1.1)))+   #?????????м?????????
  theme(plot.subtitle = element_text(hjust = 0.5,colour = "skyblue",size = rel(0.9)))+   #?????????м?????????
  theme(plot.caption = element_text(hjust = 0.5,colour = "red",size = rel(0.9)))+   #ͼ???????м?????????
  guides(fill="none")+
  theme(legend.position ="right")+   #ͼ??λ?ã?bottom/right??/c(0-1,0-1)??ͼ????
  theme(legend.background = element_rect(fill = "white",color="black"))+
  scale_fill_discrete(name="hu",labels=c("a","b"))+
  facet_grid(.~Cultivar)+   # scale="free"(????Ӧ?̶?)
  theme(strip.text = element_text(face="bold",size = rel(1.1)),strip.background=element_rect(fill="lightblue",color="black",size = 1.1))

#????ͼ
a <- table(hu$IR_E)
pie(a,labels = c("x","y","z"))

#????ͼ??
1:pdf("myplot.pdf",width = 4,height = 4,units = "cm")   #??????????ҳͼ??
2:ggsave("myplot.pdf",width =  4,height = 4,units = "cm")   #??????

#?ʺ?
 hu12 <- seq(0,pi,length=300)
 x=cos(hu12)
 y=sin(hu12)
 y1=y-0.01
 y2=y-0.02
 y3=y-0.03
 y4=y-0.04
 y5=y-0.05
 y6=y-0.06
 z <- data.frame(x=x,y=y)
 z1 <- data.frame(x=x,y=y1)
 z2 <- data.frame(x=x,y=y2)
 z3 <- data.frame(x=x,y=y3)
 z4 <- data.frame(x=x,y=y4)
 z5 <- data.frame(x=x,y=y5)
 z6 <- data.frame(x=x,y=y6)
 ggplot()+
   geom_line(data = z,mapping = aes(x,y),color="red",size=2.3)+
   geom_line(data = z1,mapping = aes(x,y1),color="orange",size=2.3)+
   geom_line(data = z2,mapping = aes(x,y2),color="yellow",size=2.3)+
   geom_line(data = z3,mapping = aes(x,y3),color="green",size=2.3)+
   geom_line(data = z4,mapping = aes(x,y4),color="cyan",size=2.3)+
   geom_line(data = z5,mapping = aes(x,y5),color="blue",size=2.3)+
   geom_line(data = z6,mapping = aes(x,y6),color="purple",size=2.3)+
   coord_cartesian(xlim = c(-0.7,0.7))+
   theme_void()+   #????????
   annotate("text",x=0.06,y=0.4,label="?????????????ϣ?",family="serif",color="darkred",size=5)+
   annotate("text",x=0.13,y=0.3,label="?˼??????ǲ?ɣ??",family="serif",color="darkred",size=5)
 ggsave(filename ="H:/Rѧϰ/picture/rainbow.jpg",plot = last_plot())  
 
 #д??excel
 hu2 <- c("a","b","c")
 hu3 <- c(1,2,3)
 hu4 <- data.frame(x=hu2,y=hu3)
 write.xlsx(x = hu4,file = "H:/Rѧϰ/word/��ϰ.xlsx")
 
 scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0
))    #??????x????y????????ԭ???غ?
 theme_bw()+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  #ɾ??????
 
 #?????ı?ǩ
 1: 
   f_labels <- data.frame(drv=c("4","f","r"), 
   label=c("4wd","Front","Rear"))
   p + geom_text(x=6,y=40,aes(label=label), data= f_labels)
 
 2:
   labels <- c(Female = "Women",Male = "Men")
   ggplot()+
   facet_grid(.~sex,labeller = labeller(sex=labels))

#ƴͼ
library(cowplot)   
   # draw_plot(plot, x = 0, y = 0, width = 1, height = 1, scale = 1),(???½?????)
   # draw_text(text, x = 0.5, y = 0.5, size = 14, hjust = 0.5, vjust = 0.5,...)
   # draw_plot_label(label, x = 0, y = 1, hjust = -0.5, vjust = 1.5, size = 16, fontface = "bold", family = NULL, colour = NULL, ...)
 ggdraw()+
   draw_plot(plot = q,x = 0,y = 0.5,width = 1,height = 0.5)+
   draw_plot(plot = p,x = 0,y = 0,width = 0.5,height = 0.5)+
   draw_plot(plot = g,x = 0.5,y = 0,width = 0.5,height = 0.5)+
   draw_plot_label(label = c("A","B","C"),x = c(0,0,0.5),y = c(1,0.5,0.5),colour = "green")+
   draw_text("text", x = 0.5, y = 0.5, size = 14, hjust = 0.5, vjust = 0.5)
   
 #??
 require(ggplot2)
 n=500
 r=0.7
 r_e=(1-r*r)^.5
 X=rnorm(n)
 Y=X*r+r_e*rnorm(n)
 Y=ifelse(X>0,Y,-Y)
 Z=1:500
 data <- as.data.frame(cbind(X,Y,Z))
 ggplot(data = data,aes(X,Y))+
   geom_point(color=Z)+
   theme(panel.background = NULL,axis.text=element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),panel.border = element_blank())

 #????ϵ??????ͼ
 require(corrplot)
 
 a <- read.xlsx("C:/Users/ASUS/Desktop/?½? Microsoft Excel ??????.xlsx")
 cor(a)->a
 corrplot(corr = a,order = "AOE",type = "upper",tl.pos = "d")+
 corrplot(corr = a,add = T,type = "lower",method = "number",order = "AOE",col="black",diag=F,tl.pos="n", cl.pos="n")
 
 
###########################################################
#????1Դ
#??��??
devtools::install_github("wch/extrafont",dependencies=TRUE)
devtools::install_github("wch/extrafontdb",dependencies=TRUE)
devtools::install_github("wch/Rttf2pt1",dependencies=TRUE) 
########################################################
#??ɫ??airbnb??facebook??etsy??oogle??X23andme??
devtools::install_github("ricardo-bion/ggtech",dependencies=TRUE)

require(ggtech) #ֻ??????ɢ?ͱ?��

data<-diamonds[diamonds$color %in%LETTERS[4:7], ]

ggplot(data,aes(carat,fill=color))+geom_histogram(bins=30)+
  
  theme_tech(theme="airbnb") + 
  
  scale_fill_tech(theme="airbnb") +
  
  labs(title="Airbnb theme", 
       
       subtitle="now with subtitles for ggplot2 >= 2.1.0")

ggplot(data,aes(carat,fill=color))+geom_histogram(bins=30)+
  theme_tech(theme="twitter") + 
  scale_fill_tech(theme="twitter") + 
  labs(title="????????", 
       subtitle="???Ǹ?????")

#
require(ggthemes)
#????ѧ?˵?????scale_colour_economist()??scale_colour_wsj(),????excel?????⡢tableau?????Լ?stata???⡣
scale_fill_gradient2_tableau() #��????��

 
#windows系统字体
require(ggplot2)
# 查看当前的ggplot2默认字体
theme_get() # 默认是sans（Arial字体）
# 查看Windows系统下的字体
windowsFonts()

require(grDevices)
myfree<-theme_set(theme_bw())
windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
             RMN=windowsFont("Times New Roman"),
             ARL=windowsFont("Arial"))
old_theme <- theme_update(
  plot.title=element_text(family="ARL", size=22, face="bold", colour="black"),
  axis.title.x=element_text(family="HEL", size=18,face="bold", colour="black"),
  axis.title.y=element_text(family="HEL", size=18, face="bold",angle=90, colour="black"),
  axis.text.x=element_text(family="RMN", size=16, colour="black"),
  axis.text.y=element_text(family="RMN", size=16, colour="black"),
  axis.ticks=element_line(colour="black"),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  panel.background=element_blank(),
  axis.line=element_line(size=1)
)

pdf("D:/1.pdf",width = 5,height = 5,family = "serif")
ggplot(data = data.frame(x = 1:10,y = 1:10),aes(x,y))+
  geom_point()
dev.off()



#交互式图
require(plotly)
set.seed(2333)
data <- diamonds[sample(nrow(diamonds), 500), ]
p <- ggplot(data,aes(carat,price,colour=color))+
  geom_point()
ggplotly(p = p)


#动态气泡图
require(gapminder) #加载数据集
require(gganimate) #动画包
require(ggplot2) 
 
head(gapminder)
ggplot(data = gapminder,mapping = aes(gdpPercap, lifeExp, size = pop, color = continent))+
  geom_point()+
  scale_x_log10()+
  theme_bw()+
  ##动画
  labs(title = "year: {frame_time}", x = "GDP per capita", y = "life expectancy")+
  transition_time(year)+
  ease_aes("linear")

anim_save("动态气泡图.gif")  
  
# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("动态气泡图2.gif")

library(rgl)
library(magick)
# This is ugly
colors <- c("royalblue1", "darkcyan", "oldlace")
iris$color <- colors[ as.numeric( as.factor(iris$Species) ) ]

# Static chart
plot3d( iris[,1], iris[,2], iris[,3], col = iris$color, type = "s", radius = .2 )

# # We can indicate the axis and the rotation velocity
# play3d( spin3d( axis = c(0, 0, 1), rpm = 10), duration = 10 )

# Save like gif
movie3d(
  movie="3dAnimatedScatterplot", 
  spin3d( axis = c(0, 0, 1), rpm = 7),
  duration = 10, 
  dir = "~",
  type = "gif", 
  clean = TRUE
)
 
# library
library(igraph)

# create data:
links=data.frame(
  source=c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target=c("B","B", "C", "D", "J","A","E", "F", "G", "H", "I","I")
)

# Turn it into igraph object
network <- graph_from_data_frame(d = links, directed = T) 

# Count the number of degree for each node:
deg <- degree(network, mode="all")

# Plot
plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )
 
 
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=F, vertices=actors)
print(g, e=TRUE, v=TRUE)
plot(g, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5),mark.shape = gender)
 
 
 
 
 
 
    