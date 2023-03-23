图形基本元素：点  points( )；线--折线lines ( )、直线abline()、线段segments()、曲线curve( )；面--矩形 rect() box( )、多边形 polygon()文本text( )、 mtext( ) 、title(  )；坐标轴 axis( )；网格线grid()；图例 legend( )

添加点：
  points(x,y)#参数设置大部分同 par() 
  polt(5,xlim=c(0,10),ylim=c(o,10),type="n#无图")#绘制底图
eg:
  plot(5, xlim=c(0,10), ylim=c(0,10), type="n");    points(3,5    ,pch=15,col="red",cex=3);   mydata=data.frame(x=1:10,y=runif(2   ,7,10))#数据框;    points(data$x,data$y,pch=6,col="blue",cex=3)

添加直线：
  abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL       ...)#a 截距，b 斜率，h是画水平线时的纵轴值，v 是画垂直线时的横   轴值，reg 是一个能用函数coef()提取系数（包含斜率和截距）的R     对象，如提取线性回归生成的对象
eg:
  mydata <-data.frame(x=1:10,y=runif(10))
  plot(mydata,ylim=c(-1,1))
  model=lm(y~x,data=mydata)#线性回归
  abline(model)
  abline(h=0,v=5,lwd=2,col="green")
  abline(a=0,b=0.1,lwd=2,lty=2)
添加线段segments：
  segments( x0, y0, x1, y1,...)#x0, y0, x1, y1  设置线段起点和终   点的坐标
eg:
  plot(5,xlim=c(0,10),ylim=c(0,10),type="n")
  segments(2,2,8,8,lwd = 2,col = "green")
  segments(2,1,8,7,lwd=2,lty = 2)
添加矩形:
  rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,   col = NA, border = NULL, lty, lwd...)#xleft, ybottom, xright,   ytop   #分别制定左下角和右上角的坐标，用来确定矩形的位置
  density   #  设置阴影线的填充密度. 当设为一个正值时，那么颜色   填充参数col将失效，只有当它被设为负数或NA或NULL时才可以填充颜   色
  angle    #    设置填充线条的角度，默认为45度
  border   #  设置边框颜色，若设置为FALSE或NA，那么将不画边框
  lty 和 lwd #分别设定边框和填充线的类型和粗度
  box(...)  #专用于给图形添加外边框
eg:
  plot(5,xlim=c(0,10),ylim=c(0,10),type="n")
  rect(1,1,3,3)
  rect(3,3,6,6,density=8)
  rect(6,6,9,9,density=8,angle = 15,col="gray",border = "green"   ,lwd=2,lty = 1)
  box(lwd=3)
添加文本 #text( ) ,title ( ) 和mtext( ):
    title(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,      line = NA, ...)
  用于添加标签: main, sub , xlab , ylab # 设置主、副、x轴、y轴的   标题
  line#设置文本与图形边缘的距离
  text(x, y , labels, ...)
  用于在绘图区域添加文本#x, y 设置添加文本的位置  
  labels #设置添加的文本内容
  mtext(text, side = 3, line = 0, ...)
  用于在绘图框边缘添加文本:#text 添加的文本内容  side 表示边框位   置，可选择1，2，3，4
eg:
  plot(1:10, main = "TEM.plot",sub="TEM~TERM",xlab = "TERM",ylab   =    "TERM " )
  text(6,4,labels = "RGraphics",srt=45,cex=1,font=2)
添加坐标轴:
  axis(side, at = NULL, labels = TRUE, ...)#side     添加坐标轴   的位置，可选值有1，2， 3，4  ;at :制定坐标轴上刻度出现的位置    ;labels :设定刻度出现位置的标签
eg:
  op=par()#将默认的par设置保存
  par(mfrow=c(2,2),mar=c(2,2,2,2))
  plot(1:12,rnorm(12),xlab = "",ylab = "",axes = F,pch=18,cex=1.5,col="green")
  axis(side = 2,at=seq(2,12,by=2),labels=LETTERS[1:6],col.axis="blue")
  box()
  plot(1:12, rnorm(12),  xlab="", ylab="", axes=F, pch=18, cex=1.5, col="green")
  axis(side=1, at=seq(2, 12, by=2), labels=LETTERS[1:6], col.axis = "red")
 axis(2)
 box()
 plot(1:12,rnorm(12),xlab = "",ylab = "",axes = F,pch=18,cex=1.5,col="green")
 axis(side = 1,at=seq(2,12,by=2),labels=LETTERS[1:6],col.axis="blue")
 box()
 plot(1:12, rnorm(12),  xlab="", ylab="", axes=F, pch=18, cex=1.5, col="green")
 axis(side=1, at=seq(2, 12, by=2), labels=LETTERS[1:6], col.axis = "red")
 axis(4)
 box()
 par(op) #回复默认的par设置
添加网格线：
  grid(nx = NULL, ny = nx,...)#nx 和 ny 分别表示横纵轴上网格线的条数
eg:
  plot(300,type="n",xlab = "",ylab = "",xlim = c(1,600),ylim=c(1,400),xaxs="i",yaxs="i")
  grid(nx=30,ny=20,col="lightgreen")
  grid(nx=6,ny=4,col = "black",lty=1,lwd=2)
  box(lwd=2)
添加图例:
  legend( x, y = NULL, legend, fill , lty, pch, lwd, angle, density, bty="n"...)#x  y   表示图例的坐标位置（左上角顶点的坐标）
  legend   # 设置图例中的文字说明
  lty、lwd和 pch  #指定图例中点线的样式，设置与图中所画的图一致
  fill、angle和density  #用于填充类的图例设置，如绘图内容为条形图时用于条形图的颜色、阴影线、线的倾斜度等的设置
  bty   #设置图例框的样式， 通常设置为"n"，不绘制边框
eg:
  plot(1:8,type = "n",xlim = c(0,10))
  segments(c(2,6),c(0,0),c(4,8),c(5,7),lty = c(1,2),lwd=2)
  legend(x=0,y=8,legend=c("upwards", "downwards"),fill=2:3,density=5,lty=1:2,bty="n")
source("https://install-github.me/dreamRs/esquisse")#作图插件
饼图 pie( )
  pie(x, labels = names(x), ...)#x:数值向量labels:设置标签
eg:
  pie.sales=c(0.12,0.3,0.26,0.16,0.04,0.12)
  names(pie.sales)=c("A","B","C","D","E","F")
  pie.col=c("purple","violetred1","green3","cornsilk","cyan","white")
  pie(x=pie.sales,labels=names(pie.sales)#可以不写,col = pie.col)
直方图hist( )
  hist(x, breaks = "Sturges", freq = NULL,probability = !freq, ...)#x为欲估计分布的数值向量；
  breaks  # 设置计算分段区间的方法，可以设置为数字，或其他 （这个参数决定了直方图的形状）
  freq  和 probability # 取逻辑值T或者F，前者决定是否以频数作图，后者决定是否以概率密度作图，两者互斥
eg：
  par(mfrow=c(2,2),mar=c(2,3,2,0.5))
  data(geyser,package="MASS")
  hist(geyser$waiting,main="(1)freq=TRUE",xlab = "waiting",density = -10,col = "red",breaks =40)
条形图barplot( )
  barplot(height, width = 1, space = NULL, names.arg = NULL, legend.text = NULL, beside = FALSE, horiz = FALSE, ...)
  height# 重要参数，它指定了条图的长度，这个参数可以接受数值向量或者一个数据矩阵（矩阵的列代表变量，行代表不同观测值）
  beside   当height为矩阵时起作，beside为TRUE，条图并排排列，设为FALSE为堆砌排列
  width可以设置条的宽度；space用以设置条之间的间距；
  names.arg为条形图的标签
  legend.text  用来添加图例；
  horiz用以设置条形图的方向（水平或垂直）
eg:
  par(mfrow=c(4,4),mar=c(2,3,2,0.5))
  barplot(height = 1:3)
  barplot(height=1:3, col=2:4, space=0.5)  
  barplot(height=1:3, col=2:4, space=0.5, horiz=T)
  barplot(height = 1:3,col = 1:3,space = 0.5,horiz=T,names.arg = LETTERS[1:3])
  barplot(height = matrix(runif(6),2,3))
  barplot(height=matrix(runif(6), 2,3), beside=FALSE, col=2:3)   
  barplot(height = matrix(runif(6),2,3),beside = FALSE,col = 3:6,ylim = c(0,2.3),legend.text = c("blue","green"))
箱图boxplot( )
  #x 或 formula   设置绘图的向量或是一个表达式
  horizontal为TURE 或者FALSE，设定箱线图是否水平放置
eg:
  data=data.frame(x=rnorm(12),y=rep(1:3,each=4))
  par(mfrow=c(2,2),mar=c(2,3,2,0.5))
  boxplot(data$x)
  boxplot(data$x~data$y)
  boxplot(data$x~data$y, col=2:4)
  boxplot(data$x~data$y, col=2:4, horiz=TRUE)
等值线图contour( ) 
  contour(x,  y , z,  nlevels = 10, method, ...)
  #x  和 y  为向量，代表矩阵数据的的坐标刻度（纵横交叉组合成z的坐标），注意他们不代表z矩阵数据所对应的坐标
  z   为输入的数据矩阵，代表数据大小，比如海拔高度  （设置x y z 时需要小心，以防错位）
  nlevels  设定等高线的条数默认为10，控制疏密
eg:
  load(file.choose())
  par(mfrow=c(1,2), mar=c(2,3,2,0.5))
  contour(x=1:30*20,y=1:15*20,z=data,xaxs="i",yaxs="i")
  contour(x=1:30, y=1:15, z=data, xaxs="i", yaxs="i", nlevels=20,  col="red")  
 颜色图 image( )  和image.plot()
  image(x, y, z,...)
  #主要的参数设置大致与contour函数相同
  image.plot() 是fields 包中的一个函数，优点是有图例
eg:
  par(mfrow=c(1,2), mar=rep(1,4),ann=F#高水平绘图函数会调用函数plot.default使对坐标轴名称、整体图像名称不做任何注解)
  image(x=1:30*20, y=1:15*20, z=data, col=terrain.colors(100), axes=F)
  contour(x=1:30*20, y=1:15*20, z=data, nlevels=20, lwd=1.5, add=T#将等高线叠加在上面)  
  box(lwd=2)
一页多图 :
  mfrow #以行为顺序画图
  mfcol #以列为顺序画图
 ?layout()  #查看layout 的帮助文件  
  图形输出
    1:pdf(file="output.pdf")   #输出文件名为output 的pdf文件
    plot(1:10)   #绘图
    dev.off()   #关闭pdf 绘图设备, 输出的图形请在工作路径下查找
  
    2:jpeg(filename="output.jpg")  #输出文件名为output 的jpg文件
    plot(1:10)  #绘图
    dev.off()   #关闭jpeg绘图设备, 输出的图形请在工作路径下查找
添加error bar--方案1 :
  install.packages("sciplot")
  library(sciplot)
example("bargraph.CI")  
人图交互:
  1. 通过在图形内点击鼠标获得鼠标点击处的坐标：
locator(n , type = "n", ...)           
  #n  设置鼠标点击的次数      type为点击鼠标之后生成的图形类型
2. 通过鼠标点击一幅散点图识别鼠标周围的数据点，并给辨识出的数据添加标签
identify(x, y=NULL, lables=seq_along(x), n=length(x)...)
  #x，y 为坐标数据      lables 为要显示出的标签，默认的为数值顺序  n 设置点击的次数
eg:
  plot(1)
  locator(n=5,type = "n",cex=2,pch=15,col="green")
  
  data=data.frame(a=c(1,2,3,4,5),b=c("A","B","C","D","E"))
  plot(data$a)
  identify(x=data$a,labels = data$b,col="red",cex=2)
q()