library(openxlsx)
dat <- read.xlsx("H:/综合/三师兄/xgfx.xlsx")

par(bg="NA",bty="l")
par(cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,cex.sub=1.5)  #“bty”（o,l,7,c,u）;cex图上元素；
par(col="green",col.lab="brown",col.axis="red",col.main="pink",col.sub="purple")
par(font=1,font.axis=2,font.lab=3,font.main=4,font.sub=1) #1常规字体，2加粗，3细斜体，4粗斜体
par(family="serif") #family文本的字体族，标准取值有：serif, sans, mono, symbol（衬线、无衬线、等宽、符号字体）
par(las=1) #坐标轴标签样式，取0、1、2、3四个整数之一，表示“总是平行于坐标轴”、“总是水平”、“总是垂直于坐标轴”和“总是竖直”
par(pch=15) #用1—25的数字表示（21—25可以填充颜色），也可以用任意字符表示。
par(lty=2,lwd=3) #lty 线条虚实样式可以设置为：0  不画线，1  实线，2 虚线，3 点线，4  点划线，5 长划线，6 点长划线；lwd 线条宽度；默认为1
par(srt=45)#srt 字符串的旋转角度，取一个角度数值
plot(1,type="n")#typetype= 'p ' 在图形中数据显示为点
type= 'l ' 在图形中数据显示为线
type= 'b ' 在图形中数据显示为点和连接线

type= 'o ' 在图形中数据点覆盖在线上
type= 'h ' 在图形中数据显示为从点到x轴的垂直线
type= 's ' 在图形中数据显示为阶梯图
type= 'n ' 在图形中数据不显示
text(1,1,"R",cex = 2,srt=45,font=2)#添加文本
par(mfrow=c(2,2),mar=c(2,2,2,2))#mfrow行列数；mar四周间距
par(xaxs="r",yaxs="i")#默认为'r' ：先把原始数据的范围向外扩大4%，然后用这个范围画坐标轴；另外一种取值'i'表示直接使用原始数据范围
plot(5, xlim=c(0,10), ylim=c(0,10), type="n",bty="l") #绘制底图
points(data$x,data$y,pch=6,col="blue",cex=3)#添加点或数据框
abline(a/b/h/v/reg="NULL")#a 截距，b 斜率，h是画水平线时的纵轴值，v 是画垂直线时的横轴值，reg 是一个能用函数coef()提取系数（包含斜率和截距）的R对象，如提取线性回归生成的对象
segments( x0, y0, x1, y1,...)#添加线段 segments( )
rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty, lwd...)#添加矩形 rect( ) 
box(...) # 专用于给图形添加外边框
line#设置 文本与图形边缘的距离
> mtext(text="R side text", side=4)  #在绘图框外添加文本,side=1,2,3,4,表示不同的边
rnorm(n,nean=0,sd=1)#产生随机数，n为个数
axis(side, at = NULL, labels = TRUE, ...)#添加坐标轴
grid(nx = NULL, ny = nx,...)#添加网格线


plot(1:10,xlab = "AGB",ylab = "Species",main="SP~AGB",sub = "GTSplot")#asp 图形纵横轴比例y/x;xlim, ylim 设置坐标系的界限;log   坐标是否取对数，T或F;axes 是否画坐标轴，T或F  ; frame.plot 是否给图形加框，T或F





