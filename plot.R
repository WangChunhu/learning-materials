library(openxlsx)
dat <- read.xlsx("H:/�ۺ�/��ʦ��/xgfx.xlsx")

par(bg="NA",bty="l")
par(cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,cex.sub=1.5)  #��bty����o,l,7,c,u��;cexͼ��Ԫ�أ�
par(col="green",col.lab="brown",col.axis="red",col.main="pink",col.sub="purple")
par(font=1,font.axis=2,font.lab=3,font.main=4,font.sub=1) #1�������壬2�Ӵ֣�3ϸб�壬4��б��
par(family="serif") #family�ı��������壬��׼ȡֵ�У�serif, sans, mono, symbol�����ߡ��޳��ߡ��ȿ����������壩
par(las=1) #�������ǩ��ʽ��ȡ0��1��2��3�ĸ�����֮һ����ʾ������ƽ���������ᡱ��������ˮƽ���������Ǵ�ֱ�������ᡱ�͡�������ֱ��
par(pch=15) #��1��25�����ֱ�ʾ��21��25���������ɫ����Ҳ�����������ַ���ʾ��
par(lty=2,lwd=3) #lty ������ʵ��ʽ��������Ϊ��0  �����ߣ�1  ʵ�ߣ�2 ���ߣ�3 ���ߣ�4  �㻮�ߣ�5 �����ߣ�6 �㳤���ߣ�lwd �������ȣ�Ĭ��Ϊ1
par(srt=45)#srt �ַ�������ת�Ƕȣ�ȡһ���Ƕ���ֵ
plot(1,type="n")#typetype= 'p ' ��ͼ����������ʾΪ��
type= 'l ' ��ͼ����������ʾΪ��
type= 'b ' ��ͼ����������ʾΪ���������

type= 'o ' ��ͼ�������ݵ㸲��������
type= 'h ' ��ͼ����������ʾΪ�ӵ㵽x��Ĵ�ֱ��
type= 's ' ��ͼ����������ʾΪ����ͼ
type= 'n ' ��ͼ�������ݲ���ʾ
text(1,1,"R",cex = 2,srt=45,font=2)#�����ı�
par(mfrow=c(2,2),mar=c(2,2,2,2))#mfrow��������mar���ܼ��
par(xaxs="r",yaxs="i")#Ĭ��Ϊ'r' ���Ȱ�ԭʼ���ݵķ�Χ��������4%��Ȼ���������Χ�������᣻����һ��ȡֵ'i'��ʾֱ��ʹ��ԭʼ���ݷ�Χ
plot(5, xlim=c(0,10), ylim=c(0,10), type="n",bty="l") #���Ƶ�ͼ
points(data$x,data$y,pch=6,col="blue",cex=3)#���ӵ�����ݿ�
abline(a/b/h/v/reg="NULL")#a �ؾ࣬b б�ʣ�h�ǻ�ˮƽ��ʱ������ֵ��v �ǻ���ֱ��ʱ�ĺ���ֵ��reg ��һ�����ú���coef()��ȡϵ��������б�ʺͽؾࣩ��R��������ȡ���Իع����ɵĶ���
segments( x0, y0, x1, y1,...)#�����߶� segments( )
rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty, lwd...)#���Ӿ��� rect( ) 
box(...) # ר���ڸ�ͼ��������߿�
line#���� �ı���ͼ�α�Ե�ľ���
> mtext(text="R side text", side=4)  #�ڻ�ͼ���������ı�,side=1,2,3,4,��ʾ��ͬ�ı�
rnorm(n,nean=0,sd=1)#�����������nΪ����
axis(side, at = NULL, labels = TRUE, ...)#����������
grid(nx = NULL, ny = nx,...)#����������


plot(1:10,xlab = "AGB",ylab = "Species",main="SP~AGB",sub = "GTSplot")#asp ͼ���ݺ������y/x;xlim, ylim ��������ϵ�Ľ���;log   �����Ƿ�ȡ������T��F;axes �Ƿ������ᣬT��F  ; frame.plot �Ƿ��ͼ�μӿ�T��F




