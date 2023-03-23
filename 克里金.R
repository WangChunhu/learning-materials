setwd("H:/综合/小波/实部")
加载包#---- 
library(sp)
library(gstat)
library(openxlsx)
library(lattice)
library(data.table)
library(dplyr)
require(ggplot2)
#----
cjly_qing <- read.xlsx("各省份年度_全部高温系数实部.xlsx",sheet = 2) %>% setDT()
cjly <- melt.data.table(data = cjly_qing,id.vars=c("nian"),variable.name="chidu",value.name="xishu")
setorder(x = cjly,"nian")
#转换为sp格式, 需要指定坐标
as.numeric(cjly$nian) -> cjly$nian
as.numeric(cjly$chidu) -> cjly$chidu

coordinates(cjly) <- c("nian" ,"chidu")
class(cjly)
spplot(obj = cjly,"xishu")
xgrid <- seq(min(cjly$nian), max(cjly$nian),by=0.09) 
ygrid <- seq(min(cjly$chidu), max(cjly$chidu),by=0.09) 
basexy <- expand.grid(xgrid, ygrid)
colnames(basexy) <- c("x", "y")
coordinates(basexy) <- ~x+y
gridded(basexy) <- TRUE
vgm1 <- variogram(xishu~1, cjly)
plot(vgm1, plot.numbers = TRUE) 
m <- fit.variogram(vgm1,vgm(.01,"Sph",32,0.002))
plot(vgm1, model=m)
krige_res <- krige(xishu~1,cjly, basexy, model = m)
spplot(krige_res, zcol = "var1.pred", col.regions = terrain.colors(100000), scales = list(draw =T),xlab="Year",ylab="Time scale/a")
system("cmd /c F:/音乐/你的承诺.mp3")
