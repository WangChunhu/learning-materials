bao1 <- c( "rgdal", "tmap")
sapply(bao1, require, character.on = T)

# 加载降雨数据
## gzcon提供了一个修改后的连接，它包装了一个现有的连接，并通过该连接解压读取或压缩写入。假设使用标准gzip头。
### 开VPN
# z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
p <- readRDS("D:/学习/key/油菜/插值/练习/precip.rds")
p
plot(p)

# 加载德克萨斯州布达里地图
# z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds"))
t <- readRDS("D:/学习/key/油菜/插值/练习/texas.rds")
t
plot(t)

# 将点边界范围替换为德克萨斯州的点边界范围
p@bbox <- t@bbox

tm_shape(t) + tm_polygons() +
  tm_shape(p) +
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = F,
          title="Sampled precipitation /n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

# 插值
# 1 泰森多边形
bao2 <- c( "spatstat", "maptools", "raster")
sapply(bao2, require, character.on = T)
# 创建细分曲面
## 尝试将任何合理类型的数据强制为空间点模式;计算空间点图案的Dirichlet细分
th  <-  as(dirichlet(as.ppp(p)), "SpatialPolygons") 
th
plot(th)
## dirichlet函数不携带投影信息,手动添加此信息
proj4string(th) <- proj4string(p)
## 细分曲面不存储属性信息,使用over（）函数（来自sp package）通过将点属性连接到细分曲面
## 点、网格和多边形的一致空间覆盖：在对象x的空间位置，从空间对象y检索索引或属性
th.z     <- over(th, p, fn=mean) 
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
## 最后，我们将把细分曲面剪裁到德克萨斯边界
th.clp   <- raster::intersect(t,th.spdf)
plot(th.clp)
## 画图
tm_shape(th.clp) + 
  tm_polygons(col="Precip_in", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted precipitation /n(in inches)") +
  tm_legend(legend.outside=TRUE)

# 2 IDW 反向距离加权,以插值点与样本点间的距离为权重进行加权平均，离插值点越近的样本点赋予的权重越大
bao3 <- c( "gstat", "sp")
sapply(bao3, require, character.on = T)

# 创建一个5万个格点的空的网格
grd <- as.data.frame(spsample(p, "regular", n=50000))
plot(grd)
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y") # 获取光栅对象的行、列或单元号的光栅单元中心坐标
gridded(grd) <- T # Create SpatialPixel object
fullgrid(grd) <- T # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(p) <- proj4string(p) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(p)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
p.idw <- gstat::idw(Precip_in ~ 1, p, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(p.idw)
r.m     <- mask(r, t)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation /n(in inches)") + 
  tm_shape(p) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# 3微调插值
# 保留交叉验证
## 从数据集中删除一个数据点，并使用数据集中的 *所有其他 *点插值其值，然后为该数据集中的每个点重复此过程（同时确保该过程插值参数在每个插值中保持恒定）。然后将插值值与省略点的实际值进行比较。
IDW.out <- vector(length = length(p))
for (i in 1:length(p)) {
  IDW.out[i] <- idw(Precip_in ~ 1, p[-i,], p[i,], idp=2.0)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ p$Precip_in, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ p$Precip_in), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

# Compute RMSE 均方根误差
sqrt( sum((IDW.out - p$Precip_in)^2) / length(p))

# Cross-validation 交叉验证
## 除了生成插值曲面外，还可以创建插值模型的95%置信区间图。在这里，我们将从使用功率参数2（idp=2.0）的IDW插值创建95%CI图
# 刀切法 jackknife：留一交叉验证法，从样本中剔除一个，估计偏差与方差
img <- gstat::idw(Precip_in~1, P, newdata=grd, idp=2.0, nmin=10,nmax=15)
n   <- length(P)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)
# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(Precip_in~1, P[-i,], newdata=grd, idp=2.0,nmax=15)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}
# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )
# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the subtraction
c1 <- apply(c1^2, 1, sum, na.rm=T ) # sum the square of the subtraction
# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)
# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 
# Plot the data
r <- raster(img.sig, layer="v")
r.m <- mask(r, W)
tm_shape(r.m) + tm_raster(n=7, title="95% confidence interval (in inches)") +tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# 4 克里金
# 首先，我们需要创建一个变异函数模型。注意，变异函数模型是根据去趋势数据计算的。通过将一阶趋势模型
#  定义一阶多项式方程
f.1 <- as.formula(Precip_in ~ X + Y) 
# Add X and Y to p
p$X <- coordinates(p)[,1]
p$Y <- coordinates(p)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=p)

# Compute the sample variogram; note that the f.1 trend model is one of the parameters passed to variogram()
var.smpl <- gstat::variogram(f.1, p, cloud = FALSE, cutoff=1000000, width=89900)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000000))

# Next, use the variogram model dat.fit to generate a kriged interpolated surface. The krige function allows us to include the trend model thus saving us from having to de-trend the data, krige the residuals, then combine the two rasters. Instead, all we need to do is pass krige the trend formula f.1.

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, p, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, t)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted precipitation /n(in inches)") +
  tm_shape(p) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# 生成方差
# The dat.krg object stores not just the interpolated values, but the variance values as well. These can be passed to the raster object for mapping as follows:
  
r <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, t)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map /n(in squared inches)") +tm_shape(p) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# 置信区间图
r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, t)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map /n(in inches)") +tm_shape(p) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


require(magrittr)
require(data.table)
# 练习数据
p1 <- openxlsx::read.xlsx("D:\\学习\\key\\油菜\\Input information-rapeseed 29 Jul.xlsx") %>% data.table() %>% .[,c(1:5,9)]
str(p1)
# 创建SpatialPointsDataFrame
p2 <- sp::SpatialPointsDataFrame(coords = p1[,4:5] %>% setcolorder(c(2L, 1L)), data = p1[,5])
p <- p2

t <- maptools::readShapePoly("E:/学习/以前/key/chinaMap/china.shp")
t <- t[t$AREA %in% c(17.563, 9.736, 0.091, 13.365 , 45.534, 7.716, 9.277, 19.390, 15.277, 34.276, 15.974, 16.135), ]
plot(t)

# t1.part <- t1[省市 %in% c("四川省", "重庆市", "湖北省", "安徽省", "江苏省", "云南省", "湖南省", "江西省", "浙江省")]


# 将点边界范围替换为德克萨斯州的点边界范围
p@bbox <- t@bbox

pdf("D:/学习/key/油菜/插值/点图.pdf", width = 15,height = 10)
tm_shape(t) + tm_polygons(col = "white") +
  tm_shape(p) +
  tm_dots(col = "K.amount", palette = "RdBu", auto.palette.mapping = F,
          title = "Sampled precipitation /n(in inches)", size = 0.1) +
  # tm_text("K.amount", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside = T)
dev.off()

# 插值
# 1 泰森多边形
bao2 <- c( "spatstat", "maptools", "raster")
sapply(bao2, require, character.on = T)
# 创建细分曲面
## 尝试将任何合理类型的数据强制为空间点模式;计算空间点图案的Dirichlet细分
th  <-  as(dirichlet(as.ppp(p)), "SpatialPolygons") 
th
plot(th)
## dirichlet函数不携带投影信息,手动添加此信息
proj4string(th) <- proj4string(p)
## 细分曲面不存储属性信息,使用over（）函数（来自sp package）通过将点属性连接到细分曲面
## 点、网格和多边形的一致空间覆盖：在对象x的空间位置，从空间对象y检索索引或属性
th.z     <- over(th, p, fn=mean) 
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
## 最后，我们将把细分曲面剪裁到德克萨斯边界
th.clp   <- raster::intersect(t,th.spdf)
plot(th.clp)
## 画图
pdf("D:/学习/key/油菜/插值/泰森多边形.pdf", width = 15,height = 10)
tm_shape(th.clp) + 
  tm_polygons(col="K.amount", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted precipitation /n(in inches)") +
  tm_legend(legend.outside=TRUE)
dev.off()

# 2 IDW 反向距离加权,以插值点与样本点间的距离为权重进行加权平均，离插值点越近的样本点赋予的权重越大
bao3 <- c( "gstat", "sp")
sapply(bao3, require, character.on = T)

# 创建一个50万个格点的空的网格
grd <- as.data.frame(spsample(p, "regular", n=500000))
plot(grd)
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y") # 获取光栅对象的行、列或单元号的光栅单元中心坐标
gridded(grd) <- T # Create SpatialPixel object
fullgrid(grd) <- T # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(p) <- proj4string(p) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(p)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
p.idw <- gstat::idw(K.amount ~ 1, p, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(p.idw)
r.m     <- mask(r, t)

# Plot
pdf("D:/学习/key/油菜/插值/IDW.pdf", width = 15,height = 10)
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation /n(in inches)") + 
  tm_shape(p) + tm_dots(size=0.03) +
  tm_legend(legend.outside=TRUE)
dev.off()

# 底图数据
require(raster)
require(stringr)
require(data.table)
require(magrittr)
require(ggplot2)
yc <-  shapefile("D:/学习/key/油菜/降尺度/r_point_ConvertCoordinateNot1.shp")
class(yc) # SpatialPointsDataFrame

# raster::crs(yc) # 读取栅格对象的投影信息
yc.data <- yc@data %>% data.table() %>% .[,c("DDLat", "DDLon") := list(str_remove(DDLat , "N"), str_remove(DDLon, "E"))] %>%  .[,area := 0.5 * 0.5 * as.numeric(grid_code)] 

# ggplot(yc.data,aes(DDLon, DDLat, color = area))+
#   geom_point(size = .0001)+
#   theme_classic()

yc.data$DDLat <- as.numeric(yc.data$DDLat)
yc.data$DDLon <- as.numeric(yc.data$DDLon)

# str(yc.data)
# 创建SpatialPointsDataFrame
p <- sp::SpatialPointsDataFrame(coords = yc.data[,5:6] %>% setcolorder(c(2L, 1L)), data = yc.data[,9]) # 插值点
p
# plot(p)

china.map <- maptools::readShapePoly("E:/学习/以前/key/chinaMap/china.shp") # 插值底图
china.part.map <- china.map[china.map$AREA %in% c(17.563, 9.736, 0.091, 13.365 , 45.534, 7.716, 9.277, 19.390, 15.277, 34.276, 15.974, 16.135), ]
plot(china.part.map)

t <- maptools::readShapePoly("D:/学习/key/油菜/降尺度/youCai_Output.shp")
plot(t)

# 将点边界范围替换为德克萨斯州的点边界范围
p@bbox <- t@bbox

require(tmap)
# pdf("D:/学习/key/油菜/插值/点图.pdf", width = 15,height = 10)
# tm_shape(t) + tm_polygons(col = "white") +
#   tm_shape(p) +
#   tm_dots(col = "area", palette = "RdBu", auto.palette.mapping = F,
#           title = "Sampled precipitation /n(in inches)", size = 0.01) +
#   # tm_text("grid_code", just="left", xmod=.5, size = 0.7) +
#   tm_layout(legend.outside = T)
# dev.off()

# 2 IDW 反向距离加权,以插值点与样本点间的距离为权重进行加权平均，离插值点越近的样本点赋予的权重越大

# 反距离权重 (IDW) 插值：彼此距离较近的事物要比彼此距离较远的事物更相似。当为任何未测量的位置预测值时，反距离权重法会采用预测位置周围的测量值。与距离预测位置较远的测量值相比，距离预测位置最近的测量值对预测值的影响更大。反距离权重法假定每个测量点都有一种局部影响，而这种影响会随着距离的增大而减小。由于这种方法为距离预测位置最近的点分配的权重较大，而权重却作为距离的函数而减小，因此称之为反距离权重法

bao3 <- c( "gstat", "sp")
sapply(bao3, require, character.on = T)

# 创建一个5万个格点的空的网格
grd <- as.data.frame(spsample(p, "regular", n=500000))
# plot(grd)
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y") # 获取光栅对象的行、列或单元号的光栅单元中心坐标
gridded(grd) <- T # Create SpatialPixel object
fullgrid(grd) <- T # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(p) <- proj4string(p) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(p)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
p.idw <- gstat::idw(formula = area ~ 1,
                    locations = p,
                    # nmin = 80,
                    # maxdist = 1,
                    newdata = grd,
                    idp = 2.0) # idp:指定反距离加权功率

result <- as.data.frame(p.idw) # 结果转为数据框
# Convert to raster object then clip to Texas
r       <- raster(p.idw)
r.m     <- mask(r, t)

# Plot
pdf("D:/学习/key/油菜/插值/IDW.面积.pdf", width = 15,height = 10)

tm_shape(china.map) + # 地图图层
  tm_polygons(col = "white") +
  tm_shape(r.m) + 
  tm_raster(n = 8,
            palette = "Blues", 
            auto.palette.mapping = FALSE,
            title = "Predicted precipitation /n(in inches)",
            legend.reverse = T) + 
  tm_shape(p) +
  tm_dots(size=0.00) +
  tm_legend(legend.outside=TRUE)
dev.off()

# # 筛选
# # 提取每一个栅格的坐标值
# r.m.col <- xFromCol(r.m)
# r.m.row <- yFromRow(r.m)
# # 将各个坐标扩展成二维坐标点
# r.m.cell <- expand.grid(r.m.col,r.m.row)
# # 获取各个栅格点数值
# r.m.cell$z <- values(r.m)
# # 剔除地图周边的空值
# r.m.hit <- r.m.cell[!is.na(r.m.cell$z),]
# # 重命名
# names(r.m.hit) <- c("Longitude", "Latitude", "value")

# 调色板
# tmaptools::palette_explorer()

