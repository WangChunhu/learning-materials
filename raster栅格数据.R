bao <- c("raster", "data.table", "magrittr", "terra")
sapply(bao, require, character.on = T)

system.file("external/test.grd", package = "raster") # 寻找包中文件的绝对路径

# 读取栅格数据
test <- raster::raster("C:/Users/huhu/AppData/Local/R/win-library/4.2/raster/external/test.grd")
test
plot(test)

# 创建栅格数据
x1 <- raster::raster() # 默认情况下创建全球范围1*1经纬度的栅格对象
x1

x2 <- raster::raster(nrows = 18, ncols = 36, xmn = -900, xmx = 900, ymn = -900, ymx = 900) # 指定维度与数据范围
x2

x3 <- raster::raster(xmn = -900, xmx = 900, ymn = -900, ymx = 900, resolution = c(50,500)) # 指定数据范围与分辨率(x,y)
x3

x4 <- raster::raster(nrows = 18, ncols = 36, resolution = c(50,500)) # 指定维度与分辨率
x4

x5 <- raster::raster(nrows = 18, ncols = 36, ext = extent(x2)) # 指定数据范围,调用ext参数可以和已知的栅格数据的范围保持一致
x5

# 多图层栅格数据
r1 <- raster(ncols = 3, nrows = 3, vals = c(1:9))
r2 <- raster(ncols = 3, nrows = 3, vals = rep(c(0,1,2), 3))
r <- raster::stack(r1, r2)
r
plot(r1)
plot(r2)
plot(r)

# 代数运算
raster::getValues(r1) # 获取栅格的数值
raster::getValues(r2 + 10) # 加减乘除、开方等运算都可以适用于栅格数据
r3 <- raster::getValues(r1 * r2)
plot(r3)
plot(r1 >= 3 & r1 < 6) # r1中数值大于等于3且小于6的栅格
plot(mean(r1, r2))

# 可视化
raster::plot(test) # 和base效果一样
par(mfrow = c(2,2), plt = c(0.1,0.95,0.15,0.9))
contour(test)
persp(test)
hist(test)
density(test)

# R矢量地图栅格化（将shapefile转换成raster）
shape = shapefile(system.file("external/lux.shp", package="raster"))
r = raster(shape, res=0.005)    
shape_r = rasterize(shape, r, 1)
plot(shape_r)
plot(shape, add=T)

# 变值
par(mfrow=c(1,2))
# value= ID_2
shape_r = rasterize(shape, r, "ID_2")
plot(shape_r)
plot(shape,add=T)
title(main="value=ID_2")
shape_r
# value= AREA
shape_r = rasterize(shape, r, "AREA")
plot(shape_r)
plot(shape,add=T)
title(main="value=AREA")
shape_r

# NA处理
shape_r = rasterize(shape, r, "ID_2")
par(mfrow=c(1,2))
shape_rc=reclassify(shape_r,cbind(NA,0),right=F)
plot(shape_r)
title(main="value=ID_2")
plot(shape_rc)
title(main="NA==0")

# 数值提取
# ponits
par(mfrow=c(1,1))
df=tibble(x=c(6.1,5.9),
          y=c(49.7,49.9))
df_sp=df
coordinates(df_sp) <- ~ x + y 
proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
#plot
plot(shape)
plot(df_sp,add=T,col="red")

# extract value
extract(shape_r,df_sp)
over(df_sp,shape)

### 重采样(连续变量：bilinear ; 类别变量：ngb)
eco.1km <- projectRaster(eco, res = 1000, method = "ngb")
rice.10000 <- projectRaster(rice, res = 0.08983153, method = "ngb", crs = "+proj=longlat +datum=WGS84 +no_defs")

# 提高分辨率
##  聚合至5000m ==》 good
union.aggre.5000 <- aggregate(shape_r, fact = 5)


par(mfrow=c(1,1))
r = raster(shape, res=0.0005)    
shape_r = rasterize(shape, r, "ID_2")
plot(shape_r)
plot(shape,add=T)
title(main="Res=0.0005")

# 数据聚合,尺度变换
## 返回光栅*或空间*对象（或范围对象）的范围对象，或从2x2矩阵（第一行：xmin，xmax；第二行：ymin，ymax）、向量（长度=4；顺序=xmin，xmax，ymin，ymax）或列表（至少有两个元素，名称为“x”和“y”）创建范围对象
ext <- raster::extent(0,1000,0,1000)
rs <- raster(nrows=1000, ncols=1000,ext)
rs[] <- sample(seq(from = 1, to = 10, by = 1), size = 1e+6, replace = TRUE)
plot(rs)
zonal_fun <- function(in_rs, na.rm=TRUE){
  ## Args:
  ## in_rs:a raster layer
  
  re <- table(in_rs) %>% prop.table() %>% .['3'] 
  return(re)
}

re <- aggregate(rs, fact=100, fun = zonal_fun,na.rm=T)
plot(re)

re <- aggregate(rs, fact=100, fun = sum,na.rm=T)
plot(re)

## 实战
china_mapdata <- openxlsx::read.xlsx("E:/学习/以前/key/chinaMap/china_mapdata.xlsx") %>% data.table()
china.map <- ggplot(data = china_mapdata, aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "white")+
  geom_path(color = "grey")+
  theme(panel.background = NULL)

shape = shapefile("D:/学习/key/油菜/降尺度/r_point_ConvertCoordinateNot1.shp")
shape
raster::crs(shape) # 读取栅格对象的投影信息
shape.data <- shape@data
r = raster(shape, res = 0.01) # 
r
shape_r = raster::rasterize(shape, r, rnorm(n = 146237, mean = 5, sd = 1.5))
plot(shape_r)

# youcai <- openxlsx::read.xlsx("D:/学习/key/油菜/降尺度/Input information-rapeseed.xlsx")
# ext <- extent(min(youcai$Long), max(youcai$Long), min(youcai$Lat), max(youcai$Lat))
# rs <- raster(nrows=1000, ncols=1000,ext)
# rs[] <- sample(seq(from = 1, to = 10, by = 1), size = 1e+6, replace = TRUE)
# plot(rs)
re <- raster::aggregate(shape_r, fact = c(20,20), fun = sum,na.rm = T)
plot(re)
# str(shape.data)
# shape.data$DDLat <- as.numeric(shape.data$DDLat %>% str_remove_all("N"))
# shape.data$DDLon <- as.numeric(shape.data$DDLon %>% str_remove_all("E"))
# plot(shape.data$DDLon, shape.data$DDLat)

# youcai.map <- ggplot(data = shape.data, aes(x = DDLon, y = DDLat))+
#   geom_point(size = .8)+
#   theme(panel.background = NULL)
china.youcai.map <- china.map + 
  geom_point(data = shape.data, aes(x = DDLon, y = DDLat), size = .0005, group = 1, color = "SeaGreen4")
china.youcai.map




# library(raster)
#get some sample data
data(meuse.grid)
gridded(meuse.grid) <- ~x+y
meuse.raster <- raster(meuse.grid)
res(meuse.raster)
#[1] 40 40

#aggregate from 40x40 resolution to 120x120 (factor = 3)
meuse.raster.aggregate <- aggregate(meuse.raster, fact=3)
res(meuse.raster.aggregate)
#[1] 120 120

#disaggregate from 40x40 resolution to 10x10 (factor = 4)
meuse.raster.disaggregate <- disaggregate(meuse.raster, fact=4)
res(meuse.raster.disaggregate)
#[1] 10 10


require(raster)
yc = shapefile("D:/学习/key/油菜/降尺度/r_point_ConvertCoordinateNot1.shp")
yc
# raster::crs(yc) # 读取栅格对象的投影信息
yc.data <- yc@data %>% data.table() %>% .[,area := 0.5 * 0.5 * as.numeric(grid_code)] %>% .[,c(3,4,9)]
data <- yc.data
gridded(yc.data) <- ~POINT_X+POINT_Y
yc.data <- raster(yc.data)
# 提取每一个栅格的坐标值
yc.data.col <- xFromCol(yc.data)
yc.data.row <- yFromRow(yc.data)
# 将各个坐标扩展成二维坐标点
yc.data.cell <- expand.grid(yc.data.col,yc.data.row)
# 获取各个栅格点数值
yc.data.cell$z <- values(yc.data)
# 剔除地图周边的空值
yc.data.hit <- yc.data.cell[!is.na(yc.data.cell$z),]
# 重命名
names(yc.data.hit) <- c("Longitude", "Latitude", "value")

res(yc.data)
par(mfrow = c(1,2))
plot(yc.data)

yc.data.aggregate <- aggregate(yc.data, fact = 2, fun = sum)
# 提取每一个栅格的坐标值
yc.data.aggregate.col <- xFromCol(yc.data.aggregate)
yc.data.aggregate.row <- yFromRow(yc.data.aggregate)
# 将各个坐标扩展成二维坐标点
yc.data.aggregate.cell <- expand.grid(yc.data.aggregate.col,yc.data.aggregate.row)
# 获取各个栅格点数值
yc.data.aggregate.cell$z <- values(yc.data.aggregate)
# 剔除地图周边的空值
yc.data.aggregate.hit <- yc.data.aggregate.cell[!is.na(yc.data.aggregate.cell$z),]
# 重命名
names(yc.data.aggregate.hit) <- c("Longitude", "Latitude", "value")

res(yc.data.aggregate)
plot(yc.data.aggregate)
par(mfrow = c(1,1))

yc.data@data@values %>% na.omit() %>% length() # 合并前点数
yc.data.aggregate@data@values %>% na.omit() %>% length() # 合并后点数


# china.map = shapefile("E:/学习/以前/key/chinaMap/china.shp")
# plot(china.map)
# china.map.raster <- raster(china.map)

# 投影转换
# -------------------------------------------------------------------------
## 矢量数据
# 如果数据中没有坐标系，则可以使用st_set_crs()函数来指定它的坐标系
data_proj <- st_transform(data,crs(data2))    #将data转换为data2的投影，并保存为data_proj
data_wgs <- st_transform(data,4326)  #将数据转换为WGS84地理坐标系

## 栅格数据
RasterData <- projectRaster(raster1,raster2)#将raster1的投影信息转换成与raster2一致
newproj <- "+proj=longlat +datum=WGS84 +no_defs" # "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84" #自定义目标投影方式的WKT文本
RasterData <- projectRaster(raster1, crs=newproj)  #用crs参数指定投影信息
wheat <- raster("D:/学习/课题/稻麦分布/冬小麦分布点栅格/2016-2019年中国冬小麦1000米分辨率种植分布并集.tif") %>% projectRaster(crs = "+proj=longlat +datum=WGS84 +no_defs")

## 
proj4string(species) <- projection(e)

# 空间数据存储 ------------------------------------------------------------------
## 矢量数据
st_write(area_wgs, "D:/Area_WGS84.shp")
write_sf(area_wgs, "D:/Area_WGS84.shp")

## 栅格数据
writeRaster(pre_s, filename = paste0("./Pre/MultiTif/", pre_s@ptr[["names"]], ".tif"))  #输出为单波段多个TIFF
writeRaster(pre_s, filename = "./Pre/pre_s.tif", names=pre_s@ptr[["names"]])  #输出为多波段一个TIFF


# 可视化 ggplot2 -------------------------------------------------------------
bao1 <- c("sf", "ggplot2", "ggspatial", "rasterVis")
sapply(bao1, require, character.on = T)

#加载区域的线数据
boder <- read_sf("D:/学习/地理数据/中国省级地图/CN-sheng-A.shp")
#对线数据进行坐标系转换
boder_wgs <- st_transform(boder,4326)
#将栅格数据转换成dataframe格式
df<- as.data.frame(as(soil,"Raster"), xy = T, na.rm = T)
#自定义颜色
colors <-c("#33A02C","#B2DF8A","#FDBF6F","#1F78B4","#999999")
ggplot()+
  #使用ggspatial包中的geom_tile()函数进行栅格数据可视化，用上图中的第三列进行fill填充
  geom_tile(data=df,aes(x=x,y=y,fill = layer))+
  #比例尺设置
  annotation_scale(location = "bl") +
  #指北针设置
  annotation_north_arrow(location="tr",
                         style =north_arrow_nautical(
                           fill =c("grey40","white"),
                           line_col ="grey20"))+ 
  #将行政边界叠加在栅格数据上
  geom_sf(data=boder_wgs,aes(fill = NULL))+
  #栅格颜色设置，将NA值的颜色设置为transparent透明
  scale_fill_gradientn(colours=colors,na.value="transparent")+
  theme_bw()+
  #主题设置
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())


# 土壤-物种点位图
#将栅格数据转换成dataframe格式
df<- as.data.frame(as(vegZone,"Raster"), xy = T, na.rm = T)

png("D:/学习/key/油菜/物种分布模型/废图/植被区划-物种点位图.png", width = 500, height = 500)
ggplot()+
  #使用ggspatial包中的geom_tile()函数进行栅格数据可视化，用上图中的第三列进行fill填充
  geom_tile(data=df,aes(x=x,y=y,fill = vegzone))+
  geom_sf(data = presence.sf, size = .1, color = "SeaGreen3")+
  geom_sf(data = absence.sf, size = .1, color = "red")+
  theme_bw()+
  #主题设置
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())
dev.off()

theme <-
  ggplot() +
  theme(text = element_text(family = "mono"),
        axis.line = element_line(color = "black", size = 0.2),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.ticks = element_line(colour = "grey"),
        
        legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.background = element_blank(),
        legend.title = element_text(face = "bold"),
        
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, size = 0.3),
        panel.grid.major = element_line(linetype = 2, colour = "grey", size = 0.5),
        
        plot.title = element_text(size = 12, hjust = 0.5,
                                  face = "bold"))

# 计算面积
## 计算多边形或非NA的光栅单元的面积。计算光栅单元的表面积与经纬度光栅特别相关，因为单元的大小以度为单位而不是以米为单位。
## “周长”方法仅适用于SpatVector对象，并计算直线的长度或多边形的周长。
## 如果坐标参考系是经度/纬度，则返回的值以平方米为单位表示面积，以米为单位表示周长。
## 在其他情况下，单位由坐标参考系设定。在大多数情况下，它也是米。
# raster::area()
require(terra)
r <- rast(nrow=18, ncol=36)
crs(r)
v <- 1:ncell(r)
v[200:400] <- NA
values(r) <- v
par(mfrow=c(2,1))
plot(r,main='随机例子')
# 每个光栅单元的面积
plot(p.mean[[2]])
p1 <- terra::rast(p.mean[[1]])
p2 <- terra::rast(p.mean[[2]])

# 将阈值作为分割点，上为1，下为NA
p11 <- p2
p11[] <- ifelse(p2[] >= 0.5, 1, NA)
plot(p11)
a <-  expanse(p11, 
              unit = "ha", 
              transform = F, # T:平面CRS转换为lon/lat以提高精度
              byValue = F # T:返回每个唯一单元格d的值
              )  
# cellSize()
a / 1000000 # M.ha
plot(a,main='每个像元代表的面积')


# 裁剪到中国
## 读取中国地图
china.map <- raster::shapefile("D:/学习/地理数据/中国省级地图/chinaMap/china.shp")

## 裁剪
china.bio.square <- terra::crop(x = bio, y = china.map)
plot(china.bio.square[[4]])

## 中国bio掩膜
china.bio <- terra::mask(x = china.bio.square, mask = china.map, inverse = F)
plot(china.bio[[12]])
#

# 读取或创建一个多层的raster数据集
## entend使气候及地理数据保持一致


# 点矢量图转栅格
a <- shapefile("D:/学习/key/油菜/物种分布模型/底图数据/presence.shp")
b <- raster::as.data.frame(a)
r = raster(a, res = 0.0045)    
shape_r = rasterize(a, r, b$grid_code)
plot(shape_r)
raster::writeRaster(x = shape_r, "D:/学习/key/油菜/物种分布模型/底图数据/presence.tif")

data <- raster::as.data.frame(union.sp)
r = raster(union.sp, res = 0.04491576) # 5000m 
union.5000.raster <- rasterize(union.sp, r, data$union.all.data)


# 栅格转点图---------------------------------------
tif <- raster("D:/学习/key/油菜/物种分布模型/底图数据/presence.tif") # 油菜种植区域
df <- data.table(grid_code = as.data.frame(tif$layer), coordinates(tif)) %>% na.omit()# presence.灰度
names(df) <- c("grid_code", "DDLon", "DDLat")
df[, c("site", "ORIG_OID", "pointid", "Shape *", "FID") := list(1, 0:(nrow(df) - 1), 1:nrow(df), "点", 0:(nrow(df) - 1))]
# head(df)


# 生成空间点对象
pre_abs <- SpatialPointsDataFrame(coords = data.table(df$DDLon, df$DDLat), data = data.frame(df$grid_code))
# sp转为sf
pre_abs.sf <- st_as_sf(x = pre_abs)
plot(pre_abs.sf, cex = .1)
# 写出sf
st_write(obj = pre_abs.sf, "C:/Users/huhu/Desktop/presence.add.shp", driver = "ESRI Shapefile")

