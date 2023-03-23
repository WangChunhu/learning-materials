# 加载基础包
## 包名
bao <- c("data.table", "magrittr", "stringr", "ggplot2", "ggspatial", "sf", "openxlsx")
## 加载
sapply(bao, require, character.on = T)

## 中国地图路径
china <- sf::st_read("D:/学习/地理数据/中国省级地图/chinaMap/china.shp") # 省名乱码
china.mapdata <- fread("D:/学习/地理数据/中国省级地图/chinaMap/chinaMap.txt")
## 替换china的省名
china$NAME <- china.mapdata$NAME
## 九段线，南海岛屿地图，黄河和长江
nine <- sf::read_sf("D:/学习/地理数据/中国省级地图/九段线/SouthSea/九段线.shp") 
island <- sf::read_sf("D:/学习/地理数据/中国省级地图/九段线/SouthSea/bou2_4l.shp")
yellow.river <- sf::read_sf("D:/学习/地理数据/长江黄河/长江黄河/黄河.shp")
yz.river <- sf::read_sf("D:/学习/地理数据/长江黄河/长江黄河/长江.shp")

## 南海地图
pdf(file = "D:/学习/课题/稻麦分布/南海地图.pdf", width = 5, height = 10, family = "serif")
ggplot()+
  geom_sf(data = china, fill = "#f6ece5")+
  geom_sf(data = nine)+
  geom_sf(data = island)+
  coord_sf(xlim = c(108, 121), ylim = c(5, 22))+
  theme_test()+
  theme(axis.text = element_text(size = 15, colour = "black"))
dev.off()



## 提取长江流域五省市地图
yz <- china[china$NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), ]

## P1 稻麦分布
### 修改的话使用china.mapdata，再添加到china
china.mapdata[NAME %in% c("四川省", "重庆市", "湖北省", "江苏省"), color := "#99E64C"]
china.mapdata[NAME == "安徽省", color := "#BFF38C"]
china.mapdata[! NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#f6ece5"]
china$color <- china.mapdata$color

pdf("D:/学习/课题/稻麦分布/稻麦分布1.pdf", width = 10, height = 10, family = "serif")
# 绘制中国地图
ggplot() +
  geom_sf(data = china, fill = china$color, color = 'black') + # 中国地图轮廓
  geom_sf(data = nine, color = 'black')+
  geom_sf(data = island, color = 'black')+
  geom_sf(data = yellow.river, color = '#00CED1')+
  geom_sf(data = yz.river, color = '#00008B')+
  geom_sf(data = yz, fill = NA, color = "#8B0000", linewidth = 1)+
    #截取中国地图，不在大图上显示九段线区域
    coord_sf(ylim = c(19, 53)) + 
  # 加省会点
  annotate("point", x = 104.04, y = 30.67, colour = "#00008B", size = 2)+
  annotate("point", x = 107.33, y = 29.80, colour = "#00008B", size = 2)+
  annotate("point", x = 114.21, y = 30.30, colour = "#00008B", size = 2)+
  annotate("point", x = 117.17, y = 31.52, colour = "#00008B", size = 2)+
  annotate("point", x = 118.78, y = 32.07, colour = "#00008B", size = 2)+
  # 加省会名称
  annotate("text", x = 104.04, y = 30.32, label = "Chengdu", size = 3)+
  annotate("text", x = 107.33, y = 29.50, label = "Chongqing", size = 3)+
  annotate("text", x = 114.21, y = 29.95, label = "Wuhan", size = 3)+
  annotate("text", x = 117.17, y = 31.17, label = "Hefei", size = 3)+
  annotate("text", x = 120.10, y = 31.72, label = "Nanjing", size = 3)+
  # 加省名称
  annotate("text", x = 104.04, y = 31.50, label = "Sichuan", size = 4, color = "#0000CD")+
  annotate("text", x = 112.21, y = 31.00, label = "Hubei", size = 4, color = "#0000CD")+
  annotate("text", x = 117.17, y = 32.22, label = "Abhui", size = 4, color = "#0000CD")+
  annotate("text", x = 120.00, y = 33.50, label = "Jiangsu", size = 4, color = "#0000CD")+
  # 添加比例尺
  annotation_scale(location='bl', text_cex = 1) +
  # 添加指北针
  annotation_north_arrow(location = 'tr', which_north = 'false',
                         style = north_arrow_nautical())+
  labs(x = "", y = "")+
  # 设置大图边框
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 20))

dev.off()

pdf("D:/学习/课题/稻麦分布/稻麦分布_图例.pdf", width = 2, height = .05)
## 制作图例
legend <- data.table(x = 1:9, y = rep(1, 9), z = letters[1:9]); legend
ggplot(legend, aes(x, y, fill = z)) +
  geom_tile(height = 20)+
  theme_void()+
  guides(fill = "none")+
  scale_fill_manual(values = c("#cdcc00", "#e5e666", "#fdfeb3", "#e7ffcd", "#BFF38C", "#99E64C", "#66cc00", "#33b200", "#017f01"))

dev.off()

## P2 稻麦产量分布
### 修改的话使用china.mapdata，再添加到china
china.mapdata[NAME %in% c("四川省"), color := "#BFF38C"]
china.mapdata[NAME %in% c("重庆市"), color := "#e7ffcd"] #ffffff
china.mapdata[NAME %in% c("湖北省"), color := "#99E64C"]
china.mapdata[NAME == "安徽省", color := "#33b200"]
china.mapdata[NAME == "江苏省", color := "#66cc00"]

china.mapdata[! NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#f6ece5"]
china$color <- china.mapdata$color

pdf("D:/学习/课题/稻麦分布/P2.pdf", width = 10, height = 10, family = "serif")
# 绘制中国地图
ggplot() +
  geom_sf(data = china, fill = china$color, color = 'black') + # 中国地图轮廓
  geom_sf(data = nine, color = 'black')+
  geom_sf(data = island, color = 'black')+
  geom_sf(data = yellow.river, color = '#00CED1')+
  geom_sf(data = yz.river, color = '#00008B')+
  geom_sf(data = yz, fill = NA, color = "#8B0000", linewidth = 1)+
  #截取中国地图，不在大图上显示九段线区域
  coord_sf(ylim = c(19, 53)) + 
  # 加省会点
  annotate("point", x = 104.04, y = 30.67, colour = "#00008B", size = 2)+
  annotate("point", x = 107.33, y = 29.80, colour = "#00008B", size = 2)+
  annotate("point", x = 114.21, y = 30.30, colour = "#00008B", size = 2)+
  annotate("point", x = 117.17, y = 31.52, colour = "#00008B", size = 2)+
  annotate("point", x = 118.78, y = 32.07, colour = "#00008B", size = 2)+
  # 加省会名称
  annotate("text", x = 104.04, y = 30.32, label = "Chengdu", size = 3)+
  annotate("text", x = 107.33, y = 29.50, label = "Chongqing", size = 3)+
  annotate("text", x = 114.21, y = 29.95, label = "Wuhan", size = 3)+
  annotate("text", x = 117.17, y = 31.17, label = "Hefei", size = 3)+
  annotate("text", x = 120.10, y = 31.72, label = "Nanjing", size = 3)+
  # 加省名称
  annotate("text", x = 104.04, y = 31.50, label = "Sichuan", size = 4, color = "#0000CD")+
  annotate("text", x = 112.21, y = 31.00, label = "Hubei", size = 4, color = "#0000CD")+
  annotate("text", x = 117.17, y = 32.22, label = "Abhui", size = 4, color = "#0000CD")+
  annotate("text", x = 120.00, y = 33.50, label = "Jiangsu", size = 4, color = "#0000CD")+
  # 添加比例尺
  annotation_scale(location='bl', text_cex = 1) +
  # 添加指北针
  annotation_north_arrow(location = 'tr', which_north = 'false',
                         style = north_arrow_nautical())+
  labs(x = "", y = "")+
  # 设置大图边框
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 20))

dev.off()

pdf("D:/学习/课题/稻麦分布/P2_图例.pdf", width = 1, height = 2)
## 制作图例
legend <- data.table(y = seq(1, 9, 2), x = rep(1, 5), z = letters[1:5]); legend
ggplot(legend, aes(x, y, fill = z)) +
  geom_tile()+
  theme_void()+
  guides(fill = "none")+
  scale_fill_manual(values = c("#e7ffcd", "#BFF38C", "#99E64C", "#66cc00", "#33b200"))

dev.off()

## P3 冬小麦及气象站点分布
china.mapdata[NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#e7ffcd"]
china.mapdata[! NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#f6ece5"]
china$color <- china.mapdata$color

### 气象战点分布
site.climate <- sf::st_read("D:/学习/课题/稻麦分布/冬小麦分布点栅格/冬小麦分布区气象战点分布.shp")

### 冬小麦分布
require(raster)
site.wheat <- raster("D:/学习/课题/稻麦分布/冬小麦分布点栅格/c20162019年中国冬小麦1000米分辨率种植分布并集-4326.tif")

df <- data.table(site = as.data.frame(site.wheat)$OBJECTID, coordinates(site.wheat)) %>% .[! site %in% c(NA, 1)]
head(df)
ggplot(data = df)+
  geom_point(aes(x, y), size = .000000001)+
  theme_classic()

# ggpubr::show_point_shapes()

pdf("D:/学习/课题/稻麦分布/冬小麦及气象站点分布.pdf", width = 10, height = 10, family = "serif")
# 绘制中国地图
ggplot() +
  # geom_sf(data = china, fill = china$color, color = 'black') + # 中国地图轮廓
  # geom_sf(data = nine, color = 'black')+
  # geom_sf(data = island, color = 'black')+
  # geom_sf(data = yellow.river, color = '#00CED1')+
  # geom_sf(data = yz.river, color = '#00008B')+
  geom_sf(data = yz, fill = NA, color = "#8B0000", linewidth = 1)+
  # 冬小麦分布
  geom_point(data = df, mapping = aes(x = x, y = y), color = "#33b200", size = .0000000000001, alpha = .2)+
  # 气象站点
  geom_sf(data = site.climate, color = '#008B8B', alpha = 1.2, size = 3, shape = 18)+
  #截取中国地图，不在大图上显示九段线区域
  # coord_sf(ylim = c(19, 53))+
  # 加省会点
  annotate("point", x = 104.04, y = 30.67, colour = "#00008B", size = 2)+
  annotate("point", x = 107.33, y = 29.80, colour = "#00008B", size = 2)+
  annotate("point", x = 114.21, y = 30.30, colour = "#00008B", size = 2)+
  annotate("point", x = 117.17, y = 31.52, colour = "#00008B", size = 2)+
  annotate("point", x = 118.78, y = 32.07, colour = "#00008B", size = 2)+
  # 加省会名称
  annotate("text", x = 104.04, y = 30.32, label = "Chengdu", size = 3)+
  annotate("text", x = 107.33, y = 29.50, label = "Chongqing", size = 3)+
  annotate("text", x = 114.21, y = 29.95, label = "Wuhan", size = 3)+
  annotate("text", x = 117.17, y = 31.17, label = "Hefei", size = 3)+
  annotate("text", x = 120.10, y = 31.72, label = "Nanjing", size = 3)+
  # 加省名称
  annotate("text", x = 104.04, y = 31.50, label = "Sichuan", size = 4, color = "#0000CD")+
  annotate("text", x = 112.21, y = 31.00, label = "Hubei", size = 4, color = "#0000CD")+
  annotate("text", x = 117.17, y = 32.22, label = "Abhui", size = 4, color = "#0000CD")+
  annotate("text", x = 120.00, y = 33.50, label = "Jiangsu", size = 4, color = "#0000CD")+
  # 添加比例尺
  annotation_scale(location='bl', text_cex = 1) +
  # 添加指北针
  annotation_north_arrow(location = 'tr', which_north = 'false',
                         style = north_arrow_nautical())+
  labs(x = "", y = "")+
  # 设置大图边框
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 20))

dev.off()


## P4 稻麦分布--中国地图辅图
### 修改的话使用china.mapdata，再添加到china
## P3 冬小麦及气象站点分布
china.mapdata[NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#e7ffcd"]
# china.mapdata[! NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#f6ece5"]
china$color <- china.mapdata$color

pdf("D:/学习/课题/稻麦分布/中国地图辅图--长江流域.pdf", width = 10, height = 10, family = "serif")
# 绘制中国地图
ggplot() +
  geom_sf(data = china, fill = china$color, color = 'black') + # 中国地图轮廓
  geom_sf(data = nine, color = 'black')+
  geom_sf(data = island, color = 'black')+
  geom_sf(data = yellow.river, color = '#00CED1')+
  geom_sf(data = yz.river, color = '#00008B')+
  geom_sf(data = yz, fill = NA, color = "#8B0000", linewidth = 1)+
  #截取中国地图，不在大图上显示九段线区域
  coord_sf(ylim = c(19, 53)) + 
  # 添加比例尺
  annotation_scale(location='bl', text_cex = 1) +
  # 添加指北针
  annotation_north_arrow(location = 'tr', which_north = 'false',
                         style = north_arrow_nautical())+
  labs(x = "", y = "")+
  # 设置大图边框
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 20))

dev.off()

# P5 产量分布
# lonlat <- openxlsx::read.xlsx("D:/学习/模型/经纬度/全国各县经纬度--详细.xlsx") %>% setDT() %>% .[省份 %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省")] %>% .[, 1:5] %>% .[, index := paste(省份, 地市, 区县, sep = "_")]
# 
# path.yield <- c("D:/学习/课题/2016-2020年长江中下游地区县级统计年鉴/安徽省/安徽省2016-2020年各区县市主要粮食作物产量及面积.xlsx",
#                 "D:/学习/课题/2016-2020年长江中下游地区县级统计年鉴/湖北省/湖北省2016-2020年各区县市主要粮食作物产量及面积.xlsx",
#                 "D:/学习/课题/2016-2020年长江中下游地区县级统计年鉴/江苏省/江苏省2016-2020年各区县市主要粮食作物产量及面积.xlsx",
#                 "D:/学习/课题/2016-2020年长江中下游地区县级统计年鉴/四川省/四川省2016-2020年各区县市主要粮食作物产量及面积.xlsx",
#                 "D:/学习/课题/2016-2020年长江中下游地区县级统计年鉴/重庆市/重庆市2016-2020年各区县市主要粮食作物产量及面积.xlsx")
# 
# data.yield.all <- NULL
# i <- 1
# for (i in 1:length(path.yield)) {
#   sheetNames <- getSheetNames(path.yield[i])
#   
#   j <- 1
#   for (j in 1:length(sheetNames)) {
#     data.yield.single <- read.xlsx(xlsxFile = path.yield[i], sheet = sheetNames[j], cols = c(1:5, 9)) %>% setDT() %>% .[指标 %chin% "产量/吨" & ! 地区 %chin% "总计" & ! 地区 %chin% "总计"] %>% na.omit()
#     data.yield.single.mean <- data.yield.single[, .(小麦 = mean(小麦, na.rm = T)), by = .(`省/直辖市`, `市`, `地区`)]
#     data.yield.all <- rbind(data.yield.all, data.yield.single.mean)
#   }
#   print(paste0(i, " / ", length(path.yield)))
# }
# 
# data.yield.all[, index := paste(`省/直辖市`, 市, 地区, sep = "_")]
# 
# data.RMSE <- lonlat[data.yield.all, on = "index"] %>% .[, c("省/直辖市", "市", "地区") := NULL]
# openxlsx::write.xlsx(data.RMSE, "D:/学习/课题/稻麦分布/小麦产量分布/data.RMSE.NA.xlsx")

data.RMSE <- openxlsx::read.xlsx("D:/学习/课题/稻麦分布/小麦产量分布/data.RMSE.NA.xlsx") %>% setDT()

## 加载RMSE及插值函数
source("D:/学习/模型/sdm_物种分布模型/sdm_自定义函数和脚本/gstat.idw_huhu.R")
source("D:/学习/模型/sdm_物种分布模型/sdm_自定义函数和脚本/parallel.R")

## RMSE

data <- data.RMSE
colName <- "小麦"; colName
lonlat.colNum <- c(1, 2)
map.path <- "D:/学习/课题/稻麦分布/冬小麦分布区矢量地图/wheatFollowingRice.shp"; map.path
write.dir <- "D:/学习/课题/稻麦分布/小麦产量分布/RMSE/"
idp <- seq(1.5, 2.5, 0.1); idp
write.name <- "RMSE"; write.name

print("开始RMSE!")
Sys.time()
##Parallelly run idw.RMSE.huhu
parallel(func = idw.RMSE.huhu,
         interpolation.colName = rep(colName, length(idp)) %>% sort(),
         idp = rep(idp, length(colName)),
         MoreArgs = list(data = data,
                         lonlat.colNum = lonlat.colNum,
                         map.path = map.path,
                         write.dir = write.dir,
                         write.name = write.name
         ),
         cores = 8,
         return = F,
         export = c("idw.RMSE.huhu", "data","lonlat.colNum", "map.path", "idp", "write.dir", "colName", "write.name"),
         packages = c("data.table", "magrittr", "stringr", "gstat", "sp", "raster"),
         combine = "list",
         errorhandling = "pass",
         verbose = T
)

##Load RMS.csv and select the min one
RMSE <- fread(paste0(write.dir, write.name, "_均方根误差.csv"))
RMSE.min <- RMSE[, .(RMSE = min(RMSE)), by = .(bio)]
RMSE.min <- RMSE[RMSE.min, on = c("bio", "RMSE")] %>% unique()
fwrite(RMSE.min, paste0(write.dir, write.name, ".min数据.csv"))

print("RMSE结束!")
Sys.time()


data.RMSE[, group := "小麦1"]

# 2. Idw
## Set the parameter of idw
lonlat.colNum <- c(1, 2) 
interpolation.colName <- "小麦" 
data <- data.RMSE
Group <- "小麦1"
Group
grid.n <- 50000
write.name <- RMSE.min$bio
write.name
idp <- RMSE.min$idp 
idp
map.path
write.dir

print("开始插值！")
Sys.time()
## Parallelly run idw
parallel(func = idw.huhu,
         Group = Group,
         idp = idp,
         write.name = write.name,
         MoreArgs = list(data = data,
                         interpolation.colName = interpolation.colName,
                         lonlat.colNum = lonlat.colNum,
                         map.path = map.path,
                         grid.n = grid.n,
                         write.dir = write.dir),
         cores = 1,
         export = c("idw.huhu", "Group", "write.name", "interpolation.colName", "data","lonlat.colNum", "map.path", "grid.n", "idp", "write.dir"),
         packages = c("data.table", "magrittr", "stringr", "gstat", "sp", "raster"),
         combine = "list",
         errorhandling = "pass",
         verbose = T
)

print("插值结束！")
Sys.time()

# 画图
# 地图数据
## 中国地图路径
china <- sf::st_read("D:/学习/地理数据/中国省级地图/chinaMap/china.shp") # 省名乱码
china.mapdata <- fread("D:/学习/地理数据/中国省级地图/chinaMap/chinaMap.txt")
## 替换china的省名
china$NAME <- china.mapdata$NAME
china.mapdata[! NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#f6ece5"]
china$color <- china.mapdata$color

## 提取长江流域五省市地图
yz <- china[china$NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), ]
boder <- sf::read_sf("D:/学习/地理数据/中国省级地图/chinaMap/china.shp")
nine <- sf::read_sf("D:/学习/地理数据/中国省级地图/九段线/SouthSea/九段线.shp") 
island <- sf::read_sf("D:/学习/地理数据/中国省级地图/九段线/SouthSea/bou2_4l.shp")
yellow.river <- sf::read_sf("D:/学习/地理数据/长江黄河/长江黄河/黄河.shp")
yz.river <- sf::read_sf("D:/学习/地理数据/长江黄河/长江黄河/长江.shp")

# 读入栅格
tif <- terra::rast("D:/学习/课题/稻麦分布/小麦产量分布/RMSE/小麦_小麦-idp.2-RMSE.118614.445.tif")
# 将栅格数据转换成dataframe格式
df <- raster::as.data.frame(tif, xy = T, na.rm = T) %>% data.table()
names(df) <- c("x", "y", "value")

# 计算百分位数
percentiles <- quantile(df$value, c(seq(0.5, 0.9, 0.1))) %>% round(digits = 0)
df[value < percentiles[1], v := 1]
df[value >= percentiles[1] & value < percentiles[2], v := 2]
df[value >= percentiles[2] & value < percentiles[3], v := 3]
df[value >= percentiles[3] & value < percentiles[4], v := 4]
df[value >= percentiles[4] & value < percentiles[5], v := 5]
df[value >= percentiles[5], v := 6]
df$v <- factor(df$v, levels = 1:6, labels = c(paste0("< ", percentiles[1], " (50% percentile)"), paste0(percentiles[1], " - ", percentiles[2], " (60% percentile)"), paste0(percentiles[2], " - ", percentiles[3], " (70% percentile)"), paste0(percentiles[3], " - ", percentiles[4], " (80% percentile)"), paste0(percentiles[4], " - ", percentiles[5], " (90% percentile)"), paste0("> ", percentiles[5])))

# 颜色设置
# RColorBrewer::display.brewer.all()
# colors <- RColorBrewer::brewer.pal(n = 9, name = "Greens") %>% .[3:9]
  
# 绘图
pdf("D:/学习/课题/稻麦分布/冬小麦产量分布.pdf", width = 10, height = 10, family = "serif")
ggplot()+
  # 使用ggspatial包中的geom_tile()函数进行栅格数据可视化，用上图中的第三列进行fill填充
  geom_tile(data = df, aes(x = x, y = y, fill = v))+
  guides(fill = guide_legend(reverse = F))+
  # 将行政边界叠加在栅格数据上
  geom_sf(data = boder, fill = china$color, color = "black", size = .5)+
  geom_sf(data = nine, color = 'black')+
  geom_sf(data = island, color = 'black')+
  # geom_sf(data = yellow.river, color = '#00CED1')+
  # geom_sf(data = yz.river, color = '#00008B')+
  # geom_sf(data = yz, fill = NA, color = "#8B0000", linewidth = 1)+
  # 加省会点
  annotate("point", x = 104.04, y = 30.67, colour = "black", size = 2)+
  annotate("point", x = 107.33, y = 29.80, colour = "black", size = 2)+
  annotate("point", x = 114.21, y = 30.30, colour = "black", size = 2)+
  annotate("point", x = 117.17, y = 31.52, colour = "black", size = 2)+
  annotate("point", x = 118.78, y = 32.07, colour = "black", size = 2)+
  # 加省会名称
  annotate("text", x = 104.04, y = 30.32, label = "Chengdu", size = 3)+
  annotate("text", x = 107.33, y = 29.50, label = "Chongqing", size = 3)+
  annotate("text", x = 114.21, y = 29.95, label = "Wuhan", size = 3)+
  annotate("text", x = 117.17, y = 31.17, label = "Hefei", size = 3)+
  annotate("text", x = 120.10, y = 31.72, label = "Nanjing", size = 3)+
  # 加标题
  annotate("text", x = 87, y = 53, label = "Average wheat yield (t, 2016-2020)", size = 7)+
  # # 加省名称
  # annotate("text", x = 104.04, y = 31.50, label = "Sichuan", size = 4, color = "#0000CD")+
  # annotate("text", x = 112.21, y = 31.00, label = "Hubei", size = 4, color = "#0000CD")+
  # annotate("text", x = 117.10, y = 32.22, label = "Abhui", size = 4, color = "white")+
  # annotate("text", x = 119.20, y = 33.50, label = "Jiangsu", size = 4, color = "white")+
  # 截取地图
  coord_sf(xlim = c(72, 143), ylim = c(19, 53)) + 
  # 比例尺设置
  annotation_scale(location = "bl", text_cex = 1.1)+
  # 指北针设置
  annotation_north_arrow(location="tr",
                         style =north_arrow_nautical(
                           fill =c("grey40","white"),
                           line_col ="grey20"))+ 
  scale_fill_manual(values = c("#ffff9b", "#d3ff78", "#7bed00", "#0ec441", "#1e9094", "#0b2c7a"))+
  labs(x = "", y = "")+
  # 主题设置
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 20), plot.title = element_text(size = 35), legend.text = element_text(size = 12), legend.position = c(0.84, 0.17))

dev.off()























