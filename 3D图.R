# install.packages("rayshader")
# 加载基础包
## 包名
bao <- c("data.table", "magrittr", "stringr", "ggplot2", "ggspatial", "sf", "rayshader")
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

## 提取长江流域五省市地图
yz <- china[china$NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), ]

## P1 稻麦分布
### 修改的话使用china.mapdata，再添加到china
china.mapdata[NAME %in% c("四川省", "重庆市", "湖北省", "江苏省"), color := "#99E64C"]
china.mapdata[NAME == "安徽省", color := "#BFF38C"]
china.mapdata[! NAME %in% c("四川省", "重庆市", "湖北省", "江苏省", "安徽省"), color := "#f6ece5"]
china$color <- china.mapdata$color

pdf("D:/学习/课题/稻麦分布/稻麦分布1.3D.pdf", width = 10, height = 10, family = "serif")
# 绘制中国地图
p <- ggplot(diamonds, aes(x, depth)) +
     stat_density_2d(aes(fill = stat(nlevel)), 
                     geom = "polygon", n = 200, bins = 50,contour = TRUE) + #绘制2D密度图
     facet_wrap(clarity~.)+ # 绘制clarity变量的分组密度图
     ggsci::scale_fill_gsea()+
  theme_test()
# p

plot_gg(p, multicore = T, # 表示如果光线跟踪为“真”，则将使用多种颜色来计算阴影矩阵
        width = 5, # 宽度
        height = 5, # 高度
        scale = 250, # 3D图的高度，默认是150，设置得越高图形越高
        windowsize = c(1400,866),
        zoom = 0.55, phi = 30)

render_camera(zoom = 0.6, # 图形放大缩小倍数
              theta = 30, # 旋转角度
              phi = 45) # 方位角, 其实render_camera类似于改摄像机位置

render_snapshot(filename = "D:/学习/课题/稻麦分布/稻麦分布1.3D.pdf", clear = F, width = 10, height = 10) # 保存或输出当前视图

dev.off()

