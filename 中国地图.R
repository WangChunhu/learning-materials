library(maps)
library(mapdata)
library(maptools)
library(ggplot2)
library(plyr)
library(sf)
library(rgdal) 
library(Rcpp)
setwd("H:/R学习/地图/地图2")

#china_map <- st_read("bou2_4p.shp")
china_map <- readShapePoly("bou2_4p.shp")
plot(china_map)
projNew <- CRS("+proj=merc +lat_0=45n +lon_0=100e")
xProj <- spTransform(china_map, projNew) 



b <- ggplot(china_map,aes(x = long,y = lat,group=group))+
  geom_polygon(fill="beige")+   #米色
  geom_path(color="grey40")+
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  labs(x="",y="",title = "中华人民共和国")

hu1 <- china_map@data   # S4类数据 用@ 取子集
hu2 <- data.frame(hu1,id=seq(0:924)-1)
china_map1 <- fortify(china_map)   # 转化为数据框
china_mapdata <- join(china_map1,hu2,type="full")  #以共同的列id进行合并
c <- ggplot(china_mapdata,aes(x = long,y = lat,group=group,fill=NAME))+
  geom_polygon()+
  geom_path(color="grey40")+
  scale_fill_manual(values = colors(),guide=F)+
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  labs(x="",y="",title = "中华人民共和国")

shanxi <- subset(china_map,NAME==c("湖北省","四川省","重庆市","湖南省","安徽省","江西省","江苏省","浙江省","上海市"))
d <- ggplot(shanxi,aes(x = long,y = lat,group=group))+
  geom_polygon(fill="beige")+
  geom_path(color="grey40")+
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  labs(x="",y="",title = "中华人民共和国")
d


library(devtools)
install_github('lchiffon/REmap')


