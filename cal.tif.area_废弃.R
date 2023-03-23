cal.tif.area <- function(map.path, map.name.output, tif.path){
  #' @describeIn 输入栅格(设置阈值后的预测面积栅格)和地图数据计算面积
  #' @param map.path 地图路径
  #' @param map.name.output 输出面积的地图名
  #' @param tif.path 栅格数据路径
  #' @return 栅格数据的面积, 单位为百万公顷
  
  # 加载包
  require(raster)
  require(terra)
  
  # 预测区域
  tif <- raster(tif.path)
  
  # 地图
  map <- raster::shapefile(map.path)
  
  # 将tif裁剪到地图(方块)
  tif.map.crop <- crop(x = tif, y = map)
  
  # 将裁减后的tif掩膜(实际地图边界线)
  map.tif <- mask(x = tif.map.crop, mask = map, inverse = F)
  
  # 将阈值作为分割点，上为1，下为NA
  map.tif.1 <- map.tif
  map.tif.1[] <- ifelse(map.tif[] == 1, 1, NA)
  
  # 计算面积(百万公顷/M.ha)
  area <-  expanse(map.tif.1 %>% rast(), 
                   unit = "ha", 
                   transform = F, # T:平面CRS转换为lon/lat以提高精度
                   byValue = F) / 1000000 # T:返回每个唯一单元格d的值
  
  # 生成返回数据框
  area.df <- data.frame(ID = map.name.output, `area/M.ha` = area)
  return(area.df)
}

# # 测试
# cal.tif.area(tif.path = "D:/学习/key/油菜/物种分布模型/met/2036-2065/result/result_ensemble.0.5/baseline_th_0.5.tif",
#              map.path = "D:/学习/地理数据/中国省级地图/分省/huBei.shp",
#              map.name.output = "湖北") -> a
