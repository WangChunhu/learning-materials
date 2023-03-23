# 插值idp验证函数 ------------------------------------------------------------------
idw.RMSE.huhu <- function(interpolation.colName = interpolation.colName, data = data, lonlat.colNum = lonlat.colNum, group = group, col.idw = col.idw, map.path = map.path, idp = idp, write.dir = write.dir, write.name = write.name){
  #' @description 使用长数据
  #' @description 根据站点bio信息插值到地图上
  #' @param interpolation.colName 插值的值列名
  #' @param data 带有站点、经纬度及插值信息的数据框(xlsx)
  #' @param lonlat.colNum 经纬度数据框，经度在前
  #' @param group 插值的项列名
  #' @param col.idw 插值项
  #' @param map.path 地图shp地址
  #' @param idp 指定反距离加权功率,为0时权重不会随距离而改变,值越大，附近的点的权重越高
  #' @param write.dir 文件写出路径
  #' @param write.name 写出文件的前缀名
  #' @returns RMSE.csv
  
  bao <- c("data.table", "magrittr", "stringr", "gstat", "sp", "raster", "rlang")
  sapply(bao, require, character.on = T)
  
  # 插值数据
  idw.data <- data %>% na.omit() %>% setDT() %>% .[eval(parse(text = group)) %in% col.idw]
  
  # 提取经纬度
  coords.data <-  data.frame(idw.data) %>% .[lonlat.colNum]
  
  # 创建SpatialPointsDataFrame
  p <- sp::SpatialPointsDataFrame(coords = coords.data,
                                  data = data.frame(idw.data) %>% .[interpolation.colName]) # 插值点
  
  # 固定插值列名为bio，方便后续formula使用
  names(p) <- "bio"
  
  # 如果map存在的话不另导入
  if (! "map" %in% ls()) {
    map <- raster::shapefile(map.path) # 插值底图
  }
  
  # 将bio等的投影设置为map的投影
  proj4string(p) <- proj4string(map)
  
  # 将点边界范围替换为中国边界范围
  p@bbox <- map@bbox
  
  ## 判断路径格式
  if(! grep("/", str_split(write.dir, "")[[1]]) %>% tail(1) == str_count(write.dir)){
    write.dir <- paste0(write.dir, "/")
  }else{
    write.dir <- write.dir
  }
  
  ## 判断路径是否存在，不存在的话新建
  if (! dir.exists(write.dir)) {
    dir.create(write.dir)
  }
  
  # 保留交叉验证 jackknife
  # bootsrap 自助法：假设有n个样本，每次有放回地从n个样本中取一个
  # jackknife 刀切法：留一交叉验证法，从样本中剔除一个，估计偏差与方
  ## 从数据集中删除一个数据点，并使用数据集中的 *所有其他 *点插值其值，然后为该数据集中的每个点重复此过程（同时确保该过程插值参数在每个插值中保持恒定）。然后将插值值与省略点的实际值进行比较。
  idw.out <- vector(length = length(p))
  i <- 1
  for (i in 1:length(p)) {
    idw.out[i] <- idw(bio ~ 1,
                      locations = p[-i,], 
                      newdata = p[i,], 
                      idp = idp)$var1.pred
  }
  
  # 计算并保存均方根误差
  RMSE <- sqrt( sum((idw.out - p$bio)^2) / length(p))
  RMSE.info <- data.frame(bio = col.idw, idp = idp, RMSE = RMSE)
  print(RMSE.info)
  if (! file.exists(paste0(write.dir, write.name, "_均方根误差.csv"))) {
    fwrite(x = RMSE.info, file = paste0(write.dir, write.name, "_均方根误差.csv"), col.names = T)
  }else{
    fwrite(x = RMSE.info, file = paste0(write.dir, write.name, "_均方根误差.csv"), append = T, col.names = F)
  }
}

# # RMSE
# interpolation.colName <- "value"
# data <- fread("../idw/插值原始值.txt", nThread = 30); head(data)
# lonlat.colNum <- c(2, 3)
# group <- "idw"
# col.idw = "1961_1_radn"
# map.path <- "/public/home/zzuaga06/yanhl/prj/huhu/huhu.met/chinaMap/china.shp"; map.path
# idp <- 3; idp
# write.dir <- "../idw/"
# write.name <- "RMSE"; write.name
# -------------------------------------------------------------------------



# 插值自定函数 ------------------------------------------------------------------
idw.huhu <- function(interpolation.colName = interpolation.colName, data = data, lonlat.colNum = lonlat.colNum, group = group, col.idw = col.idw, map.path = map.path, cellsize = cellsize, idp = idp, write.dir = write.dir, write.name = write.name){
  #' @description 使用长数据
  #' @description 根据站点bio信息插值到地图上
  #' @param interpolation.colName 插值的值列名
  #' @param data 带有站点、经纬度及插值信息的数据框(xlsx)
  #' @param lonlat.colNum 经纬度数据框，经度在前
  #' @param group 插值的项列名
  #' @param col.idw 插值项
  #' @param map.path 地图shp地址
  #' @param grid.n 格点数，默认5千万
  #' @param idp 指定反距离加权功率,为0时权重不会随距离而改变,值越大，附近的点的权重越高
  #' @param write.dir 会在此路径下创建一个名为"预插值"的文件夹作为写出文件的路径
  #' @param write.name 插值出的栅格名
  #' @returns tif栅格 & jackknife差异图 & bio/idp/RMSE.csv
  
  bao <- c("data.table", "magrittr", "stringr", "gstat", "sp", "raster", "rlang")
  sapply(bao, require, character.on = T)
  
  # 插值数据
  idw.data <- data %>% na.omit() %>% setDT() %>% .[eval(parse(text = group)) %in% col.idw]
  
  # 提取经纬度
  coords.data <-  data.frame(idw.data) %>% .[lonlat.colNum]
  
  # 创建SpatialPointsDataFrame
  p <- sp::SpatialPointsDataFrame(coords = coords.data,
                                  data = data.frame(idw.data) %>% .[interpolation.colName]) # 插值点
  
  # 固定插值列名为bio，方便后续formula使用
  names(p) <- "bio"
  
  # 如果map存在的话不另导入
  if (! "map" %in% ls()) {
    map <- raster::shapefile(map.path) # 插值底图
  }
  
  # 将bio等的投影设置为map的投影
  proj4string(p) <- proj4string(map)
  
  # 将点边界范围替换为中国边界范围
  p@bbox <- map@bbox
  
  # 创建一个grid.n个格点的空的网格 (SpatialGrid 对象)
  # grd <- as.data.frame(spsample(x = p, type = "regular", n = grid.n)) # 按格子数
  grd <- as.data.frame(spsample(x = p, type = "regular", cellsize = cellsize)) # 按格子大小1万米
  names(grd) <- c("X", "Y")
  coordinates(grd) <- c("X", "Y") # 获取光栅对象的行、列或单元号的光栅单元中心坐标
  gridded(grd) <- T # 创建 SpatialPixel 对象
  fullgrid(grd) <- T # 创建 SpatialGrid 对象
  
  # 将grd的投影设置为map的投影
  proj4string(grd) <- proj4string(p)
  
  # idw插值
  p.idw <- gstat::idw(formula = bio ~ 1, # 插值表达式，一般为var ~ 1
                      locations = p, # 已知点对象，不包含var的话需指定"data"参数
                      # nmax = Inf, 插值任意位置的属性值至多所需的已知点数目
                      # nmin = 0, 至少所需的已知点数目
                      # maxdist = Inf, 距离阈值；与插值点超过这个距离的已知点不参与该插值点属性值的计算
                      newdata = grd, # 插值对象
                      idp = idp) # idp:指定反距离加权功率,为0时权重不会随距离而改变,值越大，附近的点的权重越高，幂参数可基于距输出点的距离来控制已知点对内插值的影响。幂参数是一个正实数，默认值为2。（一般0.5到3的值可获得最合理的结果）。
  
  # 将p.idw转为栅格并裁剪到地图
  r <- raster(p.idw)
  r.m <- mask(r, map)
  
  ## 判断路径格式
  if(! grep("/", str_split(write.dir, "")[[1]]) %>% tail(1) == str_count(write.dir)){
    write.dir <- paste0(write.dir, "/")
  }else{
    write.dir <- write.dir
  }
  
  ## 判断路径是否存在，不存在的话新建
  if (! dir.exists(write.dir)) {
    dir.create(write.dir)
  }
  
  
  # 保存栅格
  writeRaster(x = r.m, filename = paste0(write.dir, write.name,  col.idw, ".tif"), overwrite=TRUE)
  
  # # 保留交叉验证 jackknife
  # # bootsrap 自助法：假设有n个样本，每次有放回地从n个样本中取一个
  # # jackknife 刀切法：留一交叉验证法，从样本中剔除一个，估计偏差与方
  # ## 从数据集中删除一个数据点，并使用数据集中的 *所有其他 *点插值其值，然后为该数据集中的每个点重复此过程（同时确保该过程插值参数在每个插值中保持恒定）。然后将插值值与省略点的实际值进行比较。
  # idw.out <- vector(length = length(p))
  # i <- 1
  # for (i in 1:length(p)) {
  #   idw.out[i] <- idw(bio ~ 1,
  #                     locations = p[-i,], 
  #                     newdata = p[i,], 
  #                     idp = idp)$var1.pred
  # }
  
  # # 计算并保存均方根误差
  # RMSE <- sqrt( sum((idw.out - p$bio)^2) / length(p))
  # RMSE.info <- data.frame(bio = interpolation.colName, idp = idp, RMSE = RMSE)
  # print(RMSE.info)
  
  # # 保存差异图
  # pdf(file = paste0(write.dir, write.name, "_差异图-",  interpolation.colName, "-idp.", idp, "-RMSE.", round(RMSE, 3), ".pdf"), width = 5, height = 5, family = "serif")
  # OP <- par(pty="s", mar=c(3,3,2,2))
  # plot(idw.out ~ p$bio, asp=1, xlab="Observed", ylab="Predicted", pch=16,col=rgb(0,0,0,0.5), main = paste0(interpolation.colName, " (idp-", idp, " / ", "RSEM-", round(RMSE, 3), ")"))
  # abline(lm(idw.out ~ p$bio), col="red", lw=2,lty=2)
  # abline(0,1)
  # par(OP)
  # dev.off()
}

# interpolation.colName <- "value"
# data <- fread("../idw/插值原始值.txt", nThread = 30); head(data)
# lonlat.colNum <- c(2, 3)
# group <- "idw"
# col.idw = "1961_1_radn"
# map.path <- "/public/home/zzuaga06/yanhl/prj/huhu/huhu.met/chinaMap/china.shp"; map.path
# cellsize = 0.08983153 # 1万米
# idp <- 3; idp
# write.dir <- "../idw/插值栅格结果/"
# write.name <- ""; write.name