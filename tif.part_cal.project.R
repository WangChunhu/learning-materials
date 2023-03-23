tif.part_cal.project <- function(total, map.part.path, area.name, project.name, sumORmean = "sum"){
  #' @description 根据总的栅格数据按部分地区的矢量地图提取后分别设置touches = T/F进行计算单个指标, 然后平均
  #' @param total 总的栅格数据
  #' @param map.part.path 部分地区的矢量地图
  #' @param area.name 部分地区的名称
  #' @param project.name 项目名称如单产等
  #' @param sumORmean 结果累计还是平均, 如单产需要平均, 总产需要累积
  
  ## 导入分省地图
  map.part <- sf::st_read(map.part.path, quiet = T)
  ## 裁剪并掩膜 touches = F
  ### 计算总产 (如果touches为TRUE，则线或多边形接触的所有单元格都将被掩膜，而不仅仅是线渲染路径上的单元格，或其中心点位于多边形内的单元格)
  part.F <- terra::crop(total, map.part) %>% terra::mask(mask = map.part, touches = F) 
  part.F.df <- terra::as.data.frame(part.F)
  names(part.F.df) <- "x"
  if (sumORmean == "sum") {
    F.part <- sum(part.F.df$x)
  }else if(sumORmean == "mean"){
    F.part <- mean(part.F.df$x)
  }else{
    stop("参数[sumORmean]的选项为: sum或mean!")
  }
  
  ## touches = T
  part.T <- terra::crop(total, map.part) %>% terra::mask(mask = map.part, touches = T) 
  part.T.df <- terra::as.data.frame(part.T)
  names(part.T.df) <- "x"
  if (sumORmean == "sum") {
    T.part <- sum(part.T.df$x)
  }else if(sumORmean == "mean"){
    T.part <- mean(part.T.df$x)
  }else{
    stop("参数[sumORmean]的选项为: sum或mean!")
  }
  
  ## 取平均
  part <- (F.part + T.part) / 2
  return(data.frame(Province = area.name, project = project.name, value = part))
}

# # test
# total <- total.yield.all
# map.part.path <- "D:/学习/地理数据/中国省级地图/分省/anHui.shp"
# area.name <- "Anhui"
# project.name <- "总产"
# sumORmean = "sum"
# 
# res <- tif.part_cal.project(total = total,
#                             map.part.path = map.part.path,
#                             area.name = area.name,
#                             project.name = project.name,
#                             sumORmean = sumORmean)
