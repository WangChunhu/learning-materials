# 自定义函数
convex.hull <- function(x, y){
  require(geometry)
  
  plot(x, y)
  # 计算凸包，其中ConVexHull$hull表示多边形的点索引
  ConVexHull <- convhulln(cbind(x, y), "FA")
  # 作图
  Plot_ConvexHull <- function(xcoord, ycoord, lcolor){
    
    hpts <- chull(x = xcoord, y = ycoord)
    hpts <- c(hpts, hpts[1])
    lines(xcoord[hpts], ycoord[hpts], col = lcolor)
  } 
  
  Plot_ConvexHull(xcoord = x, ycoord = y, lcolor = "IndianRed3")
  return(ConVexHull)
}

# 例子
n <- 100 
x <- rnorm(n, 5, 2)
y <- rnorm(n, 20, 10)
z <- convex.hull(x, y)

# z <- convex.hull(x = yc.data$DDLon, y = yc.data$DDLat)


