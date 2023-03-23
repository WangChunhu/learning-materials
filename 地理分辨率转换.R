degree2meter <- function(degree){
  #' @description 将地图中的度分辨率转换为米
  #' @param degree 度 (10进制)
  
  meter <- degree * (2 * pi * 6371004) / 360 # 圆半径=2*pi*r, 除360为1度
  return(meter)
}

# test
# degree2meter(degree = 0.05)

# 一度=111319.4908米= 地球周长/360=40075016.688米/360度;

# 111319.4908/20 = 5565.975 # 0.05度=1度/20

meter2degree <- function(meter){
  #' @description 将地图中的米分辨率转换为度
  #' @param meter 米
  
  degree <- (meter * 180) / (pi * 6378137)
  return(degree)
}

# test
# meter2degree(meter = 5000)
