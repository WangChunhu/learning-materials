boxplot.stats.huhu.dt <- function(dt){
  #' @description 数据框(data.table)异常值替换为NA
  #' @param dt 单列data.table
  #' @return 无返回值, 直接在原dt中修改
  
  require(data.table)
  # 如果不是则改为dt
  if (! "data.table" %in% class(dt)){
    setDT(dt)
  }
  # 列名统一为x
  names(dt) <- "x" 
  
  # 异常值检测
  out <- boxplot.stats(dt$x)
  # 异常值位置提取
  out.location <- which(dt$x %in% out$out)
  
  # 将异常值重置为NA
  set(dt, out.location, 1L, NA)
}

# test
# dt <- data.frame(x = c(0:5, 100, 6:11, 105))
# boxplot.stats.huhu(dt = dt)

boxplot.stats.huhu.c <- function(c){
  #' @description 向量异常值替换为NA
  #' @param c 向量
  #' @return 无返回值, 直接在原向量中修改
  
  require(data.table)
  # 异常值检测
  out <- boxplot.stats(c)
  # 异常值位置提取
  out.location <- which(c %in% out$out)
  
  # 将向量转换为dt后修改
  c.dt <- data.table(c)
  
  # 将异常值重置为NA
  set(c.dt, out.location, 1L, NA)
  
  # 返回向量
  return(c.dt$c)
}

# test
# c <- c(1:6, 20, 21, 1:3)
# boxplot.stats.huhu.c(c = c)


