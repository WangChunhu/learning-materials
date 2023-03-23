# 并行计算----------------------------------------------------------------
parallel = function(func, ..., MoreArgs = NULL, cores = NULL, combine = "list", export = NULL, packages = NULL, errorhandling = c("stop", "remove", "pass"), verbose = F){
  #' @param func 被并行函数
  #' @param ... func的多个动态参数
  #' @param cores 要运行的线程数
  #' @param MoreArgs func的静态参数(list)
  #' @param combine 线程间结果拼接模式(list, rbind, cbind, c等)
  #' @param export 要用到的环境变量及函数等
  #' @param packages 需要加载的包
  #' @param errorhandling 错误处理方式
  #' @param verbose 是否提示信息
  
  # 加载并行包
  require(foreach)
  require(doParallel)
  
  # 内核数，不指定的话，为虚拟内核数-1
  if (is.null(cores)) {
    cores = parallel::detectCores(logical = T) - 2
  }else{
    cores = cores
  }
  
  # 打开
  cl = parallel::makeCluster(cores)
  # 注册
  doParallel::registerDoParallel(cl)
  # 并行计算
  dots = list(...)  # 动态参数list
  result = foreach::foreach(i = seq(length(dots[[1]])), .combine = combine, .export = export, .packages = packages, .errorhandling = errorhandling, .verbose = verbose) %dopar% do.call(func, c(lapply(dots, `[`, i), MoreArgs))

  # 关闭
  parallel::stopCluster(cl)
  
  #返回结果
  return(result)
}

# # 测试代码
# parallel(func = paste,
#          1:10, 1:10, 1:10,
#          MoreArgs = list(sep = "_"),
#          combine = "rbind")


# interpolation.colName_fara <- rep(interpolation.colName, length(idp))
# idp_para <- rep(idp, length(interpolation.colName)) %>% sort()
# 
# parallel(func = idw.huhu,
#          interpolation.colName = interpolation.colName,
#          idp = 1:10,
#          
#          MoreArgs = list(data.path = bio.site.path,
#                          lonlat.colNum = lonlat.colNum,
#                          map.path = map.path,
#                          grid.n = grid.n,
#                          idp = idp,
#                          write.dir = write.dir),
#          export = c("idw.huhu", "interpolation.colName", "bio.site.path","lonlat.colNum", "map.path", "grid.n", "idp", "write.dir"),
#          packages = c("data.table", "magrittr", "stringr", "gstat", "sp", "raster"),
#          combine = "rbind",
#          errorhandling = "pass",
#          verbose = T
# )
