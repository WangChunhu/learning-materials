require(R6)

apsim.met_get <- R6Class(classname = "apsim.met_get",
                         portable = F,
                         # 公有成员
                         public = list(
                           # 公有属性
                           lons = NULL,
                           lats = NULL,
                           year.start = NULL,
                           year.end = NULL,
                           wrt.dir = NULL,
                           filenames = NULL,
                           parallel.cores = 4,
                           
                           # 属性初始化
                           initialize = function(lons, lats, year.start, year.end, wrt.dir, filenames, parallel.cores){
                             lons <<- lons
                             lats <<- lats
                             year.start <<- year.start
                             year.end <<- year.end
                             wrt.dir <<- wrt.dir
                             filenames <<- filenames
                             parallel.cores <<- parallel.cores
                           },
                           # 公有方法
                           NASA.RTP_get_parallel = function(){
                             #' @description 调用私有方法 NASA.RTP_get()并行下载降雨，距离地面2M的2M最高温和最低温以及降雨数据
                             parallel(func = NASA.RTP_get,
                                      lon = lons,
                                      lat = lats,
                                      MoreArgs = list(
                                        year.start = year.start,
                                        year.end = year.end
                                      ),
                                      cores = parallel.cores,
                                      export = c("NASA.RTP_get", "lons", "lats", "year.start", "year.end"),
                                      packages = c("nasapower", "data.table", "magrittr"),
                                      combine = "rbind",
                                      errorhandling = "pass",
                                      verbose = T)
                           },
                           
                           NASA.apsim.met_get_parallel = function(){
                             #' @description 调用私有方法NASA.apsim.met_get()从NASA数据库并行下载apsim.met文件并存放到本地
                             parallel(func = NASA.apsim.met_get,
                                      lon = lons,
                                      lat = lats,
                                      filename = filenames,
                                      MoreArgs = list(
                                        wrt.dir = wrt.dir,
                                        year.start = year.start,
                                        year.end = year.end
                                      ),
                                      cores = parallel.cores,
                                      export = c("NASA.apsim.met_get", "lons", "lats", "filenames", "wrt.dir", "year.start", "year.end"),
                                      packages = c("apsimx", "data.table", "magrittr"),
                                      combine = "rbind",
                                      errorhandling = "pass",
                                      verbose = T)
                           },
                           
                           daymet.apsim.met_get_parallel = function(){
                             #' @description 调用私有方法daymet.apsim.met_get()从daymet数据库并行下载apsim.met文件并存放到本地
                             parallel(func = daymet.apsim.met_get,
                                      lon = lons,
                                      lat = lats,
                                      filename = filenames,
                                      MoreArgs = list(
                                        wrt.dir = wrt.dir,
                                        year.start = year.start,
                                        year.end = year.end
                                      ),
                                      cores = parallel.cores,
                                      export = c("daymet.apsim.met_get", "lons", "lats", "filenames", "wrt.dir", "year.start", "year.end"),
                                      packages = c("apsimx", "daymetr", "data.table", "magrittr"),
                                      combine = "rbind",
                                      errorhandling = "pass",
                                      verbose = T)
                           }
                         ), 
                         # 私有成员
                         private = list(
                           # 私有方法
                           NASA.RTP_get = function(lon, lat, year.start, year.end){
                             #' @description 根据给定的经纬度和开始结束日期从NASA数据库(https://power.larc.nasa.gov/api/temporal/)下载农业气象数据，本函数只下载降雨，距离地面2M的2M最高温和最低温以及降雨数据；
                             #' @param lon,lat 经纬度信息，如108, 32，本函数只支持一对将维度；
                             #' @param year.start,year.end 日期范围，如"2001", "2020"，必须按此格式写
                             #' @return 返回包括"year", "day", "radn", "maxt", "mint", "rain"的数据框
                             
                             RTP = nasapower::get_power(community = "ag", # 农业, 大小写都行
                                                        pars = c("ALLSKY_SFC_PAR_TOT", "T2M_MAX", "T2M_MIN", "PRECTOTCORR"), # 分别为"radn", "maxt", "mint", "rain"
                                                        dates = c(paste0(year.start, "-1-1"), paste0(year.end, "-12-31")), 
                                                        lonlat = c(lon, lat),
                                                        temporal_api = "daily") %>% data.table() %>% .[, ! c(4:5, 7)]
                             names(RTP) = c("lon", "lat", "year", "day", "radn", "maxt", "mint", "rain")
                             return(RTP)
                           },
                           # example = RTP.get(lon = 108, lat = 32, year.start = "2001", year.end = "2020")
                           # head(example)
                           
                           NASA.apsim.met_get = function(lon, lat, year.start, year.end, wrt.dir, filename){
                             #' @description 从NASA数据库下载apsim.met文件并存放到本地
                             #' @param wrt.dir 写出met文件的路径, 最后的"/"可有可无
                             #' @param filename 写出met文件的文件名, 命名为"*.met"
                             
                             get_power_apsim_met(lonlat = c(lon, lat), dates = c(paste0(year.start, "-1-1"), paste0(year.end, "-12-31")), wrt.dir = wrt.dir, filename = filename)
                           },
                           
                           daymet.apsim.met_get = function(lon, lat, year.start, year.end, wrt.dir, filename){
                             #' @description 从daymet数据库下载apsim.met文件并存放到本地
                             
                             get_daymet_apsim_met(lonlat = c(lon, lat), years = c(year.start, year.end), wrt.dir = wrt.dir, filename = filename, silent = T)
                           },
                           
                           NASA.ag.info_get = function(ag.list.single){
                             #' @description 根据ag，即农业以及时间尺度temporal_api = "daily"获取可以得到的参数信息, 供主动激活中的NASA.ag.info_getALL函数调用
                             #' @param ag.list.single NASA.ag.list中的其中一个
                             #' @return ag.list中的其中一个的具体参数信息
                             
                             # 读出原有名字
                             key = names(ag.list.single)
                             # 统一重命名为a
                             names(ag.list.single) = "a"
                             # 使用a作为名字读取属性
                             ag.info = data.table(KEY = key, type = ag.list.single$a$type, temporal = ag.list.single$a$temporal, source = ag.list.single$a$source, community = ag.list.single$a$community, calculated = ag.list.single$a$calculated, inputs = ifelse(is.null(ag.list.single$a$inputs), NA, ag.list.single$a$inputs), units = ag.list.single$a$units, name = ag.list.single$a$name, definition = ag.list.single$a$definition)
                             return(ag.info)
                           },
                           
                           ## 并行函数----------------------------------------------
                           parallel = function(func, ..., cores = NULL, MoreArgs = NULL, combine = "list", export = NULL, packages = NULL, errorhandling = c("stop", "remove", "pass"), verbose = F){
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
                             result = foreach::foreach(i = seq(length(dots[[1]])), .combine = combine, .export = export, .packages = packages, .errorhandling = errorhandling, .verbose = verbose) %dopar% do.call(func,c(lapply(dots,`[`,i),MoreArgs))
                             # 关闭
                             parallel::stopCluster(cl)
                             
                             #返回结果
                             return(result)
                           }
                         ), 
                         # 主动激活
                         active = list(
                           # 主动激活方法
                           NASA.ag.info_getALL = function(){
                             #' @description 获取农业方面以日为尺度的可下载的所由参数信息
                             ag.list = nasapower::query_parameters(community = "ag", temporal_api = "daily")
                             
                             i = 1
                             ag.info.all = NULL
                             for (i in 1:length(ag.list)) {
                               ag.info.single = NASA.ag.info_get(ag.list.single = ag.list[i])
                               ag.info.all = rbind(ag.info.all, ag.info.single, fill = T)
                             }
                             return(ag.info.all)
                           },
                           
                           libs.check = function(){
                             # 检查是否需要下载包, 需要的话则下载, 不需要的话返回NULL
                             libs = c("nasapower", "apsimx", "daymetr", "data.table", "magrittr", "foreach", "doParallel")
                             libs.exist = sapply(X = libs, FUN = "require", character.on = T)
                             if (F %in% libs.exist) {
                               sapply(libs[which(libs.exist == F)], install.packages, dependencies = T)
                             }
                           },
                           
                           help = function(){
                             cat("类名:apsim.met.get\n目的:根据给定的经纬度和开始结束日期从NASA数据库(https://power.larc.nasa.gov/api/temporal/)或daymet(https://daymet.ornl.gov/)下载农业气象数据，本函数只下载降雨，距离地面2M的最高温和最低温以及降雨数据\n包名:nasapower, apsimx, daymetr, data.table, magrittr, foreach和doParallel\n属性:lons: 经度向量(数值型)\n    lats: 纬度向量(数值型)\n    year.start: 开始日期(数值型) (如:2001: 实际为2001-1-1)\n    year.end: 结束日期(数值型) (如:2020: 实际为2020-12-31)\n    wrt.dir: 写出met文件的路径, 最后的\"/\"可有可无\n    filenames: 写出met文件的文件名, 命名为\"*.met\"\n    parallel.cores: 并行核数(数值型)(默认四线程) ")
                           }
                         )
                         )
                               

# # 查看类
# apsim.met.get
# apsim.met.get$public_fields # 公有属性
# 
# # 实例化
# example <- apsim.met.get$new(lons = c(116, 108), lats = c(25, 26), year.start = 2000, year.end = 2002, wrt.dir = "D:/学习/杨蕊/apsim-湖北冬小麦/test", filename = c("test5.met", "test6.met"), parallel.cores = 2)
# example$wrt.dir
# 
# # 检查是否需要下载包, 不需要则返回NULL
# example$libs.check
# # 查看帮助信息
# example$help
# 
# # 下载降雨，距离地面2M的2M最高温和最低温以及降雨数据
# example.res <- example$NASA.RTP_get_parallel()
# 
# # 从NASA数据库下载apsim.met文件并存放到本地
# example$NASA.apsim.met_get_parallel()
# 
# # 从daymetr数据库下载apsim.met文件并存放到本地, 空间范围未知
# example <- apsim.met.get$new(lons = c(-93, -94), lats = c(42, 43), year.start = 2000, year.end = 2002, wrt.dir = "D:/学习/杨蕊/apsim-湖北冬小麦/test", filename = c("test7.met", "test8.met"), parallel.cores = 2)
# example$daymet.apsim.met_get_parallel()
# 
# # 获取农业方面以日为尺度的可下载的所由参数信息
# NASA.ag.info <- example$NASA.ag.info_getALL
# # write.csv(x = ag.info, file = "D:/学习/杨蕊/apsim-湖北冬小麦/农业气象可用参数下载/农业所有可用参数_日尺度.csv")
# # 查看指定参数的信息
# RTP <- NASA.ag.info[KEY %chin% c("ALLSKY_SFC_PAR_TOT", "T2M_MAX", "T2M_MIN", "PRECTOTCORR")]
# # write.csv(x = RTP, file = "D:/学习/杨蕊/apsim-湖北冬小麦/农业气象可用参数下载/RTP_日尺度.csv")





















 