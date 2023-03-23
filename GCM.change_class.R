require(R6)

# 创建GCM.change类
#' @description 统计不同SSP和GCM下各站点的温度, 降雨及辐射的变化程度
#' @return 气象文件的温度变化度数, 降雨和辐射变化百分比及计算的原始数据

GCM.change = R6Class(classname = "GCM.change", 
                     portable = F, # F: 跨R包的继承时，兼容性不太好
                     # 公有成员
                     public = list(
                       # 公有属性
                       SSPs = NULL, # SSP类别 (向量)
                       sites = NULL, # 站点名 (向量)
                       GCMs = NULL, # GCM名称 (向量)
                       path.mets = NULL, # 气象文件路径 (向量)
                       baseline.year.start = NULL, # 当前起始年份 (单个年份值)
                       baseline.year.end = NULL, # 当前终止年份 (单个年份值)
                       future.year.start = NULL, # 未来起始年份 (单个年份值)
                       future.year.end  = NULL, # 未来终止年份 (单个年份值)
                       grow.month.start = NULL, # 生育期起始月份 (单个年份值)
                       grow.month.end = NULL, # 生育期终止月份 (单个月份值)
                       parallel.cores = 4, # 并行核数 (单个月份值)
                       
                       # 属性初始化
                       initialize = function(SSPs, sites, GCMs, path.mets, baseline.year.start, baseline.year.end, future.year.start, future.year.end, grow.month.start, grow.month.end, parallel.cores){
                         SSPs <<- SSPs
                         sites <<- sites
                         GCMs <<- GCMs
                         path.mets <<- path.mets
                         baseline.year.start <<- baseline.year.start
                         baseline.year.end <<- baseline.year.end
                         future.year.start <<- future.year.start
                         future.year.end <<- future.year.end
                         grow.month.start <<- grow.month.start
                         grow.month.end <<- grow.month.end
                         parallel.cores <<- parallel.cores
                       },
                       
                       # 公有方法
                       GCM.change_grow.stage_all_parallel = function(){
                         # 调用私有方法GCM.change_grow.stage_all()并行统计降雨，温度及辐射全生育期平均
                         parallel(func = GCM.change_grow.stage_all,
                                  SSP = SSPs,
                                  site = sites,
                                  GCM = GCMs,
                                  path.met = path.mets,
                                  MoreArgs = list(
                                    baseline.year.start = baseline.year.start,
                                    baseline.year.end = baseline.year.end,
                                    future.year.start = future.year.start,
                                    future.year.end = future.year.end,
                                    grow.month.start = grow.month.start,
                                    grow.month.end = grow.month.end
                                  ),
                                  cores = parallel.cores,
                                  export = c("GCM.change_grow.stage_all", "SSPs", "sites", "GCMs", "path.mets", "baseline.year.start", "baseline.year.end", "future.year.start", "future.year.end", "grow.month.start", "grow.month.end", "read_apsim_met"),
                                  packages = c("data.table", "magrittr", "stringr"),
                                  return = T,
                                  combine = "rbind",
                                  errorhandling = "pass",
                                  verbose = T)
                       },
                       
                       GCM.change_grow.stage_by.month_parallel = function(){
                         # 调用私有方法GCM.change_grow.stage_by.month()并行统计降雨，温度及辐射全生育期按月平均
                         parallel(func = GCM.change_grow.stage_by.month,
                                  SSP = SSPs,
                                  site = sites,
                                  GCM = GCMs,
                                  path.met = path.mets,
                                  MoreArgs = list(
                                    baseline.year.start = baseline.year.start,
                                    baseline.year.end = baseline.year.end,
                                    future.year.start = future.year.start,
                                    future.year.end = future.year.end,
                                    grow.month.start = grow.month.start,
                                    grow.month.end = grow.month.end
                                  ),
                                  cores = parallel.cores,
                                  export = c("GCM.change_grow.stage_all", "SSPs", "sites", "GCMs", "path.mets", "baseline.year.start", "baseline.year.end", "future.year.start", "future.year.end", "grow.month.start", "grow.month.end", "read_apsim_met"),
                                  packages = c("data.table", "magrittr", "stringr"),
                                  return = T,
                                  combine = "rbind",
                                  errorhandling = "pass",
                                  verbose = T)
                       }
                     ),
                     # 私有成员
                     private = list(
                       # 私有方法
                       ## 降雨，温度及辐射全生育期平均----------------------------------------------
                       GCM.change_grow.stage_all = function(SSP, site, GCM, path.met, baseline.year.start, baseline.year.end, future.year.start, future.year.end, grow.month.start, grow.month.end){
                         # 读入met文件
                         met = read_apsim_met(file = path.met) %>% setDT() %>% .[, 1:6]
                         # 生成日期（年-月-日）列
                         met = met[, orignday := as.Date(paste0(met$year,"-1-1"))] %>% .[, date := orignday + day - 1] %>% .[,!"orignday"]
                         # 提取生育期内数据
                         grow.met = met[month(date) >= grow.month.start | month(date) <= grow.month.end]
                         # 追加一列years添加标签Baseline和Future, 并筛选出来
                         grow.met[year(date) >= baseline.year.start & year(date) <= baseline.year.end, years := "Baseline"]
                         grow.met[year(date) >= future.year.start & year(date) <= future.year.end, years := "Future"]
                         grow.met = grow.met[years %in% c("Baseline","Future")]
                         # 添加最高温和最低温的平均值meanT
                         grow.met[,meanT := (maxt + mint)/2]
                         # 根据GCM, 站点, years(Baseline和Future)计算met平均值
                         mean.grow.met = grow.met[,lapply(.SD, function(x){mean(x)}), .SDcols = c("meanT", "rain", "radn"), by = .(years)]
                         
                         rm(grow.met, met); gc()
                         # 添加站点, GCM等信息
                         mean.grow.met.change = mean.grow.met[, c("SSP", "site", "GCM") := list(SSP, site, GCM)] %>% dcast(formula = SSP + GCM + site ~ years, value.var = c("meanT", "rain", "radn")) %>% .[,c("meanT.change", "rain.change", "radn.change") := list(meanT_Future - meanT_Baseline, (rain_Future - rain_Baseline)/rain_Baseline * 100, (radn_Future - radn_Baseline)/radn_Baseline * 100)] # 温度为变化度数, 降雨和辐射为变化百分比
                         # 返回值
                         return(mean.grow.met.change)
                       },
                       
                       ## 降雨，温度及辐射全生育期按月平均----------------------------------------------
                       GCM.change_grow.stage_by.month = function(SSP, site, GCM, path.met, baseline.year.start, baseline.year.end, future.year.start, future.year.end, grow.month.start, grow.month.end){
                         # 读入met文件
                         met = read_apsim_met(file = path.met) %>% setDT() %>% .[, 1:6]
                         # 生成日期（年-月-日）列
                         met = met[, orignday := as.Date(paste0(met$year,"-1-1"))] %>% .[, date := orignday + day - 1] %>% .[,!"orignday"]
                         # 提取生育期内数据
                         grow.met = met[month(date) >= grow.month.start | month(date) <= grow.month.end]
                         # 追加一列years添加标签Baseline和Future, 并筛选出来
                         grow.met[year(date) >= baseline.year.start & year(date) <= baseline.year.end, years := "Baseline"]
                         grow.met[year(date) >= future.year.start & year(date) <= future.year.end, years := "Future"]
                         grow.met = grow.met[years %in% c("Baseline","Future")]
                         # 添加最高温和最低温的平均值meanT
                         grow.met[,meanT := (maxt + mint)/2]
                         # 添加月一列
                         grow.met[, month := month(date)]
                         # 根据GCM, 站点, years(Baseline和Future)计算met平均值
                         mean.grow.met = grow.met[,lapply(.SD, function(x){mean(x)}), .SDcols = c("meanT", "rain", "radn"), by = .(years, month)]
                         
                         rm(grow.met, met); gc()
                         # 添加站点, GCM等信息
                         mean.grow.met.change = mean.grow.met[, c("SSP", "site", "GCM") := list(SSP, site, GCM)] %>% dcast(formula = SSP + GCM + site + month ~ years, value.var = c("meanT", "rain", "radn")) %>% .[,c("meanT.change", "rain.change", "radn.change") := list(meanT_Future - meanT_Baseline, (rain_Future - rain_Baseline)/rain_Baseline * 100, (radn_Future - radn_Baseline)/radn_Baseline * 100)] # 温度为变化度数, 降雨和辐射为变化百分比
                         # 返回值
                         return(mean.grow.met.change)
                       },
                       
                       ## 并行函数----------------------------------------------
                       parallel = function(func, ..., cores = NULL, MoreArgs = NULL, return = F, combine = "list", export = NULL, packages = NULL, errorhandling = c("stop", "remove", "pass"), verbose = F){
                         #' @param func 被并行函数
                         #' @param ... func的多个动态参数
                         #' @param cores 要运行的线程数
                         #' @param MoreArgs func的静态参数(list)
                         #' @param return 是否有返回值
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
                         if (return == F) {
                           foreach::foreach(i = seq(length(dots[[1]])), .combine = combine, .export = export, .packages = packages, .errorhandling = errorhandling, .verbose = verbose) %dopar% do.call(func,c(sapply(dots,`[`,i),MoreArgs))# 数据与参数组成list传入函数
                         }else{
                           result = foreach::foreach(i = seq(length(dots[[1]])), .combine = combine, .export = export, .packages = packages, .errorhandling = errorhandling, .verbose = verbose) %dopar% do.call(func,c(lapply(dots,`[`,i),MoreArgs))
                         }
                         
                         # 关闭
                         parallel::stopCluster(cl)
                         
                         #返回结果
                         if (return == F) {
                           return()
                         }else{
                           return(result)
                         }
                       },
                       
                       ## 因为原read_apsim_met()函数不能正常读取met文件, 所以在次进行了修改, nlines = 100-----------------
                       read_apsim_met = function(file, verbose = F){
                         #' @param file met文件
                         
                         if(!grepl("[Mm][Ee][Tt]$",file)) stop("file should have a .met extension")
                         
                         file.path  =  file
                         
                         ## Read the header
                         header  =  scan(file = file.path, 
                                         what = "character", 
                                         sep = "\n",
                                         blank.lines.skip = FALSE,
                                         nlines = 100, 
                                         quiet = TRUE)
                         ## hdrl is for keeping track of header lines
                         hdrl  =  0; skip.lines  =  0 
                         ## attrs  =  c("name","site","latitude","longitude","tav","amp","clnms","clunits")
                         name  =  NULL; site  =  NULL; latitude  =  NULL; longitude  =  NULL; 
                         tav  =  NULL; amp  =  NULL; clnms  =  NULL; clunits  =  NULL; comments  =  NULL
                         constants  =  vector(mode = "list",30); constant.count  =  0; fnd  =  FALSE
                         comment.lines  =  0
                         ## This is as ugly as it gets but so are met files
                         for(i in 1:100){
                           if(grepl("^!",header[i])){comment.lines  =  comment.lines + 1; next}
                           if(grepl("[weather.met.weather]",header[i],fixed=TRUE)){name  =  header[i];hdrl  =  hdrl + 1; fnd  =  TRUE}
                           if(grepl("^site",header[i],ignore.case=TRUE)){site  =  header[i];hdrl  =  hdrl + 1; fnd  =  TRUE}
                           if(grepl("^latitude",header[i],ignore.case=TRUE)){latitude  =  header[i];hdrl  =  hdrl + 1; fnd  =  TRUE} 
                           if(grepl("^longitude",header[i],ignore.case=TRUE)){longitude  =  header[i];hdrl  =  hdrl + 1; fnd  =  TRUE} 
                           if(grepl("^tav",header[i])){tav  =  header[i];hdrl  =  hdrl + 1; fnd  =  TRUE}
                           if(grepl("^amp",header[i])){amp  =  header[i];hdrl  =  hdrl + 1; fnd  =  TRUE}
                           if(grepl("year",header[i]) && grepl("radn",header[i])){clnms  =  header[i];hdrl  =  hdrl + 1; fnd  =  TRUE}
                           if(grepl("()",header[i],fixed=TRUE)){clunits  =  header[i];skip.lines  =  i;hdrl  =  hdrl + 1; fnd  =  TRUE}
                           if(grepl("=",header[i],fixed=TRUE) && fnd == FALSE){
                             constant.count  =  constant.count + 1
                             constants[constant.count]  =  header[i]
                             hdrl  =  hdrl + 1
                           } 
                           fnd  =  FALSE
                         }
                         
                         constants  =  unlist(constants[1:constant.count])
                         
                         if(constant.count == 0){
                           constants  =  NA
                         }
                         
                         if(verbose){
                           cat("Found ",hdrl," header lines \n")
                           cat("Found ",comment.lines," comment lines \n")
                           cat("Found ",skip.lines," skip lines \n")
                           cat("Found ",constant.count,"constants \n")
                         }
                         
                         ## I only check the first 6 column names but there might be more
                         clnms  =  sub("^\\s+","",clnms)
                         clnms.s  =  strsplit(clnms,"\\s+")[[1]]
                         if(sum(clnms.s %in% c("year","day","radn","maxt","mint","rain")) < 6){
                           cat("All column names:",clnms,"\n") 
                           warning("column names might be wrong")
                         }
                         
                         clunits  =  sub("^\\s+","",clunits)
                         clunits.s  =  strsplit(clunits,"\\s+")[[1]]
                         ## Sounds like there is no point in checking units
                         ## As they are a complete mess
                         
                         met  =  utils::read.table(file = file.path, 
                                                   header = FALSE, 
                                                   as.is = TRUE,
                                                   na.strings = c(NA,-99),
                                                   comment.char = "!", 
                                                   col.names = clnms.s,
                                                   skip = skip.lines)
                         
                         attr(met, "filename")  =  file
                         attr(met, "site")  =  ifelse(is.null(site),NA,site)
                         attr(met, "latitude")  =  latitude
                         attr(met, "longitude")  =  ifelse(is.null(longitude),NA,longitude)
                         attr(met, "tav")  =  tav
                         attr(met, "amp")  =  amp
                         attr(met, "colnames")  =  clnms.s
                         attr(met, "units")  =  clunits.s
                         attr(met, "constants")  =  constants
                         attr(met, "comments")  =  ifelse(is.null(comments),NA,comments)
                         class(met)  =  c("met","data.frame")
                         return(met)
                       }
                     ), 
                     
                     # 主动激活
                     active = list(     
                       ## 帮助信息函数
                       help = function(){
                         cat("类名:GCM.change\n目的:统计不同SSP和GCM下各站点的温度, 降雨及辐射的变化程度\n属性:SSPs: SSP类别 (向量)\n    sites: 站点名 (向量)\n    GCMs: GCM名称 (向量)\n    path.mets: 气象文件路径 (向量)\n    baseline.year.start: 当前起始年份 (单个年份值)\n    baseline.year.end: 当前终止年份 (单个年份值)\n    future.year.start: 未来起始年份 (单个年份值)\n    future.year.end: 未来终止年份 (单个年份值)\n    grow.month.start: 生育期起始月份 (单个月份值)\n    grow.month.end: 生育期终止月份 (单个月份值)\n    parallel.cores: 并行核数(默认四线程) ")
                       }
                     ))

# # 例子
# GCM.change
# class(GCM.change)
# 
# 
# # 参数准备
# SSPs = c("ssp245", "ssp245")
# sites = c("50136", "50137")
# GCMs = c("ACM", "ACM")
# path.mets = c("D:/学习/模型/气象文件示例/50136_S245_ACM_r0.MET", "D:/学习/模型/气象文件示例/50137_S245_ACM_r0.MET")
# baseline.year.start = 1991
# baseline.year.end = 2020
# future.year.start = 2036
# future.year.end = 2065
# grow.month.start = 9
# grow.month.end = 6
# parallel.cores = 2
# 
# # 类实例化
# example = GCM.change$new(SSPs = SSPs,
#                          sites = sites,
#                          GCMs = GCMs,
#                          path.mets = path.mets,
#                          baseline.year.start = baseline.year.start,
#                          baseline.year.end = baseline.year.end,
#                          future.year.start = future.year.start,
#                          future.year.end = future.year.end,
#                          grow.month.start = grow.month.start,
#                          grow.month.end = grow.month.end,
#                          parallel.cores = parallel.cores)
# example
# class(example)
# example$help
# 
# # 执行函数
# res = example$GCM.change_grow.stage_all_parallel()
# res2 = example$GCM.change_grow.stage_by.month_parallel()

