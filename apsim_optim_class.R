require(R6)

apsim_optim = R6Class(classname = "apsim_optim",
                      portable = F,
                      public = list(
                        # 公有属性
                        regions = NULL,
                        varietys = NULL,
                        parms = NULL,
                        file.apsims = NULL,
                        paths.obs = NULL,
                        work.dir = NULL,
                        file.origin.crop = NULL,
                        index = NULL,
                        dir.met = NULL,
                        optim_upper = NULL,
                        optim_lower = NULL,
                        file.output = NULL,
                        # 初始化
                        initialize = function(regions, varietys, parms, file.apsims, paths.obs, work.dir, file.origin.crop, index, dir.met, optim_upper, optim_lower, file.output){
                          regions <<- regions
                          varietys <<- varietys
                          parms <<- parms
                          file.apsims <<- file.apsims
                          paths.obs <<- paths.obs
                          work.dir <<- work.dir
                          file.origin.crop <<- file.origin.crop
                          index <<- index
                          dir.met <<- dir.met
                          optim_upper <<- optim_upper
                          optim_lower <<- optim_lower
                          file.output <<- file.output
                        },
                        # 公有方法
                        ## 调参并行函数
                        apsim_optim_parallel = function(){
                          cores.computer = parallel::detectCores(logical = T)
                          if (length(regions) <= cores.computer) {
                            cores = length(regions)
                          } else {
                            cores = cores.compute
                          }
                          
                          parallel(func = apsim_optim_huhu,
                                   region = regions, 
                                   variety = varietys,
                                   parm = parms,
                                   file.apsim = file.apsims,
                                   path.obs = paths.obs,
                                   MoreArgs = list(
                                     work.dir = work.dir,
                                     file.origin.crop = file.origin.crop, 
                                     index = index,
                                     dir.met = dir.met, 
                                     optim_upper = optim_upper,
                                     optim_lower = optim_lower,
                                     file.output = file.output
                                   ),
                                   cores = cores,
                                   combine = "list",
                                   export = c("apsim_optim_huhu", "regions", "varietys", "parms", "file.apsims", "cores", "work.dir", "file.origin.crop", "index", "dir.met", "paths.obs", "optim_upper", "optim_lower", "file.output"),
                                   packages = c("apsimx", "data.table", "stringr", "magrittr"),
                                   errorhandling = "pass",
                                   verbose = T)
                        },
                        
                        ## 播放音乐
                        music_play = function(path.music){
                          tuneR::play(path.music)
                        }
                      ),
                      private = list(
                        # 私有方法
                        ## 调参函数
                        apsim_optim_huhu = function(region, variety, work.dir, file.apsim, file.origin.crop, index, dir.met, path.obs, parm, optim_upper, optim_lower, file.output){
                          #' @description 根据给定的apsim, 品种, met, 以及outfile文件进行参数优化
                          #' @param region 不同的区域, 名称用户自己写
                          #' @param variety 品种名称, 名称用户自己写
                          #' @param work.dir 工作路径
                          #' @param file.apsim apsim文件
                          #' @param file.origin.crop 作物初始文件
                          #' @param index 调参对象
                          #' @param dir.met 气象文件夹路径
                          #' @param path.obs 观测值文件路径, 第一列为outfile, 第二列为Date, 后续看情况, 例子中为生物量
                          #' @param parms 要优化的参数, 不确定的话可以使用apsimx::inspect_apsim_xml()来检查提取
                          #' @param optim_upper 调参上限, 如1.2为现有值*1.2
                          #' @param optim_lower 调参下限, 如0.8为现有值*0.8
                          #' @param file.output 输出文件名
                          
                          libs = c("apsimx", "data.table", "stringr", "magrittr", "foreach", "doParallel", "ggplot2", "tuneR")
                          sapply(X = libs, FUN = "require", character.on = T)
                          
                          # 设置不警告以及apsimx包版本检查
                          apsim_options(warn.versions = F)
                          if(packageVersion(pkg = "apsimx") < 1.975){stop("You need a newer version of apsimx for this example")}
                          
                          # 设置工作路径
                          setwd(dir = work.dir)
                          
                          # 创建./optim/region文件夹, 不同品种在一个品种文件，不用创建品种文件夹
                          aim.dir = paste0("./", file.output, "/", region, "/")
                          if (! dir.exists(aim.dir)) {dir.create(aim.dir, recursive = T)}
                          
                          # 复制apsim和xml文件到aim.dir
                          file.copy(from = file.apsim, to = paste0(aim.dir, file.apsim), overwrite = T, copy.mode = T)
                          file.copy(from = file.origin.crop, to = paste0(aim.dir, file.origin.crop), overwrite = T, copy.mode = T)
                          setwd(dir = aim.dir)
                          
                          # 读入观测值
                          obs = read.csv(file = path.obs)
                          obs$Date = as.Date(obs$Date)
                          
                          ## Optimization step
                          ## Find relevant parameters
                          file.crop = file.origin.crop
                          i = 1
                          parms = str_split(string = parm, pattern = "-") %>% unlist()
                          p.vector = vector(length = length(parms))
                          for (i in 1:length(parms)) {
                            assign(x = paste0("p", i), # 提取优化参数路径并赋值
                                   value = inspect_apsim_xml(file = file.crop, # 品种文件
                                                             src.dir = ".",
                                                             parm = parms[i], 
                                                             verbose = T, 
                                                             print.path = T))
                            
                            if (i == 1) {
                              p.vector[i] = paste0("c(p", i)
                            }else if (i == max(1:length(parms))) {
                              p.vector[i] = paste0("p", i, ")")
                            }else{
                              p.vector[i] = paste0("p", i)
                            }
                          }
                          
                          p.optim = if (length(p.vector) != 1) {
                            eval(parse(text = paste(p.vector, collapse = ", ")))
                          }else {
                            eval(parse(text = paste(p.vector, ")", collapse = ", ")))
                          }
                          
                          cat("优化参数路径为: \n"); p.optim
                          
                          ## Run optimization
                          ### 建议您保留原始文件的备份。此函数将在优化过程中编辑和覆盖文件
                          ### 初始值必须为作物文件中参数的值
                          ### apsim, xml和met文件放在同一路径下
                          ### 此函数假定优化作物文件, 但也可以优化apsim文件
                          
                          ### 复制气象文件
                          path.met = dir(path = dir.met, pattern = "\\.met", full.names = T)
                          file.copy(from = path.met, to = ".", overwrite = T, copy.mode = T) # 调参完后删除
                          
                          # 优化前观测值和模拟值的匹配程度
                          sim <- apsim(file = file.apsim, value = "report")
                          pdf(file = paste0("../", region, "_before.optimization.pdf"), width = 10, height = 10, family = "serif")
                          p <- ggplot()+ 
                            facet_wrap(~outfile) + 
                            geom_point(data = obs, aes(x = Date, y = eval(parse(text = index))))+
                            geom_line(data = sim, aes(x = Date, y = eval(parse(text = index))))+
                            theme_test()
                          print(p)
                          dev.off()
                          
                          ### 调参
                          Sys.time()
                          op = optim_apsim(file = file.apsim,
                                           src.dir = ".",
                                           crop.file = file.crop,
                                           parm.paths = p.optim,
                                           index = c("outfile", "Date"), # 筛选apsim的out文件内容
                                           data = obs, # 观测数据
                                           type = "optim", # 初始值为1
                                           # optim()函数, method = "L-BFGS-B", 是Byrd等人（1995）的方法，它允许框约束，即每个变量都可以给定一个下限和/或上限。初始值必须满足约束条件。这使用BFGS准牛顿方法的有限记忆修改。如果提供了非平凡边界，则将选择此方法，并显示警告。
                                           method = "L-BFGS-B", # optim()函数
                                           # 观测值上下浮动
                                           lower = rep(optim_lower, length(p.optim)), # optim()函数, method = "L-BFGS-B", 跟调参数等长的向量
                                           upper = rep(optim_upper, length(p.optim)), # optim()函数, method = "L-BFGS-B", 跟调参数等长的向量
                                           hessian = T, # optim()函数, 返回数值微分的Hessian矩阵, 会计算置信区间和标准误差
                                           # xml.parm = c(T), # 优化.apsim文件和“crop.file”中的参数时使用的可选逻辑向量。如果缺少“crop.files”，则假定要优化的参数在.apsim中。如果不缺少“crop.file”，则假定它们位于“crop.file”中。如果参数同时位于两者中，则需要在此参数中指定。
                                           weights = "mean") # 计算目标函数（残差平方和）时, 会组合不同的变量, 由于单位不同，所以通常会加权处理, 如mean, var等。
                          # parm.vector.index = , # 使用parm.vector.index时，不能同时编辑矢量的两个单独元素。这应该仅用于针对向量的单个元素
                          
                          op
                          
                          Sys.time()
                          
                          # 写出优化后的文本
                          op.res = data.table(region = region,
                                              variety = variety,
                                              parameter = parms, 
                                              origin_value = lapply(1:length(parms), function(x) {op$iaux.parms[[x]]}) %>% unlist(),
                                              par = op$op$par,
                                              optim.result = lapply(1:length(parms), function(x) {op$iaux.parms[[x]] * op$op$par[x]}) %>% unlist())
                          fwrite(op.res, paste0("../调参结果.csv"), append = T)
                          
                          # 优化后观测值和模拟值的匹配程度
                          sim <- apsim(file = file.apsim, value = "report")
                          pdf(file = paste0("../", region, "_optimized.pdf"), width = 10, height = 10, family = "serif")
                          p <- ggplot()+ 
                            facet_wrap(~outfile) + 
                            geom_point(data = obs, aes(x = Date, y = eval(parse(text = index))))+
                            geom_line(data = sim, aes(x = Date, y = eval(parse(text = index))))+
                            theme_test()
                          print(p)
                          dev.off()
                          
                          # 清除met文件
                          path.met = dir(path = ".", pattern = "\\.met", full.names = T)
                          file.remove(path.met)
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
                      active = list(
                        libs.check = function(){
                          # 检查是否需要下载包, 需要的话则下载, 不需要的话返回NULL
                          libs = c("apsimx", "data.table", "stringr", "magrittr", "foreach", "doParallel", "ggplot2", "tuneR")
                          libs.exist = sapply(X = libs, FUN = "require", character.on = T)
                          if (F %in% libs.exist) {
                            sapply(libs[which(libs.exist == F)], install.packages, dependencies = T)
                          }
                        }
                      ))
                       

# apsim_optim
# 
# # 设置参数
# regions = c("up", "down") # 用于区别不同地区, 根据实际情况填写
# varietys = c("base_cultivar", "base_cultivar") # 用于区别不同品种, 根据实际情况填写
# parms = rep(x = "base_cultivar/tt_start_grain_fill-base_cultivar/potential_grain_filling_rate", length(regions)) # 必须指定品种, 如base_cultivar, 且每个地区的参数用"-"隔开, 不同地区可以指定不同参数
# file.apsims = c("CC_Rye_BARC_KELLY.apsim", "CC_Rye_BARC_KELLY.apsim")
# paths.obs = c("D:/学习/课题/调参数据/multi-sim-example/Observed-biomass-rye.csv", "D:/学习/课题/调参数据/multi-sim-example/Observed-biomass-rye.csv")
# work.dir = "D:/学习/课题/调参数据/multi-sim-example"
# file.origin.crop = "Rye_cultivars.xml"
# dir.met = "D:/学习/课题/调参数据/multi-sim-example"
# optim_upper = 1.2
# optim_lower = 0.8
# 
# test = apsim_optim$new(regions = regions,
#                        varietys = varietys,
#                        parms = parms,
#                        file.apsims = file.apsims,
#                        paths.obs = paths.obs,
#                        work.dir = work.dir,
#                        file.origin.crop = file.origin.crop,
#                        dir.met = dir.met,
#                        optim_upper = optim_upper,
#                        optim_lower = optim_lower)
# 
# test$apsim_optim_parallel()
# test$music_play("D:/音乐/多余的解释-许嵩.mp3")


