require(R6)

apsim_optim_arrange = R6Class(classname = "apsim_optim_arrange",
                              portable = F,
                              public = list(
                                # 公有属性
                                datas = NULL,
                                workdir = NULL,
                                name.orign_apsim = NULL,
                                file.xml = NULL,
                                region = NULL,
                                site = NULL,
                                lon = NULL,
                                lat = NULL,
                                var = NULL,
                                sow_date = NULL,
                                name.met = NULL,
                                name.cultivar = NULL,
                                name.SoilCrop = NULL,
                                sow.date = NULL,
                                start.date_set = NULL,
                                end.date_set = NULL,
                                # 初始化
                                initialize = function(datas, workdir, name.orign_apsim, file.xml, region, site, lon, lat, var, sow_date, name.met, name.cultivar, name.SoilCrop, sow.date, start.date_set, end.date_set){
                                  datas <<- datas
                                  workdir <<- workdir
                                  name.orign_apsim <<- name.orign_apsim
                                  file.xml <<- file.xml
                                  region <<- region
                                  site <<- site
                                  lon <<- lon
                                  lat <<- lat
                                  var <<- var
                                  sow_date <<- sow_date
                                  name.met <<- name.met
                                  name.cultivar <<- name.cultivar
                                  name.SoilCrop <<- name.SoilCrop
                                  sow.date <<- sow.date
                                  start.date_set <<- start.date_set
                                  end.date_set <<- end.date_set
                                },
                                
                                # 公有方法
                                ## 调参准备文件
                                apsim_optim_arrange_purrr = function(){
                                  purrr::walk(.x = datas, 
                                              .f = apsim_optim_arrange,
                                              workdir = workdir,
                                              name.orign_apsim = name.orign_apsim,
                                              file.xml = file.xml,
                                              region = region,
                                              site = site,
                                              lon = lon,
                                              lat = lat,
                                              var = var,
                                              sow_date = sow_date,
                                              name.met = name.met,
                                              name.cultivar = name.cultivar,
                                              name.SoilCrop = name.SoilCrop,
                                              sow.date = sow.date,
                                              start.date_set = start.date_set,
                                              end.date_set = end.date_set
                                  ) 
                                },
                                
                                ## 播放音乐
                                music_play = function(path.music){
                                  tuneR::play(path.music)
                                }
                              ),
                              private = list(
                                # 私有方法
                                ## 准备apsim文件 (替换播种日期, 土壤文件, met文件, 品种文件)
                                apsim_optim_arrange = function(workdir, data, name.orign_apsim, file.xml, region, site, lon, lat, var, sow_date, name.met, name.cultivar, name.SoilCrop, sow.date, start.date_set, end.date_set){
                                  #' @description 修改原始apsim以备调参使用, 如果不止包括同一地区
                                  #' @param workdir 工作路径(原始apsim文件所在路径，并将在此路径下扩展apsim文件)
                                  #' @param data 调参数据表, 包括地区, 地点, 经纬度, 品种, 播期和apsim模拟名等
                                  #' @param name.orign_apsim 原始apsim文件名
                                  #' @param region 调参数据表的地区列名
                                  #' @param site 调参数据表的地点列名
                                  #' @param lon 调参数据表的经度列名
                                  #' @param lat 调参数据表的纬度列名
                                  #' @param var 调参数据表的品种列名
                                  #' @param sow_date 调参数据表的播期列名
                                  #' @param name_met 原始文件中met文件的名称
                                  #' @param name.cultivar 原始文件中品种的名称
                                  #' @param name.SoilCrop 原始文件中土壤作物的名称
                                  #' @param sow.date 原始文件中的播期
                                  #' @param start.date_set 要设置的模拟起始日期, 如 "01/01/2021"
                                  #' @param end.date_set 要设置的模拟终止日期, 如 "31/12/2022"
                                  
                                  # 设置工作路径
                                  setwd(workdir)
                                  
                                  # 设置英文时间
                                  Sys.setlocale(category = "LC_TIME", locale = "English")
                                  
                                  # eval(parse): 必须指定不同的名字
                                  reg = region
                                  s = site
                                  lo = lon
                                  la = lat
                                  v = var
                                  sd = sow_date
                                  
                                  try({
                                    #### 复制原始apsim文件
                                    name.optim_apsim = paste0(str_sub(string = name.orign_apsim, start = 1L, end = -7L), "_", data[, eval(parse(text = reg))][1], ".apsim")
                                    print(paste0("即将准备的apsim文件: ", name.optim_apsim))
                                    file.copy(from = name.orign_apsim, to = name.optim_apsim, overwrite = T)
                                    #### 将origin.apsim的单模拟扩展为需要的模拟数
                                    simulation_num = nrow(data)
                                    ##### 不存在的话提取单个模拟
                                    if (file.exists("single_simulation.out")) {
                                      file.remove("single_simulation.out")
                                    }
                                    shell(cmd = sed_range_p(file = name.optim_apsim, pattern1 = "simulation name=", pattern2 = "\\/simulation", outFile = "single_simulation.out"))
                                    ##### 添加模拟至所需要数
                                    i = 1
                                    for (i in 1:(simulation_num - 1)) {
                                      shell(cmd = sed_r_row(file = name.optim_apsim, inputFile = "single_simulation.out", row = as.numeric(shell(cmd = wc_l(file = name.optim_apsim), intern = T))))
                                    }
                                    print(paste0(name.optim_apsim, " ==》共扩展至: ", simulation_num, " 个模拟! 请注意数目是否正确!"))
                                    #### 循环更改所需参数
                                    i = 1
                                    for (i in 1:nrow(data)) {
                                      # 提取out文件名行号
                                      row_out = shell(cmd = grep_n(pattern = "filename output=", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric()
                                      # 提取out_title行号
                                      row_out.title = row_out + 1
                                      # 提取模拟名行号
                                      row_simulation = shell(cmd = grep_n(pattern = "simulation name=", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric()
                                      # 提取met文件行号
                                      row_met = shell(cmd = grep_n(pattern = "filename name=\"filename\" input=\"yes\"", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric()
                                      # 提取播期行号
                                      row_sow.date = shell(cmd = grep_n(pattern = "Enter sowing date (dd-mmm)", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric()
                                      # 提取品种行号
                                      row_var.date = shell(cmd = grep_n(pattern = "Enter cultivar", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric()
                                      
                                      # 改模拟名
                                      shell(cmd = sed_n_s(row = row_simulation[i], from = shell(cmd = sed_n_sel(row = row_simulation[i], pattern = '"', file = name.optim_apsim, col = 2), intern = T), to = data[, eval(parse(text = s))][i], file = name.optim_apsim))
                                      # 改out文件名
                                      shell(cmd = sed_n_s(file = name.optim_apsim, row = row_out[i], from = str_split_fixed(str_split_fixed(string = shell(cmd = sed_n_sel(row = row_out[i], pattern = '"', file = name.optim_apsim, col = 3), intern = T), pattern = "<", n = 2)[, 1], pattern = ">", n = 2)[, 2], to = paste0(data[, eval(parse(text = s))][i], ".out")))
                                      # 改out标题
                                      shell(cmd = sed_n_s(file = name.optim_apsim, row = row_out.title[i], from = str_split_fixed(str_split_fixed(string = shell(cmd = sed_n_sel(row = row_out.title[i], pattern = 'title', file = name.optim_apsim, col = 2), intern = T), pattern = "<", n = 2)[, 1], pattern = ">", n = 2)[, 2], to = data[, eval(parse(text = s))][i]))
                                      # 改met文件
                                      shell(cmd = sed_n_s(row = row_met[i], from = name.met, to = paste0(data[, eval(parse(text = s))][i], ".met"), file = name.optim_apsim))
                                      # 改播期
                                      shell(cmd = sed_n_s(row = row_sow.date[i], from = sow.date, to = format(x = data[, eval(parse(text = sd))][i], format = "%d-%b"), file = name.optim_apsim))
                                      # 改品种名
                                      shell(cmd = sed_n_s(row = row_var.date[i], from = name.cultivar, to = data[, eval(parse(text = v))][i], file = name.optim_apsim))
                                      # 替换土壤文件
                                      ## 下载
                                      soil_profile = NULL
                                      soil_profile = get_isric_soil_profile(lonlat = c(data[, eval(parse(text = lo))][i], data[, eval(parse(text = la))][i]))
                                      soil_profile$crops = name.SoilCrop
                                      soil_profile$soilwat = NA
                                      ## 替换
                                      ### 执行脚本之前先把origin.apsim的soil模块行数手动统一到6行
                                      edit_apsim_replace_soil_profile(file = name.optim_apsim,
                                                                      src.dir = "./",
                                                                      wrt.dir = "./",
                                                                      soil.profile = soil_profile,
                                                                      edit.tag = "",
                                                                      overwrite = T, 
                                                                      root = data[, eval(parse(text = s))][i])
                                      # 替换硝态氮
                                      edit_apsim(file = name.optim_apsim, 
                                                 src.dir = "./", 
                                                 wrt.dir = "./", 
                                                 node = "Soil", 
                                                 soil.child = "Sample", 
                                                 parm = "NO3", 
                                                 value = c(46.645035296607517, 46.180909440630344, 45.721401714081495, 45.266466165805269, 44.816057301867893, 44.37013008100746), 
                                                 check.length = F,
                                                 overwrite = T, 
                                                 root = data[, eval(parse(text = s))][i])
                                      
                                      # 替换铵态氮
                                      edit_apsim(file = name.optim_apsim, 
                                                 src.dir = "./", 
                                                 wrt.dir = "./", 
                                                 node = "Soil", 
                                                 soil.child = "Sample", 
                                                 parm = "NH4", 
                                                 value = c(0.68344374060963409, 0.67664336176747764, 0.66991064782536991, 0.66324492550630432, 0.65664552823249667, 0.65011179605871738), 
                                                 check.length = F,
                                                 overwrite = T, 
                                                 root = data[, eval(parse(text = s))][i])
                                      
                                      print(paste0(i, " / ", nrow(data)))
                                    }
                                    
                                    # 修改起始日期
                                    ## 提取起始日期行号
                                    row_start.date = shell(cmd = grep_n(pattern = "start_date", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric() %>% .[1]
                                    ## 提取起始日期
                                    start_date = str_split_fixed(string = str_split_fixed(string = shell(cmd = sed_n_p(row = row_start.date, file = name.optim_apsim), intern = T), pattern = ">", n = 3)[, 2] , pattern = "<", n = 2)[, 1]
                                    ## 全局替换
                                    shell(cmd = `sed_s_#`(from = start_date, to = start.date_set, file = name.optim_apsim))
                                    # 修改终止日期
                                    ## 提取起始日期行号
                                    row_end.date = shell(cmd = grep_n(pattern = "end_date", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric() %>% .[1]
                                    ## 提取起始日期
                                    end_date = str_split_fixed(string = str_split_fixed(string = shell(cmd = sed_n_p(row = row_end.date, file = name.optim_apsim), intern = T), pattern = ">", n = 3)[, 2], pattern = "<", n = 2)[, 1]
                                    ## 全局替换
                                    shell(cmd = `sed_s_#`(from = end_date, to = end.date_set, file = name.optim_apsim))
                                    # 修改品种文件
                                    ## 提取品种文件行号
                                    row_xml = shell(cmd = grep_n(pattern = "filename input=", file = name.optim_apsim), intern = T) %>% str_remove(":") %>% as.numeric() %>% .[1]
                                    ## 提取品种文件名
                                    name_xml = str_split_fixed(str_split_fixed(string = str_split_fixed(string = shell(cmd = sed_n_p(row = row_xml, file = name.optim_apsim), intern = T), pattern = '"', n = 3)[, 3], pattern = "<", n = 2)[, 1], pattern = ">", n = 2)[, 2]
                                    ## 全局替换
                                    shell(cmd = `sed_s_#`(from = name_xml, to = file.xml, file = name.optim_apsim))
                                  })
                                  if (file.exists("'")) {
                                    file.remove("'")
                                  }
                                  
                                  # 移除单个模拟文件
                                  file.remove("single_simulation.out")
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
                                },
                                
                                ## linux2R
                                sed_s = function(file, from, to){
                                  #' @description sed 替换(sed -i 's/from/to/' file)
                                  #' @return NULL
                                  paste0("sed -i 's/", from, "/", to, "/' ", file)
                                },
                                
                                `sed_s_#` = function(file, from, to){
                                  #' @description sed 替换(sed -i 's#from#to#' file)
                                  #' @return NULL
                                  paste0("sed -i 's#", from, "#", to, "#' ", file)
                                },
                                
                                sed_n_s = function(file, row, from, to){
                                  #' @description sed 替换指定行(sed -i 'row s/from/to/' file)
                                  #' @return NULL
                                  paste0("sed -i '", row, "s/", from, "/", to, "/' ", file)
                                },
                                
                                sed_n_sel = function(file, row, pattern, col){
                                  #' @description 根据特征提取file指定行的字符(sed -n 'row p' file | awk -F "pattern" '{print $col}')
                                  #' @return 字符
                                  paste0("sed -n '", row, "p' ", file, " | awk -F '", pattern, "' '{print $", col, "}'")
                                },
                                
                                sed_n_p = function(file, row){
                                  #' @description 输出指定行(sed -n 'row p' file)
                                  #' @return 指定行内容
                                  paste0("sed -n '", row, "p' ", file)
                                },
                                
                                sed_range_p = function(file, outFile, pattern1, pattern2){
                                  #' @description 提取file特征之间的所有行并重定向(sed -n '/pattern1/,/pattern2/p' file > outFile)
                                  #' @return 无输出，但做了重定向
                                  paste0("sed -n '/", pattern1, "/, /", pattern2, "/p' ", file, " > ", outFile)
                                },
                                
                                sed_r_pattern = function(file, inputFile, pattern){
                                  #' @description 根据特征(其后)给file中插入文件(sed -i '/pattern/r inputFile' file)
                                  paste0("sed -i '/", pattern, "/r ", inputFile, "' ", file)
                                },
                                # shell(cmd = sed_r_pattern(file = name.orign_apsim, inputFile = "single_simulation.out", pattern = "\\/simulation"))
                                
                                sed_r_row = function(file, inputFile, row){
                                  #' @description 根据行号(其后)给file中插入文件(sed -i '/pattern/r inputFile' file)
                                  paste0("sed -i '", row, "r ", inputFile, "' ", file)
                                },
                                
                                grep_n = function(file, pattern){
                                  #' @description grep并返回行号(grep -n pattern file | awk '{print $1}')
                                  #' @return 行号
                                  paste0("grep -n '", pattern, "' ", file, " | awk '{print $1}'")
                                },
                                
                                wc_l = function(file){
                                  #' @description 返回file的行数
                                  paste0("wc -l ", file, " | awk '{print $1}'")
                                }
                              ),
                              active = list(
                                libs.check = function(){
                                  # 检查是否需要下载包, 需要的话则下载, 不需要的话返回NULL
                                  libs = c("apsimx", "data.table", "stringr", "magrittr", "foreach", "doParallel", "purrr", "tuneR")
                                  libs.exist = sapply(X = libs, FUN = "require", character.on = T)
                                  if (F %in% libs.exist) {
                                    sapply(libs[which(libs.exist == F)], install.packages, dependencies = T)
                                  }
                                }
                              ))

                      
# ### 读入调参地点信息
# #### && 先把土壤数据全部统一到6层
# data.orign <- openxlsx::read.xlsx("./调参地点信息.xlsx", detectDates = T) %>% setDT()
# head(data.orign)
# 
# #### 基本参数(初始值)
# ##### 工作路径(原始apsim文件所在路径，并将在此路径下扩展apsim文件)
# workdir <- "D:/学习/课题/调参数据/正式调参"
# #####  分地区的调参地点信息列表
# datas <- split.data.frame(x = data.orign, f = data.orign$region)
# str(datas)
# ##### 原始apsim文件
# name.orign_apsim <- "Optimisation_origin.apsim"
# #### 品种文件
# file.xml <- "Wheat_opt.xml"
# #### 调参地点信息列名
# region <- "region" # 地区
# site <- "site"
# lon <- "lon"
# lat <- "lat"
# var <- "var" # 品种
# sow_date <- "sow_date"
# ##### 文件中已设置
# name.met <- "Huhu.met"
# name.cultivar <- "Huhu.cultivar"
# name.SoilCrop <- "wheat"
# sow.date  <- "25-Oct"
##### 文件中要设置
# start.date_set <- "01/01/2021"
# end.date_set <- "31/12/2022"
# apsim.arrange <- apsim_optim_arrange$new(
#     datas = datas,
#     workdir = workdir,
#     name.orign_apsim = name.orign_apsim,
#     file.xml = file.xml,
#     region = region,
#     site = site,
#     lon = lon,
#     lat = lat,
#     var = var,
#     sow_date = sow_date,
#     name.met = name.met,
#     name.cultivar = name.cultivar,
#     name.SoilCrop = name.SoilCrop,
#     sow.date = sow.date,
#     start.date_set = start.date_set,
#     end.date_set = end.date_set
#   )
#   
# apsim.arrange$libs.check # # 检查包的缺失情况
# apsim.arrange$apsim_optim_arrange_purrr()
# apsim.arrange$music_play(path.music = "D:/音乐/多余的解释-许嵩.mp3")


