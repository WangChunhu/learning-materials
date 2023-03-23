# 单环流单站点计算bio等环境变量自定义函数-------------------------------------------------
singleSite.env.cal <- function(site.fileName, SSPs.fileName, baseline, Future.1, Future.2, month.start, month.end, frost.T, heat.T, bio.0 = 10){ 
  #' @description 自定义函数 # 根据站点文件名提取站点名称
  #' @param site.fileName 站点名，不包含路径
  #' @param SSPs.fileName 环流名，不包含路径
  #' @param baseline 历史气候时间段，c(T1, T2)
  #' @param Future.1 未来气候时间段1，c(T1, T2)
  #' @param Future.2 未来气候时间段2，c(T1, T2)
  #' @param month.start 生育期起始月份
  #' @param month.start 生育期终止月份
  #' @param frost.T 霜冻临界值，℃
  #' @param heat.T 热害临界值，℃
  #' @param bio.0 生物学零度 ℃
  #' @return 站点名、环流名、气候段、bio1:19、年累计辐射、年有效积温、霜冻天数、热害天数，data.table
  
  site.num <- stringr::str_split(string = site.fileName, pattern = "_") %>% unlist() %>% .[1]
  
  # 读入单站点的met文件
  site.met <- read_apsim_met(file = paste0("./", SSPs.fileName, "./", site.fileName), verbose = F) %>% data.table()
  # 添加date列,并筛选出油菜生育期，10月1日-次年5月31日
  site.met$orignday <- as.Date(paste0(site.met$year,"-1-1"))
  site.met <- site.met[,date := orignday + day - 1] %>% .[,!"orignday"]
  grow.met <- site.met[month(date) >= month.start | month(date) <= month.end] %>% .[, month := month(date)]
  
  # 筛选历史及将来气候
  grow.met[year >= baseline[1] & year <= baseline[2], "years"] <- "Baseline"
  grow.met[year >= Future.1[1] & year <= Future.1[2], "years"] <- "Future-1"
  grow.met[year >= Future.2[1] & year <= Future.2[2], "years"] <- "Future-2"
  grow.met <- grow.met[years %in% c("Baseline", "Future-1", "Future-2")] %>% .[, c("site", "SSP") := list(site.num, SSPs.fileName)] %>% .[, num := 1] %>% .[, bio.0 := bio.0] 
  
  # 生育期内气候参数
  ## bio
  ### 扩大到月尺度
  bio.data.T <- grow.met[, lapply(.SD, mean), .SDcols = c("maxt", "mint"), by = .(years, year, month)] # 平均
  bio.data.rain <- grow.met[, lapply(.SD, sum), .SDcols = c("rain"), by = .(years, year, month)] # 累计
  bio.data <- cbind(bio.data.T, bio.data.rain) %>% .[, !6:8]
  ### 根据years和year分割data.data为list
  bio.data.list <- split.data.frame(x = bio.data, f = ~ bio.data$year)
  ### 计算bio
  biovars.huhu <- function(df){
    dismo::biovars(prec = df$rain, tmin = df$mint, tmax = df$maxt)
  }
  
  bio.list <- lapply(bio.data.list, biovars.huhu)
  bio <- do.call("rbind", bio.list) %>% data.table() %>% .[, year := unique(bio.data$year)]
  bio[year >= baseline[1] & year <= baseline[2], "years"] <- "Baseline"
  bio[year >= Future.1[1] & year <= Future.1[2], "years"] <- "Future-1"
  bio[year >= Future.2[1] & year <= Future.2[2], "years"] <- "Future-2"
  
  ### 计算各years平均bio
  bio.mean <- bio[, lapply(.SD, mean), .SDcols = c(names(bio)[-c(20, 21)]), by = .(years)]
  
  # 生育期内年平均累计辐射总量
  radn <- grow.met[, .(radn = sum(radn)), by = .(years, year)] %>% .[, .(sum.year.radn = mean(radn)), by = (years)]
  
  # 生育期内年平均有效积温，取10℃为生物学零度
  T.acc <- grow.met[, .(T.acc = sum((maxt + mint)/2 - `bio.0`)), by = .(years, year)] %>% .[, .(T.acc = mean(T.acc)), by = (years)]
  
  # 生育期内年平均累计霜冻天数
  frost <- grow.met[mint < frost.T , .(frost.days = sum(num)), by = .(years, year)] %>% .[, .(frost.days = mean(frost.days)), by = .(years)]
  if (nrow(frost) < 3) {
    if (nrow(frost) == 0) {
      frost <- data.table(years = c("Baseline", "Future-1", "Future-2"), frost.days = 0)
    }else{
      years.judge = c("Baseline", "Future-1", "Future-2")
      years.na = data.table(years = years.judge[!years.judge %in% frost$years], frost.days = 0)
      frost <- rbind(frost, years.na) %>% setorder("years")
    } 
  }
  
  # 生育期内年平均累计热害天数
  heat <- grow.met[maxt > heat.T , .(heat.days = sum(num)), by = .(years, year)] %>% .[, .(heat.days = mean(heat.days)), by = .(years)]
  if (nrow(heat) < 3) {
    if (nrow(heat) == 0) {
      heat <- data.table(years = c("Baseline", "Future-1", "Future-2"), heat.days = 0)
    }else{
      years.judge = c("Baseline", "Future-1", "Future-2")
      years.na = data.table(years = years.judge[!years.judge %in% heat$years], heat.days = 0)
      heat <- rbind(heat, years.na) %>% setorder("years")
    } 
  }
  
  # 合并k站点所有环境变量数据
  env <- cbind(bio.mean, radn[, !1], T.acc[, !1], frost[, !1], heat[, !1]) %>% .[, c("site", "SSP") := list(site.num, SSPs.fileName)] %>% setcolorder(c(25L, 26L, 1L:24L))
  
  return(env)
}
# -------------------------------------------------------------------------

# 多环流多站点计算bio等环境变量自定义函数(嵌套singleSite.env.cal使用)---------------------
multiSite.env.cal <- 
  function(SSPs.fileName, SSPs.path){
  #' @param SSPs.fileName 气候模型文件名
  #' @param SSPs.path 气候模型文件路径
  
  # 设置工作空间到气候环流文件目录
  if (getwd() != SSPs.path) {
    setwd(SSPs.path)
  }
  
  # 单环流模式下所有站点文件名
  site.fileName.all <- dir(path = SSPs.fileName, pattern = "\\.MET")
  
  # 计算单环流env
  i.env.list <- lapply(site.fileName.all,
                       singleSite.env.cal, 
                       SSPs.fileName = SSPs.fileName, 
                       baseline = baseline,
                       Future.1 = Future.1,
                       Future.2 = Future.2,
                       month.start = month.start,
                       month.end = month.end,
                       frost.T = frost.T,
                       heat.T = heat.T,
                       bio.0 = bio.0)
  
  # 合并所有环流env
  i.env <- do.call("rbind", i.env.list)
  
  return(i.env)
}