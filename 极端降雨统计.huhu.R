waterLogging.cal.huhu <- function(SSP, site, GCM, path.met){
  #' @description 以2.5sd为极端标准按月累计按年平均计算气象文件的极端涝渍次数
  #' @param SSP SSP类别
  #' @param site 站点名
  #' @param GCM GCM名称
  #' @param path.met 气象文件路径
  #' @return 气象文件的极端涝渍次数(years[Baseline/Future], month)
  
  # 读入met文件
  met <- read_apsim_met(file = path.met) %>% setDT() %>% .[, c(1:2, 6)]
  # 生成日期（年-月-日）列
  met <- met[, orignday := as.Date(paste0(met$year,"-1-1"))] %>% .[, date := orignday + day - 1] %>% .[, !c(2, 4)] %>% .[, month := month(date)]
  # 按1971-2070计算平均值和标准偏差
  years.mean_sd <- met[year >= 1971 & year < 2071] %>% .[, .(mean = mean(rain, na.rm = T), sd = sd(rain, na.rm = T))]
  ## 生育期
  month.start <- 9
  month.end <- 6
  # 提取生育期
  grow.met <- met[month(date) >= month.start | month(date) <= month.end]
  # 追加一列years添加标签Baseline(1991-2020)和Future(2036-2065), 并筛选出来
  grow.met[year(date) >= 1991 & year(date) <= 2020, years := "Baseline"]
  grow.met[year(date) >= 2036 & year(date) <= 2065, years := "Future"]
  grow.met <- grow.met[years %in% c("Baseline","Future")]
  
  rm(met); gc()

  # 计算极端涝渍, 即每个降雨量与上一步mean和2.5sd的关系 ((x-mean) - 2.5sd 和 (x > mean))
  grow.met.sd <- grow.met[, c("2.5sd", "diff") := list((rain - years.mean_sd$mean) >= (years.mean_sd$sd * 2.5), rain > years.mean_sd$mean)] %>% na.omit() %>% .[`2.5sd` %in% T & diff %in% T, !c("2.5sd", "diff")]
  
  rm(grow.met, years.mean_sd); gc()
  # 按月统计极端涝渍, 并按年平均
  grow.met.sd_month <- grow.met.sd[, .(极端降雨次数 = length(rain)), by = .(years, year, month)] %>% .[, .(`极端降雨次数/年平均` = mean(极端降雨次数)), by = .(years, month)]
  
  rm(grow.met.sd); gc()
  # 添加站点, GCM等信息
  grow.met.sd_month[, c("SSP", "site", "GCM") := list(SSP, site, GCM)] %>% setcolorder(c(4L:6L, 1L:3L))
  
  return(grow.met.sd_month)
}

# # test
# # 参数
# ## met文件信息
# path.met_df <- data.table(SSP = "245", Station = 50136, GCM = "ACM", Path = "D:/学习/模型/气象文件示例/50136_S245_ACM_r0.MET")
# 
# ## waterLogging.cal.huhu参数
# SSP <- path.met_df$SSP
# site <- path.met_df$Station
# GCM <- path.met_df$GCM
# path.met <- path.met_df$Path
# 
# res <- waterLogging.cal.huhu(SSP = SSP,
#                              site = site,
#                              GCM = GCM,
#                              path.met = path.met)









