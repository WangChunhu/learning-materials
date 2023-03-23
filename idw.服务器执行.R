library(getopt, quietly = T)
# Rscript执行

spec <- matrix(c('help', 'h', 0,'loical', 'GSMs插值函数',
                 'times', 't', 1, 'integer', '任务次数, 默认为1, 这次最大为189 [27GCM * 7]'),
               byrow=T,
               ncol=5)

args <- getopt(spec = spec) # usage：默认为FALSE,这时参数无实际意义，而是以用法的形式输出。

# 输出用法
if (is.null(args$times)) {
  cat(paste(getopt(spec = spec, usage = T), "\n"))
  q(status=1)
}

# 设置默认值
if (is.null(args$times)) {
 args$times  <-  1
 }

# 自定义函数路径
func.path <- "/public/home/zzuaga06/yanhl/prj/huhu/huhu.met/"

# 导入自定义函数
source(paste0(func.path, "idw.huhu.R")) # 插值函数
source(paste0(func.path, "并行专业.R")) # 并行函数

# 加载包
bao <- c("data.table", "magrittr", "stringr", "gstat", "sp", "raster")
sapply(bao, require, character.on = T)

# 插值数据
bio.RMSE.min <- openxlsx::read.xlsx(paste0(func.path,"future_2036-2065_插值/所有SSP245下油菜站点油菜生育期内的future1_met_vif10.插值数据_服务器插值.xlsx")) %>% data.table()
head(bio.RMSE.min)

## 地图地址
map.path <- paste0(func.path, "chinaMap/china.shp") # 地图shp文件

# idw参数

# 去重得到插值列
idw.col <- unique(bio.RMSE.min[, c(1, 5)])
head(idw.col)
print(paste0("共", nrow(idw.col), "个任务!"))

# 插值 
# idw参数
lonlat.colNum <- c(3, 4) # 经纬度数据框，经度在前
interpolation.colName <- "value" # 插值的值列名
data <- bio.RMSE.min
Group <- idw.col$group
print(paste0("当前GCM: ", Group[args$times]))
grid.n <- 50000000 # 格点数
write.name <- idw.col$group
idp <- idw.col$idp # 指定反距离加权功率,为0时权重不会随距离而改变,值越大，附近的点的权重越高
print(paste0("当前idp: ", idp[args$times]))
write.dir <- "/public/home/zzuaga06/yanhl/prj/huhu/huhu.met/future_2036-2065_插值" # 文件写出路径
write.dir

# 多bio，多idp
print("开始插值:")
idw.huhu(interpolation.colName = interpolation.colName,
         data = data,
         Group = Group[args$times],
         lonlat.colNum = lonlat.colNum,
         map.path = map.path,
         grid.n = grid.n,
         idp = idp[args$times],
         write.dir = write.dir,
         write.name = write.name[args$times])

print("插值完成!")
