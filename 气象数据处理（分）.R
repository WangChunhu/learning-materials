require(data.table)
require(readxl)
require(dplyr)
require(openxlsx)
setwd("H:/综合/文献+标准+测定方法/气象/数据/Climate Data")

g <- Sys.time()

a <- read_excel("SURF_CLI_CHN_MUL_DAY_CES_STATION.xls")
setDT(a[,1:2]) %>% setnames(c("区站号","省份"),c("Station","Province")) %>% setkey("Station") -> site_station
b <- read.csv("stationinf.csv")
jwd <- setDT(b[,c(1,5,6)],key = "Station")
jwd_ss <- jwd[site_station] %>% setorder("Province") %>% setcolorder(c(4L,1L:3L))
shengfen <- jwd_ss[Province %in% c("湖北","四川","重庆","湖南","安徽","江西","江苏","浙江","上海")] %>% setDT(key = "Station")

tem_file <- list.files(pattern ="SURF_CLI_CHN_MUL_DAY_CES-TEM-12001-" )
d <- as.character(getwd())
shengfen_tem_last <- NULL
i=1958
for (i in 1958:2018) {
  c <- grep(pattern = i,x = tem_file) #返回匹配值的索引
  tem <- NULL
  for (j in c) {
    e <- NULL
    e <- read.table(file = paste(d,"/",tem_file[j],sep = ""))
    tem <- rbind(tem,e) %>% setDT()
  }
  k = 1
  shengfen_tem <- NULL
  for (k in 1:nrow(shengfen)) {
    f <- NULL
    f <- tem[V1 == shengfen$Station[k]]
    shengfen_tem <- rbind(shengfen_tem,f)
  }
  shengfen_tem_last <- rbind(shengfen_tem_last,shengfen_tem) %>% setDT(key = "V1")
}

shengfen_lastfile <- shengfen[shengfen_tem_last][,-c(5,6,7,14,15,16)] %>% setnames(c( "latitude1","longitude1","V5","V6","V7","V8","V9","V10"),c( "latitude","longitude","year","month","day","T","Tmax","Tmin"))

shengfen_lastfile <- shengfen_lastfile[,"q" := list("a")][month %in% c(7:9)]

Ta <- shengfen_lastfile[,.(T)] 
Tm <- shengfen_lastfile[,.(Tmax)]
i=1
for (i in 1:nrow(shengfen_lastfile)) {
  if(Ta[i] >=300 | Tm[i] >= 350){
    hu7 <- NULL
    hu7 <- 1 
  }else{
    hu7 <- NULL
    hu7 <- 0
  }
  shengfen_lastfile <- set(shengfen_lastfile,i,11,hu7)
}

shengfen_lastfile <- unique(shengfen_lastfile[,Q := q])
shengfen_lastfile[,c(1,2,5,6)] %>% unique() %>% setDT()-> m
shengfen_llastfile <- NULL
i = 1
for (i in 1:nrow(m)) {
  shengfen_lastfile[shengfen_lastfile$Province == m$Province[i] & shengfen_lastfile$Station == m$Station[i] & 
                      shengfen_lastfile$year == m$year[i] & shengfen_lastfile$month == m$month[i]] %>% setDT() -> o
  k=2
  for (k in 2:nrow(o)) {
    if(o$Q[k] == 1){
      set(x = o,i = k,j = 12L,value = as.numeric(o$Q[k]) + as.numeric(o$Q[k-1]))
    }else{
      set(x = o,i = k,j = 12L,value = 0)
    }
  }
  shengfen_llastfile <- rbind(shengfen_llastfile,o)
}
write.xlsx(x = shengfen_llastfile,file = "H:/综合/文献+标准+测定方法/气象/数据/all.xlsx")

all <- setDT(x = shengfen_llastfile)[,c(1,2,5,6,11,12)]
all[,c(1,2,3,4)] %>% unique() -> m

tongji <- NULL
i = 1
for (i in 1:nrow(m)) {
  all[all$Province ==  m$Province[i] & all$Station == m$Station[i] & all$year == m$year[i] & all$month == m$month[i]] -> o
  q <- as.numeric(o$q)
  o[q == 1] %>% nrow() -> htd_num
  tongji <- rbind(tongji,htd_num)
}

all_tongji <- cbind(m,tongji)
setnames(all_tongji,"V1","ht_days")

tongji_one <- NULL
i = 1
for (i in 1:nrow(m)) {
  all[all$Province ==  m$Province[i] & all$Station == m$Station[i] & all$year == m$year[i] & all$month == m$month[i]] -> o
  Q <- as.numeric(o$Q)
  o[Q == 1] %>% nrow() -> htd_one_num
  tongji_one <- rbind(tongji_one,htd_one_num)
}

all_tongji <- cbind(all_tongji,tongji_one)
setnames(all_tongji,"V1","duan")

tongji_TF <- NULL
i = 1
for (i in 1:nrow(m)) {
  all[all$Province ==  m$Province[i] & all$Station == m$Station[i] & all$year == m$year[i] & all$month == m$month[i]] -> o
  Q <- as.numeric(o$Q)
  o[Q == 3] %>% nrow() -> num1
  o[Q == 5] %>% nrow() -> num2
  num <- num1 - num2
  tongji_TF <- rbind(tongji_TF,num) 
}
all_tongji <- cbind(all_tongji,tongji_TF)
setnames(all_tongji,"V1","three_four")

tongji_FS <- NULL
i = 1
for (i in 1:nrow(m)) {
  all[all$Province ==  m$Province[i] & all$Station == m$Station[i] & all$year == m$year[i] & all$month == m$month[i]] -> o
  Q <- as.numeric(o$Q)
  o[Q == 5] %>% nrow() -> num1
  o[Q == 7] %>% nrow() -> num2
  num <- num1 - num2
  tongji_FS <- rbind(tongji_FS,num) 
}
all_tongji <- cbind(all_tongji,tongji_FS)
setnames(all_tongji,"V1","five_six")

tongji_Sm <- NULL
i = 1
for (i in 1:nrow(m)) {
  all[all$Province ==  m$Province[i] & all$Station == m$Station[i] & all$year == m$year[i] & all$month == m$month[i]] -> o
  Q <- as.numeric(o$Q)
  o[Q == 7] %>% nrow() -> num
  tongji_Sm <- rbind(tongji_Sm,num) 
}
all_tongji <- cbind(all_tongji,tongji_Sm)
setnames(all_tongji,"V1","seven_more")

write.xlsx(x = all_tongji,file = "气象数据统计.xlsx")


a <- setDT(all_tongji)
b <- a[,lapply(.SD,mean),.SDcols = c("three_four","five_six","seven_more"),by=c("Province","year","month")]

write.xlsx(x = b,file = "气象数据统计--按站点.xlsx")

shanghai <- read.xlsx("上海的系数.xlsx") %>% setDT()

shanghai_zh <- melt.data.table(data = shanghai,id.vars=c("年"),variable.name="尺度",value.name="系数")
setorder(x = shanghai_zh,"年")
write.xlsx(x = shanghai_zh,file = "上海的系数_转换.xlsx")

j <- Sys.time()
l <- j - g
l









