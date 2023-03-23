bao  <-  c("data.table", "cowplot", "readxl", "stringr", "lubridate", "ggplot2", "ggrepel")
sapply(bao, require, character.on = T)

# 获取R进程的语言环境的详细信息或设置其方面
## Sys.getlocale (category = "LC_ALL")
## Sys.getlocale (category = "LC_TIME")

## 设置系统时间形式
Sys.setlocale(category = "LC_TIME", locale = "English")

# 设置工作空间
setwd("D:/学习/key/油菜/apsim结果/GCM.change")
dir()

# 读入统计数据
change <- fread("GCM.change.统计.csv") %>% .[, c(1:3, 10:12)]
names(change)

# 读入站点信息
info.site <- fread("D:/学习/key/油菜/APSIM最新原始数据/site_config.correct.txt", select = c(1:2, 5))

# 匹配数据及站点
info.change <- info.site[change, on = .(station_No == site)]

rm(change, info.site); gc()

# 按省平均
info.change.p <- info.change[, .(meanT.change = mean(meanT.change), rain.change = mean(rain.change), radn.change = mean(radn.change)), by = .(Province, SSP, GCM)] # %>% melt.data.table(id.vars = c("Province", "SSP",  "GCM"),measure.vars = c("meanT.change", "rain.change", "radn.change") ,variable.name = "met.change", value.name = "change")
str(info.change.p)

# Met change plot 按省---------------------------------------------------------
pdf(file =  "change.fall_temperature_radn_change-按省.pdf", width = 26, height = 22, family = "serif")
## 245 
data <- info.change.p[SSP %in% 245]

p <- ggplot(data = data, aes(x = rain.change, y = meanT.change))+
  facet_wrap(facets = GCM ~ ., ncol = 6)+
  geom_point(aes(color = radn.change), alpha = 0.9 , size = 6)+
  geom_hline(yintercept = 0, color = "Turquoise1")+
  geom_vline(xintercept = 0, color = "Turquoise1")+
  scale_color_gradient2(low = "darkblue",
                       mid = "white",
                       high = "red", 
                       midpoint = 0,
                       space = "Lab",
                       name="Radn change\n(%)")+
  # geom_label_repel(aes(label = Province), label.size = 0.25)+
  labs(title = "Statistics by province on SSP245", x = "fall change (%)", y = "Temperature change (°C)")+
  theme_test()+
  theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
p

## 585
data <- info.change.p[SSP %in% 585]

p <- ggplot(data = data, aes(x = rain.change, y = meanT.change))+
  facet_wrap(facets = GCM ~ ., ncol = 6)+
  geom_point(aes(color = radn.change), alpha = 0.9 , size = 6)+
  geom_hline(yintercept = 0, color = "Turquoise1")+
  geom_vline(xintercept = 0, color = "Turquoise1")+
  scale_color_gradient2(low = "darkblue",
                        mid = "white",
                        high = "red", 
                        midpoint = 0,
                        space = "Lab",
                        name="Radn change\n(%)")+
  # geom_label_repel(aes(label = Province), label.size = 0.25)+
  labs(title = "Statistics by province on SSP585", x = "fall change (%)", y = "Temperature change (°C)")+
  theme_test()+
  theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
p

dev.off()

# Met change plot 按站点---------------------------------------------------------
pdf(file =  "change.fall_temperature_radn_change-按站点.pdf", width = 26, height = 22, family = "serif")
## 245 
data <- info.change[SSP %in% 245]

p <- ggplot(data = data, aes(x = rain.change, y = meanT.change))+
  facet_wrap(facets = GCM ~ ., ncol = 6)+
  geom_point(aes(color = radn.change), size = 1)+
  geom_hline(yintercept = 0, color = "Turquoise1")+
  geom_vline(xintercept = 0, color = "Turquoise1")+
  scale_color_gradient2(low = "darkblue",
                        mid = "white",
                        high = "red", 
                        midpoint = 0,
                        space = "Lab",
                        name="Radn change\n(%)")+
  # geom_label_repel(aes(label = Province), label.size = 0.25)+
  labs(title = "Statistics by site on SSP245", x = "fall change (%)", y = "Temperature change (°C)")+
  theme_test()+
  theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
p

## 585
data <- info.change[SSP %in% 585]

p <- ggplot(data = data, aes(x = rain.change, y = meanT.change))+
  facet_wrap(facets = GCM ~ ., ncol = 6)+
  geom_point(aes(color = radn.change), size = 1)+
  geom_hline(yintercept = 0, color = "Turquoise1")+
  geom_vline(xintercept = 0, color = "Turquoise1")+
  scale_color_gradient2(low = "darkblue",
                        mid = "white",
                        high = "red", 
                        midpoint = 0,
                        space = "Lab",
                        name="Radn change\n(%)")+
  # geom_label_repel(aes(label = Province), label.size = 0.25)+
  labs(title = "Statistics by site on SSP585", x = "fall change (%)", y = "Temperature change (°C)")+
  theme_test()+
  theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
p

dev.off()

## 合并两个pdf
pdftools::pdf_combine(input = c("change.fall_temperature_radn_change-按省.pdf", "change.fall_temperature_radn_change-按站点.pdf"), output = "change.fall_temperature_radn_change.pdf")

# fwrite(all.GCM,"D:/学习/key/油菜/气象文件/mean.GCM.Baselie.Future.csv") # 原始

# 按月---------------------------------------------------------------
# 读入统计数据
change <- fread("GCM.change.统计-按月.csv", select = c(1:4, 11:13)) # %>% melt.data.table(id.vars = c("SSP", "GCM", "site", "month"), measure.vars = c("meanT.change", "rain.change", "radn.change"), variable.name = "var", value.name = "value")
names(change)
head(change)

# 读入站点信息
info.site <- fread("D:/学习/key/油菜/APSIM最新原始数据/site_config.correct.txt", select = c(1:2, 5))

# 匹配数据及站点
info.change <- info.site[change, on = .(station_No == site)]

rm(change, info.site); gc()
head(info.change)

# 按省平均
info.change.p <- info.change[, .(meanT.change = mean(meanT.change), rain.change = mean(rain.change), radn.change = mean(radn.change)), by = .(Province, SSP, GCM, month)] # %>% melt.data.table(id.vars = c("Province", "SSP",  "GCM"),measure.vars = c("meanT.change", "rain.change", "radn.change") ,variable.name = "met.change", value.name = "change")
head(info.change.p)
str(info.change.p)

# Met change plot 按省---------------------------------------------------------
pdf(file =  "change.fall_temperature_radn_change-按省-按月.pdf", width = 26, height = 22, family = "serif")
## 245 
data <- info.change.p[SSP %in% 245]
months <- unique(data$month)

for (i in months) {
  data.single <- data[month %in% i]
  p <- ggplot(data = data.single, aes(x = rain.change, y = meanT.change))+
    facet_wrap(facets = GCM ~ ., ncol = 6)+
    geom_point(aes(color = radn.change), alpha = 0.9 , size = 6)+
    geom_hline(yintercept = 0, color = "Turquoise1")+
    geom_vline(xintercept = 0, color = "Turquoise1")+
    scale_color_gradient2(low = "darkblue",
                          mid = "white",
                          high = "red", 
                          midpoint = 0,
                          space = "Lab",
                          name="Radn change\n(%)")+
    # geom_label_repel(aes(label = Province), label.size = 0.25)+
    labs(title = paste0("Statistics by province on SSP245 - month ", i), x = "fall change (%)", y = "Temperature change (°C)")+
    theme_test()+
    theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
  print(p)
}


## 585
data <- info.change.p[SSP %in% 585]
months <- unique(data$month)

for (i in months) {
  data.single <- data[month %in% i]
  p <- ggplot(data = data.single, aes(x = rain.change, y = meanT.change))+
    facet_wrap(facets = GCM ~ ., ncol = 6)+
    geom_point(aes(color = radn.change), alpha = 0.9 , size = 6)+
    geom_hline(yintercept = 0, color = "Turquoise1")+
    geom_vline(xintercept = 0, color = "Turquoise1")+
    scale_color_gradient2(low = "darkblue",
                          mid = "white",
                          high = "red", 
                          midpoint = 0,
                          space = "Lab",
                          name="Radn change\n(%)")+
    # geom_label_repel(aes(label = Province), label.size = 0.25)+
    labs(title = paste0("Statistics by province on SSP585 - month ", i), x = "fall change (%)", y = "Temperature change (°C)")+
    theme_test()+
    theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
  print(p)
}

dev.off()

# Met change plot 按站点---------------------------------------------------------
pdf(file =  "change.fall_temperature_radn_change-按站点-按月.pdf", width = 26, height = 22, family = "serif")
## 245 
data <- info.change[SSP %in% 245]
months <- unique(data$month)

for (i in months) {
  data.single <- data[month %in% i]
  p <- ggplot(data = data.single, aes(x = rain.change, y = meanT.change))+
    facet_wrap(facets = GCM ~ ., ncol = 6)+
    geom_point(aes(color = radn.change), size = 1)+
    geom_hline(yintercept = 0, color = "Turquoise1")+
    geom_vline(xintercept = 0, color = "Turquoise1")+
    scale_color_gradient2(low = "darkblue",
                          mid = "white",
                          high = "red", 
                          midpoint = 0,
                          space = "Lab",
                          name="Radn change\n(%)")+
    # geom_label_repel(aes(label = Province), label.size = 0.25)+
    labs(title = paste0("Statistics by site on SSP245 - month ", i), x = "fall change (%)", y = "Temperature change (°C)")+
    theme_test()+
    theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
  print(p)
}


## 585
data <- info.change[SSP %in% 585]
months <- unique(data$month)

for (i in months) {
  data.single <- data[month %in% i]
  p <- ggplot(data = data.single, aes(x = rain.change, y = meanT.change))+
    facet_wrap(facets = GCM ~ ., ncol = 6)+
    geom_point(aes(color = radn.change), size = 1)+
    geom_hline(yintercept = 0, color = "Turquoise1")+
    geom_vline(xintercept = 0, color = "Turquoise1")+
    scale_color_gradient2(low = "darkblue",
                          mid = "white",
                          high = "red", 
                          midpoint = 0,
                          space = "Lab",
                          name="Radn change\n(%)")+
    # geom_label_repel(aes(label = Province), label.size = 0.25)+
    labs(title = paste0("Statistics by site on SSP585 - month ", i), x = "fall change (%)", y = "Temperature change (°C)")+
    theme_test()+
    theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
  print(p)
}

dev.off()

## 合并两个pdf
pdftools::pdf_combine(input = c("change.fall_temperature_radn_change-按省.pdf", "change.fall_temperature_radn_change-按站点.pdf"), output = "change.fall_temperature_radn_change.pdf")

# GS系列---------------------------------------------------------------------------
## 所有GCM平均
path.s245 <- "Z:/apsim/Oilseed_rape.245.1000/apsim.result.huhu/summary.dateRange.all.huhu.csv"
path.s585 <- "Y:/yanhl/prj/Oilseed_rape.585.1000/apsim.result.huhu/summary.dateRange.all.huhu.csv"

data.sel <- function(path.data, Stage, Variety, ssp){
  data.sel <- fread(path.data, select = c("info", "VarietySown", "S", "dateRange", "GS_rain", "GS_radn", "GS_Tmax", "GS_Tmin")) %>% 
    .[, GS_T := (GS_Tmax + GS_Tmin)/2] %>% .[, c("GS_Tmax", "GS_Tmin") := NULL] %>% 
    .[dateRange %chin% c("baseline", "future1") & S %chin% Stage & VarietySown %chin% Variety] %>% 
    dcast.data.table(formula = info ~ dateRange, value.var = c("GS_rain", "GS_radn", "GS_T")) %>% 
    .[,c("meanT.change", "rain.change", "radn.change") := list(GS_T_future1 - GS_T_baseline, (GS_rain_future1 - GS_rain_baseline)/GS_rain_baseline * 100, (GS_radn_future1 - GS_radn_baseline)/GS_radn_baseline * 100)] %>% 
    .[, c("GS_rain_baseline", "GS_rain_future1", "GS_radn_baseline", "GS_radn_future1", "GS_T_baseline", "GS_T_future1") := NULL] %>% 
    .[, c("S", "VarietySown", "ssp") := list(Stage, Variety, ssp)]
  
  return(data.sel)
}

data.s245.s <- data.sel(path.data = path.s245, Stage = "S4", Variety = "s", ssp = 245)
data.s245.t <- data.sel(path.data = path.s245, Stage = "S4", Variety = "t", ssp = 245)
data.s585.s <- data.sel(path.data = path.s585, Stage = "S4", Variety = "s", ssp = 585)
data.s585.t <- data.sel(path.data = path.s585, Stage = "S4", Variety = "t", ssp = 585)

head(data.s245.s)

data <- list(data.s245.s, data.s245.t, data.s585.s, data.s585.t)
ssp <- c(245, 245, 585, 585)
S <- c("s", "t", "s", "t")

pdf(file =  "change.fall_temperature_radn_change-按站点-GS.pdf", width = 14, height = 7, family = "serif")
i <- 1
for (i in 1:length(data)) {
  data.single <- data[[i]]
  p <- ggplot(data = data.single, aes(x = rain.change, y = meanT.change))+
    # facet_wrap(facets = GCM ~ ., ncol = 6)+
    geom_point(aes(color = radn.change), size = 5)+
    geom_hline(yintercept = 0, color = "Turquoise1")+
    geom_vline(xintercept = 0, color = "Turquoise1")+
    scale_color_gradient2(low = "darkblue",
                          mid = "white",
                          high = "red", 
                          midpoint = 0,
                          space = "Lab",
                          name="Radn change\n(%)")+
    # geom_label_repel(aes(label = Province), label.size = 0.25)+
    labs(title = paste0("Statistics by site on SSP245 - GS - ssp", ssp[i], " - VarietySown: ", S[i]), x = "fall change (%)", y = "Temperature change (°C)")+
    theme_test()+
    theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
  print(p)
}

dev.off()

## 所有GCM单独
data <- fread("Y:/yanhl/prj/Oilseed.1000.result/data.dcast.pre.1000.csv", select = c("site", "long", "lat", "test2", "VarietySown", "meanT.change", "rain.change", "radn.change", "ssp", "GCM"))

ssp <- unique(data$ssp)
gcm <- unique(data$GCM)
st <- unique(data$VarietySown)

grid <- expand.grid(ssp = ssp, gcm = gcm, st = st) %>% setDT() %>% setorder(ssp, gcm)
grid$gcm <- as.character(grid$gcm)
grid$st <- as.character(grid$st)

pdf(file =  "change.fall_temperature_radn_change-按站点-GS-分GCM.pdf", width = 14, height = 7, family = "serif")
i <- 1
for (i in 1:nrow(grid)) {
  data.single <- data[ssp %in% grid$ssp[i] & GCM %chin% grid$gcm[i] & VarietySown %chin% grid$st[i]]
  
  p <- ggplot(data = data.single, aes(x = rain.change, y = meanT.change))+
    # facet_wrap(facets = GCM ~ ., ncol = 6)+
    geom_point(aes(color = radn.change), size = 5)+
    geom_hline(yintercept = 0, color = "Turquoise1")+
    geom_vline(xintercept = 0, color = "Turquoise1")+
    scale_color_gradient2(low = "darkblue",
                          mid = "white",
                          high = "red", 
                          midpoint = 0,
                          space = "Lab",
                          name="Radn change\n(%)")+
    # geom_label_repel(aes(label = Province), label.size = 0.25)+
    labs(title = paste0("Statistics by site on SSP245 -GS -ssp ", grid$ssp[i], " -GCM: ", grid$gcm[i], " -VarietySown: ", grid$st[i]), x = "fall change (%)", y = "Temperature change (°C)")+
    theme_test()+
    theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 25), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))
  print(p)
  print(paste0(i, " / ", nrow(grid)))
}

dev.off()










# # Met change plot ---------------------------------------------------------
# ## 245 
# data <- info.change.p[SSP %in% 245]
# 
# p <- ggplot(data = data, aes(x = rain.change, y = meanT.change))+
#   # facet_wrap(facets = "site",nrow = 2,ncol = 2)+
#   geom_point(aes(fill = radn.change, shape = Province), color = "grey", size = 4,alpha = 0.9)+
#   scale_fill_gradient2(low = "darkblue",
#                        mid = "white",
#                        high = "red", 
#                        midpoint = 0,
#                        space = "Lab",
#                        name="Radn change\n(%)")+
#   # scale_shape_manual(values = c(21, 22, 23, 24, 25))+
#   # geom_label_repel(aes(label=GCM),label.size = 0.25)+
#   xlab("changefall change (%)")+
#   ylab("Temperature change (°C)")+
#   theme_bw()+
#   theme(panel.grid = element_blank())
# p
# 
# ggsave(plot=p,filename = "D:/学习/key/油菜/气象文件/changefall_temperature_radn_change.pdf",width = 6,height = 5)
# 
# # 标签图
# all.GCM.change[,label := str_sub(GCM, -5L, -1L)]
# 
# p1 <- ggplot(data = all.GCM.change,aes(x=change.change,y=meanT.change))+
#   # facet_wrap(facets = "site",nrow = 2,ncol = 2)+
#   geom_point(aes(color = site.name),size=2,alpha=0.9)+
#   scale_fill_gradient2(low = "darkblue",
#                        mid = "white",
#                        high = "red",midpoint = 0,
#                        space = "Lab",name="Radn change\n(%)")+
#   scale_shape_manual(values = c(21,22,23,24,25))+
#   geom_label_repel(aes(label=label),label.size = 0.25)+
#   xlab("changefall change (%)")+
#   ylab("Temperature change (°C)")+
#   theme_bw()+
#   theme(
#     panel.grid = element_blank()
#     # ,legend.position = "none"
#   )
# p1
# ggsave(plot=p1,filename = "D:/学习/key/油菜/气象文件/label_changefall_temperature_radn_change.pdf",width = 12,height = 12)
# 
# # 箱线图
# all.met <- melt.data.table(all.GCM,
#                            id.vars=c("GCM", "site.name", "years"),
#                            value.name = c("value")
# )
# 
# all.met$variable <- factor(all.met$variable,levels = c("meanT", "change", "radn"))
# all.met$group <- paste0(all.met$site,"_",all.met$years)
# p2 <- ggplot(data = all.met,aes(x=site.name,y=value))+
#   facet_wrap(facets = "variable",nrow = 1,scales = "free_y")+
#   geom_boxplot(aes(group=group,fill=years),
#                alpha=0.6,
#                outlier.shape=NA,
#                notch=FALSE)+
#   xlab("")+
#   ylab("")+
#   theme_bw()+
#   theme(
#     panel.grid = element_blank(),
#     legend.title = element_blank(),
#     axis.text.x = element_text(hjust = 1,angle = 45)
#   )
# p2
# ggsave(filename ="D:/学习/key/油菜/气象文件/Met_box.pdf",plot = p2,height = 5,width = 10)

  







