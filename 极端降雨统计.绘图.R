# 加载基础包
## 包名
bao <- c("data.table", "magrittr", "stringr", "ggplot2")
## 加载
sapply(bao, require, character.on = T)

# 设置工作空间
setwd("D:/学习/key/油菜/apsim结果/极端降雨统计.245-585")
dir()

# 读入统计数据
rain <- fread("极端降雨统计.csv") %>% setnames("\xbc\xab\xb6˽\xb5\xd3\xea\xb4\xce\xca\xfd", "极端降雨统计")
names(rain)

# 读入站点信息
info.site <- fread("D:/学习/key/油菜/APSIM最新原始数据/site_config.correct.txt", select = c(1:2, 5))

# 匹配数据及站点
info.rain <- info.site[rain, on = .(station_No == site)]

rm(rain, info.site); gc()

# 按省平均
info.rain.p <- info.rain[, .(`极端降雨统计/省平均` = mean(极端降雨统计)), by = .(Province, SSP, GCM, years, month)]
str(info.rain.p)
## 因子化月份
info.rain.p$month <- factor(info.rain.p$month, levels = c("9", "10", "11", "12", "1", "2", "3", "4", "5", "6"))

pdf(file = "极端降雨统计1.pdf", width = 30, height = 40, family = "serif")
## SSP245
data <- info.rain.p[SSP %in% 245]
p <- ggplot(data = data, mapping = aes(x = month, y = Province))+
  facet_wrap(facets = GCM ~ years, ncol = 6)+
  geom_tile(mapping = aes(fill = `极端降雨统/省平均`))+
  scale_fill_gradient2()+
  labs(title = "SSP245")+
  theme_test()+
  theme(plot.title = element_text(hjust = .5, size = 25), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15), axis.title = element_text(size = 25),legend.title = element_text(size = 25), legend.text = element_text(size = 20), strip.text = element_text(size = 20), strip.background = element_rect(fill = "WhiteSmoke"))
p

## SSP585
data <- info.rain.p[SSP %in% 585]
p <- ggplot(data = data, mapping = aes(x = month, y = Province))+
  facet_wrap(facets = GCM ~ years, ncol = 6)+
  geom_tile(mapping = aes(fill = `极端降雨统/省平均`))+
  scale_fill_gradient2()+
  theme_test()+
  labs(title = "SSP585")+
  theme(plot.title = element_text(hjust = .5, size = 25), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15), axis.title = element_text(size = 25),legend.title = element_text(size = 25), legend.text = element_text(size = 20), strip.text = element_text(size = 20), strip.background = element_rect(fill = "WhiteSmoke"))
p

## future - baseline
info.rain.p_ssp <- dcast.data.table(data = info.rain.p, formula = Province + GCM + years + month ~ SSP, value.var = "极端降雨统计/省平均") %>% .[, diff := `585` - `245`]
data <- info.rain.p_ssp
p <- ggplot(data = data, mapping = aes(x = month, y = Province))+
  facet_wrap(facets = GCM ~ years, ncol = 6)+
  geom_tile(mapping = aes(fill = diff))+
  scale_fill_gradient2(low="#0000ff", high="#ff0000", mid="#ffffff")+
  theme_test()+
  labs(title = "SSP585 - SSP245")+
  theme(plot.title = element_text(hjust = .5, size = 25), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15), axis.title = element_text(size = 25),legend.title = element_text(size = 25), legend.text = element_text(size = 20), strip.text = element_text(size = 20), strip.background = element_rect(fill = "WhiteSmoke"))
p

dev.off()


pdf(file = "极端降雨统计2.pdf", width = 20, height = 20, family = "serif")
## SSP245_diff
data <- info.rain.p[SSP %in% 245] %>% dcast.data.table(formula = Province + GCM + month ~ years, value.var = "极端降雨统/省平均") %>% .[, diff := Future - Baseline]
p <- ggplot(data = data, mapping = aes(x = month, y = Province))+
  facet_wrap(facets = GCM ~ ., ncol = 6)+
  geom_tile(mapping = aes(fill = diff))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  labs(title = "SSP245_diff")+
  theme_test()+
  theme(plot.title = element_text(hjust = .5, size = 25), axis.text = element_text(size = 17), axis.title = element_text(size = 25),legend.title = element_text(size = 25), legend.text = element_text(size = 20), strip.text = element_text(size = 20), strip.background = element_rect(fill = "WhiteSmoke"))
p

## SSP585_diff
data <- info.rain.p[SSP %in% 585] %>% dcast.data.table(formula = Province + GCM + month ~ years, value.var = "极端降雨统/省平均") %>% .[, diff := Future - Baseline]
p <- ggplot(data = data, mapping = aes(x = month, y = Province))+
  facet_wrap(facets = GCM ~ ., ncol = 6)+
  geom_tile(mapping = aes(fill = diff))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  labs(title = "SSP585_diff")+
  theme_test()+
  theme(plot.title = element_text(hjust = .5, size = 25), axis.text = element_text(size = 17), axis.title = element_text(size = 25),legend.title = element_text(size = 25), legend.text = element_text(size = 20), strip.text = element_text(size = 20), strip.background = element_rect(fill = "WhiteSmoke"))
p

dev.off()

# 合并pdf
pdftools::pdf_combine(input = c("极端降雨统计1.pdf", "极端降雨统计2.pdf"), output = "极端降雨统计.pdf")



