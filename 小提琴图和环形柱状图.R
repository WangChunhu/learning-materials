# 加载基础包
# 包名
bao <- c("data.table", "magrittr", "stringr", "ggplot2")
# 加载
sapply(bao, require, character.on = T)

# 设置工作空间
setwd("D:/长大/同学/杨蕊/20221211-作图")
dir()

# 1 小提琴图----------------------------
data.all <- openxlsx::read.xlsx("产量.xlsx", sheet = 1) %>% setDT() %>% melt.data.table(measure.vars = c("鄂西北地区-总", "江汉平原地区-总", "鄂西北地区-旱地小麦", "鄂西北地区-稻茬小麦", "江汉平原地区-旱地小麦", "江汉平原地区-稻茬小麦"), variable.name = "地区-类别", value.name = "产量") %>% na.omit() %>% .[, c("地区", "类别") := list(str_split_fixed(`地区-类别`, "-", 2)[, 1], str_split_fixed(`地区-类别`, "-", 2)[, 2])] %>% .[, ! "地区-类别"]

mark <- openxlsx::read.xlsx("产量.xlsx", sheet = 2) %>% setDT() %>% melt.data.table(id.vars = "指标", measure.vars = c("鄂西北地区-总", "江汉平原地区-总", "鄂西北地区-旱地小麦", "鄂西北地区-稻茬小麦", "江汉平原地区-旱地小麦", "江汉平原地区-稻茬小麦"), variable.name = "地区-类别", value.name = "value") %>% .[, c("地区", "类别") := list(str_split_fixed(`地区-类别`, "-", 2)[, 1], str_split_fixed(`地区-类别`, "-", 2)[, 2])] %>% .[, ! "地区-类别"]

cv.all <- mark[指标 %in% "cv"] %>% .[, 产量 := 900] %>% .[, value := round(value, 2)]
mean.all <- mark[指标 %in% "mean"]

pdf(file = "all.pdf", width = 7, height = 8, family = "serif")
# 总
data <- data.all[类别 %in% "总"]
cv <- cv.all[类别 %in% "总"]
mean <- mean.all[类别 %in% "总"]

ggplot(data = data, mapping = aes(x = 地区, y = 产量))+
  # facet_grid(. ~ 地区)+
  geom_violin(mapping = aes(fill = 地区), color = NA, alpha = .5, scale = "area", show.legend = F)+ # , draw_quantiles = c(0.5)
  geom_boxplot(width = .1)+
  geom_point(mapping = aes(x = 地区, y = value), data = mean, size = 3, color = c("PaleTurquoise3", "Bisque"))+
  geom_point(mapping = aes(x = 地区, y = 产量), data = cv, size = c(cv$value[1], cv$value[2]), color = c("PaleTurquoise3", "Bisque"), alpha = .5)+
  geom_text(mapping = aes(x = 地区, y = 产量, label = value), data = cv, size = 4)+
  scale_y_continuous(limits = c(700, 10000), breaks = c(seq(0, 10000, 2000)))+
  scale_fill_manual(values = c("PaleTurquoise3", "Bisque"))+
  theme_test()+
  labs(x = "")+
  theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 20), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = "WhiteSmoke"))

dev.off()

pdf(file = "part.pdf", width = 12, height = 8, family = "serif")
# 分
data <- data.all[!类别 %in% "总"]
data$类别 <- factor(data$类别, levels = c("旱地小麦", "稻茬小麦"))
cv <- cv.all[!类别 %in% "总"]
cv$类别 <- factor(cv$类别, levels = c("旱地小麦", "稻茬小麦"))
mean <- mean.all[!类别 %in% "总"]
mean$类别 <- factor(mean$类别, levels = c("旱地小麦", "稻茬小麦"))

ggplot(data = data, mapping = aes(x = 类别, y = 产量))+
  facet_grid(. ~ 地区)+
  geom_violin(mapping = aes(fill = 类别), color = NA, alpha = .5, scale = "area", show.legend = F)+ # , draw_quantiles = c(0.5)
  geom_boxplot(width = .1)+
  geom_point(mapping = aes(x = 类别, y = value), data = mean, size = 3, color = c("PaleTurquoise3", "Bisque", "PaleTurquoise3", "Bisque"))+
  geom_point(mapping = aes(x = 类别, y = 产量), data = cv, size = c(cv$value[1], cv$value[2], cv$value[3], cv$value[4]), color = c("PaleTurquoise3", "Bisque","PaleTurquoise3","Bisque"), alpha = .5)+
  geom_text(mapping = aes(x = 类别, y = 产量, label = value), data = cv, size = 4)+
  scale_y_continuous(limits = c(700, 10000), breaks = c(seq(0, 10000, 2000)))+
  scale_fill_manual(values = c("PaleTurquoise3", "Bisque", "PaleTurquoise3", "Bisque"))+
  theme_test()+
  labs(x = "", y = "")+
  theme(plot.title = element_text(hjust = .5, size = 30), axis.text = element_text(size = 20), axis.title = element_text(size = 30),legend.title = element_text(size = 30), legend.text = element_text(size = 25), strip.text = element_text(size = 25), strip.background = element_rect(fill = NA), strip.text.x = element_blank())

dev.off()

require(patchwork)

pdf(file = "all.pdf", width = 16, height = 10, family = "serif")
p <- all + part
print(p)
dev.off()

# -----------------------------------

# 2 环形柱状图----------------------
require(geomtextpath) # 带有弧度的文字

data.p1 <- openxlsx::read.xlsx(xlsxFile = "农户特征图3.xlsx", sheet = 1) %>% setDT() %>% .[, group := paste(类别一, 地区, 类别二, sep = "_")]
## id <= nrow(data)/2
data.p1[, id := 1:nrow(data.p1)] %>% .[, c("angle", "hjust") := list(ifelse(id <= 14, 96 - id * 13, 96 - id * 13 + 180), ifelse(id <= 14, .1, .8))]

# 因子化group
data.p1$group <- factor(data.p1$group, levels = unique(data.p1$group))

pdf(file = "环形柱状图.pdf", width = 10, height = 10, family = "serif")
ggplot(data = data.p1, mapping = aes(x = group, y = value, fill = group))+
  geom_col()+
  # 内圈环线
  annotate(geom = "segment", x = "户主受教育程度_鄂西北地区_初中", y = -2, xend = "户主受教育程度_江汉平原地区_大学及以上", yend = -2, linewidth = 1.5)+
  annotate(geom = "segment", x = "产量水平_鄂西北地区_中产农户", y = -2, xend = "产量水平_江汉平原地区_高产农户", yend = -2, linewidth = 1.5)+
  annotate(geom = "segment", x = "种植模式_鄂西北地区_稻茬小麦", y = -2, xend = "种植模式_江汉平原地区_旱地小麦", yend = -2, linewidth = 1.5)+
  annotate(geom = "segment", x = "土壤质地_鄂西北地区_壤土", y = -2, xend = "土壤质地_江汉平原地区_粘土", yend = -2, linewidth = 1.5)+
  # 坐标轴
  annotate(geom = "segment", x = 0, y = 0, xend = 0, yend = 80, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 0, xend = .1, yend = 0, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 20, xend = .1, yend = 20, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 40, xend = .1, yend = 40, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 60, xend = .1, yend = 60, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 80, xend = .1, yend = 80, linewidth = 1)+
  annotate(geom = "text", x = .3, y = 0, label = 0, size = 3)+
  annotate(geom = "text", x = .3, y = 20, label = 20, size = 3)+
  annotate(geom = "text", x = .3, y = 40, label = 40, size = 3)+
  annotate(geom = "text", x = .3, y = 60, label = 60, size = 3)+
  annotate(geom = "text", x = .3, y = 80, label = 80, size = 3)+
  annotate(geom = "segment", x = 9, y = 0, xend = 9, yend = 80, linewidth = 1)+
  annotate(geom = "segment", x = 9, y = 0, xend = 9.1, yend = 0, linewidth = 1)+
  annotate(geom = "segment", x = 9, y = 20, xend = 9.1, yend = 20, linewidth = 1)+
  annotate(geom = "segment", x = 9, y = 40, xend = 9.1, yend = 40, linewidth = 1)+
  annotate(geom = "segment", x = 9, y = 60, xend = 9.1, yend = 60, linewidth = 1)+
  annotate(geom = "segment", x = 9, y = 80, xend = 9.1, yend = 80, linewidth = 1)+
  annotate(geom = "text", x = 9.3, y = 0, label = 0, size = 3, angle = -25)+
  annotate(geom = "text", x = 9.3, y = 20, label = 20, size = 3, angle = -25)+
  annotate(geom = "text", x = 9.3, y = 40, label = 40, size = 3, angle = -25)+
  annotate(geom = "text", x = 9.3, y = 60, label = 60, size = 3, angle = -25)+
  annotate(geom = "text", x = 9.3, y = 80, label = 80, size = 3, angle = -25)+
  annotate(geom = "segment", x = 16, y = 0, xend = 16, yend = 80, linewidth = 1)+
  annotate(geom = "segment", x = 16, y = 0, xend = 16.1, yend = 0, linewidth = 1)+
  annotate(geom = "segment", x = 16, y = 20, xend = 16.1, yend = 20, linewidth = 1)+
  annotate(geom = "segment", x = 16, y = 40, xend = 16.1, yend = 40, linewidth = 1)+
  annotate(geom = "segment", x = 16, y = 60, xend = 16.1, yend = 60, linewidth = 1)+
  annotate(geom = "segment", x = 16, y = 80, xend = 16.1, yend = 80, linewidth = 1)+
  annotate(geom = "text", x = 16.3, y = 0, label = 0,size = 3, angle = -20)+
  annotate(geom = "text", x = 16.3, y = 20, label = 20,size = 3, angle = -20)+
  annotate(geom = "text", x = 16.3, y = 40, label = 40,size = 3, angle = -20)+
  annotate(geom = "text", x = 16.3, y = 60, label = 60,size = 3, angle = -20)+
  annotate(geom = "text", x = 16.3, y = 80, label = 80,size = 3, angle = -20)+
  annotate(geom = "segment", x = 21, y = 0, xend = 21, yend = 80, linewidth = 1)+
  annotate(geom = "segment", x = 21, y = 0, xend = 21.1, yend = 0, linewidth = 1)+
  annotate(geom = "segment", x = 21, y = 20, xend = 21.1, yend = 20, linewidth = 1)+
  annotate(geom = "segment", x = 21, y = 40, xend = 21.1, yend = 40, linewidth = 1)+
  annotate(geom = "segment", x = 21, y = 60, xend = 21.1, yend = 60, linewidth = 1)+
  annotate(geom = "segment", x = 21, y = 80, xend = 21.1, yend = 80, linewidth = 1)+
  annotate(geom = "text", x = 21.3, y = 0, label = 0,size = 3)+
  annotate(geom = "text", x = 21.3, y = 20, label = 20,size = 3)+
  annotate(geom = "text", x = 21.3, y = 40, label = 40,size = 3)+
  annotate(geom = "text", x = 21.3, y = 60, label = 60,size = 3)+
  annotate(geom = "text", x = 21.3, y = 80, label = 80,size = 3)+
  scale_fill_manual(values = data.p1$color)+
  guides(fill = "none")+
  scale_y_continuous(limits = c(-60, 100), breaks = seq(0, 80, 20))+ # 控制中央圈大小
  coord_polar(direction = 1)+ # 环形
  geom_text(mapping = aes(x = group, y = value + 10, label = 类别三, angle = angle, hjust = hjust), size = 5)+ # 调整末端文字角度及位置
  # geom_label(mapping = aes(x = group, label = 类别一))+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

dev.off()

# 
data.p2 <- openxlsx::read.xlsx(xlsxFile = "农户特征图3.xlsx", sheet = 2) %>% setDT() %>% .[, group := paste(类别一, 类别二, 类别三, sep = "_")]
## id <= nrow(data)/2
data.p2[, id := 1:nrow(data.p2)] %>% .[, c("angle", "hjust") := list(ifelse(id <= 9, 96 - id * 21, 96 - id * 21 + 180), ifelse(id <= 9, .1, .8))]

# 因子化group
data.p2$group <- factor(data.p2$group, levels = unique(data.p2$group))

pdf(file = "环形柱状图2.pdf", width = 10, height = 10, family = "serif")
ggplot(data = data.p2, mapping = aes(x = group, y = value, fill = group))+
  geom_col()+
  # 内圈环线
  annotate(geom = "segment", x = "种植模式_旱地小麦_中产农户", y = -2, xend = "种植模式_稻茬小麦_高产农户", yend = -2, linewidth = 1.5)+
  annotate(geom = "segment", x = "土壤质地_粘土_中产农户", y = -2, xend = "土壤质地_砂土_高产农户", yend = -2, linewidth = 1.5)+
  # 坐标轴
  annotate(geom = "segment", x = 0, y = 0, xend = 0, yend = 80, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 0, xend = .1, yend = 0, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 20, xend = .1, yend = 20, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 40, xend = .1, yend = 40, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 60, xend = .1, yend = 60, linewidth = 1)+
  annotate(geom = "segment", x = 0, y = 80, xend = .1, yend = 80, linewidth = 1)+
  annotate(geom = "text", x = .2, y = 0, label = 0, size = 3)+
  annotate(geom = "text", x = .2, y = 20, label = 20, size = 3)+
  annotate(geom = "text", x = .2, y = 40, label = 40, size = 3)+
  annotate(geom = "text", x = .2, y = 60, label = 60, size = 3)+
  annotate(geom = "text", x = .2, y = 80, label = 80, size = 3)+
  annotate(geom = "segment", x = 7, y = 0, xend = 7, yend = 80, linewidth = 1)+
  annotate(geom = "segment", x = 7, y = 0, xend = 7.1, yend = 0, linewidth = 1)+
  annotate(geom = "segment", x = 7, y = 20, xend = 7.1, yend = 20, linewidth = 1)+
  annotate(geom = "segment", x = 7, y = 40, xend = 7.1, yend = 40, linewidth = 1)+
  annotate(geom = "segment", x = 7, y = 60, xend = 7.1, yend = 60, linewidth = 1)+
  annotate(geom = "segment", x = 7, y = 80, xend = 7.1, yend = 80, linewidth = 1)+
  annotate(geom = "text", x = 7.2, y = 0, label = 0, size = 3, angle = 25)+
  annotate(geom = "text", x = 7.2, y = 20, label = 20, size = 3, angle = 25)+
  annotate(geom = "text", x = 7.2, y = 40, label = 40, size = 3, angle = 25)+
  annotate(geom = "text", x = 7.2, y = 60, label = 60, size = 3, angle = 25)+
  annotate(geom = "text", x = 7.2, y = 80, label = 80, size = 3, angle = 25)+
  scale_fill_manual(values = data.p2$color)+
  guides(fill = "none")+
  scale_y_continuous(limits = c(-60, 100), breaks = seq(0, 80, 20))+ # 控制中央圈大小
  coord_polar(direction = 1)+ # 环形
  geom_text(mapping = aes(x = group, y = value + 10, label = 类别四, angle = angle, hjust = hjust), size = 5)+ # 调整末端文字角度及位置
  # geom_label(mapping = aes(x = group, label = 类别一))+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

dev.off()




