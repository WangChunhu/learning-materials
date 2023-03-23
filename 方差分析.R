libs <- c("data.table", "stringr", "magrittr")
sapply(libs, require, character.on = T)

# Set workdir
workdir <- "D:/学习/key/牲畜/20221227"
setwd(workdir)
dir()

# 读入数据
data <- openxlsx::read.xlsx("Figure 1 - Copy.xlsx") %>% setDT() %>% melt.data.table(id.vars = c("P.source", "Temp(°)", "P.rate.(kg.ha-1)", "Time.(d)"), variable.name = "rep", value.name = "value") %>% na.omit() %>% .[, factor := paste(P.source, `Time.(d)`, `P.rate.(kg.ha-1)`, sep = "-")]; head(data)

uniq <- unique(data$factor)

# 方差分析-->单因素
i <- 1
all <- NULL
for (i in 1:length(uniq)) {
  single <- data[factor %in% uniq[i]]
  aov <- aov(formula = value ~ `Temp(°)`, data = single)
  ### 多重检验, LSD:最小显著差数检验法
  lsd.aov <- agricolae::LSD.test(y = aov, trt = "Temp(°)", p.adj = "holm", alpha = 0.05) 
  mean_sd <- setDT(lsd.aov$means[, 1:2]) %>% .[, group := row.names(lsd.aov$means)]
  LETTER <- setDT(lsd.aov[[5]])
  mean_sd_LETTER <- mean_sd[LETTER, on = .(value)] 
  single.last <- mean_sd_LETTER[, factor := unique(single$factor)]
  all <- rbind(all, single.last)
  print(paste(i, length(uniq), sep = " / "))
}

# 数据预处理
all[, c("P.source", "Time.(d)", "P.rate.(kg.ha-1)") := list(str_split_fixed(factor, "-", 3)[, 1], str_split_fixed(factor, "-", 3)[, 2], str_split_fixed(factor, "-", 3)[, 3])] %>% setnames("group", "Temp(°)")
head(all)
all$`P.rate.(kg.ha-1)` <- paste0("P", all$`P.rate.(kg.ha-1)`)
all$`P.rate.(kg.ha-1)` <- factor(all$`P.rate.(kg.ha-1)`, levels = c("P0", "P50", "P100", "P200", "P400"))
all$`Time.(d)` <- as.numeric(all$`Time.(d)`)

# 画图
require(ggplot2)
pdf("Figure 1.pdf", width = 15, height = 15, family = "serif")
ggplot(data = all, mapping = aes(x = `Time.(d)`, y = value))+
  facet_grid(`P.rate.(kg.ha-1)` ~ P.source, scales = "free")+
  geom_point(mapping = aes(color = `Temp(°)`), size = 3)+
  geom_line(mapping = aes(color = `Temp(°)`), linewidth = 1)+
  ggrepel::geom_text_repel(mapping = aes(x = `Time.(d)`, y = value, label = groups, color = `Temp(°)`), size = 8, show.legend = F)+
  geom_errorbar(mapping = aes(x = `Time.(d)`, y = value, ymin = value - std, ymax = value + std, color = `Temp(°)`))+
  ggsci::scale_color_npg()+
  theme_test()+
  theme(strip.background = element_blank(), strip.text = element_text(size =  25), axis.title = element_text(size = 30), axis.text = element_text(size = 25), legend.title = element_text(size = 25), legend.text = element_text(size = 20))
dev.off()





