bao <- c("data.table", "magrittr", "stringr")
sapply(bao, require, character.on = T)


# metrica.huhu(index, date)自定义函数 -------------------------------------------------------------------
# 暂停维护
metrica.huhu <- function(index, date){
  if (!dir.exists(date)) {
    dir.create(date)
  }
  
  # 读入所有观测值
  observed <- fread("validation.mif", header = T) %>% .[Variable %in% index] %>% .[!Region %in% "World"] # "Production|+|Pasture"
  observed.names <- names(observed)
  
  # 读入所有预测值
  predicted <- fread("report.mif", header = T) %>% .[Variable %in% index] %>% .[!Region %in% "World"]
  predicted.names <- names(predicted)
  
  # 列取交集
  inter <- intersect(observed.names, predicted.names)
  # inter
  
  # 提取观测计算值
  ## 去除N/A列
  observed.cal <- observed[, .SD, .SDcols = inter] %>% t() %>% data.table() %>% .[!V1 %in% "N/A"] %>% t() %>% data.table() 
  names(observed.cal) <- inter[seq(1, ncol(observed.cal))]
  ## 观测值和预测值共有列名
  names.share <- names(observed.cal)
  ## 数值化数据并melt
  observed.cal <- observed.cal[, lapply(.SD, as.numeric), by = .(Model, Scenario, Region, Variable, Unit)] %>% melt.data.table(data = ., id.vars = c("Model", "Scenario", "Region" , "Variable", "Unit"), variable.name = "years", value.name = "observed")
  
  ## 去除N/A列
  predicted.cal <- predicted[, .SD, .SDcols = inter] %>% t() %>% data.table() %>% .[!V1 %in% "N/A"] %>% t() %>% data.table()
  names(predicted.cal) <- inter[seq(1, ncol(predicted.cal))]
  predicted.cal <- predicted.cal[, .SD, .SDcols = names.share]
  ## 数值化数据并melt
  predicted.cal <- predicted.cal[, lapply(.SD, as.numeric), by = .(Model, Scenario, Region, Variable, Unit)] %>% melt.data.table(data = ., id.vars = c("Model", "Scenario", "Region" , "Variable", "Unit"), variable.name = "years", value.name = "predicted")
  
  # 合并观测值和预测值
  df <- observed.cal[predicted.cal, on = c("Region", "Variable", "Unit", "years")]
  write.csv(df, paste0(date, "/", str_remove_all(index, "\\+") %>% str_remove_all(pattern = "\\|"), ".csv"))
  
  # Create list of selected metrics
  selected.metrics <- c("MAE","RMSE", "RRMSE", "R2", "NSE", "KGE", "PLA", "PLP")
  
  # huhu --------------------------------------------------------------------
  
  scatter_plot.huhu <- function(df){
    # 计算metrics
    metrics.table <- rlang::eval_tidy(data = df, 
                                      rlang::quo(metrica::metrics_summary(data = df, 
                                                                          obs = observed,
                                                                          pred = predicted, 
                                                                          type = "regression",
                                                                          metrics_list = selected.metrics))) %>% dplyr::mutate_if(base::is.numeric, ~base::round(., 2))
    
    B1.PO <- rlang::eval_tidy(data = df, rlang::quo(sqrt(sum((predicted - mean(predicted))^2)/length(predicted))/sqrt(sum((observed - mean(observed))^2)/length(observed))))
    B0.PO <- rlang::eval_tidy(data = df, rlang::quo(mean(predicted) - (B1.PO * mean(observed))))
    
    require(ggplot2)
    if (!require(ggpp)) {
      install.packages("ggpp")
    }
    if (!require(egg)) {
      install.packages("egg")
    }
    
    xy.limit <- ceiling(max(df$predicted))
    
    pdf(file = paste0(date, "/", str_remove_all(index, "\\+") %>% str_remove_all(pattern = "\\|"), ".pdf"), width = 6, height = 5, family = "serif")
    p <- 
      ggplot(data = df, aes(observed, predicted))+ # , col = years
      geom_point(alpha = .35, size = 8, aes(col = years), shape =19)+
      xlim(0, xy.limit)+
      ylim(0, xy.limit)+
      geom_abline(size = 1)+
      geom_abline(linetype = "F1", size = 2, col = "#f46036", slope = B1.PO, intercept = B0.PO)+
      ggpp::annotate(geom = "table", x = xy.limit/4*3, y = xy.limit/2, label = metrics.table, hjust = 0, vjust = 1)+
      ggpp::annotate(geom = "text", x = xy.limit/4*3, y = xy.limit/2 + xy.limit/10, label = paste0("y = ", round(B0.PO, 2), "+", round(B1.PO, 2), "x"), hjust = 0)+
      egg::theme_article()+
      theme(axis.text = element_text(size = 10))
    print(p)
    dev.off()
  }
  
  # 画并输出图
  scatter_plot.huhu(df = df)
} 

metrica.region.huhu <- function(index.ob, index.pre, date){ # 同时以形状显示region
  #' @param indexs.ob 要计算的观测值的名称
  #' @param indexs.pre 要计算的预测值的名称
  #' @param date 将生成的保存结果的文件夹的名称
  
  if (!dir.exists(date)) {
    dir.create(date)
  }
  
  # 读入所有观测值
  observed <- fread("validation.mif", header = T) %>% .[Variable %in% index.ob] %>% .[!Region %in% "World"] # "Production|+|Pasture"
  observed.names <- names(observed)
  
  # 读入所有预测值
  predicted <- fread("report.mif", header = T) %>% .[Variable %in% index.pre] %>% .[!Region %in% "World"]
  predicted.names <- names(predicted)
  
  # 列取交集
  inter <- intersect(observed.names, predicted.names)
  # inter
  
  # 提取观测计算值
  ## 去除N/A列
  observed.cal <- observed[, .SD, .SDcols = inter]
  observed.cal[observed.cal == "N/A"] <- NA
  observed.cal <- observed.cal[!is.na(`1995`)]
  names.observed.cal <- names(observed.cal) %>% data.table() %>% t() %>% data.table()
  observed.cal <- rbind(names.observed.cal, observed.cal, use.names = F) %>% t() %>% na.omit() %>% t() %>% data.table()
  names(observed.cal) <- observed.cal[1] %>% t() %>% .[,1] 
  observed.cal <- observed.cal[!1]
  ## 观测值和预测值共有列名
  names.share <- names(observed.cal)
  ## 数值化数据并melt
  observed.cal <- observed.cal[, lapply(.SD, as.numeric), by = .(Model, Scenario, Region, Variable, Unit)] %>% melt.data.table(data = ., id.vars = c("Model", "Scenario", "Region" , "Variable", "Unit"), variable.name = "years", value.name = "observed")
  
  ## 去除N/A列
  predicted.cal <- predicted[, .SD, .SDcols = inter]
  predicted.cal[predicted.cal == "N/A"] <- NA
  predicted.cal <- predicted.cal[!is.na(`1995`)]
  names.predicted.cal <- names(predicted.cal) %>% data.table() %>% t() %>% data.table()
  predicted.cal <- rbind(names.predicted.cal, predicted.cal, use.names = F) %>% t() %>% na.omit() %>% t() %>% data.table()
  names(predicted.cal) <- predicted.cal[1] %>% t() %>% .[,1] 
  predicted.cal <- predicted.cal[!1]

  predicted.cal <- predicted.cal[, .SD, .SDcols = names.share]
  ## 数值化数据并melt
  predicted.cal <- predicted.cal[, lapply(.SD, as.numeric), by = .(Model, Scenario, Region, Variable, Unit)] %>% melt.data.table(data = ., id.vars = c("Model", "Scenario", "Region" , "Variable", "Unit"), variable.name = "years", value.name = "predicted")
  
  # 使预测值和观测值的index保持一致
  predicted.cal[, Variable := observed.cal$Variable[1]]
  # 合并观测值和预测值
  df <- observed.cal[predicted.cal, on = c("Region", "Variable", "Unit", "years")]
  write.csv(df, paste0(date, "/", str_remove_all(index.ob, "\\+") %>% str_remove_all(pattern = "\\|"), ".csv"))
  
  # Create list of selected metrics
  selected.metrics <- c("MAE","RMSE", "RRMSE", "R2", "NSE", "KGE", "PLA", "PLP")
  
  rm(observed, observed.cal, predicted)
  gc()
  
  # huhu --------------------------------------------------------------------
  
  scatter_plot.huhu <- function(df, Model){
    # 计算metrics
    metrics.table <- rlang::eval_tidy(data = df, 
                                      rlang::quo(metrica::metrics_summary(data = df, 
                                                                          obs = observed,
                                                                          pred = predicted, 
                                                                          type = "regression",
                                                                          metrics_list = selected.metrics))) %>% dplyr::mutate_if(base::is.numeric, ~base::round(., 2))
    
    B1.PO <- rlang::eval_tidy(data = df, rlang::quo(sqrt(sum((predicted - mean(predicted))^2)/length(predicted))/sqrt(sum((observed - mean(observed))^2)/length(observed))))
    B0.PO <- rlang::eval_tidy(data = df, rlang::quo(mean(predicted) - (B1.PO * mean(observed))))
    
    require(ggplot2)
    if (!require(ggpp)) {
      install.packages("ggpp")
    }
    if (!require(egg)) {
      install.packages("egg")
    }
    
    xy.limit <- max(df$observed)
    
    pdf(file = paste0(date, "/", str_remove_all(index.ob, "\\+") %>% str_remove_all(pattern = "\\|"), "_", Model, ".pdf"), width = 7, height = 6, family = "serif")
    p <- 
      ggplot(data = df, aes(observed, predicted))+  
      scale_shape_manual(values = c(15:18, 8, 7, 9, 10, 11:14))+
      xlim(0, xy.limit)+
      ylim(0, xy.limit)+
      geom_abline(size = 1)+
      geom_abline(linetype = "F1", size = 2, col = "#f46036", slope = B1.PO, intercept = B0.PO)+
      ggpp::annotate(geom = "table", x = xy.limit/4*3, y = xy.limit/2, label = metrics.table, hjust = 0, vjust = 1)+
      ggpp::annotate(geom = "text", x = xy.limit/4*3, y = xy.limit/2 + xy.limit/10, label = paste0("y = ", round(B0.PO, 2), "+", round(B1.PO, 2), "x"), hjust = 0)+
      egg::theme_article()+ 
      geom_point(alpha = .35, size = 6, aes(col = years, shape = Region))+
      theme(axis.text = element_text(size = 10))
    print(p)
    dev.off()
  }
  
  # 画并输出图
  ## observed可能有不同模型
  uniq.df <- unique(df$Model)
  Model <- unique(df$Model) %>% str_replace_all(., ":", "_") # 原名称有不合路径名的字符
  
  for (i in 1:length(uniq.df)) {
    scatter_plot.huhu(df = df[Model %in% uniq.df[i]], Model = Model[i])
  }
}
# -------------------------------------------------------------------------

setwd("D:/学习/key/牲畜/metrica")
dir()

if (!require(metrica)) {
  install.packages("metrica")
}
require(metrica)

# 输入参数
indexs.ob <- c("Resources|Carbon|+|Soil",
               "Emissions|NH3|Land|+|Agriculture",
               "Emissions|NO3-|Land|Agriculture",
               "Emissions|CH4|Land|Agriculture|+|Enteric Fermentation"
              )

indexs.pre <- c("Resources|Carbon|+|Soil",
               "Emissions|NH3|Land|+|Agriculture",
               "Emissions|NO3-|Land|+|Agriculture",
               "Emissions|CH4|Land|Agriculture|+|Enteric fermentation"
)

# 输出文件
date <- "metrica.2022.12.28"

# 执行程序
mapply(FUN = metrica.region.huhu, 
       index.ob = indexs.ob, 
       index.pre = indexs.pre, 
       MoreArgs = list(date = date)
       )


# 非mif文件-----------------------------------------------------------------------------
require(metrica)

setwd("D:/学习/key/牲畜/metrica/20221228")
dir()


# 导入文件
data <- openxlsx::read.xlsx("Validation results.xlsx") %>% setDT()

# 参数
df <- data[, 1:3]
names(df) <- c("Genotype", "observed", "predicted")

# Create list of selected metrics
selected.metrics <- c("MAE","RMSE", "RRMSE", "R2", "NSE", "KGE", "PLA", "PLP")

metrics.table <- rlang::eval_tidy(data = df, 
                                  rlang::quo(metrica::metrics_summary(data = df, 
                                                                      obs = observed,
                                                                      pred = predicted, 
                                                                      type = "regression",
                                                                      metrics_list = selected.metrics))) %>% dplyr::mutate_if(base::is.numeric, ~base::round(., 2))

B1.PO <- rlang::eval_tidy(data = df, rlang::quo(sqrt(sum((predicted - mean(predicted))^2)/length(predicted))/sqrt(sum((observed - mean(observed))^2)/length(observed))))
B0.PO <- rlang::eval_tidy(data = df, rlang::quo(mean(predicted) - (B1.PO * mean(observed))))

require(ggplot2)

xy.limit <- max(df$observed)

pdf(file = "Yield.metrics.pdf", width = 6.5, height = 6, family = "serif")

ggplot(data = df, aes(observed, predicted))+
  xlim(0, xy.limit)+
  ylim(0, xy.limit)+
  geom_abline(size = 1)+
  geom_abline(linetype = "F1", size = 2, col = "#f46036", slope = B1.PO, intercept = B0.PO)+
  ggpp::annotate(geom = "table", x = xy.limit/4*3, y = xy.limit/2, label = metrics.table, hjust = 0, vjust = 1)+
  ggpp::annotate(geom = "text", x = xy.limit/4*3, y = xy.limit/2 + xy.limit/10, label = paste0("y = ", round(B0.PO, 2), "+", round(B1.PO, 2), "x"), hjust = 0)+
  egg::theme_article()+ 
  geom_point(alpha = .4, size = 6, aes(col = Genotype))+
  ggsci::scale_color_npg()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20), legend.title = element_text(size = 18), legend.text = element_text(size = 13), legend.position = c(0.1, 0.8))

dev.off()

# 参数
df <- data[, c(1, 4:5)]
names(df) <- c("Genotype", "observed", "predicted")

# Create list of selected metrics
selected.metrics <- c("MAE","RMSE", "RRMSE", "R2", "NSE", "KGE", "PLA", "PLP")

metrics.table <- rlang::eval_tidy(data = df, 
                                  rlang::quo(metrica::metrics_summary(data = df, 
                                                                      obs = observed,
                                                                      pred = predicted, 
                                                                      type = "regression",
                                                                      metrics_list = selected.metrics))) %>% dplyr::mutate_if(base::is.numeric, ~base::round(., 2))

B1.PO <- rlang::eval_tidy(data = df, rlang::quo(sqrt(sum((predicted - mean(predicted))^2)/length(predicted))/sqrt(sum((observed - mean(observed))^2)/length(observed))))
B0.PO <- rlang::eval_tidy(data = df, rlang::quo(mean(predicted) - (B1.PO * mean(observed))))

require(ggplot2)

xy.limit <- max(df$observed)

pdf(file = "MA.metrics.pdf", width = 6.5, height = 6, family = "serif")

ggplot(data = df, aes(observed, predicted))+
  xlim(0, xy.limit)+
  ylim(0, xy.limit)+
  geom_abline(size = 1)+
  geom_abline(linetype = "F1", size = 2, col = "#f46036", slope = B1.PO, intercept = B0.PO)+
  ggpp::annotate(geom = "table", x = xy.limit/4*3, y = xy.limit/2, label = metrics.table, hjust = 0, vjust = 1)+
  ggpp::annotate(geom = "text", x = xy.limit/4*3, y = xy.limit/2 + xy.limit/10, label = paste0("y = ", round(B0.PO, 2), "+", round(B1.PO, 2), "x"), hjust = 0)+
  egg::theme_article()+ 
  geom_point(alpha = .4, size = 6, aes(col = Genotype))+
  ggsci::scale_color_npg()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20), legend.title = element_text(size = 18), legend.text = element_text(size = 13), legend.position = c(0.1, 0.8))

dev.off()








 