libs <- c("data.table", "stringr", "magrittr")
sapply(libs, require, character.on = T)

# Set workdir
workdir <- "D:/学习/key/牲畜/20221227"
setwd(workdir)
dir()

data2 <- openxlsx::read.xlsx("Fig.2.xlsx", cols = 1:6) %>% data.table()
t <- unique(data2$`Temp(°)`); t

i <- 1
metrics.table.all <- NULL
for (i in 1:length(t)) {
  single <- data2[`Temp(°)` %in% t[i]] %>% na.omit()
  metrics.table <- rlang::eval_tidy(data = single, 
                                    rlang::quo(metrica::metrics_summary(data = single, 
                                                                        obs = LP_PO4_Av,
                                                                        pred = Simulated.PO4, 
                                                                        type = "regression",
                                                                        metrics_list = selected.metrics))) %>% dplyr::mutate_if(base::is.numeric, ~base::round(., 2)) %>% data.table() %>% .[, `Temp(°)` := t[i]]
  metrics.table.all <- rbind(metrics.table.all, metrics.table)
}

openxlsx::write.xlsx(metrics.table.all, "Fig.2--metrics.table.all.xlsx")
