all <- .packages(all.available = T)
baoing <- c("data.table","ggplot2","openxlsx","agricolae","magrittr","cowplot")
i = 1
for (i in 1:length(baoing)) {
  ifelse (baoing[i] %in% all == T,print(paste(baoing[i],"您已经经下载过了！")),install.packages(baoing[i],dependencies=T,INSTALL_opts = c('--no-lock'))) 
}   #install.packages(c())
sapply(baoing,require,character.only = T)

.packages(all.available = T) #已经下载的包
(.packages()) #已经加载的包

