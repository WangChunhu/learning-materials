#install.packages(c("cowplot","data.table","openxlsx","agricolae","magrittr","stringr"))

bao <- c("cowplot","data.table","openxlsx","agricolae","magrittr","stringr","devtools")

#下载
sapply(bao, install.packages,character.on = T)

#加载
sapply(bao, require,character.on = T)


