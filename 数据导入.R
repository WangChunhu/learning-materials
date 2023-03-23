#导入sas数据（sas。get()）
library(hmisc)
datadir <- "H:\\综合\\软件问题\\SAS"
sasexe <- "D:\\SAS92\\sas.exe "
mydata <- sas.get(libraryName=datadir,member="clients",sasprog=sasexe)
