setwd("E:/生信/TM-1")

bao_tm <- c("data.table","magrittr","ggplot2","stringr","openxlsx")
sapply(bao_tm,require,character.on = T)

tip <- fread("Gossypium_hirsutum_v1.1.gene.txt")
cds <- fread("Gossypium_hirsutum_v1.1.cds.txt")
pro <- fread("Gossypium_hirsutum_v1.1.pep.txt")
all <- fread("Gossypium_hirsutum_v1.1.txt")
GRAS <- read.xlsx("GRAS-MADS.xlsx",sheet = 2) %>% setDT()


