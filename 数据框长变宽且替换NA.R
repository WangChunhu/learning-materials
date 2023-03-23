require(stringr)

bed <- fread("I:/高老师/PIF-20211231/启动子/修/cotton.PIF.features.修.tbtools.bed") %>% setorder(V1,V4,V2)
write.xlsx(bed,"I:/高老师/PIF-20211231/启动子/修/cotton.PIF.features.修.tbtools.order.bed.xlsx")

bed <- read.xlsx("I:/高老师/PIF-20211231/启动子/修/cotton.PIF.features.修.tbtools.order.bed.xlsx") %>% data.table()
bed.qdz.num <- bed[, .(num = .N),by = .(V1,V4)]
bed.qdz.num.h <- dcast(data = bed.qdz.num,formula = V1 ~ V4)
bed.qdz.num.h[is.na(bed.qdz.num.h)] <- 0
write.xlsx(bed.qdz.num.h,"I:/高老师/PIF-20211231/启动子/修/PIF启动子各元件数目.xlsx")
