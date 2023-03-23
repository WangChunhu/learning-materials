#require(BiocManager)
#BiocManager::install("edgeR")
require(edgeR)
require(data.table)
require(stringr)
require(magrittr)
require(openxlsx)

#https://www.jianshu.com/p/1a3114ea7662 #无重复
#https://www.jianshu.com/p/699b945f8e01 #有重复
#http://yangl.net/2016/09/27/edger_usage/ #经典
#原始数据整理出count

#无重复
T01 <- data.table::fread("I:/生信/转录组学分析/赵静若/赵静若-结果/Cotton_H234-T01_good.genes.results")
T02 <- data.table::fread("I:/生信/转录组学分析/赵静若/赵静若-结果/Cotton_H234-T02_good.genes.results")
T0 <- cbind(T01[,c(1,6)],T02[,6]) %>% data.table::setnames("TPM","T01") %>% data.table::setnames("TPM","T02")
data <- T0
data <- data.frame(data)
data.id <- data[1]
names(data.id) <- "id"
rownames(data) <- data.id$id
data <- data[-1] 
countData <- data.matrix(data)
#分组信息
group_list <- factor(c(rep("Contral",1),rep("Treat",1)))
#设置分组信息,并做TMM标准化
exprSet <- DGEList(counts = countData,group = group_list)
#设置BCV值
bcv <- 0.1
et <- exactTest(exprSet, dispersion=bcv^2)
write.csv(topTags(et, n = nrow(exprSet$counts)),"D:/result.csv", quote = FALSE)
#差异表达基因筛选
result <- read.xlsx("/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/result.xlsx") %>% data.table()
up <- result[logFC >= 1]
down <- result[logFC <= -1]

write.xlsx(x = up,file = "/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/up.xlsx")
write.xlsx(x = down,file = "/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/down.xlsx")



#有重复
T01 <- data.table::fread("/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/Cotton_H234-T01_good.genes.results")
T02 <- data.table::fread("/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/Cotton_H234-T02_good.genes.results")
T0 <- cbind(T01[,c(1,6)],T02[,6]) %>% data.table::setnames("TPM","T01") %>% data.table::setnames("TPM","T02")
gene_id <- T0[,1] %$% str_sub(gene_id,1,11) %>% data.table()
T0_l <- cbind(gene_id,T0) %>% .[,!2] %>% data.table::setnames(".","gene_id") %>% .[!gene_id %in% c("Gh_Contig00","Gh_Contig01")]
T0_l <- T0_l %>% .[,c("T01.2","T02.2") := list(T0_l$T01 + 1,T0_l$T02 + 1)] %>% setcolorder(c(1,2,4,3,5))
T0_lf <- cbind(T0_l,data.table(floor(T0_l$T01)),data.table(floor(T0_l$T01.2)),data.table(floor(T0_l$T02),data.table(floor(T0_l$T02.2)))) %>% .[,!2:5] %>% setnames("V1","T01") %>% setnames("V1","T01.2") %>% setnames("V1","T02") %>% setnames("V1","T02.2") %>% data.frame()
rownames(T0_lf) <- T0_l$gene_id
T0_lf_1 <- T0_lf[-1] 
countData <- data.matrix(T0_lf_1)
#分组信息
group_list <- factor(c(rep("Contral",2),rep("Treat",2)))
#设置分组信息,并做TMM标准化
y <- DGEList(counts = countData,group = group_list)
# 计算样本内标准化因子
y <- calcNormFactors(y) 
#计算普通的离散度
y <- estimateCommonDisp(y) 
#计算基因间范围内的离散度
y <- estimateTagwiseDisp(y)
# 进行精确检验
et <- exactTest(y) 
# 输出排名靠前的差异表达基因信息
topTags(et)
write.csv(topTags(et, n = nrow(y$counts)),"/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/result_rep.csv", quote = FALSE)
result_rep <- fread("/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/result_rep.csv") %>% data.table()
up <- result_rep[logFC >= 1 & FDR <= 0.01]
down <- result_rep[logFC <= -1 & FDR <= 0.01]
write.xlsx(x = up,file = "/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/up_rep.xlsx")
write.xlsx(x = down,file = "/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/down_rep.xlsx")



#差异基因函数 无重复
dff_extract_single <- function(data.count,file.name){
  data <- data.frame(data.count)
  data.id <- data[1]
  names(data.id) <- "id"
  rownames(data) <- data.id$id
  data <- data[-1] 
  countData <- data.matrix(data)
  #分组信息
  group_list <- factor(c(rep("Contral",1),rep("Treat",1)))
  #设置分组信息,并做TMM标准化
  y <- DGEList(counts = countData,group = group_list)
  #按照基因count之和，对数据进行降序排列；
  o <- order(rowSums(y$counts), decreasing=TRUE)
  y <- y[o,]
  tail(y$counts)
  #推荐根据CPM值进行过滤，此法考虑了文库大小；
  keep <- filterByExpr(y) #过滤标准：1. Keeps genes that have count-per-million (CPM) above k in n samples, where k is determined by min.count and by the sample library sizes and n is determined by the design matrix;2. Each kept gene is required to have at least min.total.count reads across all the samples.
  table(keep)
  y <- y[keep, , keep.lib.sizes=FALSE]
  tail(y$counts)
  #设置BCV值（生物变异系数）
  bcv <- 0.1
  et <- exactTest(y, dispersion=bcv^2)
  write.csv(topTags(et, n = nrow(y$counts)),paste0(file.name,"_edgeR.csv"), quote = FALSE)
  #差异表达基因筛选
  edgeR <- fread(paste0(file.name,"_edgeR.csv"))
  up <- edgeR[logFC > 1 & FDR < 0.05] %>% .[,diff := "up"]
  down <- edgeR[logFC < -1 & FDR < 0.05] %>% .[,diff := "down"]
  diff <- rbind(up,down)
  openxlsx::write.xlsx(diff,paste0(file.name,"_diff.xlsx"))
}

#差异基因函数 有重复
dff_extract.rep <- function(data.count,wt.num,treat.num,file.name){
  data <- data.frame(data.count)
  data.id <- data[1]
  names(data.id) <- "id"
  rownames(data) <- data.id$id
  data <- data[-1] 
  countData <- data.matrix(data)
  #分组信息
  group_list <- factor(c(rep("Contral",wt.num),rep("Treat",treat.num)))
  #设置分组信息,并标准化
  y <- DGEList(counts = countData,group = group_list)
  #按照基因count之和，对数据进行降序排列；
  o <- order(rowSums(y$counts), decreasing=TRUE)
  y <- y[o,]
  tail(y$counts)
  #推荐根据CPM值进行过滤，此法考虑了文库大小；
  keep <- filterByExpr(y) #过滤标准：1. Keeps genes that have count-per-million (CPM) above k in n samples, where k is determined by min.count and by the sample library sizes and n is determined by the design matrix;2. Each kept gene is required to have at least min.total.count reads across all the samples.
  table(keep)
  y <- y[keep, , keep.lib.sizes=FALSE]
  tail(y$counts)
  # 对数据进行TMM normalization，计算标准化因子
  y <- calcNormFactors(y) 
  #首先，检查样本中的异常值，使用plotMDS()作图。
  plotMDS(y) #Multidimensional scaling plot中，样本之间的距离对应于这些样品之间主要生物变异系数（biological coefficient of variation，BCV）。从横轴方向（dimension 1）来看，tumor和normal样本可以清楚分开，而在纵轴方向上，样本的编号（patient number）也是基本一致的。这可以证实具有配对样本特性，且肿瘤组织比正常组织有更高的异质性。
  #计算普通的离散度
  y <- estimateCommonDisp(y) 
  #计算基因间范围内的离散度
  y <- estimateTagwiseDisp(y)
  # 进行精确检验
  et <- exactTest(y) 
  # 输出排名靠前的差异表达基因信息
  topTags(et)
  #写出结果
  write.csv(topTags(et, n = nrow(y$counts)),paste0(file.name,"_edgeR.csv"), quote = FALSE)
  edgeR <- fread(paste0(file.name,"_edgeR.csv"))
  up <- edgeR[logFC > 1 & FDR < 0.05] %>% .[,diff := "up"]
  down <- edgeR[logFC < -1 & FDR < 0.05] %>% .[,diff := "down"]
  diff <- rbind(up,down)
  openxlsx::write.xlsx(diff,paste0(file.name,"_diff.xlsx"))
}




