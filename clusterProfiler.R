#BiocManager::install("org.At.tair.db")
require(clusterProfiler) #biocmanager下载
require(org.At.tair.db)

columns(org.At.tair.db) #查看库列名
keytypes(org.At.tair.db) #哪些类型可以使用函数select或keys以及keytype参数
head(keys(org.At.tair.db))

gene_symbol <- c("LON3","COAE","GCR2","HMG2","UGT76E6","MLP28")
eg = bitr(gene_symbol, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.At.tair.db") #将SYMBOL转换为ENTREZID
head(eg)
















goids <-c("GO:0009055","GO:0020037","GO:0016705","GO:0005506","GO:0055114")# 基因的ENSEMBL编号
cols <- c("SYMBOL", "GENENAME")# 基因的SYMBOL和GENENAME
select_go <- select(org.At.tair.db, keys=ensids, columns=cols, keytype="GO")# 使用select函数从org.Hs.eg.db数据库中提取这些基因的信息

ego <- enrichGO(
  gene          = gene,
  keyType = "ENTREZID",
  OrgDb         = org.At.tair.db,
  ont           = "CC",
  pAdjustMethod = "BH",
  pvalueCutoff  = 0.01,
  qvalueCutoff  = 0.05,
  readable      = TRUE)

k <- keys(org.At.tair.db, keytype = "ENTREZID")[1:30]
select(org.At.tair.db,
       keys = k,
       columns = c("GO", "ONTOLOGY"),
       keytype="ENTREZID") -> gene

k <- keys(org.At.tair.db, keytype = "SYMBOL")[1:30]
k <- c("RUS6","FES1","NSL1","HSL1")
select(org.At.tair.db,
       keys = k,
       columns = c("GO", "ONTOLOGY","ENTREZID"),
       keytype="SYMBOL")
