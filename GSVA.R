require(magrittr)
require(data.table)
require(stringr)
require(openxlsx)
# BiocManager::install("GSVA")
require(GSVA)

setwd("G:/zsdf/刘媛媛")
dir()

require(msigdbr) #注意symbol号aim和lib的统一
msigdbr_species() #可以看到，这个包涵盖了20个物种
human <- msigdbr(species = "Homo sapiens")
human[1:5,1:5]
table(human$gs_cat) #查看目录，与MSigDB一样，包含9个数据集
table(human$gs_subcat)
human_KEGG_bp = msigdbr(species = "Mus musculus",
                      category = "C2", #GO在C5
                      subcategory = "CP:KEGG") %>% 
  dplyr::select(gs_name,gene_symbol)#这里可以选择gene symbol，也可以选择ID，根据自己数据需求来，主要为了方便
head(human_KEGG_bp)

human_REACTOME_bp = msigdbr(species = "Mus musculus",
                        category = "C2", #GO在C5
                        subcategory = "CP:BIOCARTA") %>% 
  dplyr::select(gs_name,gene_symbol)#这里可以选择gene symbol，也可以选择ID，根据自己数据需求来，主要为了方便
head(human_REACTOME_bp)
human_bp <- rbind(human_KEGG_bp,human_REACTOME_bp)
head(human_bp)

human_bp.use <- subset(human_bp, gs_name %in% c("KEGG_PROTEASOME","BIOCARTA_HDAC_PATHWAY"))

genesets = split(human_bp$gene_symbol, human_bp$gs_name)
str(head(genesets))

genesets.use <- split(human_bp.use$gene_symbol, human_bp.use$gs_name)
str(head(genesets.use))

# 表达矩阵数据
data <- read.xlsx("1.xlsx") %>% data.table() %>% .[!GENE_SYMBOLS %in% ""] %>% as.matrix()
row.names(data) <- data[,1]
data.matrix <- as.data.frame(data) %>% .[-1] 
data.matrix <- apply(data.matrix,2,as.numeric)
str(data.matrix)
data.matrix[1:3,1:3]
row.names(data.matrix) <- row.names(data)
row.names(data.matrix) %>% head()

data_es <- gsva(expr = data.matrix,gset.idx.list = genesets.use)
data_es 
write.csv(data_es,"data.kegg.biocarta.csv",row.names = T)

# 
# bio <- genesets.use[[1]]
# bio






# BiocManager::install("GSVA")
# BiocManager::install("GSVAdata")
require(GSVA)
require(GSVAdata)

setwd("G:/zsdf/刘媛媛")
dir()

# 参数一
# 表达矩阵数据
# 表达矩阵可以是单纯的matrix格式，也可以是ExpressionSet/SummarizedExperiment对象；
# 对于microarray或者经过log-CPMs, log-RPKMs or log-TPMs之类标准化处理的RNA-seq数据，使用默认的参数即可；如果是原始count的RNA-seq表达水平，需要添加kcdf="Poisson"参数。

# 参数二
# 基因集数据基因集数据
#通路基因集的最直接、简单的格式就是list对象，每个元素包含特定基因集的所有基因，元素名为通路基因名；
# The Molecular Signatures Database (MSigDB)数据库包含了多种主流的注释基因集供下载使用 https://www.gsea-msigdb.org/gsea/msigdb/
# gset.idx.list通路基因集
c2.cp.kegg = clusterProfiler::read.gmt("c2.cp.kegg.v7.5.1.entrez.gmt")
c2.cp.reactome = clusterProfiler::read.gmt("c2.cp.reactome.v7.5.1.entrez.gmt")
genesets = rbind(c2.cp.kegg,c2.cp.reactome)
head(genesets)
genesets = split(genesets$gene, genesets$term)
str(head(genesets))


# 例1：胶质母细胞瘤亚型GSVA分析
# 目的：对于胶质母细胞瘤GBM的四种亚型（proneural, classical, neural and mesenchymal），给定的4个不同大脑细胞类型基因集的富集程度分布情况。

# 表达矩阵数据
data(gbm_VerhaakEtAl)
exprs(gbm_eset)[1:4,1:4] # 部分展示

# 基因集数据基因集数据
data(brainTxDbSets)
str(brainTxDbSets) # 查看数据类型

# gsva()分析
gbm_es <- gsva(gbm_eset, brainTxDbSets)
class(gbm_es) # 查看变量的类
exprs(gbm_es)[1:4,1:3] #部分展示

# 热图可视化
subtypeOrder <- c("Proneural", "Neural", "Classical", "Mesenchymal")
sampleOrderBySubtype <- sort(match(gbm_es$subtype, subtypeOrder),
                             index.return=TRUE)$ix
geneSetOrder <- c("astroglia_up", "astrocytic_up", "neuronal_up",
                  "oligodendrocytic_up")

pheatmap::pheatmap(exprs(gbm_es)[geneSetOrder, sampleOrderBySubtype],
                   show_colnames = F, cluster_cols = F, 
                   annotation_col = pData(gbm_es[,sampleOrderBySubtype]))

# 例2：白血病亚型GSVA分析+差异分析
# 目的：研究常规儿童急性淋巴母细胞白血病(ALL)与MLL(混合系白血病基因)易位的通路富集差异，并进行差异分析，得到在两种亚型中明显差异表达的通路基因集。

# 表达矩阵数据
data(leukemia)
dim(leukemia_eset)
exprs(leukemia_eset)[1:5,1:37] 

#探针的基因名注释
require(hgu95a.db)
ids = as.data.frame(hgu95aENTREZID)
fData(leukemia_eset)$ENTREZID = ids$gene_id[match(rownames(leukemia_eset), ids$probe_id)]
leukemia_eset = leukemia_eset[!is.na(fData(leukemia_eset)$ENTREZID),] # 去掉缺失值
leukemia_eset = leukemia_eset[!duplicated(fData(leukemia_eset)$ENTREZID),] # 去掉重复
rownames(leukemia_eset) = fData(leukemia_eset)$ENTREZID # 换ENTREZID
exprs(leukemia_eset)[1:4,1:4]

#样本分组
table(leukemia_eset$subtype)

length(genesets)
str(head(genesets))

# gsva()分析
leukemia_es <- gsva(leukemia_eset, genesets, min.sz=10, max.sz=500)
exprs(leukemia_es)[1:4,1:3]  

# limma差异分析
require(limma)
mod <- model.matrix(~ factor(leukemia_es$subtype))
colnames(mod) <- c("ALL", "MLLvsALL")
fit <- lmFit(leukemia_es, mod)
fit <- eBayes(fit)
tt <- topTable(fit, coef=2, n=Inf)
head(tt)

# 差异分析可视化：火山图
require(ggplot2)
require(ggrepel)
#自定义阈值 adj.P.Val < 0.05 ；abs(tt$logFC) > 0.25
tt$change = as.factor(ifelse(tt$adj.P.Val < 0.05 & abs(tt$logFC) > 0.25,
                             ifelse(tt$logFC > 0.25 ,'UP','DOWN'),'NOT'))

up_gs = rownames(subset(tt,logFC > 0))[1:3]
down_gs = rownames(subset(tt,logFC < 0))[1:3]
label_gs = tt[c(up_gs,down_gs),]
label_gs$name = rownames(label_gs)
ggplot(data=tt, 
       aes(x=logFC, y=-log10(P.Value), 
           color=change)) +
  geom_point(alpha=0.4, size=1.75) +
  theme_set(theme_set(theme_bw(base_size=20)))+
  xlab("log2 fold change") + ylab("-log10 p-value") +
  theme(plot.title = element_text(size=15,hjust = 0.5))+
  scale_colour_manual(values = c('blue','black','red')) +
  geom_text_repel(
    data = label_gs,
    aes(label = name),min.segment.length = 0)

# 差异分析可视化：分组热图
tt_sig = subset(tt, adj.P.Val < 0.1)
pheatmap::pheatmap(leukemia_es[rownames(tt_sig),],
                   show_colnames = F, cluster_cols = F,
                   annotation_col = pData(leukemia_es))

####################################################################################
# 创建ExpressionSet格式文件
# BiocManager::install("CLL")
require(CLL)

data(sCLLex)
sCLLex
exprMatrix <- exprs(sCLLex)
exprMatrix[1:5,1:5]
dim(exprMatrix)
meta <- pData(sCLLex)
table(meta$Disease)

metadata <- data.frame(labelDescription=c('SampleID', 'Disease'),
                       row.names=c('SampleID', 'Disease'))
phenoData <- new("AnnotatedDataFrame",data=meta,varMetadata=metadata)
myExpressionSet <- ExpressionSet(assayData=exprMatrix,
                                 phenoData=phenoData,
                                 annotation="hgu95av2")
####################################################################################



# liu 只包含一个样本
# 表达矩阵数据
data <- fread("909745.txt") %>% .[,V3 := V2 + 1] %>% .[!V1 %in% ""] %>% as.matrix()
row.names(data) <- data[,1]
data.matrix <- as.data.frame(data) %>% .[-1] 
data.matrix$V2 <- as.numeric(data.matrix$V2)
data.matrix$V3 <- as.numeric(data.matrix$V3)
data.matrix <- as.matrix(data.matrix)
data.matrix %>% head()
meta1 <- data.frame(SampleID = "V2",express = "express1") %>% as.matrix()
rownames(meta1) <- "V2"
meta1 <- as.data.frame(meta1)
metadata1 <- data.frame(labelDescription=c('SampleID', 'express'),
                        row.names=c('SampleID', 'express'))
phenoData1 <- new("AnnotatedDataFrame",data=meta1,varMetadata=metadata1)
myExpressionSet <- ExpressionSet(assayData=data.matrix,
                                 phenoData=phenoData1,
                                 annotation="hgu95av2")

leukemia_eset <- myExpressionSet
#探针的基因名注释
require(hgu95a.db)
ids = as.data.frame(hgu95aENTREZID)
fData(leukemia_eset)$ENTREZID = ids$gene_id[match(rownames(leukemia_eset), ids$probe_id)]
leukemia_eset = leukemia_eset[!is.na(fData(leukemia_eset)$ENTREZID),] # 去掉缺失值
leukemia_eset = leukemia_eset[!duplicated(fData(leukemia_eset)$ENTREZID),] # 去掉重复
rownames(leukemia_eset) = fData(leukemia_eset)$ENTREZID # 换ENTREZID
exprs(leukemia_eset)

#样本分组
table(leukemia_eset$subtype)

c2.cp.kegg = clusterProfiler::read.gmt("c2.cp.kegg.v7.5.1.entrez.gmt")
c2.cp.reactome = clusterProfiler::read.gmt("c2.cp.reactome.v7.5.1.entrez.gmt")
genesets = rbind(c2.cp.kegg,c2.cp.reactome)
head(genesets)
genesets = split(genesets$gene, genesets$term)
str(head(genesets))

length(genesets)
str(head(genesets))
# gsva()分析
leukemia_es <- gsva(leukemia_eset, genesets, min.sz=10, max.sz=500)
exprs(leukemia_es)[1:4,1:3]  

# limma差异分析
require(limma)
mod <- model.matrix(~ factor(leukemia_es$subtype))
colnames(mod) <- c("ALL", "MLLvsALL")
fit <- lmFit(leukemia_es, mod)
fit <- eBayes(fit)
tt <- topTable(fit, coef=2, n=Inf)
head(tt)

# 差异分析可视化：火山图
require(ggplot2)
require(ggrepel)
#自定义阈值 adj.P.Val < 0.05 ；abs(tt$logFC) > 0.25
tt$change = as.factor(ifelse(tt$adj.P.Val < 0.05 & abs(tt$logFC) > 0.25,
                             ifelse(tt$logFC > 0.25 ,'UP','DOWN'),'NOT'))

up_gs = rownames(subset(tt,logFC > 0))[1:3]
down_gs = rownames(subset(tt,logFC < 0))[1:3]
label_gs = tt[c(up_gs,down_gs),]
label_gs$name = rownames(label_gs)
ggplot(data=tt, 
       aes(x=logFC, y=-log10(P.Value), 
           color=change)) +
  geom_point(alpha=0.4, size=1.75) +
  theme_set(theme_set(theme_bw(base_size=20)))+
  xlab("log2 fold change") + ylab("-log10 p-value") +
  theme(plot.title = element_text(size=15,hjust = 0.5))+
  scale_colour_manual(values = c('blue','black','red')) +
  geom_text_repel(
    data = label_gs,
    aes(label = name),min.segment.length = 0)

# 差异分析可视化：分组热图
tt_sig = subset(tt, adj.P.Val < 0.1)
pheatmap::pheatmap(leukemia_es[rownames(tt_sig),],
                   show_colnames = F, cluster_cols = F,
                   annotation_col = pData(leukemia_es))


