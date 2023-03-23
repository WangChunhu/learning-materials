
# bao <- c("BiocManager","data.table","stringr","magrittr","openxlsx")
# sapply(X = bao,FUN = install.packages,character.on = T)
# require(BiocManager)
# BiocManager::install("edgeR",force = TRUE)

require(edgeR)
require(data.table)
require(stringr)
require(magrittr)
require(openxlsx)


#https://www.jianshu.com/p/1a3114ea7662 #无重复
#https://www.jianshu.com/p/699b945f8e01 #有重复
#http://yangl.net/2016/09/27/edger_usage/ #经典

#定量
#原始数据整理出count

#无重复
T01 <- data.table::fread("I:/生信/转录组学分析/赵静若/赵静若-结果/Cotton_H234-T01_good.genes.results")
T02 <- data.table::fread("I:/生信/转录组学分析/赵静若/赵静若-结果/Cotton_H234-T02_good.genes.results")
T0 <- cbind(T01[,c(1,6)],T02[,6]) %>% data.table::setnames("TPM","T01") %>% data.table::setnames("TPM","T02")
gene_id <- T0[,1] %$% str_sub(gene_id,1,11) %>% data.table()
T0_l <- cbind(gene_id,T0) %>% .[,!2] %>% data.table::setnames(".","gene_id") %>% .[!gene_id %in% c("Gh_Contig00","Gh_Contig01")]
T0_lf <- cbind(T0_l,data.table(floor(T0_l$T01)),data.table(floor(T0_l$T02))) %>% .[,!2:3] %>% setnames(c("V1"),c("T01")) %>% setnames(c("V1"),c("T02")) %>% data.frame()
rownames(T0_lf) <- T0_l$gene_id
T0_lf_1 <- T0_lf[-1] 
countData <- data.matrix(T0_lf_1)
#分组信息
group_list <- factor(c(rep("Contral",1),rep("Treat",1)))
#设置分组信息,并做TMM标准化
exprSet <- DGEList(counts = countData,group = group_list)
#设置BCV值
bcv <- 0.1
et <- exactTest(exprSet, dispersion=bcv^2)
write.csv(topTags(et, n = nrow(exprSet$counts)),"/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/result.csv", quote = FALSE)
#差异表达基因筛选
result <- read.xlsx("/media/huhu/学习/生信/转录组学分析/赵静若/赵静若-结果/result.xlsx") %>% data.table()
up <- result[logFC >= 1]
down <- result[logFC <= -1]
write.xlsx(x = up,file = "I:/生信/转录组学分析/赵静若/赵静若-结果/up.xlsx")
write.xlsx(x = down,file = "I:/生信/转录组学分析/赵静若/赵静若-结果/down.xlsx")


#有重复
require(edgeR)
require(data.table)
require(stringr)
require(magrittr)
require(openxlsx)

setwd("I:/张振楠/转录组9-8")
files <- dir(pattern = "results")
sample <- str_split_fixed(string = files,pattern = "\\.",n = 3)[1:length(files)]

#TPM
i <- 1
dan <- NULL
all <- NULL
for (i in 1:length(sample)) {
  dan <- fread(files[i])[,c(1,6)]
  names(dan) <- c("id",sample[i])
  all <- cbind(all,dan)
}

#爬虫总id
id <- all$id
rm(dan)
gc()

data_dff_all <- all[,c(1:2,4,6,8,10,12)] %>% setcolorder(c(1L,6L:7L,2L:4L))  #不能用seq

#各组数据
WT_M_dff.data <- data_dff_all[,1:5] %>% setnames("id","WT_M") %>% data.frame()
WT_P_dff.data <- data_dff_all[,c(1:3,6:7)] %>% setnames("id","WT_P") %>% data.frame()
rm(data_dff_all)
gc()

#注释文件
zhushi <- read.xlsx("GH_注释.xlsx") %>% data.table() %>% .[,1:3] %>% setnames("gene_id","V1")

#差异基因函数
dff_extract <- function(data){
  file.name <- names(data)[1]
  rownames(data) <- all$id
  data <- data[-1]
  countData <- data.matrix(data)
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
  #写出结果
  write.csv(topTags(et, n = nrow(y$counts)),paste0(file.name,"-result_rep.csv"), quote = FALSE)
  rm(y,et,countData)
  gc()
}

#差异基因计算
dff_extract(data = WT_M_dff.data)
dff_extract(data = WT_P_dff.data)

#表达量提取（FPKM）
i <- 1
dan_F <- NULL
all_F <- NULL
for (i in 1:length(sample)) {
  dan_F <- fread(files[i])[,c(1,7)]
  names(dan_F) <- c("id",sample[i])
  all_F <- cbind(all_F,dan_F)
}
data_F_dff_all <- all_F[,c(1:2,4,6,8,10,12)] %>% setcolorder(c(1L,6L:7L,2L:4L))  #不能用seq
##表达量提取函数
express_extract <- function(data.name,throw.cols){
  data <- read.xlsx(data.name) %>% data.table() %>% setnames("V1","id")
  res <- data_F_dff_all[data,on = "id"] %>% data.frame() %>%  .[-as.numeric(throw.cols)]
}
WT_M_up.express <- express_extract(data.name = "WT_M-up_rep.xlsx",throw.cols = 6:7)
write.xlsx(WT_M_up.express,"WT_M_up.express.zhushi.xlsx")
WT_M_down.express <- express_extract(data.name = "WT_M-down_rep.xlsx",throw.cols = 6:7)
write.xlsx(WT_M_down.express,"WT_M_down.express.zhushi.xlsx")
WT_P_up.express <- express_extract(data.name = "WT_P-up_rep.xlsx",throw.cols = 4:5)
write.xlsx(WT_P_up.express,"WT_P_up.express.zhushi.xlsx")
WT_P_down.express <- express_extract(data.name = "WT_P-down_rep.xlsx",throw.cols = 4:5)
write.xlsx(WT_P_down.express,"WT_P_down.express.zhushi.xlsx")

#火山图
require(EnhancedVolcano)
require(data.table)
WT_P <- fread("I:/张振楠/转录组9-8/WT_P-result_rep.csv")
WT_M <- fread("I:/张振楠/转录组9-8/WT_M-result_rep.csv")

#画图
##火山图
###自定义函数
vol <- function(data){
  data$threshold[data$FDR < 0.05 & data$logFC > 1] = "Up"
  data$threshold[data$FDR < 0.05 & data$logFC < -1] = "Down"
  data$threshold[data$FDR >= 0.05 | (data$logFC >= -1 & data$logFC <= 1)] = "Non"
  p <-
    ggplot(data,aes(x=logFC,y=-log10(FDR),colour=threshold))+xlab("log2(Fold Change)")+ylab("-log10(FDR)")+
    geom_point(size=4,alpha=0.8)+
    scale_color_manual(values =c("forestgreen","#9F9F9F","red2"))+
    geom_hline(aes(yintercept=-log10(0.05)),colour="black",size=1,linetype=2)+
    geom_vline(aes(xintercept=-1), colour="black",size=1,linetype=2)+
    geom_vline(aes(xintercept=1), colour="black",size=1,linetype=2)
  
  p +
    theme_classic()+
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 40),
          title = element_text(size = 40),
          #legend.position = "top",
          legend.text = element_text(size = 30),
          legend.title = element_text(size = 35),
          #legend.direction = "vertical"
    )
}

pdf("I:/张振楠/转录组9-8/富集分析/作图/FDR/图/火山图.pdf",width = 17,height = 15)
vol(data = WT_P)
vol(data = WT_M)
dev.off()

#差异基因
bao <- c("data.table","magrittr","ggvenn")
sapply(bao, require , character.on = T)

##WT_M.差异基因
WT_M_edge.res <- fread("I:/张振楠/转录组9-8/WT_M-result_rep.csv")
WT_M.dff <- WT_M_edge.res[FDR < 0.05 & abs(logFC) > 1]
WT_M.dff.up <- WT_M.dff[logFC > 1] %>% .[,1]
WT_M.dff.down <- WT_M.dff[logFC < -1] %>% .[,1]
##WT_P.差异基因
WT_P_edge.res <- fread("I:/张振楠/转录组9-8/WT_P-result_rep.csv")
WT_P.dff <- WT_P_edge.res[FDR < 0.05 & abs(logFC) > 1]
WT_P.dff.up <- WT_P.dff[logFC > 1] %>% .[,1]
WT_P.dff.down <- WT_P.dff[logFC < -1] %>% .[,1]

rm(WT_M_edge.res,WT_P_edge.res)
gc()


##取交集
###差异基因交集
dff.inter <- WT_M.dff[V1 %in% WT_P.dff$V1] %>% .[,1]
gh.name <- fread("I:/张振楠/转录组9-8/富集分析/blast后所有1740个gh基因的名字_包括NA.csv") %>% setnames("query","V1")
dff.inter.symbol <- gh.name[dff.inter,on = "V1"] #富集分析用
up.inter <- WT_M.dff.up[V1 %in% WT_P.dff.up$V1]
up.inter.symbol <- gh.name[up.inter,on = "V1"]
down.inter <- WT_M.dff.down[V1 %in% WT_P.dff.down$V1]
down.inter.symbol <- gh.name[down.inter,on = "V1"]
##韦恩图
pdf("I:/张振楠/转录组9-8/富集分析/作图/FDR/图/韦恩图.pdf",width = 10,height = 10)
###总
data <- list("WT_P" = WT_P.dff$V1,
             "WT_M" = WT_M.dff$V1)
ggvenn(data = data,
       columns = c("WT_P","WT_M"),
       stroke_color = "white",
       fill_color = c("#4D9221","#FF0000FF"),
       set_name_color =c("#4D9221","#FF0000FF"),
       text_size = 8,
       set_name_size = 12
)

###up
data <- list("WT_P.up" = WT_P.dff.up$V1,
             "WT_M.up" = WT_M.dff.up$V1)
ggvenn(data = data,
       columns = c("WT_P.up","WT_M.up"),
       stroke_color = "white",
       fill_color = c("#4D9221","#FF0000FF"),
       set_name_color =c("#4D9221","#FF0000FF"),
       text_size = 8,
       set_name_size = 12
)

###up
data <- list("WT_P.down" = WT_P.dff.down$V1,
             "WT_M.down" = WT_M.dff.down$V1)
ggvenn(data = data,
       columns = c("WT_P.down","WT_M.down"),
       stroke_color = "white",
       fill_color = c("#4D9221","#FF0000FF"),
       set_name_color =c("#4D9221","#FF0000FF"),
       text_size = 8,
       set_name_size = 12
)
dev.off()

##热图
###取热图数据
GH.zhushi <- openxlsx::read.xlsx("I:/张振楠/转录组9-8/GH_注释.xlsx",cols = 1:3) %>% data.table() %>% .[,注释 := paste(gene_name,注释,sep = "__")] %>% .[,!2] 
all.FPKM <- fread("I:/张振楠/转录组9-8/富集分析/所有基因FPKM值.csv")
dff.inter.FPKM <- all.FPKM[id %in% dff.inter$V1]
dff.inter.FPKM.zhushi <- GH.zhushi[dff.inter.FPKM,on = "id"] %>% .[,name := paste(id,注释,sep = "__")] %>% .[,!1:2] %>% setcolorder(c(7L,1:6L))
fwrite(dff.inter.FPKM.zhushi,"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/dff.inter.FPKM.zhushi.csv")
up.inter.FPKM <- all.FPKM[id %in% up.inter$V1]
up.inter.FPKM.zhushi <- GH.zhushi[up.inter.FPKM,on = "id"] %>% .[,name := paste(id,注释,sep = "__")] %>% .[,!1:2] %>% setcolorder(c(7L,1:6L))
fwrite(up.inter.FPKM.zhushi,"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/up.inter.FPKM.zhushi.csv")
down.inter.FPKM <- all.FPKM[id %in% down.inter$V1]
down.inter.FPKM.zhushi <- GH.zhushi[down.inter.FPKM,on = "id"] %>% .[,name := paste(id,注释,sep = "__")] %>% .[,!1:2] %>% setcolorder(c(7L,1:6L))
fwrite(down.inter.FPKM.zhushi,"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/down.inter.FPKM.zhushi.csv")
###Tbtools

#加载包
bao <- c("data.table","openxlsx","magrittr","stringr","Rcrawler")
sapply(bao, require,character.on = T)

#h2v
h2v <- function(data){ 
  apply(data,1,function(x){
    x  %>% data.frame()
  }) %>% unlist() %>% data.frame()
}

#提取目标基因pep序列——自定义函数
##GH_pep
pep <- fread("I:/张振楠/chip/TM-1_V2.1.gene.pep_1h.fa")

##设置工作空间
setwd("I:/张振楠/转录组9-8/富集分析")
files <- dir(pattern = "xlsx")
##提取pep
i <- 1
all <- NULL
for (i in 1:length(files)) {
  ##目标id
  aim <- read.xlsx(files[i]) %>% data.table() %>% .[,1] %>% .[,V1 := paste0(">",V1)] #colname = V1
  ##匹配并转换为fa文件写出
  aim.pep <- pep[aim,on = "V1"] %>% h2v()
  all <- rbind(all,aim.pep)
  fwrite(x = aim.pep,file =  paste0(str_split_fixed(string = files[i],pattern = "_rep",n = 2)[1],".pep"),col.name = F)
}
fwrite(x = all,file = "all.pep",col.names = F)

#blast结果
blast <- fread("I:/张振楠/转录组9-8/富集分析/pep.blast",header = F) %>% setnames(c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12"),c("query","subject","比对相似性","匹配长度","错配长度","gap","q.start","q.end","s.start","s.end","e","score"))
gene.num <- unique(blast$query) #1740/2545
i <- 1
dan <- NULL
blast.last <- NULL
for (i in 1:length(gene.num)) {
  dan <- blast[query %in% gene.num[i]] %>% .[1,1:2]
  blast.last <- rbind(blast.last,dan)
  print(paste0(i," / 1740"))
}
fwrite(x = blast.last,file = "blast.result",col.names = T,sep = "\t")at

#注释拟南芥名
blast <- fread("blast.result") #blast=1740,共2545个基因，无blast=805
blast_rmdot <- blast[,subject := str_split_fixed(string = blast$subject,pattern = "\\.",n = 2)[,1]]
at.name <- fread("I:/生信/基因家族分析/拟南芥信息/at.name.rep.csv") %>% .[,1:2] %>% .[,tf := duplicated(locus_name)] %>% .[tf %in% F] %>% .[,1:2] %>% setnames("locus_name","subject")
rm(blast)
gc()
gh.name <- at.name[blast_rmdot,on = "subject"] #blast=1740，无名=676
fwrite(gh.name,"blast后所有1740个gh基因的名字_包括NA.csv")  

#指定富集分析的物种库
#BiocManager::install("org.At.tair.db")
##KEGG/GO
#加载包
bao <- c("openxlsx","ggplot2","data.table","stringr","enrichplot","clusterProfiler","GOplot","DOSE","ggnewscale","topGO","circlize","ComplexHeatmap")
sapply(bao, require,character.on = T)

# data <- dff.inter.symbol
# data <- up.inter.symbol
data <- down.inter.symbol
require(org.At.tair.db)
GO_database <- org.At.tair.db
KEGG_database <- "ath" #KEGG分析指定物种，物种缩写索引表详见http://www.genome.jp/kegg/catalog/org_list.html /ghi
#gene ID转换
gene <- bitr(geneID = data$symbol,fromType = 'SYMBOL',toType = 'ENTREZID',OrgDb = GO_database)
#fwrite(gene,"gene.list",sep = "\t")
#GO分析:
GO <- enrichGO(gene$ENTREZID,#GO富集分析
               OrgDb = GO_database,
               keyType = "ENTREZID",#设定读取的gene ID类型
               ont = "ALL",#(ont为ALL因此包括 Biological Process,Cellular Component,Mollecular Function三部分）
               pvalueCutoff = 0.05,#设定p值阈值
               qvalueCutoff = 0.05,#设定q值阈值
               readable = T)
dim(GO[GO$ONTOLOGY=='BP',]) #91
dim(GO[GO$ONTOLOGY=='cc',]) #0
dim(GO[GO$ONTOLOGY=='MF',]) #24
#write.xlsx(GO@result,"WT_P.down_GO.xlsx")
#KEGG分析:
#data(geneList, package="DOSE")
KEGG <- enrichKEGG(data$subject,#KEGG富集分析
                   organism = KEGG_database,
                   pvalueCutoff = 0.05,
                   qvalueCutoff = 0.2)
#write.xlsx(KEGG@result,"WT_P.down_KEGG.xlsx")
#GO图
pdf("I:/张振楠/转录组9-8/富集分析/作图/FDR/图/组合前/DOWN.GO富集.pdf",width = 22,height = 10) 
#GO/KEGG富集柱状图:
data.GO <- GO@result %>% data.table() %>% .[pvalue < .05] %>% .[Count >= 5]
data.GO.10 <- rbind(data.GO[ONTOLOGY %in% "BP"][1:10],data.GO[ONTOLOGY %in% "MF"][1:10],data.GO[ONTOLOGY %in% "CC"][1:10]) %>% .[!ONTOLOGY %in% NA]
data.GO <- data.GO.10
data.GO <- setorder(data.GO,"ONTOLOGY","pvalue")
data.GO$ONTOLOGY <- factor(data.GO$ONTOLOGY,levels = unique(data.GO$ONTOLOGY))
data.GO$Description <- factor(data.GO$Description,levels = rev(unique(data.GO$Description)))
ggplot(data = data.GO,aes(Description,Count))+
  geom_col(aes(fill = ONTOLOGY),width = 0.6)+
  geom_point(aes(color = pvalue,size = 10))+
  scale_fill_manual(values = c("#00AAFFFF","#EBB25EFF","#FF80FFFF"))+
  scale_color_gradient(low = "red",high = "green",limits=c(0,0.01),breaks=c(0,0.005,0.01))+
  scale_size(guide = "none")+
  #facet_grid(.~ONTOLOGY)+
  theme_classic()+
  coord_flip()+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 25),legend.text = element_text(size = 20),legend.title = element_text(size = 25),strip.text = element_text(size = 20))

data.GO <- GO@result %>% data.table() %>% setorder("qvalue") %>% .[1:20] %>% .[Count >= 5]
data.GO <- setorder(data.GO,"qvalue")
data.GO$ONTOLOGY <- factor(data.GO$ONTOLOGY,levels = unique(data.GO$ONTOLOGY))
data.GO$Description <- factor(data.GO$Description,levels = rev(unique(data.GO$Description)))
data.GO <- data.GO[,c("GeneRatio","BgRatio") := list(str_split_fixed(GeneRatio,"/",2)[,1] %>% as.numeric(),str_split_fixed(BgRatio,"/",2)[,1] %>% as.numeric())] %>% .[,RichFactor := GeneRatio / BgRatio]
ggplot(data = data.GO,aes(Description,RichFactor,shape = ONTOLOGY))+ #,color = pvalue
  geom_point(aes(color = qvalue,size = Count))+
  scale_color_gradient(low = "red",high = "green",limits=c(0,0.001),breaks=c(0,0.001))+
  theme_classic()+
  scale_size(limits = c(0,20),breaks = c(5,10,15))+
  coord_flip()+
  scale_y_continuous(limits = c(0,0.3),breaks = c(0.1,0.2,0.3))+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 25),legend.text = element_text(size = 20),legend.title = element_text(size = 25),strip.text = element_text(size = 20))+
  guides(shape = guide_legend(override.aes = list(size = 5)))
dev.off()

#KEGG图
pdf("I:/张振楠/转录组9-8/富集分析/作图/FDR/图/组合前/DOWN.KEGG富集.pdf",width = 22,height = 5)
data.KEGG <- KEGG@result %>% data.table() %>% setorder("pvalue") %>% .[Count >= 4]
data.KEGG$pvalue <- round(data.KEGG$pvalue,digits = 3)
data.KEGG$Description <- factor(data.KEGG$Description,levels = rev(unique(data.KEGG$Description)))
ggplot(data = data.KEGG,aes(Description,Count))+
  geom_col(aes(fill = pvalue),width = .2)+
  scale_fill_gradient(low = "red",high = "green",limits=c(0,0.003),breaks=c(0.000,0.001,0.002,0.003))+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(limits = c(0,5),breaks = c(1,3,5))+
  theme(axis.text = element_text(size = 20),axis.title = element_text(size = 25),legend.text = element_text(size = 20),legend.title = element_text(size = 25),strip.text = element_text(size = 20))

data.KEGG <- KEGG@result %>% data.table() %>% setorder("qvalue") %>% .[1:2]
data.KEGG$qvalue <- round(data.KEGG$qvalue,digits = 3)
data.KEGG$Description <- factor(data.KEGG$Description,levels = rev(unique(data.KEGG$Description)))
data.KEGG <- data.KEGG[,c("GeneRatio","BgRatio") := list(str_split_fixed(GeneRatio,"/",2)[,1] %>% as.numeric(),str_split_fixed(BgRatio,"/",2)[,1] %>% as.numeric())] %>% .[,RichFactor := GeneRatio / BgRatio]
data.KEGG$RichFactor <- round(data.KEGG$RichFactor,digits = 3)
ggplot(data = data.KEGG,aes(Description,RichFactor))+
  geom_point(aes(color = qvalue,size = Count))+
  scale_color_gradient(low = "red",high = "green",limits=c(0,0.05),breaks=c(0.00,0.02,0.04))+
  theme_classic()+
  coord_flip()+
  scale_size(limits = c(0,5),breaks = c(1,3,5))+
  scale_y_continuous(limits = c(0,0.1),breaks = c(0.03,0.06,0.09))+
  theme(axis.text = element_text(size = 25),axis.title = element_text(size = 30),legend.text = element_text(size = 20),legend.title = element_text(size = 30))
dev.off()

#富集基因与所在功能集/通路集的关联网络图：
pdf("I:/张振楠/转录组9-8/富集分析/作图/二次/图/jiaoji.基因-通路关联网络图.pdf",width = 30,height = 20)
enrichplot::cnetplot(GO,circular=FALSE,colorEdge = TRUE)+ #基因-通路关联网络图
  theme(title = element_text(size = 30),axis.text = element_text(size = 25),legend.text = element_text(size = 25),legend.title = element_text(size = 30),strip.text = element_text(size = 25))+
  labs(title = "GO注释  基因-通路关联网络图")

enrichplot::cnetplot(KEGG,circular=FALSE,colorEdge = TRUE)+ #circluar为指定是否环化，基因过多时建议设置为FALSE
  theme(title = element_text(size = 30),axis.text = element_text(size = 25),legend.text = element_text(size = 25),legend.title = element_text(size = 30),strip.text = element_text(size = 25))+
  labs(title = "KEGG注释  基因-通路关联网络图")


#热图形式展现关联关系:
enrichplot::heatplot(GO,showCategory = 50)+
  theme(axis.text = element_text(size = 15))#基因-通路关联热图

enrichplot::heatplot(KEGG,showCategory = 50)+
  theme(axis.text = element_text(size = 40))

dev.off()
# #富集到的功能集/通路集之间的关联网络图：
# GO2 <- pairwise_termsim(GO)
# KEGG2 <- pairwise_termsim(KEGG)
# enrichplot::emapplot(GO2,showCategory = 50, color = "p.adjust", layout = "kk")+
#   theme(axis.text = element_text(size = 20),legend.text = element_text(size = 20),legend.title = element_text(size = 25),strip.text = element_text(size = 20))#通路间关联网络图
# 
# enrichplot::emapplot(KEGG2,showCategory =50, color = "p.adjust", layout = "kk")+
#   theme(axis.text = element_text(size = 20),legend.text = element_text(size = 20),legend.title = element_text(size = 25),strip.text = element_text(size = 20))

#保存KEGG富集到的通路至本地文件并选择通路进行展示:
# KEGG$ID
# write.table(KEGG$ID, file = "/Users/ZYP/Downloads/KEGG_GO/KEGG_IDs.txt", #将所有KEGG富集到的通路写入本地文件查看
#               append = FALSE, quote = TRUE, sep = " ",
#               eol = "\n", na = "NA", dec = ".", row.names = TRUE,
#               col.names = TRUE, qmethod = c("escape", "double"),
#               fileEncoding = "")
# browseKEGG(KEGG,"ath00900")#选择其中的hsa05166通路进行展示
# browseKEGG(KEGG,"ath00650")
#GO富集功能网络图:
GO_BP<-enrichGO( gene$ENTREZID,#GO富集分析BP模块
                 OrgDb = GO_database,
                 keyType = "ENTREZID",
                 ont = "BP",
                 pvalueCutoff = 0.05,
                 pAdjustMethod = "BH",
                 qvalueCutoff = 0.05,
                 minGSSize = 10,
                 maxGSSize = 500,
                 readable = T)
#BiocManager::install("Rgraphviz")
require(Rgraphviz)
plotGOgraph(GO_BP)#GO-BP功能网络图
GO_CC<-enrichGO( gene$ENTREZID,#GO富集分析CC模块
                 OrgDb = GO_database,
                 keyType = "ENTREZID",
                 ont = "CC",
                 pvalueCutoff = 0.05,
                 pAdjustMethod = "BH",
                 qvalueCutoff = 0.05,
                 minGSSize = 10,
                 maxGSSize = 500,
                 readable = T)
plotGOgraph(GO_CC)#GO-CC功能网络图
GO_MF<-enrichGO( gene$ENTREZID,#GO富集分析MF模块
                 OrgDb = GO_database,
                 keyType = "ENTREZID",
                 ont = "MF",
                 pvalueCutoff = 0.05,
                 pAdjustMethod = "BH",
                 qvalueCutoff = 0.05,
                 minGSSize = 10,
                 maxGSSize = 500,
                 readable = T)
plotGOgraph(GO_MF)#GO-MF功能网络图


#制作org。db (标准注释库)
# BiocManager::install("AnnotationHub")
# BiocManager::install("biomaRt")

# require(AnnotationHub)
# require(biomaRt)
# hub <- AnnotationHub::AnnotationHub()
# query(hub, "Gossypium hirsutum")
# org.GH.db <- hub[['AH93847']]

#处理并合并go和kegg
#加载包
bao <- c("openxlsx","ggplot2","data.table","stringr","enrichplot","clusterProfiler","GOplot","DOSE","ggnewscale","topGO","circlize","ComplexHeatmap")
sapply(bao, require,character.on = T)

setwd("I:/张振楠/转录组9-8/富集分析")
##自定义函数
go_kegg <- function(zhushi_file,go,kegg){
  zhushi_file <- fread(zhushi_file)
  go <- read.xlsx(go) %>% data.table() %>% .[,!c(4:8,10)]
  i <- 1
  dan <- NULL
  all <- NULL
  for (i in 1:nrow(go)) {
    dan <- go[i]
    dan_str <- str_split(dan$geneID,pattern = "/") %>% unlist() %>% data.table() %>% setnames(".","geneID")
    dan_l <- data.table(dan_str,dan) %>% .[,!5]
    all <- rbind(all,dan_l)
    print(paste0(i,"/",nrow(go)))
  }
  go.zhushi <- all
  
  i <- 1
  dan <- NULL
  all <- NULL
  id <- go.zhushi$geneID %>% unique()
  for (i in 1:length(id)) {
    dan <- go.zhushi[geneID %in% id[i]] %>% .[,GO_annotation := paste(ONTOLOGY,ID,Description,sep = "--")] %>% .[,!2:4]
    dan_l <- data.table(dan$geneID[1],paste(dan$GO_annotation,collapse = "  |  "))
    all <- rbind(all,dan_l)
    print(paste0(i,"/",length(id)))
  }
  names(all) <- c("symbol","GO_annotation")
  go.zhushi <- all
  
  kegg <- read.xlsx(kegg) %>% data.table() %>% .[,!c(3:7,9)]
  i <- 1
  dan <- NULL
  all <- NULL
  for (i in 1:nrow(kegg)) {
    dan <- kegg[i]
    dan_str <- str_split(dan$geneID,pattern = "/") %>% unlist() %>% data.table() %>% setnames(".","geneID")
    dan_l <- data.table(dan_str,dan) %>% .[,!4]
    all <- rbind(all,dan_l)
    print(paste0(i,"/",nrow(kegg)))
  }
  kegg.zhushi <- all
  
  i <- 1
  dan <- NULL
  all <- NULL
  id <- kegg.zhushi$geneID %>% unique()
  for (i in 1:length(id)) {
    dan <- kegg.zhushi[geneID %in% id[i]] %>% .[,kegg_annotation := paste(ID,Description,sep = "--")] %>% .[,!2:3]
    dan_l <- data.table(dan$geneID[1],paste(dan$kegg_annotation,collapse = "  |  "))
    all <- rbind(all,dan_l)
    print(paste0(i,"/",length(id)))
  }
  names(all) <- c("subject","kegg_annotation")
  kegg.zhushi <- all
  go_l <- go.zhushi[zhushi_file,on = "symbol"] 
  go.kegg_l <- kegg.zhushi[go_l,on = "subject"] %>% .[,!c(12:15)] %>% setcolorder(c(5L,10L,1L,3L,4L,2L,11L,6L:9L)) %>% .[,!8:11]
  return(go.kegg_l)
}
#go_kegg() 执行
jiaoji_up.l <- go_kegg(zhushi_file = "UP-UP.jiaoji.csv",go = "jiaoji_up_GO.xlsx",kegg = "jiaoji_up_KEGG.xlsx")
jiaoji_down.l <- go_kegg(zhushi_file = "down-down.jiaoji.csv",go = "jiaoji_down_GO.xlsx",kegg = "jiaoji_down_KEGG.xlsx")
WT_M_up.l <- go_kegg(zhushi_file = "WT_M.UP.atName.csv",go = "WT_M.UP_GO.xlsx",kegg = "WT_M.UP_KEGG.xlsx")
WT_M_down.l <- go_kegg(zhushi_file = "WT_M.down.atName.csv",go = "WT_M.down_GO.xlsx",kegg = "WT_M.down_KEGG.xlsx")
WT_P_up.l <- go_kegg(zhushi_file = "WT_P.UP.atName.csv",go = "WT_P.UP_GO.xlsx",kegg = "WT_P.UP_KEGG.xlsx")
WT_P_down.l <- go_kegg(zhushi_file = "WT_P.down.atName.csv",go = "WT_P.down_GO.xlsx",kegg = "WT_P.down_KEGG.xlsx")

#提取差异基因FPKM值
setwd("I:/张振楠/转录组9-8")
files <- dir(pattern = "results")
sample <- str_split_fixed(string = files,pattern = "\\.",n = 3)[1:length(files)]

#FPKM
i <- 1
dan <- NULL
all <- NULL
for (i in 1:length(sample)) {
  dan <- fread(files[i])[,c(1,7)]
  names(dan) <- c("id",sample[i])
  all <- cbind(all,dan)
}

rm(dan)
gc()

data_dff_all <- all[,c(1:2,4,6,8,10,12)] %>% setcolorder(c(1L,6L:7L,2L:4L)) %>% setnames("id","query") #不能用seq
setwd("I:/张振楠/转录组9-8/富集分析")
fwrite(data_dff_all,"所有基因FPKM值.csv")
jiaoji_up.l.FPKM <- data_dff_all[jiaoji_up.l,on = "query"]
fwrite(jiaoji_up.l.FPKM,"I:/张振楠/转录组9-8/富集分析/结果表格/jiaoji_up.l.FPKM.csv")
jiaoji_down.l.FPKM <- data_dff_all[jiaoji_down.l,on = "query"]
fwrite(jiaoji_down.l.FPKM,"I:/张振楠/转录组9-8/富集分析/结果表格/jiaoji_down.l.FPKM.csv")
WT_M_up.l.FPKM <- data_dff_all[WT_M_up.l,on = "query"] %>% .[,!6:7]
fwrite(WT_M_up.l.FPKM,"I:/张振楠/转录组9-8/富集分析/结果表格/WT_M_up.l.FPKM.csv")
WT_M_down.l.FPKM <- data_dff_all[WT_M_down.l,on = "query"] %>% .[,!6:7]
fwrite(WT_M_down.l.FPKM,"I:/张振楠/转录组9-8/富集分析/结果表格/WT_M_down.l.FPKM.csv")
WT_P_up.l.FPKM <- data_dff_all[WT_P_up.l,on = "query"] %>% .[,!4:5]
fwrite(WT_P_up.l.FPKM,"I:/张振楠/转录组9-8/富集分析/结果表格/WT_P_up.l.FPKM.csv")
WT_P_down.l.FPKM <- data_dff_all[WT_P_down.l,on = "query"] %>% .[,!4:5]
fwrite(WT_P_down.l.FPKM,"I:/张振楠/转录组9-8/富集分析/结果表格/WT_P_down.l.FPKM.csv")

###kegg前两个通路
gh.name <- fread("I:/张振楠/转录组9-8/富集分析/blast后所有1740个gh基因的名字_包括NA.csv")
kegg2GH <- function(data.kegg){
  kegg2 <- data.KEGG[1:2,8] 
  kegg2.id.row1 <- paste(kegg2[1]) %>% str_split(pattern = "/") %>% unlist()
  kegg2.id.row2 <- paste(kegg2[2]) %>% str_split(pattern = "/") %>% unlist()
  kegg2.id.GH.row1 <- gh.name[subject %in% kegg2.id.row1] %>% .[,3]
  kegg2.id.GH.row2 <- gh.name[subject %in% kegg2.id.row2] %>% .[,3]
  FPKM.row1 <- all.FPKM[id %in% kegg2.id.GH.row1$query]
  FPKM.row2 <- all.FPKM[id %in% kegg2.id.GH.row2$query]
  FPKM.zhushi.row1 <- GH.zhushi[FPKM.row1,on = "id"] %>% .[,name := paste(id,注释,sep = "__")] %>% .[,!1:2] %>% setcolorder(c(7L,1:6L))
  FPKM.zhushi.row2 <- GH.zhushi[FPKM.row2,on = "id"] %>% .[,name := paste(id,注释,sep = "__")] %>% .[,!1:2] %>% setcolorder(c(7L,1:6L))
  FPKM <- list(FPKM.zhushi.row1,FPKM.zhushi.row2)
  return(FPKM)
}
jiaoji.kegg2 <- kegg2GH(data.kegg = data.kegg)
fwrite(jiaoji.kegg2[[1]],"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/jiaoji.kegg2.row1.csv")
fwrite(jiaoji.kegg2[[2]],"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/jiaoji.kegg2.row2.csv")
jiaoji.up.kegg2 <- kegg2GH(data.kegg = data.kegg)
fwrite(jiaoji.up.kegg2[[1]],"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/jiaoji.up.kegg2.row1.csv")
fwrite(jiaoji.up.kegg2[[2]],"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/jiaoji.up.kegg2.row2.csv")
jiaoji.down.kegg2 <- kegg2GH(data.kegg = data.kegg)
fwrite(jiaoji.down.kegg2[[1]],"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/jiaoji.down.kegg2.row1.csv")
fwrite(jiaoji.down.kegg2[[2]],"I:/张振楠/转录组9-8/富集分析/作图/FDR/作图数据/jiaoji.down.kegg2.row2.csv")

