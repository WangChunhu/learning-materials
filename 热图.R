library(ggplot2)
require(pheatmap)
data <- openxlsx::read.xlsx("I:/马佳羿/转录组/eggnog.pval图数据/WT_M.dff.fpkm.zhushi.at.zhushi.热图.xlsx")
data.plot <- data[,2:5]
rownames(data.plot) <- data$注释
p <- log2(data.plot)
p[p == -Inf | p == Inf] <- 0
p[is.na(p)] <- 0
pheatmap(log2(data.plot),cluster_cols = F)



#绘制行聚类树
phr <- hclust(dist(p)) %>% ggtree(layout="rectangular", branch.length="none")
# #绘制列聚类树
# phc <- hclust(dist(t(p))) %>% ggtree() + layout_dendrogram()
p$mtxars <- rownames(p)
#宽表转长表
p1 <- gather(p, 1:4, key="condition", value='expr')
#绘制热图
pp <- ggplot(p1,aes(condition,mtxars,fill=expr)) + geom_tile()+
  theme_minimal()+
  scale_fill_viridis_c() +
  scale_y_discrete(position="right")+
  xlab(NULL) + ylab(NULL)
#利用aplot包将聚类树与热图拼接
pp %>% insert_left(phr, width=.1) #%>% insert_top(phc, height=.1) 
ggsave("I:/马佳羿/转录组/eggnog.pval图数据/test.pdf",width = 30,height = 5000,limitsize = F)

