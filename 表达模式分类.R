#表达模式分析

express.pattern <- function(data,cluster.num,linePlot.writePath,x.axis.title,y.axis.title,width = 8,height = 8){
  bao <- c("data.table","magrittr","openxlsx","stringr","pheatmap","ggplot2")
  sapply(bao, require , character.on = T)
  data <- data.frame(data)
  name <- names(data)[1]
  names(data)[names(data) == name] <- "id"
  id <- data$id
  rownames(data) <- data$id
  data <- data[-1]
  clus_data <- pheatmap(mat = data,scale = "row",clustering_method = "complete",clustering_distance_rows = "euclidean",clustering_distance_cols = "euclidean",angle_col = 45,show_rownames = T,display_numbers = F,cluster_cols = F,cutree_rows = cluster.num)
  row_clus <- cutree(clus_data$tree_row,k = cluster.num) %>% data.table()
  group <- row_clus[,.(num = .N),by = .(.)]
  data <- data.table(data) %>% .[,id := id]
  data[,group := row_clus]
  data.v <- melt.data.table(data = data,id.vars = c("group","id")) %>% setorder(group,id) %>% .[,group := paste0("cluster ",group)]
  mean <- data.v[,.(mean = mean(value)),by = .(group,variable)] 
  
  pdf(linePlot.writePath,width = width,height = height)
  p <- ggplot(data = data.v,mapping = aes(variable,value,group = id))+
    geom_line(color = "grey",alpha = 1)+
    facet_wrap(group ~ .)+
    theme_classic()+
    geom_line(data = mean,mapping = aes(variable,mean,group = group),size = 1,color = "#0000FFFF")+
    geom_point(data = mean,mapping = aes(variable,mean,group = group),size = 2,color = "#0000FFFF")+
    labs(x = x.axis.title,y = y.axis.title)+ #x = "Term of Leaf",y = "centered log2(fpkm+1)"
    theme(text = element_text(face = "bold"),axis.text.x = element_text(size = 10,angle = 45,hjust = 1,face = "bold"),axis.text.y = element_text(size = 10,face = "bold"))
  print(p)
  dev.off()
}

setwd("I:/高老师/PIF-20211231/组织诱导表达")
require(ggplot2)
exp <- openxlsx::read.xlsx("PIF.GH.expresssion.TBtools.xlsx",sheet = 2,cols = 1:5)
express.pattern(data = exp,
                cluster.num = 6,
                linePlot.writePath = "Cold分类折线图.pdf",
                x.axis.title = "Term of Cold",
                y.axis.title = "centered log2(fpkm+1)"
)

