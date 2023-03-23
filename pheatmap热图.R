require(pheatmap)
require(data.table)
require(magrittr)
require(openxlsx)
require(stringr)

data <- read.xlsx("I:/许福春/20220214单细胞/new/20220317公司拟时/GDR20070293_sup_16_result/2.exp_heatmap/c01346810.diff.tf.exp.xlsx",sheet = 1)


pheatmap.ST <- function(data,data.plot.col = 3:10,cell.col = c("navy","white","firebrick3"),group_row,group_col,cluster_rows = F,cluster_cols = F,scale = "row",cutree_rows,cutree_cols,show_rownames = T,display_numbers = F,border_color = F,fontsize = 10,fontsize_col = fontsize,fontsize_row = fontsize,angle_col = 45){
  data.heat <- data[data.plot.col]
  row.names(data.heat) <- data.heat[,1]
  data.heat <- data.heat[-1]
  #行分类
  anno_row <- data[group_row]
  rownames(anno_row) <- rownames(data.heat)
  #列分类
  anno_col <- data[group_col] %>% na.omit()
  row.names(anno_col) <- names(data.heat)
  #绘制热图
  pheatmap(mat = data.heat,color = colorRampPalette(cell.col)(100),cluster_rows = cluster_rows,cluster_cols = cluster_cols,scale = scale,show_rownames = show_rownames,display_numbers = display_numbers,annotation_row = anno_row,annotation_col = anno_col,border_color = border_color,fontsize = fontsize,fontsize_col = fontsize_col,fontsize_row = fontsize_row,angle_col = angle_col,na_col = "gray",cutree_rows = cutree_rows,cutree_cols = cutree_cols)
}

#例子
pheatmap.ST(data = data,data.plot.col = c(4:11),group_row = "1pathway",group_col = "group_col",show_rownames = F,cluster_rows = T,cutree_rows = 3,cluster_cols = T,cutree_cols = 2)




pheatmap.ST <- function(data,data.plot.col = 3:10,cell.col = c("navy","white","firebrick3"),group_row,cluster_rows = F,cluster_cols = F,scale = "row",cutree_rows,cutree_cols,show_rownames = T,display_numbers = F,border_color = F,fontsize = 10,fontsize_col = fontsize,fontsize_row = fontsize,angle_col = 45){
  data.heat <- data[data.plot.col]
  row.names(data.heat) <- data.heat[,1]
  data.heat <- data.heat[-1]
  #行分类
  anno_row <- data[group_row]
  rownames(anno_row) <- rownames(data.heat)
  # #列分类
  # anno_col <- data[group_col] %>% na.omit()
  # row.names(anno_col) <- names(data.heat)
  #绘制热图
  pheatmap(mat = data.heat,color = colorRampPalette(cell.col)(100),cluster_rows = cluster_rows,cluster_cols = cluster_cols,scale = scale,show_rownames = show_rownames,display_numbers = display_numbers,annotation_row = anno_row,border_color = border_color,fontsize = fontsize,fontsize_col = fontsize_col,fontsize_row = fontsize_row,angle_col = angle_col,na_col = "gray",cutree_rows = cutree_rows,cutree_cols = cutree_cols)
}

#例子
pdf("I:/许福春/20220214单细胞/new/20220317公司拟时/GDR20070293_sup_16_result/2.exp_heatmap/wang-c01346810.diff.tf-pathway.exp.heatmap-换颜色.pdf",family =  "serif")
pheatmap.ST(data = data,data.plot.col = c(4:11),cell.col = c("DarkOrchid3","grey11","yellow1"),group_row = "1pathway",show_rownames = F)
pheatmap.ST(data = data,data.plot.col = c(4:11),cell.col = c("DeepSkyBlue3","Burlywood2","IndianRed3"),group_row = "1pathway",show_rownames = F)
dev.off()




