hc =hclust(dist(mtcars))
#clus5 = cutree(hc, 5)

require(ggplot2)
require(ggtree)
data=fortify(hc)

ggtree(data, layout="circular", size=0.8, col="deepskyblue3") +    # 树体：树文件、树形、粗细、颜色
  geom_tiplab(size=3, color="purple4", hjust=-0.05) +   # 枝名：大小、颜色、高度
  geom_tippoint(size=1.5, color="deepskyblue3") +  # 端点：大小、颜色
  geom_nodepoint(color="orange", alpha=1/4, size=2) +   # 末节点：颜色、透明度、大小
  theme_tree2() +    # x轴标尺
  xlim(NA, max(data$x)*1.3)  # x轴宽度
