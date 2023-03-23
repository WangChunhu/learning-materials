require(canvasXpress)
#香港源

# 读数据文件
y <- read.table("http://www.canvasxpress.org/data/cX-irist-dat.txt", header=TRUE, sep="\t", 
                quote="", row.names=1, fill=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
# 读取分组信息
z <- read.table("http://www.canvasxpress.org/data/cX-irist-var.txt", header=TRUE, sep= "\t", 
                quote="", row.names=1, fill=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
# 绘制三维散点图，主要参数为数据、分组、分组列、置信椭圆列、图表类型以及相关标签 
canvasXpress(data = y,
             varAnnot = z,
             colorBy = "Species",
             ellipseBy = "Species",
             graphType = "Scatter3D",
             title = "Iris Data Set",
             xAxis = list("Sepal.Length"),
             yAxis = list("Petal.Width"),
             zAxis = list("Petal.Length"))


 # 绘制矩阵散点图
canvasXpress(data = y,
             varAnnot = z,
             graphType = "Scatter2D",
             scatterPlotMatrix = TRUE,
             colorBy = "Species",
             showTransition = TRUE)

#
a <- data.frame(x = 1:10,y = 5:14,z = 10:19,by = rep(1:2,5))

canvasXpress(data = a,
             graphType = "Heatmap",
             title =  "第一个canvasXpress图")
 