# 读取txt文本，使用jiebaR分词，使用wordcloud2绘制词云
require(jiebaR)
require(wordcloud2)
require(htmlwidgets)
require(webshot)

# 读取文件
aim <- scan("D:/以前/R/wordcloud/wordcloud.txt",what = "",encoding = "UTF-8")
# 调用停用词表
word_stop.lib <- worker(stop_word = "D:/以前/R/wordcloud/stopwords-master/baidu_stopwords.txt")

# 分词
cut <- segment(code = aim,jiebar = word_stop.lib)

# 频率表
tab <- table(cut)
tab <- sort(tab,decreasing = T)

# 前100个 
tab_100 <- tab[1:100]

# 生成词云
p <- wordcloud2(tab_100, shape = 'star',size = .5)
htmlwidgets::saveWidget(p,"D:/以前/R/wordcloud/eg.html",selfcontained = F) #保存为网页格式
# webshot::install_phantomjs()
setwd("D:/以前/R/wordcloud/")
webshot::webshot("eg.html","eg.jpg",delay = 3,vwidth = 1000,vheight = 1000)

# shape默认为'circle'(圆形)，其他值有：
# 'cardioid'（苹果形或心形）
# 'star'（星形）
# 'diamond'（钻石）
# 'triangle-forward'（三角形）
# 'triangle'（三角形）
# 'pentagon'（五边形）


# 自定义图片形状
figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")



# 数据框形式
data("demoFreqC")
head(demoFreqC)
wordcloud2(demoFreqC, color = "random-light", backgroundColor = "grey")
letterCloud(demoFreq,word = "R", color = "random-light",backgroundColor = "gray",size = 0.3)

