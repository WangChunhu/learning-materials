library(devtools)
install_git("https://github.com/Lchiffon/REmap") #REmap调用百度地图API,函数主要有：remap()，remapB()、remapC()、remapH()四个,目前要github网站上下载
library(REmap)

set.seed(125)
out = remap(demoC,title = "REmap",subtitle = "theme:Dark")
plot(out)







