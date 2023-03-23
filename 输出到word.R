echo=FALSE  #最终的文档中不会显示代码

results="hide"   #隐藏结果，显示图像

include=FALSE   #隐藏代码和运行的输出（写报告时可使用include=FALSE来隐藏所有的代码，从而突出图像。）

fig.show="hide"    #隐藏图像

#{r fig.width=8,fig.height=6}   #通过fig.width和fig.height来设置宽和高

fig.path='Figs/'   #把图片保存在Figs子文件夹中,默认情况下图片不会被保存

results="hide"则会隐藏结果显示图像

eval=FALSE   #显示代码而不显示运行结果

warning=FALSE  & message=FALSE   #最终文档中不会显示R软件任何的提示信息

include=TRUE    #可以只输出图像。


result=FALSE   #可以隐藏文本输出
fig.show='hide'   #隐藏图形输出
error=TRUE   #代码中出现错误仍然可以生成报告
error=FALSE   #即使只有一个错误，文档也会失败
#setup这个代码段名称具有特殊含义。当处于笔记本模式时，名称为setup的代码段会在任何其他代码运行前自动运行一次

#网页
<网页>


#格式化文本
（1）标题
# 一级标题

段落内容
注意标题和内容之间要有空行

## 二级标题
（2）=和-标记
在标题下面加上任意个=表示一级标题
在标题下面加上任意个-表示一级标题
(3)有序列表和无序列表
无序列表使用-、+、*作为列表标记
有序列表使用数字和英文句点标记
（4）引用>
  链接 : [Title](URL)
  加粗 : **Bold**
  斜体字 : *Italics*
  删除线 : ~~text~~
  高亮 : ==text==
  段落 : 段落之间空一行
换行符 : 一行结束时输入两个空格
列表 : * 添加星号成为一个新的列表项。
引用 : > 引用内容
内嵌代码 : ??`alert('Hello World');`
画水平线 (HR) : --------
分割线：***或---（三个及以上）

#行间代码   #嵌入R代码到文本当中，在代码的两侧用点’来包围

&emsp;&emsp;  #首行空两个字
#Rmarkdown的编译会忽视回车键。因此要分段落需要一个段落的行末连续输入两个空格即可

kable函数的参数：
kable(x, format, digits = getOption("digits"), row.names = NA, col.names = NA, align,caption = NULL, format.args = list(), escape = TRUE, ...)
参数的意义如下：

x             通常为要显示的matrix或者data frame.
format    参数为字符串，可设定为latex, html, markdown, pandoc, and rst;如果不指定的话，函数会自动选择合适的format，也可通过在全局设定中设定knitr.table.format.
digits    数字显示的位数设定
row.names 逻辑性变量，是否显示行名，默认是显示
col.names 字符向量，用于列名的显示
align 对齐方式'l' (left), 'c' (center) and/or 'r' (right).
caption   表格名称
format.args   （这个参数包括escape参数作者不甚理解，待后续理解了后更新。）
escape
...   其他参数

其中值得注意的一点是缺失值默认是显示NA。如果要更改的话需要设定options(knitr.kable.NA = '')。
另外一个有用的选择是用kableExtra中的kable_styling函数，其作用是输出的表格不会占据整个页面的宽度。

#图片
![图片alt](图片地址 ''图片title'')

图片alt就是显示在图片下面的文字，相当于对图片内容的解释。
图片title是图片的标题，当鼠标移到图片上时显示的内容。title可加可不加

#超链接
[超链接名](超链接地址 "超链接title")
title可加可不加

#列表
无序列表用 - + * 任何一种都可以

有序列表
1.列表内容
2.列表内容
3.列表内容

注意：序号跟内容之间要有空格（与下一级有三个空格）

#表格
表头|表头|表头
---|:--:|---:
  内容|内容|内容
内容|内容|内容

第二行分割表头和内容。
- 有一个就行，为了对齐，多加了几个
文字默认居左
-两边加：表示文字居中
-右边加：表示文字居右

流程图方向有下面几个值

TB 从上到下
BT 从下到上
RL 从右到左
LR 从左到右
TD 同TB

