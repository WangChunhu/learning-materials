# 
# 学术前沿研讨

## 设置系统时间形式
Sys.setlocale(category = "LC_TIME", locale = "English")

# 1 数据类型 --------------------------------------------------------------------
## 1.1 数值型
a <-  1
a # print
str(a)
class(a)

a <- 1L
a # print
str(a)
class(a)

## 1.2 字符型
b <- "王春虎"
b <- "1"
b
str(b)
class(b)

## 1.3 bool型 TRUE FALSE
c <- F # T
c
str(c)
class(c)

## 1.4 向量
### 1.4.1 c 创建
b <- c("王", "春", "虎", 1, T)
b
str(b)
class(b)

b[1]

### 1.4.2 : (数值型)
a <- 1:10 # seq(1, 10) / c(1, 2, ..., 10)
a # 向量
str(a)

#### 取值
a[] # 单个/连续/离散

### 1.4.3 其他函数
c <- rep(x = 1:3, 3)
c
str(c)

d <- paste0("王", rep(x = 1:3, 3))
d
class(d)

## 1.5 数据框 data.frame创建
e <- openxlsx::read.xlsx(xlsxFile = "D:/长大/学术前沿研讨作业/学术前沿研讨.xlsx")
str(e)
class(e)

## 1.6 列表 list创建
f <- list(a, b)
str(f)
class(f)

# 2 包的使用 (F1)-------------------------------------------------------------------
## 2.1 下载
### 2.1.1 命令


### 2.1.2 Rstudio界面
## 2.2 加载 (加载进来才能使用)
### 2.2.1 常规 require / library
require(data.table)
require(stringr) # 在一个函数中，如果一个包不存在，执行到require将会继续执行
library(magrittr) # 在一个函数中，如果一个包不存在，执行到library将会停止执行

### 2.2.2 apply族
bao <- c("data.table", "stringr", "magrittr")
sapply(X = bao, FUN = require, character.on = T)

### 2.2.3 只加载所需函数, 降低内存使用
openxlsx::read.xlsx(xlsxFile = "D:/长大/学术前沿研讨作业/学术前沿研讨.xlsx")
a <- openxlsx::read.xlsx(xlsxFile = "D:/长大/学术前沿研讨作业/学术前沿研讨.xlsx") # = 也可以
openxlsx::read.xlsx(xlsxFile = "D:/长大/学术前沿研讨作业/学术前沿研讨.xlsx") -> b # = 不可以


# 3 data.table DT[i, j, by]--------------------------------------------------------------
## 引用地址1: https://www.heywhale.com/mw/project/59f92e8ec5f3f511952ea49a
## 引用地址2: https://zhuanlan.zhihu.com/p/26388833
## 引用地址3: https://zhuanlan.zhihu.com/p/26656483
## 引用地址4: https://zhuanlan.zhihu.com/p/26748434?from_voters_page=true

## 加载包
require(data.table)

## 3.1 创建
setDT() # 从其他类型转换, 会将原数据也转为data.table格式
setDF() # 完全转为data.frame格式
as.data.table() / data.table() # 从其他类型转换
fread() # 从本地读取


set.seed(45L) # 设置随机数种子
## 3.2 使用
DT <- data.table(V1 = c(1L,2L), # 格式
                 V2 = LETTERS[1:3],
                 V3 = round(rnorm(4),4), # rnorm: 给定平均值和标准偏差生成随机数
                 V4 = 1:12)
DT
str(DT)
class(DT)

# DT[i, j, by]
### 3.2.1 取行
DT[1]
DT[2:3]
DT[V4 >= 5 & V4 <= 10]
# DT[V4 %between% c(5, 10)]
DT[V2 %in% "A"]
DT[V1 %chin% 1] # data.table
DT[V3 %like% "3"]

### 3.2.2 取列
DT[, 1]
DT[, V1] # 返回向量, 通常用$
DT[, .(V1)] # 返回数据框 list()
DT[, 2:3]
DT[, .(V2, V3)] # list
DT[, V1:V4]
DT[, c("V2", "V3")] # data.frame写法
dt[c("A", "B", "D"), V1:V3, on = c("V2"), mult = "first"] # 选择每组的第一个（按照on分组)
#### with使用变量取列或使用data.frame写法或.SD ==> 方便写函数
#### 这里with参数其实相当于基础的with函数，当with=T时，就相当于基础函数使用了with函数，在这里面使用列名不需要data$，直接当成变量名就可以了，也就不用加引号，如果不想这样，想让使用的变量代表一个外面定义的字符串向量，则让with=F，相当于不使用with函数。
k <- "V1"
DT[, k] # 报错
data.frame(DT) %>% .[k]
DT[, k, with = F] 

# 构造数据框
dt2 <- expand.grid(gcm = 1:27, 
                   year = paste0("y", 1:100),
                   s = c("s1", "s2", "s3", "s4", "s5", "s6", "s7"), # 格式
                   v = c("v1", "v2", "v3"),
                   n = c("n1", "n2", "n3")) # rnorm: 给定平均值和标准偏差生成随机数)

### 3.2.3 同时取行列
DT[2:3, 2:3]
DT[V2 %in% c("A", "B"), .(V3, V4)] # 智能识别
DT[c("A", "B"), .(V3, V4), on = "V2"] # 使用on参数提取某一列是某一个值的行 ==> 方便写函数
# 只有在用on或者key时，nomatch, mult, roll, rollends, 等参数才会生效
# ### on的另一个用法merge, 可以反选 ,nomatch = 0/NULL(不显示未匹配的)
# dt2[dt1, on = "name1"] # 这里保留dt1的，dt2中没有的填上NA
# dt2[dt1, on = "name1", nomatch = 0] # 取交叉部分
# dt1[!dt2, on = "name1"] # 取dt2没有的部分
# dt1[dt2, on = .(name1 == friend)] # 当要融合的内容列名不相同时，用==匹配在一起
# dt1[dt2, on = "name1 == friend"] # 与上等价
# ##### 如果融合依据是数字，还可以用<= >=等连接，将满足这个不等式的匹配在一起
# dt1[dt2, .(name1, w = weight), on = "name1 == friend"] #在第二个参数的位置选择返回哪些列,同时修改列名
# dt1[dt2, on = "name1", mult = "first"] # 选择每组的第一个（按照on分组, 两个数据框匹配貌似不行，只能截取数据）
# dt1[dt2, on = "name1", mult = "last"] # 选择每组的最后一个
# # 加by=.EACHI和计算的参数还可以同时分组计算，先提取部分因子在计算

### 3.2.4 计算
DT[, sum(V3)]
DT[, .(sum(V3)), by = .(V2)] # by， keyby(会自动运行setkey，并且排序)
DT[, .(V3.mean = mean(V3), V3.sd = sd(V3)), by = V2]
DT[! V2 %in% "C", .(V3.mean = mean(V3), V3.sd = sd(V3)), by = .(V1, V2)]

### 3.2.5 .N
DT[, .N] # nrow
DT[, .N, by = V2]

DT[.N] # 返回最后一行, 不能分组，要分组用on, mult/ key, mult
DT[.N - 1, .(V3)] # 返回倒数第二行
dt[.(c("A", "B")), on = "V2", mult = "last"]

### 3.2.6 .I 表示（分组后）每一行在原数据框中是第几行
dt[, .(I = .I), by = V2]

### 3.2.7 .GRP 给分组标号, 如果不使用by参数，则为1。使用by，则是组的计数（第一组的值是1，第二组是2）
dt[, grp := .GRP, by = V2][]

### 3.2.8 添加或更新列 := (+-*/等等)
DT[, V5 := round(exp(V1), 2)][] # 显示结果 []
DT[, c("V1","V2") := list(round(exp(V1),2), LETTERS[1:12])][]

### 3.2.9 移除列, 直接修改DT
DT[, V5 := NULL][] %>% head() # 控制屏幕输出长度
DT[, c("V1","V2") := NULL][] # 不能传递变量
#### 移除, 不修改DT, 需赋值给其它变量或自己
DT[, !5]
DT <- DT[, !5]
DT[, !"V3"]
DT[, !c("V4", "V5")]

### 3.2.10 setkey / setindex(单种选择) 设置键值, 会自动按键值进行排序 
set.seed(45L) # 设置随机数种子
DT <- data.table(V1 = c(1L,2L),
                 V2 = LETTERS[1:3],
                 V3 = round(rnorm(4),4), # rnorm: 给定平均值和标准偏差生成随机数
                 V4 = 1:12)
#### 单个主键
setkey(DT, V2)
# haskey(DT)
key(DT) # 查看键值

DT["A"] # 筛选
DT[.("A")] # 推荐
DT[.(c("A", "B"))]
##### 以下几个参数需要设置key
DT[.(c("A", "B")), mult = "first"] # mult参数是用来控制i匹配到的哪一行的返回结果默认情况下会返回该分组的所有元素,last/all
DT[.(c("A", "D"))]
DT[.(c("A", "D")), nomatch = 0] # nomatch参数用于控制，当在i中没有到匹配数据的返回结果，默认为NA，也能设定为0。0意味着对于没有匹配到的行将不会返回。
DT[.(c("A", "C")), sum(V4)]
DT[.(c("A", "C")), sum(V4), by = .EACHI] # 按每一个已知i的子集分组

#### 多个主键
setkey(DT, V1, V2)
key(DT)
DT[.(2,c("A", "C"))]
DT[.(2,c("A", "C")), mult = "first"]

### 3.2.11 .SD 含了各个分组，除了by中的变量的所有元素。.SD只能在位置j中使用
DT[, print(.SD)] # DT[, .SD] , 只打印,无返回值
a <- DT[, .SD, by = V2] # 相当于按V2排序
DT[, .SD[c(1, .N)], by = V2] # 以V2为分组，选择每组的第一和最后一行
#### lapply + .SD, 同时计算多列/多功能
str(DT) # 运算时查看列是否为数值
##### 不是数值的话转为数值
DT$V1 <- as.character(DT$V1)
DT[, lapply(.SD, sum), by = V2] # 不支持字符类型
DT$V1 <- as.numeric(DT$V1)
DT[, lapply(.SD, sum), by = V2]
##### 多列同时转
DT[, lapply(.SD, as.numeric), .SDcols = c("V1", "V3", "V4"), by = V2] # V2一定要加,否则会删除
##### 多功能
DT[, lapply(.SD,function(x) {c(mean(x), sd(x), min(x), max(x))}), by = .(V1, V2)] # 自定义函数
DT[, .(mean.V3 = mean(V3), sd.V3 = sd(V3), min.V3 = min(V3), max.V3 = max(V3), mean.V4 = mean(V4), sd.V4 = sd(V4), min.V4 = min(V4), max.V4 = max(V4)), by = .(V1, V2)]

#### 选择列进行运算
DT[, lapply(.SD, sum), .SDcols = c("V3", "V4"), by = V2]

### 3.2.12 串联操作
dt <- DT[, .(V4.Sum = sum(V4)), by = V1]
dt
dt1 <- dt[V4.Sum > 36]
dt1

dt1 <- DT[, .(V4.Sum = sum(V4)), by = V1][V4.Sum > 36 ] # $选择
dt1

dt1 <- DT[, .(V4.Sum = sum(V4)), by = V1] %>% .[V4.Sum > 36 ] # 更倾向于这种
dt1

### 3.2.13 set家族, 直接修改原数据
DT
#### 3.2.13.1 set
set(x = DT, i = 1L, j = 2L, value = "B")[]

##### 交互式编辑
# fix修改之后，改变的是原数据框
# edit修改后原数据框未变，需要将修改后的结果赋值给一个新的变量
fix(DT)
DT

d <- data.table()
d
fix(d)
d

#### 3.2.13.2 setname 修改列名
setnames(DT, "V1", "v1")[]
setnames(DT, c("V2", "V3", "V4"), c("v2", "v3", "v4"))[]

#### 3.2.13.3 setcolorder, 修改列的顺序
setcolorder(DT, c("v2", "v1", "v4", "v3"))[]

#### 3.2.13.4 setorder, 排序
DT
setorder(DT, v2)[]
setorder(DT, -v2)[]
setorder(DT, v2, -v1)[]

#### 3.2.13.5 setDT和setDF
# 不占内存, 修改变量本身
DT <- as.data.table(DF) # 经过了两次复制, 先把DF转为data.table格式, 再赋值给DT

### 3.2.14 copy 深度拷贝一个data.table
DT
DT.assgn <- DT
DT.copy <- copy(DT) 
DT; DT.assgn; DT.copy
DT[, V5 := 12:1]
DT; DT.assgn; DT.copy

### 3.2.15 tstrsplit
name <- 1:3
dates <- c("2016-3-4", "2016-3-14", "2016-3-24")
nd <- data.table(name, dates)
strsplit(dates, "-") # 和stringr中的分裂函数一致
tstrsplit(dates, "-") # 好像把strsplit得到的结果转置了一样, 返回值为列表
nd[, c("year", "month", "day") := tstrsplit(dates, "-")][] # 实现拆分

# 3.2.16 集合操作函数, 数据框列名必须相同
# 增加了all参数，控制重复值。基础函数只能返回去重之后的结果
# 函数变化：union intersect setdiff setequal 前面都加了一个f
# 基础函数作用于两个向量，data.table中函数作用于两个data.table数据框，而且列名需要相同
x <- data.table(a = 1:7, b = letters[1:7])
y <- data.table(a = 5:11, b = letters[5:11])
x
y
fintersect(x, y)            # 返回相交部分并去重
fintersect(x, y, all=TRUE)  # 相交，保留重复值
fsetdiff(x, y)              # x中有y中没有的，去重
fsetdiff(x, y, all=TRUE)    # 保留重复值
funion(x, y)                # 并集，去重
funion(x, y, all=TRUE)      # 保留重复值
fsetequal(x, y)             # 返回一个F，二者不完全相等

# rank 返回排名
# frank比rank函数速度更快，而且增加参数ties.method参数的一种取值”dense”，即当有两个值相等并列第二时，让二者都为2，之后的数排名不是第4，而是3，这样结果数值不会发生跳跃

x = c(2, 1, 4, 5, 3, NA, 4)
frank(x) # 自动将NA当成最大的了
frank(x, na.last=F) # 自动将NA当成最小的
frank(x, na.last="keep") # NA仍然是NA
frank(x, ties.method = "min")
frank(x, ties.method = "dense")

DT = data.table(x, y=c(1, 1, 1, 0, NA, 0, 2))
frank(DT, cols="x")

# 滞后
# shift函数,参数如下
# n控制变换阶数
# fill控制填充内容
# type取"lag"或者"lead"，看去除后面的值向后靠（前面添NA），还是去除前面的值向前靠（后面添NA）
y <- x <- 1:5
xy <- data.table(x,y)
shift(x, n=1, fill=NA, type="lag")
shift(x, n=1:2, fill=0, type="lag")
xy[,(c("a","b")):=shift(.SD,1,0,"lead")][] # 添加两列
xy[,shift(.SD,1,0,"lead",give.names = T)][] # 自动生成名字
shift(xy, n=1, fill=0, type="lag", give.names=T) # 生成list

# 上下合并数据框
# 使用rbindlist函数，先将数据框转化为list再进行合并
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(A=4:5,B=letters[4:5])
DT3 = data.table(B=letters[4:5],A=4:5)
DT4 = data.table(B=letters[4:5],C=factor(1:2))
l1 = list(DT1,DT2)
l2 = list(DT1,DT3)
l3 = list(DT1,DT4)
rbindlist(l1)
rbindlist(l1,idcol=T) # 多出一列，对数据框分组（来自不同数据框）
names(l1) <- c("DT1", "DT2")
# setattr(l1, 'names', c("DT1", "DT2"))
rbindlist(l1,idcol="DT")
rbindlist(l2) # 不同列名直接合并
rbindlist(l2,use.names=T) # 将相同列名的合并在一起
rbindlist(l3) # 不同列名直接合并
rbindlist(l3,fill=T) # 选择相同列名合并，不匹配的填入NA

# options设置
d <- data.table(a=1:200, b=2:201)
d # 200行数据自动只输出前5行和后5行
op <- options(datatable.print.topn=10) # 设置打出前10行和后10行
d # 打出前10行和后10行
options(op) # 恢复默认值5

f <- data.table(a=1:50, b=2:51)
f # 50行全打了出来
op <- options(datatable.print.nrows = 30) # 设置行数超过30行时就省略打出
f # 只打出前5行和后5行
options(op) # 恢复默认值100

# alt + up/down 将光标所在行向上/下移动
# shift + alt + up/down 将光标所在行复制粘贴在上面/下面一行
# alt + left/right 光标瞬间移动到行首/行尾
# alt + shift + left/right 选择本行光标左/右侧的所有内容















