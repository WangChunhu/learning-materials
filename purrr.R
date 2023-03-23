require(tidyverse)

# map()：依次应用一元函数到一个序列的每个元素上，基本等同 lapply()
# map2()：依次应用二元函数到两个序列的每对元素上
# pmap()：应用多元函数到多个序列的每组元素上，可以实现对数据框逐行迭代
# map 系列默认返回列表型，可根据想要的返回类型添加后缀：_int, _dbl, _lgl, _chr, _df, 甚至可以接着对返回的数据框df做行/列合并：_dfr, _dfc
# 如果只想要函数依次作用的过程，而不需要返回结果，改用 walk 系列即可
# 所应用的函数，有 purrr公式风格简写（匿名函数），支持一元，二元，多元函数

# map_chr(.x, .f): 返回字符型向量
# map_lgl(.x, .f): 返回逻辑型向量
# map_dbl(.x, .f): 返回实数型向量
# map_int(.x, .f): 返回整数型向量
# map_dfr(.x, .f): 返回数据框列表，再 bind_rows 按行合并为一个数据框
# map_dfc(.x, .f): 返回数据框列表，再 bind_cols 按列合并为一个数据框

# purrr 风格公式（匿名函数）
# purrr 包实现迭代循环是用 map(x, f)，f 是要应用的函数，想用匿名函数来写它，它要应用在序列 x 上，就是要和序列 x 相关联，那么就限定用序列参数名关联好了，即将该序列参数名 作为匿名函数的参数使用：
# 一元函数：序列参数是 .x
# 比如，f(x) = x^2 + 1, 其 purrr 风格公式（匿名函数）就写为：~ .x ^ 2 + 1
# 二元函数：序列参数是 .x, .y
# 比如，f(x, y) = x^2 - 3 y, 其 purrr 风格公式（匿名函数）就写为：~ .x ^ 2 - 3 * .y
# 多元函数：序列参数是 ..1, ..2, ..3, 等
# 比如，f(x, y, z) = ln(x + y + z), 其 purrr 风格公式（匿名函数）就写为：~ log(..1 + ..2 + ..3)
# 注：所有序列参数，可以用 ... 代替，比如，sum(..1, ..2, ..3) 同 sum(...)

# map
## 例1 计算 iris 前4列，每列的均值, 即依次将 mean() 函数，应用到第1列，第2列，...
df <- iris[, 1:4]; head(df)
## 说明：df 是数据框（特殊的列表），作为序列其元素依次是：df[[1]], df[[2]], ...... 所以，map(df, mean) 相当于依次计算：mean(df[[1]]), mean(df[[2]]), ......
map(.x = df, .f = mean, na.rm = T) # 默认返回列表
map_dbl(.x = df, .f = mean, na.rm = T)
data <- map_df(.x = df, .f = mean, na.rm = T)
map_dfc(.x = df, .f = mean, na.rm = T)
map_dfr(.x = df, .f = mean, na.rm = T)
map_dbl(.x = df, .f = ~ mean(.x, na.rm = T))
map_dbl(.x = df, .f = ~ mean(..1, na.rm = T))

# iris中，因子列应用as.character，其他列应用as.integer
str(iris)
map_if(.x = iris, .p = is.factor, .f = as.character, .else = as.integer)
str(iris)
# 指定位置处应用is.numeric函数
iris %>% map_at(c(4, 5), is.numeric)
# 指定name应用toupper
iris %>% map_at("Species", toupper)

libs <- c("data.table", "stringr", "magrittr")
# sapply(libs, require, character.on = T)
walk(.x = libs, .f = require, character.on = T)

# map2
map2_dbl(.x = 1:10, .y = 11:20, .f = sum)
map2_dbl(.x = 1:10, .y = 11:20, .f = sum, na.rm = T)
map2_dbl(.x = 1:10, .y = 11:20, .f = ~ sum(.x, .y, na.rm = T))
map2_dbl(.x = 1:10, .y = 11:20, .f = ~ sum(..1, ..2, na.rm = T))
map2_chr(.x = 1:10, .y = 11:20, .f = sum)
map2_int(.x = 1:10, .y = 11:20, .f = sum)
map2_raw(.x = 1, .y = 11, .f = sum)

height = c(1.58, 1.76, 1.64)
weight = c(52, 73, 68)
map2_dbl(.x = height, .y = weight, .f = ~ .y / .x^2)

# pmap: 多元迭代不就是依次在数据框的每一行上迭代吗！！
df <- tibble(
  n = c(1,3,5),
  mean = c(5,10,-3),
  sd = c(1,5,10)
); df

df.c <- map_if(.x = df, .p = is.numeric, .f = as.character, .else = NULL)
map_at(.x = df, .at = c(1, 1), .f = ~ return(0))

set.seed(123)
pmap(.l = df, .f = rnorm) # 特别注意，这里 df 中的列名，必须与 rnorm() 函数的参数名相同（列序随便）

names(df) = c("n", "m", "s"); df
set.seed(123)
## 若要避免这种局限，可以使用 purrr 风格公式写法：
pmap(.l = df, .f = ~ rnorm(n = ..1, mean = ..2, sd = ..3))

# reduce 
## 类级操作，即函数 reduce可先对序列前两个元素应用函数，再对结果与第3个元素应用函数，再对结果与第4个元素应用函数，……直到所有的元都被“reduced”
sum(1:100)
reduce(.x = 1:100, .f = sum)

# accumulate
## 函数 accumulate() 与 reduce() 作用方式相同，不同之处是：reduce() 只保留最终的结果，而 accumulate() 会保留所有中间结果。
accumulate(.x = 1:100, .f = sum)
## 一个小的应用场景，生成一系列的回归公式
vars <- str_c("x", 2:5); vars
accumulate(.x = vars, .f = ~ str_c(.x, .y , sep = " + "), .init = "y ~ x1")

# 滑动平均
stats::filter(x = 1:10, filter = rep(1, 3)) # 相邻三个
stats::filter(x = 1:10, filter = rep(1, 4), sides = 1)  # 相邻四个
stats::filter(x = 1:10, filter = rep(1, 3), sides = 1, circular = TRUE)




