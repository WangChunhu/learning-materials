# 缺失值处理
df <- data.frame(x = c(1:10, NA, 11:18, NA, 10), y = c(NA, 1:6, NA, 7:17, NA, 20))

## 1 显示NA----------------
require(VIM)
### 画出NA分布情况
VIM::matrixplot(df)

### 可视化缺失值
VIM::aggr(df)

### 输出带有缺失值的样本
NA.sample <- df[!complete.cases(df), ]

## 2 处理NA------------------
### 只保留无缺失值的样本
normal <- na.omit(df)

### 使用均值填补
which(is.na(df$x))
df.mean <- df
df.mean$x[is.na(df.mean$x)] <- mean(df.mean$x, na.rm = T)

### 使用中位数填补
which(is.na(df$y))
df.median <- df
df.median$y[is.na(df.median$y)] <- median(df.median$y, na.rm = T)

### 使用前面或后面的数进行填补, 前面或后面得有数据
require(zoo)
df.front <- df
df.front$x <- zoo::na.locf(df.front$x) # 前面
df.front$y <- zoo::na.locf(df.front$y, fromLast = T) # 后面







