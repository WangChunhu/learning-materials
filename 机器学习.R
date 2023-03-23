# 1 GLM 广义线性模型(扩展了线性模型的框架, 包含非正态变量的分析)----------
## 结果变量是类别型: [Logistic回归模型] 二分类(是/否;有效/无效) / 多分类(差/良好/优秀) ==> 非正态分布
##           计数型: [泊松回归模型] 如一年内哮喘发生的次数等, 为非负有限值, 且均值和方差通常相关(正态分布间相互独立)

## 连接函数
summary() # 展示拟合模型细节
coefficients() / coef() # 列出拟合模型的参数 (截距和斜率)
confint() # 给出模型参数的置信区间
residuals() # 残差值
anova() # 生成两个拟合模型的方差分析表
plot() # 生成评价拟合模型的诊断图
predict() # 使用拟合模型对新数据进行预测
deviance() # 偏差
df.residual() # 残差自由度


## 1.1 二值Logistic回归[glm: family = binomial(link = "logit")]----------
# install.packages(c("AER", "robust", "qcc"))
glm(formula = y ~ x1 + x2 + x3, 
    family = binomial(link = "logit"),
    data = mydata)

require(AER)
require(data.table)

data("Affairs", package = "AER")
summary(Affairs)
str(Affairs)
?Affairs

Affairs <- setDT(Affairs)
table(Affairs$affairs) # 出轨次数

# 原Affairs$affairs为次数, 应该使用泊松分布, 但是我们要逻辑回归
Affairs[affairs > 0, "yes"] <- 1
Affairs[!affairs > 0, "yes"] <- 0
Affairs$yes <- factor(x = Affairs$yes, levels = c(0, 1), labels = c("no", "yes"))
str(Affairs)
table(Affairs$yes)

# 模型拟合
fit <- glm(formula = yes ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, 
           family = binomial(link = "logit"), 
           data = Affairs)
fit
summary(fit) # 挑出相关的变量

# 再次拟合
fit.cor <- glm(formula = yes ~ age + yearsmarried + religiousness + rating, 
               family = binomial(link = "logit"), 
               data = Affairs)
fit.cor
summary(fit.cor) # AIC越小越好

# 比较模型
anova(fit, fit.cor, test = "Chisq") # 卡方检验

# 稳健逻辑回归 overdispersion
fit.od <- glm(formula = yes ~ age + yearsmarried + religiousness + rating, 
              family = quasibinomial(link = "logit"), 
              data = Affairs)
fit.od
summary(fit.od)

# 卡方检验(类资料统计推断中应用): 统计样本的实际观测值与理论推断值之间的偏离程度，实际观测值与理论推断值之间的偏离程度就决定卡方值的大小，如果卡方值越大，二者偏差程度越大
pchisq(summary(fit.od)$dispersion * fit.cor$df.residual, fit.cor$df.residual, lower  = F) # p值 0.340122 > 0.05 没有过度离失, 可以使用fit.cor

# 获取回归系数
coef(fit.cor) # coefficients(fit.cor)
exp(coef(fit.cor)) # OR值

# 预测1
test <- data.table(rating = c(1, 2, 4, 3, 5),
                   age = mean(Affairs$age),
                   yearsmarried = mean(Affairs$yearsmarried),
                   religiousness = mean(Affairs$religiousness)
                   )
test

test$prob <- predict(object = fit.cor, newdata = test, type = "response")
test

# 预测2
test <- data.table(rating = mean(Affairs$rating),
                   age = seq(17, 57, 10),
                   yearsmarried = mean(Affairs$yearsmarried),
                   religiousness = mean(Affairs$religiousness)
)
test

test$prob <- predict(object = fit.cor, newdata = test, type = "response")
test
# ----------

## 1.2 泊松回归(计数变量)[glm: family = poisson(link = "log")]----------
glm(formula = y ~ x1 + x2 + x3, 
    family = poisson(link = "log"),
    data = mydata)

### 1
require(AER)
require(data.table)

data("Affairs", package = "AER")
table(Affairs$affairs)

fit <- glm(formula = affairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, 
           family = poisson(link = "log"), 
           data = Affairs)
fit
summary(fit)

fit.cor <- glm(formula = affairs ~ age + yearsmarried + religiousness + occupation + rating, 
           family = poisson(link = "log"), 
           data = Affairs)
fit.cor
summary(fit.cor)

# 比较模型
anova(fit, fit.cor, test = "Chisq")

### 2
# install.packages("robust")
require(robust)
require(data.table)
require(magrittr)

data("breslow.dat", package = "robust")
summary(breslow.dat)
str(breslow.dat)
?breslow.dat

bre <- setDT(breslow.dat) %>% .[, c(6:8, 10)]
rm(breslow.dat)
gc()
str(bre)
table(bre$sumY)
names(bre)

fit <- glm(formula = sumY ~ Base + Age + Trt, 
           family = poisson(link = "log"), 
           data = bre)
fit
summary(fit)

coef(fit)
exp(coef(fit))

## 稳健泊松回归
fit.od <- glm(formula = sumY ~ Base + Age + Trt, 
           family = quasipoisson(link = "log"), 
           data = bre)
fit.od
summary(fit.od)

pchisq(summary(fit.od)$dispersion * fit$df.residual, fit$df.residual, lower  = F) # 5.844949e-102 < 0.05, 过度离失, 所以使用fit.od
require(qcc)
deviance(fit) / df.residual(fit) # 10.1717
qcc.overdispersion.test(bre$sumY, type = "poisson") # p-value = 0, 有过度离失

fit.od.od <- glm(formula = sumY ~ Base, 
                 family = quasipoisson(link = "log"), 
                 data = bre)
fit.od.od
summary(fit.od.od)
# ----------

## 1.3 无序多分类逻辑回归[nnet::multinom ](多元逻辑回归模型), 多个二元逻辑回归----------
libs <- c("nnet", "foreign", "ggplot2", "reshape2")
sapply(libs, require, character.on =T)
  
data("Affairs", package = "AER")
summary(Affairs)
str(Affairs)
?Affairs

Affairs <- setDT(Affairs)
table(Affairs$affairs) # 出轨次数

### 创建多元逻辑回归模型数据集
Affairs[affairs == 0, degree := "无"]
Affairs[affairs > 0 & affairs <= 3, degree := "少"]
Affairs[affairs > 3, degree := "多"]
Affairs$degree <- factor(x = Affairs$degree, levels = c("无", "少", "多"))
str(Affairs)
table(Affairs$degree)
with(data = Affairs, table(gender, degree)) # 画表格
### 设定参照变量因子
Affairs$degree.ck <- relevel(x = Affairs$degree, ref = "无")
head(Affairs)

fit <- nnet::multinom(formula = degree.ck ~ age + yearsmarried,
                      data = Affairs)
fit
summary(fit)

exp(coef(fit))
tail(pp <- fitted(fit), 20) # 模型预测数据集本身的结果

### 计算P值
z <- coef(fit) / summary(fit)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

### 预测
test <- data.table(age = seq(17, 57, 10), yearsmarried = mean(Affairs$yearsmarried))
test
predict(object = fit, newdata = test, type = "probs")

test <- data.table(age = mean(Affairs$age), yearsmarried = Affairs$yearsmarried) %>% unique() %>% setorder(yearsmarried)
test
predict <- predict(object = fit, newdata = test, type = "probs", se = T)
res <- cbind(test, predict)
res
# ----------

## 1.4 有序(等级)多分类逻辑回归(多元逻辑回归模型)
# install.packages("MASS", dependencies = T)----------
require(MASS)

data("Affairs", package = "AER")
summary(Affairs)
str(Affairs)
?Affairs

Affairs <- setDT(Affairs)
table(Affairs$affairs) # 出轨次数

### 创建多元逻辑回归模型数据集
Affairs[affairs == 0, degree := "低"]
Affairs[affairs > 0 & affairs <= 3, degree := "中"]
Affairs[affairs > 3, degree := "高"]
Affairs$degree <- factor(x = Affairs$degree, levels = c("低", "中", "高"))
str(Affairs)
table(Affairs$degree)
with(data = Affairs, table(gender, degree)) # 画表格

### 拟合模型
fit <- MASS::polr(formula = degree ~ gender + age + yearsmarried + children + education,
                  data = Affairs, 
                  Hess = T)
fit
summary(fit)
confint(fit) # 置信区间
exp(coef(fit)) # OR值

### 计算p值
ctable <- coef(summary(fit))
ctable

p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
p
ctable <-cbind(ctable, "p value" = p)
ctable
# ----------

## 1.5 逻辑回归及判别分析-----------
libs <- c("MASS", "data.table", "magrittr")
sapply(libs, require, character.on =T)

### 导入并预处理数据
data("biopsy", package = "MASS")
biopsy <- setDT(biopsy) %>% .[, ID := NULL] %>% na.omit()
names(biopsy) <- c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
str(biopsy)

### 设置训练集和测试集
set.seed(123)
ind <- sample(x = 2, size = nrow(biopsy), replace = T, prob = c(0.7, 0.3))

train <- biopsy[ind == 1]
test <- biopsy[ind == 2]

### 均衡性检验
table(train$class)
table(train$class)[1] / table(train$class)[2]
table(test$class)
table(test$class)[1] / table(test$class)[2]


### 建模
fit <- glm(formula = class ~ .,
           family = binomial(link = "logit"),
           data = train)
summary(fit)

fit.cor <- glm(formula = class ~ thick + nucl,
           family = binomial(link = "logit"),
           data = train)
summary(fit.cor)

confint(fit.cor) # 回归系数的置信区间
exp(coef(fit.cor)) # OR值, 优势比(>1, y会增加)

### 共线性检验
require(car)
car::vif(fit)
car::vif(fit.cor) # 阈值:5

### 预测
#### 训练集的预测概率
predict.train <- predict(object = fit.cor, newdata = train, type = "response") # response:概率

 
## -----------------


# 2. 树
## 2.1 回归树(rpart):在划分过程中最大程度的减少RSS (RSS:残差平方和[residual sum of squares])-------
# BiocManager::install("ElemStatLearn")

libs <- c("rpart", "data.table")
sapply(libs, require, character.on = T)

### 载入数据
data("Affairs", package = "AER")
Affairs <- setDT(Affairs)
str(Affairs)
table(Affairs$affairs)



##------------

## 2.2 分类树:决定于误差率--------


##------------

## 2.3 随机森林-------
### 2.3.1 随机森林回归
libs <- c("randomForest", "data.table", "magrittr")
sapply(libs, require, character.on = T)

### 导入并预处理数据
data("biopsy", package = "MASS")
biopsy <- setDT(biopsy) %>% .[, ID := NULL] %>% na.omit()
names(biopsy) <- c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
str(biopsy)

### 设置训练集和测试集
set.seed(123)
sample <- sample(x = 2, size = nrow(biopsy), replace = T, prob = c(0.7, 0.3))

train <- biopsy[sample == 1]
test <- biopsy[sample == 2]

### 均衡性检验
table(train$class)
table(train$class)[1] / table(train$class)[2]
table(test$class)
table(test$class)[1] / table(test$class)[2]

### 初始拟合模型
set.seed(234)
fit <- randomForest(formula = thick ~ ., data = train[, !10], ntree = 1000)
fit
summary(fit)

### 获取误差最小时的树个数:286
plot(fit)
which.min(fit$mse)

### 选择误差最小时的树个数重新拟合模型
set.seed(234)
fit.min <- randomForest(formula = thick ~ ., data = train[, !10], ntree = 286)
fit.min
summary(fit.min)

### 变量重要性图
varImpPlot(x = fit.min, scale = T, main = "huhu")
importance(x = fit.min)

### 预测
predict <- predict(object = fit.min, newdata = test, type = "response")
predict




























