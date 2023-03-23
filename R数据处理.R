require(ggplot2)
require(magrittr)
require(data.table)
require(stringr)

# 颜色自由
source("D:/学习/R/R自定义函数/颜色自由.R")
extract2plot_colours(path.pictures = "D:/娱乐/壁纸/", num_col = 5)

# 开始 ------------------------------------------------------------------------
# 1  glm: 广义线性模型
# 2  knn: k近邻算法
# 3  rpart: 决策树
# 4  rf: 随机森林
# 5  gbm: 梯度提升机, 采用决策树作为弱分类器的gbm算法被称为GBDT，有时又被称为MART
# 6  svm: 支持向量机
# 7  mlp: 神经网络
# 8  maxlike: 最大似然法
# 9  glmnet: Ridge岭回归和Lasso回归
# 10 gam: 广义相加模型
# 11 rbf: 神经网络
# 12 mars: 多元自适应样条

# 开始 end --------------------------------------------------------------------

# 1 随机数模拟-----------------------------
## 1.1 正态分布 (钟形)
nor <- rnorm(n = 300, mean = 0, sd = 1)

ggplot()+
  geom_density(mapping = aes(nor), bw = .4, fill = "red", alpha = .4)+
  labs(x = "", title = "正态分布")+
  theme_test(base_family = "STKauti")+
  theme(plot.title = element_text(hjust = .5))
  
### 根据随机数, 估计数据分布的参数
MASS::fitdistr(x = nor, densfun = "normal")

## 1.2 泊松分布 (λ)
pois <- rpois(n = 300, lambda = 2)

ggplot()+
  geom_density(mapping = aes(pois), bw = .4, fill = "red", alpha = .4)+
  labs(x = "", title = "泊松分布")+
  theme_test(base_family = "STKauti")+
  theme(plot.title = element_text(hjust = .5))

### 根据随机数, 估计数据分布的参数
MASS::fitdistr(x = pois, densfun = "Poisson")

## 1.3 均匀分布
unif <- runif(n = 300, min = 0, max = 1)

ggplot()+
  geom_histogram(mapping = aes(unif), bins = 15, fill = "red", alpha = .4)+
  labs(x = "", title = "均匀分布")+
  theme_test(base_family = "STKauti")+
  theme(plot.title = element_text(hjust = .5))

## 1.4 F分布
f <- rf(n = 300, df1 = 10, df2 = 10)

ggplot()+
  geom_density(mapping = aes(f), bw = .4, fill = "red", alpha = .4)+
  labs(x = "", title = "F分布")+
  theme_test(base_family = "STKauti")+
  theme(plot.title = element_text(hjust = .5))

## 1.5 多元正态分布
set.seed(123)

### 变量之间的协方差矩阵
sigma <- matrix(data = c(10, 3, 3, 4), nrow = 2, ncol = 2)

### 生成二元随机数, 并转为df
nor.2d <- MASS::mvrnorm(n = 800, mu = c(0, 4), Sigma = sigma)
df_nor.2d <- as.data.frame(nor.2d)
names(df_nor.2d) <- c("x", "y")

### 可视化
ggplot(data = df_nor.2d, mapping = aes(x = x, y = y))+
  geom_point(color = "red")+
  geom_density2d()+
  theme_test()+
  labs(title = "二元正态分布")+
  theme(plot.title = element_text(hjust = .5))

### 三维密度曲线图
#### 二维核密度估计
kde <- MASS::kde2d(x = df_nor.2d$x, y = df_nor.2d$y, n = 50)
plotly::plot_ly(x = kde$x, y = kde$y, z = kde$z, type = "surface")

### 计算方差矩阵
cov(df_nor.2d) # 应接近于sigma矩阵

### 计算均值
colMeans(df_nor.2d)

### 估计二元正态分布的参数
require(stats4)
mle.fit <- tmvtnorm::mle.tmvnorm(X = nor.2d, lower = c(-Inf, -Inf), upper = c(Inf, Inf))
summary(mle.fit)
mle.fit@coef
# 1 end -----------------------------------------------------------------------

# 2 假设检验-------------------------------
## 数据分布检验
## 数据相关性检验
## t检验(均值检验等)
## 方差齐性检验

## 2.1 检验数据是否正态分布
require(energy)

set.seed(12)
norm <- rnorm(n = 500, mean = 2, sd = 5)

ggplot()+
  geom_density(mapping = aes(norm), bw = .4, fill = "red", alpha = .4)+
  labs(x = "", title = "正态分布")+
  theme_test(base_family = "STKauti")+
  theme(plot.title = element_text(hjust = .5))

### 2.1.1 QQ图经验法, 若直线通过原点且斜率为1, 则接受正态分布的假设
car::qqPlot(norm, distribution = "norm")

### 2.1.2 ks检验, 前提是:假设其不是正态分布
ks.test(x = norm, "pnorm") # p<0.05, 说明假设对了, 则推翻其为mean为0, sd为1的标准正态分布
ks.test(x = norm, "pnorm", mean = 2, sd = 5) # p>0.05, 说明假设错了, 则接受其为mean为2, sd为5的正态分布

## 2.1.3 检验两组数据是否属于同一种分布, 假设不属于
pois <- rpois(n = 100, lambda = 2)

ggplot()+
  geom_density(mapping = aes(pois), bw = .4, fill = "red", alpha = .4)+
  labs(x = "", title = "泊松分布")+
  theme_test(base_family = "STKauti")+
  theme(plot.title = element_text(hjust = .5))

ks.test(x = norm, y = pois) # p值<0.05, 假设成立, 正态分布和泊松分布不属于同一种分布

## 2.1.4 t检验
### 主要用于样本含量较小（例如n < 30），总体标准差σ未知的正态分布。t检验是用t分布理论来推论差异发生的概率，从而比较两个平均数的差异是否显著
### 2.1.4.1 单样本, 检验正态分布的样本的均值是否为指定值
t1 <- rnorm(n = 1000, mean = 10, sd = 0.1)
t.test(x = t1, mu = 0) # mu表示平均值的真实值, 假设平均值!=0, p<0.05, 假设成立
t.test(x = t1, mu = 10) # 假设平均值!=10, p>0.05, 假设不成成立, 则mean=10

### 2.1.4.2 两独立正态分布的样本(双样本)的均值是否相等或差值为指定值
t2 <- rnorm(n = 1000, mean = 6, sd = 0.1)
t.test(x = t2, mu = 10)
t.test(x = t1, y = t2, mu = 0) # mu表示双样本平均值的差值, 假设差值!=0, p<0.05, 假设成立
t.test(x = t1, y = t2, mu = 4) # mu表示双样本平均值的差值, 假设差值!=4, p>0.05, 假设不成立, 则差值=4

### 2.1.4.3 比较两个平均数的差异是否显著(==>方差分析)
t.test(var1 ~ var2, data = data) # 假设两个平均数有差异, 如果p<0.05, 接受假设

## 2.1.5 方差齐性检验(一般指F检验, 比较方差), 首选Levene检验
### 假设一系列服从正态分布的母体，都有相同的标准差。这是最典型的F检验，该检验在方差分析（ANOVA）中也非常重要。
### 假设一个回归模型很好地符合其数据集要求，检验多元线性回归模型中被解释变量与解释变量之间线性关系在总体上是否显著。
### F检验对于数据的正态性非常敏感，因此在检验方差齐性的时候,Levene检验(不依赖总体分布),Bartlett检验或者Brown–Forsythe检验的稳健性都要优于F检验

### 2.1.5.1 F检验
#### F值时F检验的统计量值,F=MSR/MSE,其中MSR=SSR/自由度,MSE=SST/自由度,一般大于给定阿尔法相对的F量时说明显著.
var.test(x = t1, y = t2, ratio = 1) # sd都为0.1, ratio为x和y的总体方差的假设比率, 假设ratio != 1(即方差不齐), p>0.05, 假设不成立, 则方差齐
t3 <- rnorm(n = 1000, mean = 6, sd = 0.2)
var.test(x = t1, y = t3, ratio = 1) # sd都为0.1和0.2, 假设ratio != 1(即方差不齐), p<0.05, 假设成立, 则方差不齐 

### 2.1.5.2 Bartlett检验
bartlett.test(list(t1, t2, t3)) # 假设方差不齐, p<0.05, 假设成立, 则方差不齐
t4 <- rnorm(n = 1000, mean = 2, sd = 0.1)
bartlett.test(list(t1, t2, t4)) # 假即方差不齐, p>0.05, 假设不成立, 则方差齐

### 2.1.5.3 Levene检验 ==> 首选
df <- data.frame(x = c(t1, t2, t3), group = c(rep(c("A", "B", "C"), c(1000, 1000, 1000))))
car::leveneTest(x ~ group, data = df) # 假设方差不齐, p<0.05, 假设成立, 则方差不齐

ggplot(data = df, mapping = aes(x = group, y = x))+
  geom_violin(mapping = aes(fill = group), alpha = .2)+
  # geom_jitter(mapping = aes(color = group))+
  theme_test()

df1 <- data.frame(x = c(t1, t2, t4), group = c(rep(c("A", "B", "C"), c(1000, 1000, 1000))))
car::leveneTest(x ~ group, data = df1) # 假设方差不齐, p>0.05, 假设不成立, 则方差齐

ggplot(data = df1, mapping = aes(x = group, y = x))+
  geom_violin(mapping = aes(fill = group), alpha = .2)+
  # geom_jitter(mapping = aes(color = group))+
  theme_test()

## 2.1.6 数据相关性检验
data("iris", package = "datasets")
iris <- iris[, 2:4]

### 相关性检验, 只能检测一对
cor.test(iris$Petal.Length, iris$Petal.Width)

res <- psych::corr.test(x = iris, # 变量必须为数值
                        use = "pairwise", # 成对检验
                        method = "pearson")
res
res$r
res$p
res$p.adj # Given a set of p-values, returns p-values adjusted using one of several methods.

# 2 end -----------------------------------------------------------------------

# 3 方差分析 (比较平均值)------------------
## 3.1 单因素
data("iris", package = "datasets")
names(iris)

### 比较数据分布
boxplot(formula = Sepal.Width ~ Species, data = iris)
gplots::plotmeans(formula = Sepal.Width ~ Species, data = iris, col = "red")

### 方差分析
aov <- aov(formula = Sepal.Width ~ Species, data = iris)
summary(aov) # 假设均值不等, p值<0.05, 接受假设

### 多重检验, LSD:最小显著差数检验法
lsd.aov <- agricolae::LSD.test(y = aov, trt = "Species", p.adj = "holm", alpha = 0.01) 
mean_sd <- setDT(lsd.aov$means[, 1:2]) %>% .[, group := row.names(lsd.aov$means)]
mean_sd; mean_sd
LETTER <- setDT(lsd.aov[[5]])
LETTER
mean_sd_LETTER <- mean_sd[LETTER, on = .(Sepal.Width)]  # unique(bq[LETTER,on = "重复",allow.cartesian=T])
mean_sd_LETTER

## 3.2 双因素
data("ToothGrowth", package = "datasets")
too <- ToothGrowth
too$dose <- factor(x = too$dose, levels = c(0.5, 1, 2), labels = c("D0.5", "D1", "D2"))
str(too)

ggpubr::ggviolin(data = too, x = "dose", y = "len", color = "supp", add = "dotplot", palette = c("red", "blue"))

ggplot(data = too, mapping = aes(x = dose, y = len))+
  geom_violin(mapping = aes(fill = supp), alpha = .4)+
  geom_jitter(mapping = aes(color = supp), width = .2)+
  theme_test()

### 3.2.1 不考虑交互作用
aov <- aov(formula = len ~ dose + supp, data = too)
summary(aov)

### 多重检验, LSD:最小显著差数检验法
lsd.aov <- agricolae::LSD.test(y = aov, trt = c("dose", "supp"), p.adj = "holm", alpha = 0.05) 
mean_sd <- setDT(lsd.aov$means[, 1:2]) %>% .[, group := row.names(lsd.aov$means)]
mean_sd; mean_sd
LETTER <- setDT(lsd.aov[[5]])
mean_sd_LETTER <- mean_sd[LETTER, on = .(len)]  # unique(bq[LETTER,on = "重复",allow.cartesian=T])
mean_sd_LETTER

### 3.2.1 考虑交互作用
aov <- aov(formula = len ~ dose * supp, data = too)
summary(aov)

### 多重检验, LSD:最小显著差数检验法
lsd.aov <- agricolae::LSD.test(y = aov, trt = c("dose", "supp"), p.adj = "holm", alpha = 0.05) 
mean_sd <- setDT(lsd.aov$means[, 1:2]) %>% .[, group := row.names(lsd.aov$means)]
mean_sd; mean_sd
LETTER <- setDT(lsd.aov[[5]])
mean_sd_LETTER <- mean_sd[LETTER, on = .(len)]  # unique(bq[LETTER,on = "重复",allow.cartesian=T])
mean_sd_LETTER

# 3 end -----------------------------------------------------------------------

# 4 列联表分析 (卡方检验[独立性检验])--------------------------------
## 卡方检验就是统计样本的实际观测值与理论推断值(行和列独立下的期望值，用极大似然估计, 期望值=单观测数*分组数/总观测数，得到单观测概率的似然估计)之间的偏离程度，实际观测值与理论推断值之间的偏离程度就决定卡方值(是每个格子实际频数A与理论频数T差值平方与理论频数之比的累计和)的大小，如果卡方值越大，二者偏差程度越大；反之，二者偏差越小；若两个值完全相等时，卡方值就为0，表明理论值完全符合。易受到样本数大小的影响, 样本量越大, 越易显著, 所以须同时提供列联系数

## 4.1 普通
test <- data.frame(A = c(52, 60, 70), B = c(64, 59, 65), C = c(24, 52, 74))
row.names(test) <- c("甲", "乙", "丙")
test

res <- chisq.test(x = test)
res # 假设行列不独立, p<0.05, 接受假设

res$residuals # 残差(实际观察值与估计值（拟合值）之间的差)

### 行和列独立下的期望值
res$expected

### 可视化
mosaicplot(x = test, color = T, main = "")

### 计算列联系数, sqrt(卡方值/(卡方值＋总人数)) sqrt(15.375 / (15.375 + 520))=0.169
#### 值的范围在 0 到 1 之间，其中 0 表示行变量和列变量之间不相关，而接近 1 的值表示变量之间的相关度很高。
vcd::assocstats(x = as.matrix(test)) # 包含卡方检验结果

##  4.2 高维
### 伯克利大学1973年按入学和性别划分的研究生院申请人数最多的6个学院的汇总数据
data("UCBAdmissions", package = "datasets")
uc <- UCBAdmissions 
uc

mosaicplot(formula = ~ Admit + Dept + Gender, data = uc, shade = T, legend = T)

### 判断是否成对独立
MASS::loglm(formula = ~ Admit + Dept + Gender, data = uc) # 假设不成对独立, p<0.05, 假设成立

### 条件独立, 给定Dept, 判断Admit和Gender是否独立
MASS::loglm(formula = ~ Admit + Dept + Gender + Admit * Dept + Gender * Dept, data = uc) # 假设Admit和Gender不独立, p<0.05, 假设成立, 即Admit和Gender相关
mosaicplot(formula = ~ Admit + Gender, data = uc, shade = T, legend = T)

# 4 end -----------------------------------------------------------------------

#==> [多元回归分析] ==> 拟合模型
# 5 多元线性回归(服从正态分布)-逐步回归----
##(线性回归进行变量选择[将变量逐个引入, 每次引入新变量还会对就变量进行重新检验, 并剔除不显著的变量])
## 自变量过多可能造成多重共线性问题, 且造成模型更复杂
## 最小二乘法主要用于解决函数模型最优解问题, 即拟合模型,“最佳”地拟合于各观测点的估计曲线，应使各观测点到该曲线的偏差的平方和达到最小。
data("iris", package = "datasets")
IRIS <- iris
iris <- setDT(iris) %>% .[, !5] %>% .[, a := 1]
class(iris)
head(iris)
summary(iris)
str(iris)
names(iris)

GGally::ggscatmat(data = IRIS, columns = 1:4, color = "Species", alpha = .5, corMethod = "pearson")+
  theme_test()

## 将数据切分为训练集和测试集
set.seed(123)
index <- sample(nrow(iris), round(nrow(iris) * 0.7))

train <- iris[index]
test <- iris[!index]

### 训练模型
train.lm <- lm(formula = Sepal.Length ~ ., data = train)
summary(train.lm)

### 模型预测
test.predict <- predict(object = train.lm, test)

### 计算均方根误差
sprintf("均方根误差为: %f", ModelMetrics::mse(actual = test$Sepal.Length, predicted = test.predict))

### 判断多重共线问题
kappa(z = train.lm, exact = T) # 值越大, 表示共线性越严重
alias(train.lm) # 其中某些变量可以由其它变量表示, 则这些变量可以剔除

## 逐步回归解决别名问题(alias)和多重共线问题
train.step <- step(object = train.lm, direction = "both") # AIC越小越好, (RSS:残差平方和[residual sum of squares])
summary(train.step)

### 解决问题后模型预测
test.predict.step <- predict(object = train.step, test)

### 计算均方根误差
sprintf("均方根误差为: %f", ModelMetrics::mse(actual = test$Sepal.Length, predicted = test.predict.step))

### vif>10
car::vif(mod = train.step) #  # 去除vif>10的变量
train.vif <- lm(formula = Sepal.Length ~ Sepal.Width, data = train)
summary(train.vif) # 反而不显著, 所以使用train.step

### 可视化预测结果
index <- order(test$Sepal.Length)
x <- sort(index)
Sepal.Length <- test$Sepal.Length[index]
test.predict.plot <- test.predict[index]
test.predict.step.plot <- test.predict.step[index]

data <- data.frame(x, Sepal.Length, test.predict.plot, test.predict.step.plot)

ggplot(data = data, mapping = aes(x = x))+
  geom_point(mapping = aes(y = Sepal.Length), color = "red", alpha = .5)+
  geom_line(mapping = aes(y = test.predict.plot), color = "blue")+
  geom_point(mapping = aes(y = test.predict.step.plot), color = "green")+
  theme_test()

## 对数多元多项式回归
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
a <- rnorm(n = 10, mean = 5, sd = 3)
data <- data.frame(ctl, trt, a)
data
lm.D9 <- lm(formula = log(a) ~ ctl + I(ctl^2) + trt + I(trt^2), data = data)
lm.D9
summary(lm.D9)


# 5 end -----------------------------------------------------------------------

# 6 逻辑回归(二分类, 广义线性模型) --------
data("Affairs", package = "AER")
summary(Affairs)
str(Affairs)

Affairs <- setDT(Affairs)
table(Affairs$affairs) # 出轨次数

# 原Affairs$affairs为次数, 应该使用泊松分布, 但是我们要逻辑回归
Affairs[affairs > 0, yes := 1] 
Affairs[!affairs > 0, yes := 0]
Affairs$yes <- factor(x = Affairs$yes, levels = c(0, 1))
str(Affairs)
table(Affairs$yes)

aff <-  melt.data.table(data = Affairs, id.vars = "yes")
head(aff)  
aff$value <- as.numeric(aff$value)

## 可视化因变量在不同变量下的分布
ggplot(data = aff, mapping = aes(x = value))+
  facet_wrap(~ variable, scales = "free_y")+
  geom_density(mapping = aes(fill = yes), alpha = .4)+
  theme_test()

## 划分数据集
index <- caret::createDataPartition(y = Affairs$yes, times = 5, p = 0.7) # y为分类变量
train <- Affairs[index$Resample1]
test <- Affairs[! index$Resample1]

# 模型拟合
fit <- glm(formula = yes ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, 
           family = binomial(link = "logit"), 
           data = train)
fit
summary(fit) # 挑出相关的变量

### 判断多重共线问题
### vif>10
car::vif(mod = fit.step) #  # 去除vif>10的变量
kappa(z = fit, exact = T) # 值越大, 表示共线性越严重
alias(fit) # 无别名问题

## 使用逐步回归对变量进行筛选
fit.step <- step(object = fit, direction = "both")
fit.step
summary(fit.step)

## 可视化剔除变量过程中的AIC值变化情况(前面不变的话是因为该变量可以由其他变量构成, 即有别名)
step.anova <- fit.step$anova
step.anova$Step <- as.factor(step.anova$Step)
ggplot(data = step.anova, mapping = aes(x = reorder(Step, -AIC), y = AIC))+
  geom_point(color = "red", size = 2)+
  geom_text(mapping = aes(y = AIC - 1, label = round(AIC, 2)))+
  labs(x = "剔除的特征")+
  theme_test()

## 对比逐步回归前后模型的精度
predict <- predict(object = fit, newdata = test, type = "response") # response:结果为0-1的概率
predict.factor <- as.factor(ifelse(predict > 0.5, 1, 0))
sprintf("逻辑回归模型的均方根误差为: %f", ModelMetrics::mse(actual = test$yes, predicted = predict.factor))
sprintf("逻辑回归模型的精度为: %f", which(test$yes == predict.factor, T) %>% length() / nrow(test))

predict.step <- predict(object = fit.step, newdata = test, type = "response")
predict.step.factor <- as.factor(ifelse(predict.step > 0.5, 1, 0))
sprintf("逐步逻辑回归模型的均方根误差为: %f", ModelMetrics::mse(actual = test$yes, predicted = predict.step.factor))
sprintf("逐步逻辑回归模型的精度为: %f", which(test$yes == predict.step.factor, T) %>% length() / nrow(test))

## 计算混淆矩阵 
table(test$yes, predict.factor)
table(test$yes, predict.step.factor)

# 6 end -----------------------------------------------------------------------

# 7 泊松回归(lambda分布, 发生次数(非负整数)类型)---------------------
## 泊松回归的最重要假设为均值与方差相等，被称为均等分散
## glm(formula = y ~ . -1, family = poisson(link = "log), data = data) # -1表示不要常量
### 1
require(AER)

data("Affairs", package = "AER")
table(Affairs$affairs)

fit <- glm(formula = affairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, 
           family = poisson(link = "log"), 
           data = Affairs)
fit
summary(fit)

## 共线性及别名
### vif>10
car::vif(mod = fit) #  # 去除vif>10的变量
kappa(z = fit, exact = T) # 值越大, 表示共线性越严重
alias(fit) # 无别名问题

## 逐步回归
fit.step <- step(object = fit, direction = "both")
fit.step
summary(fit.step)

# 比较模型
anova(fit, fit.step, test = "Chisq")

coef(fit.setp)
exp(coef(fit.setp)) # >1增加; <1减少

## 稳健泊松回归
fit.od <- glm(formula = affairs ~ age + yearsmarried + religiousness + occupation + rating, 
              family = quasipoisson(link = "log"), 
              data = Affairs)
fit.od
summary(fit.od)

pchisq(summary(fit.od)$dispersion * fit.step$df.residual, fit.step$df.residual, lower  = F) # 0 < 0.05, 结果显著, 过度离失(观测到的响应变量的方差大于期望的二项分布的方差), 所以使用fit.od

# 7 end -----------------------------------------------------------------------

# 8 Ridge(岭回归)和Lasso回归 [L1/L2正则化]----
##  Lasso相比于普通最小二乘估计，可以在变量众多的时候快速有效地提取出重要变量，简化模型, 使模型更稳定,
## 假设，只有有限个X的回归系数不为0，但其余的都是0。也就是说他们跟Y并没有啥子特别显著的关系。找到其中重要的X，对我们理解数据有重要的意义。
## 设定 y = β* x+b, y是预测值，β是系数（coefficient，也可以叫权重 weight），x是特征值
## 针对多维度特征的回归模型，可以考虑选用lasso 或者ridge，因为特征之间很有可能具有多重共线性（彼此包含或关联），而step.lm，lasso等算法能自动处理这个问题，针对高度相似的特征，lasso会随机选其中一个。
## Lasso和Ridge的公式非常相似（β2平方和β1的区别），不过ridge的参数β不会等于0，所以ridge会对每一个特征都得出系数结果，但lasso的参数β可以等于0，即会筛除大部分的特征，所以lasso 模型多用于做特征选择
## 在glmnet包中，alpha=1是lasso模型，alpha=0则为Ridge模型，两者取个中间值的算法叫Elastic Net，原理都类似，只不过是同时参考了两方算法， 具体可以试试看哪个模型效果好就用哪个
## 需要合适的lambda, 所以需要试
## Lasso 的全称为 least absolute shrinkage and selection operator，又译最小绝对值收敛和选择算子、套索算法。
## Lasso 回归对式（2）加入 L1 正则化，其 cost function 如下：
## 岭回归对式（2）加入 L2 正则化，其 cost function 如下：
## L1正则化最大的特点是能稀疏矩阵，进行庞大特征数量下的特征选择
## L2正则能够有效的防止模型过拟合，解决非满秩下求逆困难的问题
## 相同： 都可以用来解决标准线性回归的过拟合问题。 不同： lasso 可以用来做 feature selection，而 ridge 不行。或者说，lasso 更容易使得权重变为 0，而 ridge 更容易使得权重接近 0。 从贝叶斯角度看，lasso（L1 正则）等价于参数 w 的先验概率分布满足拉普拉斯分布，而 ridge（L2 正则）等价于参数 w 的先验概率分布满足高斯分布

data("Affairs", package = "AER")
summary(Affairs)
str(Affairs)

Affairs <- setDT(Affairs)
table(Affairs$affairs) # 出轨次数

data <- Affairs[, c(1, 3, 4, 6:9)]
## 可视化相关系数
cor <- cor(data) # 只能是数字
corrplot::corrplot.mixed(corr = cor, tl.pos = "d", tl.col = "black")

## 划分数据集
set.seed(123)
index <- caret::createDataPartition(y = data$affairs, times = 5, p = 0.7) # y为分类变量
train <- data[index$Resample5]
test <- data[! index$Resample5]

## 数据标准化
data.pp <- caret::preProcess(x = data, method = c("center", "scale"))
train.pp <- predict(data.pp, train)
test.pp <- predict(data.pp, test)

data.pp$mean # 查看标准化使用的均值
data.pp$std

## rideg回归 (alpha = 0)
### 在训练集上寻找合适的lambda参数
lambdas <- seq(0, 5, length.out = 200) # length.out: 数据长度
x <- train.pp[, 2:7] %>% as.matrix()
y <- train.pp[, affairs] # 使用向量
set.seed(123)
#### 原始样本被随机划分为n倍大小相等的子样本。在nfold subsamples中，保留单个subsample作为测试模型的验证数据，剩余nfold-1个subsample作为训练数据。
model.ridge <- glmnet::cv.glmnet(x = x, y = y, alpha = 0, lambda = lambdas, parallel = T, nfolds = 8) # cv交叉验证
model.ridge
summary(model.ridge)
### 查看lambdas对模型均方误差的影响
plot(model.ridge)
### 可视化岭回归模型的回归系数的轨迹
plot(model.ridge$glmnet.fit, "lambda", label = T)
### 回归效果最好的lambda(均方误差最小)
ridge.min <- model.ridge$lambda.min
ridge.min
### 使用ridge.min拟合ridge模型
ridge.best <- glmnet::glmnet(x = x, y = y, alpha = 0, lambda = ridge.min)
ridge.best
summary(ridge.best)
coef(ridge.best)
### 预测
predict.test <- predict(object = ridge.best, as.matrix(test.pp[, 2:7]))
sprintf("标准化后的平均绝对误差为: %f", ModelMetrics::mae(test.pp$affairs, predict.test))
#### 将预测值逆标准化和原始数据进行比较
predict.test.O <- test.pp[, 1] * data.pp$std[1] + data.pp$mean[1]
sprintf("标准化前的平均绝对误差为: %f", ModelMetrics::mae(test.pp$affairs, predict.test.O$affairs))

## lasso回归 (alpha = 1)
### 在训练集上寻找合适的lambda参数
lambdas <- seq(0, 5, length.out = 200) # length.out: 数据长度
x <- train.pp[, 2:7] %>% as.matrix()
y <- train.pp[, affairs] # 使用向量
set.seed(123)
#### 原始样本被随机划分为n倍大小相等的子样本。在nfold subsamples中，保留单个subsample作为测试模型的验证数据，剩余nfold-1个subsample作为训练数据。
model.ridge <- glmnet::cv.glmnet(x = x, y = y, alpha = 1, lambda = lambdas, parallel = T, nfolds = 8) # cv交叉验证
model.ridge
summary(model.ridge)
### 查看lambdas对模型均方误差的影响
plot(model.ridge) # 随着lambda增加, 使用的变量减少, 均方误差在增加
### 可视化模型的回归系数的轨迹
plot(model.ridge$glmnet.fit, "lambda", label = T)
### 回归效果最好的lambda(均方误差最小)
ridge.min <- model.ridge$lambda.min
ridge.min
### 使用ridge.min拟合ridge模型
ridge.best <- glmnet::glmnet(x = x, y = y, alpha = 1, lambda = ridge.min)
ridge.best
summary(ridge.best)
coef(ridge.best)
### 预测
predict.test <- predict(object = ridge.best, as.matrix(test.pp[, 2:7]))
sprintf("标准化后的平均绝对误差为: %f", ModelMetrics::mae(test.pp$affairs, predict.test))
# #### 将预测值逆标准化和原始数据进行比较
# predict.test.O <- test.pp[, 1] * data.pp$std[1] + data.pp$mean[1]
# sprintf("标准化前的平均绝对误差为: %f", ModelMetrics::mae(test.pp$affairs, predict.test.O$affairs))

## 广义lasso回归(二分类)
### 原Affairs$affairs为次数, 应该使用泊松分布, 但是我们要逻辑回归
data("Affairs", package = "AER")
data <- setDT(Affairs) %>% .[, c(1, 3, 4, 6:9)]
data[affairs > 5, yes := 1]
data[!affairs > 5, yes := 0]
data$yes <- factor(x = data$yes, levels = c(0, 1))
str(data)
table(data$yes)
head(data)

## 划分数据集
set.seed(123)
index <- caret::createDataPartition(y = data$yes, times = 5, p = 0.7) # y为分类变量
train <- data[index$Resample5]
test <- data[! index$Resample5]

## 数据标准化
data.pp <- caret::preProcess(x = data, method = c("center", "scale"))
train.pp <- predict(data.pp, train)
test.pp <- predict(data.pp, test)

data.pp$mean # 查看标准化使用的均值
data.pp$std

### 在训练集上寻找合适的lambda参数
lambdas <- seq(0, 100, length.out = 2000) # length.out: 数据长度
x <- train.pp[, 1:7] %>% as.matrix()
y <- train.pp[, yes] # 使用向量
set.seed(123)
#### 原始样本被随机划分为n倍大小相等的子样本。在nfold subsamples中，保留单个subsample作为测试模型的验证数据，剩余nfold-1个subsample作为训练数据。
model.ridge <- glmnet::cv.glmnet(x = x, y = y, alpha = 1, lambda = lambdas, family = "binomial", type.measure = "class", parallel = T, nfolds = 8) # cv交叉验证
model.ridge
summary(model.ridge)
### 查看lambdas对模型均方误差的影响
plot(model.ridge) # 随着lambda增加, 使用的变量减少, 均方误差在增加
### 可视化岭回归模型的回归系数的轨迹
plot(model.ridge$glmnet.fit, "lambda", label = T)
### 回归效果最好的lambda(均方误差最小)
ridge.min <- model.ridge$lambda.min
ridge.min
### 使用ridge.min拟合ridge模型
ridge.best <- glmnet::glmnet(x = x, y = y, alpha = 1, lambda = ridge.min, family = "binomial")
ridge.best
summary(ridge.best)
coef(ridge.best)
### 预测
predict.test <- predict(object = ridge.best, as.matrix(test.pp[, 1:7]))
sprintf("标准化后的平均绝对误差为: %f", ModelMetrics::mae(test.pp$yes, predict.test))

# 8 end -----------------------------------------------------------------------

#==> [多元统计分析](不需要区分自变量和因变量) ==> 降维/分类/变量之间的关系等
# 9 聚类分析(无监督)-----------------------
## 监督学习和无监督学习，这里给一个简单的介绍：一般而言，是否有监督，就看输入数据是否有标签，输入数据有标签，则为有监督学习，否则为无监督学习。
data("iris", package = "datasets")
head(iris)
num.iris <- iris[, 1:4]
str(num.iris)

## 查看数据相关性
num.iris.cor <- cor(num.iris)
corrplot::corrplot.mixed(corr = num.iris.cor, tl.col = "black")

## 标准化
num.iris.scale <- scale(num.iris)

## 9.1 系统聚类及可视化
hc1 <- hclust(d = dist(num.iris.scale), method = "ward.D2") # dist默认欧氏距离; method指定处理合并后分裂的度量方式
hc1$labels <- paste(iris$Species, 1:150, sep = "_")
plot(hc1, hang = -1)
rect.hclust(hc1, k = 3, border = "red") # 

## 9.2 k-means聚类 (只能为椭圆状)
### 算法最大的特点是简单，好理解，运算速度快，但是只能应用于[连续型]的数据，并且一定要在聚类前需要手工指定要分成几类。
### K-Means 聚类算法的大致思想就是“物以类聚，人以群分”：
### 首先输入 k 的值，即我们指定希望通过聚类得到 k 个分组；
### 从数据集中随机选取 k 个数据点作为初始大佬（质心）；
### 对集合中每一个小弟，计算与每一个大佬的距离，离哪个大佬距离近，就跟定哪个大佬；
### 这时每一个大佬手下都聚集了一票小弟，这时候召开选举大会，每一群选出新的大佬（即通过算法选出新的质心）；
### 如果新大佬和老大佬之间的距离小于某一个设置的阈值（表示重新计算的质心的位置变化不大，趋于稳定，或者说收敛），可以认为我们进行的聚类已经达到期望的结果，算法终止；
### 如果新大佬和老大佬距离变化很大，需要迭代3~5步骤

### 例1
### 通过计算组内平方和和组间平方和确定合适的分组数量
withiness <- NULL
between <- NULL
i <- 1
for (i in 1:15) {
  set.seed(1234)
  k <- kmeans(x = num.iris.scale, centers = i)
  withiness[i] <- k$tot.withinss
  between[i] <- k$betweenss
}
withiness
between

k <- data.table(k = 1:15, withiness = withiness, between = between) %>% melt.data.table(id.vars = "k", variable.name = "group", value.name = "value")
head(k)
### 可视化两个平方和的趋势, k>3时曲线较为平缓, 所以取k = 3
ggplot(data = k, mapping = aes(x = k, y = value))+
  geom_line(mapping = aes(color = group))+
  geom_point(mapping = aes(color = group))+
  theme_test()

### 计算不同k值的轮廓值
nk <- 2:15
set.seed(1234)
silhouettes <- sapply(nk, function(k){
  fpc::cluster.stats(dist(num.iris.scale), kmeans(num.iris.scale, centers = k)$cluster)$avg.silwidth
}) 
silhouettes
plot(nk, silhouettes, type = "l", xlab="number of clusters", ylab = "average silhouette width")
nk[which.max(silhouettes)]
#### 得到最大的簇个数的k值为2

# 利用factoextra包的fviz_nbclust() 函数寻找最佳的聚类数
install.packages('factoextra')
library(factoextra)
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
# 图形结果看，曲线在K＝4的节点上，曲线趋势趋于平缓，因此可以选择聚4类。


### k = 3
set.seed(1234)
k3 <- kmeans(x = num.iris.scale, centers = 3)
table(k3$cluster)
k3[["centers"]]
cluster::clusplot(x = num.iris.scale, clus = k3$cluster, main = "k-means cluster number = 3")

### 计算并可视化轮廓系数
sis3 <- cluster::silhouette(x = k3$cluster, dist = dist(num.iris.scale, method = "euclidean"))
plot(sis3, main = "Iris k-means silhouette", col = c("red", "green", "blue")) # 轮廓系数平均值为0.48, 且各处理各样本的轮廓系数均>0, 所以聚类为3类较好(每个样本的轮廓系数介于-1至1之间, 越接近1越好)

### k = 2
set.seed(1234)
k2 <- kmeans(x = num.iris.scale, centers = 2)
table(k2$cluster)
k2[["centers"]]
cluster::clusplot(x = num.iris.scale, clus = k2$cluster, main = "k-means cluster number = 2")

### 计算并可视化轮廓系数
sis3 <- cluster::silhouette(x = k3$cluster, dist = dist(num.iris.scale, method = "euclidean"))
plot(sis3, main = "Iris k-means silhouette", col = c("red", "green", "blue")) # 轮廓系数平均值为0.48, 且各处理各样本的轮廓系数均>0, 所以聚类为3类较好(每个样本的轮廓系数介于-1至1之间, 越接近1越好)

### 提取分类
res3 <- setDT(iris) %>% .[, cluster := k3$cluster]
res2 <- setDT(iris) %>% .[, cluster := k2$cluster]

## 9.3 密度聚类 (可以为任意形状)
### 使用DBSCAN算法聚类(fpc::dbscan)
data.dbs <- iris[, c(3:5)]
ggplot(data = data.dbs, mapping = aes(x = Petal.Length, y = Petal.Width))+
  geom_point(mapping = aes(color = Species, shape = Species))


dbs <- fpc::dbscan(data = data.dbs[, 1:2], eps = 0.05, scale = T, MinPts = 5)
dbs

eps <- seq(0.05, 0.3, 0.05)
names <- c("one", "two", "three", "four", "five", "six")
i <- 1
for (i in 1:length(eps)) {
  dbs.i <- fpc::dbscan(data = data.dbs[, 1:2], eps = eps[i], scale = T, MinPts = 5)
  data.dbs[[names[i]]] <- as.factor(dbs.i$cluster)
}

head(data.dbs)
ggplot(data = data.dbs, mapping = aes(x = Petal.Length, y = Petal.Width))+
  geom_point(mapping = aes(color = three, shape = three))+
  theme_test()

# 9 end -----------------------------------------------------------------------

# 10 对应分析(列联表数据)------------------
## 对应分析前先进行卡方检验, 如果变量之间独立, 则无需进行对应分析, 反之则需要使用对应分析分析因子之间的相关性
smoke <- data.frame(a = c(4, 4, 25, 18, 10), 
                    b = c(2, 3, 10, 24, 6), 
                    c = c(3, 7, 12, 33, 7), 
                    d = c(2, 4, 4, 12, 2), 
                    row.names = LETTERS[1:5])
smoke

## 10.1 简单对应分析, 建立变量中类别之间的对应关系
### 卡方检验, 假设独立
res.chisp <- chisq.test(x = smoke)
res.chisp # p>0.5, 拒绝假设, 即行列有相关性
### 计算列联系数, sqrt(卡方值/(卡方值＋总人数)) sqrt(15.375 / (15.375 + 520))=0.169
#### 值的范围在 0 到 1 之间，其中 0 表示行变量和列变量之间不相关，而接近 1 的值表示变量之间的相关度很高。
vcd::assocstats(x = as.matrix(smoke)) # 列联系数=0.28, 相关性不是很高

### 马赛克图展示吸烟情况分布
mosaicplot(x = smoke, main = "", color = c("red", "blue", "green", "orange"))

### 对应分析
ca <- ca::ca(smoke) # pca
ca
summary(ca)
plot(ca, main = "smoke") # 距离越近, 关系越密切

## 10.2 高维对应分析
### 伯克利大学1973年按入学和性别划分的研究生院申请人数最多的6个学院的汇总数据
data("UCBAdmissions", package = "datasets")
uc <- UCBAdmissions 
uc

mosaicplot(formula = ~ Admit + Dept + Gender, data = uc, shade = T, legend = T)

### 判断是否成对独立
MASS::loglm(formula = ~ Admit + Dept + Gender, data = uc) # 假设不成对独立, p<0.05, 假设成立

### 条件独立, 给定Dept, 判断Admit和Gender是否独立
MASS::loglm(formula = ~ Admit + Dept + Gender + Admit * Dept + Gender * Dept, data = uc) # 假设Admit和Gender不独立, p<0.05, 假设成立, 即Admit和Gender相关
mosaicplot(formula = ~ Admit + Gender, data = uc, shade = T, legend = T)

### 对应分析
mca <- ca::mjca(uc)
mca
summary(mca)

plot(mca, mass = c(T, T), col = c("black", "red", "green", "blue"), main = c("三维列联表对应分析"))

# 10 end -----------------------------------------------------------------------

# 11 主成分分析-----------------------------
## 每个主成分都是原始变量的线性组合, 且各主成分之间互不相关
data("iris", package = "datasets")
data.num <- setDT(iris) %>% .[, 1:4]; head(data.num)

psych::fa.parallel(x = scale(data.num, center = T, scale = T), fa = "pc") # 推荐1个

## 对给定的数据矩阵执行主成分分析，并将结果作为prcomp类的对象返回。
### 同时进行标准化
com1 <- prcomp(x = data.num, center = TRUE, scale. = TRUE)

#提取主成分的方差贡献率
summ <- summary(com1)
imp <- summ$importance[2,]*100; imp # 前两个主成分解释了95.8%的信息, 所以使用前两个

#可视化
#提取PC score；
df1 <- com1$x; head(df1)
#将标签列合并进来；
df1 <- data.frame(df1, iris$Species); head(df1)

# 生成坐标轴标题；
xlab <- paste0("PC1(",round(summ$importance[2,1]*100,2),"%)"); xlab
ylab <- paste0("PC2(",round(summ$importance[2,2]*100,2),"%)"); ylab

require(ggplot2)
ggplot(data = df1, mapping = aes(x = PC1, y = PC2, color = iris.Species))+
  stat_ellipse(mapping = aes(fill = iris.Species), type = "norm", geom = "polygon", alpha = 0.2, color = NA)+
  geom_point(size = 6, alpha = 0.7)+
  labs(x = xlab, y = ylab)+
  guides(fill = F)+
  scale_fill_manual(values = c("cyan4", "darkred", "LightSkyBlue"))+
  scale_colour_manual(values = c("cyan4", "darkred", "LightSkyBlue"))+
  theme_test()+
  theme(text = element_text(size = 22))

# 提取主成分数据区[20-支持向量机 svm]

# 11 end -----------------------------------------------------------------------

# 12 典型相关分析(多变量与多变量之间)-------
## 相关系数==> 两个变量之间的线性相关关系
## 复相关系数==> 一个变量与多个变量之间
## 典型相关分析 ==> 主要利用主成分降维的思想，分别对两组变量提供主成分，从而将研究两组变量的相关性问题转化成研究两个变量的相关性问题。

data("olympic", package = "ade4")
data <- olympic$tab %>% setDT()
names(data)
summary(data)

x <- data[, .(poid, disq, jave, perc)] # 与上肢相关的数据
y <- data[, ! c("poid", "disq", "jave", "perc")] # 与下肢相关的数据

## 典型相关分析1
### 同时进行标准化
cor <- candisc::cancor(x = x, y = y, xcenter = T, ycenter = T, xscale = T, yscale = T)
summary(cor) # 第一对典型相关变量的相关系数为0.587; Raw canonical coefficients输出的是第一组变量（共m个）的典型载荷（按列读取）

## 可视化
### 斜率代表系数的大小, 
par(mfrow = c(2, 2)) ### 正相关, 说明上肢与下肢的能力有正相关关系
plot(cor, which = 1)
plot(cor, which = 2)
plot(cor, which = 3)
plot(cor, which = 4)

## 典型相关分析2
cor2 <- CCA::matcor(X = x, Y = y) # 计算两个数据集内和数据集之间的相关矩阵。
cor2

## 可视化
CCA::img.matcor(correl = cor2, type = 2) # cross为XYcor的交叉部分

# 12 end -----------------------------------------------------------------------

# 13 判别分析(有监督)-----------------------
data("iris", package = "datasets")
iris <- setDT(iris)
## 划分数据集
set.seed(123)
index <- caret::createDataPartition(y = iris$Species, times = 5, p = 0.7) # y为分类变量
train <- iris[index$Resample1]
test <- iris[! index$Resample1]
table(train$Species)
table(test$Species)

## 线性判别
lda <- MASS::lda(formula = Species ~ ., data = train)
lda

## 预测
lda.predict <- predict(object = lda, test)
lda.predict
table(test$Species, lda.predict$class)

## 可视化
plot(lda, abbrev = 1)
klaR::partimat(formula = Species ~ ., data = train, method = "lda", main = "线性判别") # 直

## 二次判别
qda <- MASS::qda(formula = Species ~ ., data = train)
qda

## 预测
qda.predict <- predict(object = qda, test)
qda.predict
table(test$Species, qda.predict$class) ## 两者结果一致

## 可视化
klaR::partimat(formula = Species ~ ., data = train, method = "qda", main = "二次判别") # 圆

# 13 end -----------------------------------------------------------------------

# 14 关联规则分析(购物篮分析)---------------
## 项目同时出现的概率
data <- fread("D:/学习/R/R资料/R数据处理数据/dataset_group关联规则分析.csv")
length(unique(data$V2)) # V2为1139条购买记录
length(unique(data$V3)) # V3为38种物品的大类

## 查看商品出现的次数
data.num <- data[, .(num = .N), by = .(V3)]; data.num
ggplot(data = data.num, mapping = aes(x = reorder(V3, num), y = num))+ #  想好看, 文字得加角度
  geom_bar(stat = "identity", fill = "lightblue")+
  geom_text(mapping = aes(x = reorder(V3, num), y = num + 50, label = num), size = 3)+
  geom_text(mapping = aes(x = reorder(V3, num), y = num + 500, label = V3), size = 3)+
  coord_polar()+
  scale_y_continuous(limits = c(-600, 1800))+ # 控制中央圈大小+
  labs(x = "商品", y = "商品出现次数")+
  theme_test()+
  theme(axis.text.x = element_blank()) 

## 将数据转化为列表
data.list <- split(x = data$V3, f = as.factor(data$V2))
sum(sapply(data.list, length))

## 过滤每条购买记录中相同的实例
data.list <- lapply(data.list, unique)
sum(sapply(data.list, length))

## 将过滤后的数据转化为交易数据集
require(arules)
require(arulesViz)
data.list <- as(object = data.list, Class = "transactions")

## 可视化
### 商品出现频率统计
arules::itemFrequency(x = data.list)
### 出现频率>0.25的商品
arules::itemFrequencyPlot(x = data.list, support = 0.25, col = "lightblue", xlab = "商品", main = "频率>0.25的商品")
### 出现次数前20多的商品
arules::itemFrequencyPlot(x = data.list, topN = 20, col = "lightblue", xlab = "商品", main = "频率>0.25的商品", horiz = T)

## 寻找关联规则
rule <- arules::apriori(data = data.list, parameter = list(support = 0.25, # 出现频率
                                                           confidence = 0.4, # 规则/关联超边缘的最小置信度的数值（默认值：0.8）
                                                           minlen = 1)) # 项目中最少包含元素个数
rule # 6个规则
summary(rule)
## 查看规则
rule.sort <- sort(rule, by = "lift") # 按提升度进行排序
arules::inspect(x = rule.sort) # lift > 1, 表示关联规则有用
## 可视化
plot(rule.sort, method = "graph")
plot(rule.sort, method = "grouped") # 用群集时必需有n >= 2的对象

## 规定右侧出现的项目
rule2 <- arules::apriori(data = data.list, parameter = list(support = 0.25, # 出现频率
                                                           confidence = 0.4, # 规则/关联超边缘的最小置信度的数值（默认值：0.8）
                                                           minlen = 1,
                                                           maxlen = 8,
                                                           target = "rules"),
                        appearance = list(rhs = "ice cream",
                                          default = "lhs")) # 左侧为lhs, 不做要求
rule2 # 1个规则
summary(rule2)
## 查看规则
rule2.sort <- sort(rule2, by = "lift") # 按提升度进行排序
arules::inspect(x = rule2.sort) # lift > 1, 表示关联规则有用
## 可视化
plot(rule2.sort, method = "graph")
plot(rule2.sort, method = "grouped") # 用群集时必需有n >= 2的对象

# 14 end -----------------------------------------------------------------------

# 15 KNN(k近邻算法, 有监督)-----------------
## NN的核心思想可以用平常的俗语表示"物以类聚，人以群分"，就是相似的东西很有可能具有相似的属性。k邻近法假设给定一个训练数据集，其中的数据标签已给。对于新的样本，找到与其k个最近邻的训练数据，这k个训练数据的多数属于某个类，就把新样本归为这个类。k近邻算法不具有显式的学习过程，不会从训练数据中学习到某个假设函数，用来预测，它实际上是利用训练数据对特征向量空间进行划分，并作为其学习出来的“模型”。
## 距离度量是KNN中用来表示样本的距离大小的评价指标，选取不同的距离度量会产生精度不同的预测结果。
## 需要解决的一个问题是选择合适的k值，k值过小或过大都会影响模型的准确性，一般考虑将k值设为3~10，或是将k值设为训练集样本数量的平方根。还有一种选择k值的方法是结合训练集和测试集，循环k值，直到挑选出使测试集的误差率最小的k值。
## 如果选择较小的k值，就是意味着用较小的相邻训练数据进行预测，预测的结果会对近邻的训练数据非常敏感，当近邻的数据中存在噪声时，预测结果往往出现较大偏差，过小的k值会导致模型变得复杂，因此也更容易发生过拟合现象。如果选择较大的k值，就相当于用较大邻域中的训练数据进行预测，与输入数据距离较远的训练数据也会对模型产生影响，往往使预测结果发生错误。过大的k值使得模型过于简单。
## 在应用knn算法进行建模前，必须要做的一件事就是特征标准化。因为knn依赖于距离的计算，现实数据中，各个特征的标度一般是不一样的，即数据范围有差异，假如某个特征的数值特别大，那么距离的度量就会强烈地被这个较大数值的特征所支配，其他特征起到的作用就很小了，模型也就失去了意义。
## 变量多的话可先进行主成分分析

data <- fread("D:/学习/R/R资料/R数据处理数据/wisc_bc_data_knn.csv")
unique(data$diagnosis)
sum(is.na(data))
data <- data[, ! 1]
data$diagnosis <- as.factor(data$diagnosis)

## 划分数据集
set.seed(123)
index <- caret::createDataPartition(y = data$diagnosis, times = 5, p = 0.7) # y为分类变量
train <- data[index$Resample5]
test <- data[! index$Resample5]

table(train$diagnosis) %>% prop.table()
table(test$diagnosis) %>% prop.table()

## 建模
### 通过trainControl函数设置10折交叉训练，并传入到trControl参数中；
### 通过preProcess参数设置训练数据标准化处理；
### 通过tuneLength参数设置k取值个数，模型以此进行参数网格搜索调优。
### 模型训练评价指标，分类问题默认为准确率。
control <- caret::trainControl(method = "cv", number = 10)
model <- caret::train(form = diagnosis ~ ., 
                      data = train,
                      method = "knn",
                      preProcess = c("center", "scale"),
                      trControl = control,
                      tuneLength = 15)
model

## 预测
predict <- caret::predict.train(object = model, newdata = test)
predict

## 计算混淆矩阵
truth <- test$diagnosis
table(truth, predict)
caret::confusionMatrix(predict, truth)

# 15 end -----------------------------------------------------------------------

# 16 朴素贝叶斯分类(常用于文本分类)---------
data("PimaIndiansDiabetes2", package = "mlbench")
data <- setDT(PimaIndiansDiabetes2)
table(data$diabetes) %>% prop.table()
data$diabetes <- as.factor(data$diabetes)
sum(is.na(data)) # 有缺失值

## 缺失值探索
mice::md.pattern(data, plot = T, rotate.names = T) # 左侧数字代表缺失或不确实的行数, 右侧数字代表缺失的个数
VIM::aggr(x = data, numbers = T, prop = F)

## caret包的preProcess函数可以进行数据的标准/归一化处理，还可以进行数据缺失值的插补工作
preproc <- caret::preProcess(x = data[, ! 9], method = "bagImpute", k = 5) # "knnImpute", "bagImpute", "medianImpute"
data <- predict(preproc, data[, ! 9])
data$diabetes <- PimaIndiansDiabetes2$diabetes # 添加回标签列

mice::md.pattern(data, plot = T, rotate.names = T) # 查看缺失值==>无

## 划分数据集
index <- caret::createDataPartition(y = data$diabetes, times = 5, p = 0.7)
train <- data[index$Resample5]
test <- data[! index$Resample5]
table(train$diabetes) %>% prop.table()
table(test$diabetes) %>% prop.table()
table(data$diabetes) %>% prop.table()

## 建模
model <- e1071::naiveBayes(formula = diabetes ~ ., data = train)
model

## 预测
pred <- predict(object = model, newdata = test)
truth <- test$diabetes
table(truth, pred)
caret::confusionMatrix(pred, truth)

## 评价模型经度
### 可以得到很多模型性能的指标，比如：灵敏性、特异性、精度、假正率等等，下面叙述的ROC曲线就是基于假正率和灵敏性这两个指标绘制的。
gmodels::CrossTable(x = test$diabetes, y = pred, prop.r = T, prop.c = T, prop.t = T, prop.chisq = F)

## ROC
pred.roc <- predict(object = model, newdata = test, type = "raw")
plot.roc <- pROC::roc(test$diabetes, pred.roc[, 2])

plot(plot.roc, print.auc = T, auc.polygon = T, grid = c(0.1, 0.2), grid.col = c("green", "red"), max.auc.polygon = T, auc.polygon.col = "skyblue", print.thres = T)

# 16 end -----------------------------------------------------------------------

# 17 rpart(决策树 decision tree) -------------------------
## 重点参数cp(复杂度系数)
data("PimaIndiansDiabetes2", package = "mlbench")
data <- setDT(PimaIndiansDiabetes2)
head(data)
table(data$diabetes) %>% prop.table()
data$diabetes <- as.factor(data$diabetes)
sum(is.na(data)) # 有缺失值

## 缺失值探索
mice::md.pattern(data, plot = T, rotate.names = T) # 左侧数字代表缺失或不确实的行数, 右侧数字代表缺失的个数
VIM::aggr(x = data, numbers = T, prop = F)

## caret包的preProcess函数可以进行数据的标准/归一化处理，还可以进行数据缺失值的插补工作
###~ "bagImpute": 使用装袋法的方式,拟合数据
preproc <- caret::preProcess(x = data[, ! 9], method = "bagImpute", k = 5) # "knnImpute", "bagImpute", "medianImpute"
data <- predict(preproc, data[, ! 9])
data$diabetes <- PimaIndiansDiabetes2$diabetes # 添加回标签列
###  data$age[is.na(data$age)] <- mean(data$age, na.rm = T) # 或中位数或众数(table)

mice::md.pattern(data, plot = T, rotate.names = T) # 查看缺失值==>无

## 划分数据集
index <- caret::createDataPartition(y = data$diabetes, times = 5, p = 0.7)
train <- data[index$Resample5]
test <- data[! index$Resample5]
table(train$diabetes) %>% prop.table()
table(test$diabetes) %>% prop.table()
table(data$diabetes) %>% prop.table()

## 拟合模型
model <- rpart::rpart(formula = diabetes ~ ., data = train, method = "class", cp = 0.000001)
model
summary(model)

## 查看变量重要性
model$variable.importance

## cp是每次分裂对应的复杂度系数
model$cptable
rpart::plotcp(model)

## 可视化rpart决策树模型
rpart.plot::rpart.plot(x = model, type = 2, extra = "auto", under = F, fallen.leaves = F, cex = 0.7, main = "rpart-决策树")

## 预测
pred.train.neg <- predict(object = model, newdata = train, type = "prob")[, 1]
pred.train.neg.5 <- as.factor(ifelse(pred.train.neg > 0.5, "neg", "pos"))
sprintf(fmt = "rpart-决策树模型在训练集上的预测精度为: %f", (which(train$diabetes == pred.train.neg.5, T) %>% length()) / nrow(train)) # 0.8587
pred.test.neg  <- predict(object = model, newdata = test, type = "prob")[, 1]
pred.test.neg.5 <- as.factor(ifelse(pred.test.neg > 0.5, "neg", "pos"))
sprintf(fmt = "rpart-决策树模型在测试集上的预测精度为: %f", (which(test$diabetes == pred.test.neg.5, T) %>% length()) / nrow(test)) # 0.77

## 计算混淆矩阵
cfm <- caret::confusionMatrix(pred.train.neg.5, train$diabetes)
cfm$table
cfm
cfm <- caret::confusionMatrix(pred.test.neg.5, test$diabetes)
cfm$table
cfm

## 决策树优化
cp.best <- model$cptable[which.min(x = model$cptable[, "xerror"]), "CP"]
cp.best

## 剪枝
model.prune <- rpart::prune(tree = model, cp =cp.best)
rpart::plotcp(model.prune)

## 剪枝后可视化
rpart.plot::rpart.plot(x = model.prune, type = 2, extra = "auto", under = F, fallen.leaves = F, cex = 0.7, main = "rpart-决策树==> 剪枝后")

## 预测
pred.train.neg <- predict(object = model.prune, newdata = train, type = "prob")[, 1]
pred.train.neg.5 <- as.factor(ifelse(pred.train.neg > 0.5, "neg", "pos"))
sprintf(fmt = "rpart-决策树模型==> 剪枝后在训练集上的预测精度为: %f", (which(train$diabetes == pred.train.neg.5, T) %>% length()) / nrow(train)) # 0.831
pred.test.neg  <- predict(object = model.prune, newdata = test, type = "prob")[, 1]
pred.test.neg.5 <- as.factor(ifelse(pred.test.neg > 0.5, "neg", "pos"))
sprintf(fmt = "rpart-决策树模型==> 剪枝后在测试集上的预测精度为: %f", (which(test$diabetes == pred.test.neg.5, T) %>% length()) / nrow(test)) # 0.77

## 计算混淆矩阵
cfm <- caret::confusionMatrix(pred.train.neg.5, train$diabetes)
cfm$table
cfm
cfm <- caret::confusionMatrix(pred.test.neg.5, test$diabetes)
cfm$table
cfm

# 17 end -----------------------------------------------------------------------

# 18 rf 随机森林 ----------------------------------------------------------------
## 包含多个决策树, 默认1000
## randomForest会根据因变量的数据类型自动选择分类还是回归
## mtry参数是随机森林建模中，构建决策树分支时随机抽样的变量个数(二次方根（分类模型）或三分之一（回归模型）)。指定m值，即随机产生m个变量用于节点上的二叉树，m的选择原则是使错误率最低
## 应用bootstrap自助法在原数据集中又放回地抽取k个样本集，组成k棵决策树，每个决策树输出一个结果。
## 对k个决策树组成的随机森林对样本进行分类或预测：分类原则：少数服从多数；预测原则：简单平均。

## 1 随机森林分类
data("PimaIndiansDiabetes2", package = "mlbench")
data <- setDT(PimaIndiansDiabetes2)
head(data)
table(data$diabetes) %>% prop.table()
data$diabetes <- as.factor(data$diabetes)
sum(is.na(data)) # 有缺失值

### 缺失值探索
mice::md.pattern(data, plot = T, rotate.names = T) # 左侧数字代表缺失或不确实的行数, 右侧数字代表缺失的个数
VIM::aggr(x = data, numbers = T, prop = F)

### caret包的preProcess函数可以进行数据的标准/归一化处理，还可以进行数据缺失值的插补工作
####~ "bagImpute": 使用装袋法的方式,拟合数据
preproc <- caret::preProcess(x = data[, ! 9], method = c("bagImpute", "center", "scale"), k = 5) # "knnImpute", "bagImpute", "medianImpute"
data <- predict(preproc, data[, ! 9])
data$diabetes <- PimaIndiansDiabetes2$diabetes # 添加回标签列
####  data$age[is.na(data$age)] <- mean(data$age, na.rm = T) # 或中位数或众数(table)

mice::md.pattern(data, plot = T, rotate.names = T) # 查看缺失值==>无

### 划分数据集
set.seed(123)
index <- caret::createDataPartition(y = data$diabetes, times = 5, p = 0.7)
train <- data[index$Resample5]
test <- data[! index$Resample5]
table(train$diabetes) %>% prop.table()
table(test$diabetes) %>% prop.table()
table(data$diabetes) %>% prop.table()

### 粗看ntree参数 
model.ntree <- randomForest::randomForest(formula = diabetes ~ ., data = train, proximity = T)
model.ntree
plot(model.ntree) # 50-150

### 选择最优mgtryt和ntree参数
#### 分类默认 sqrt(8)=3个
set.seed(123)
try <- 2:(ncol(train) - 1)
ntree <- seq(50, 150, 1)

#### 选择自定义函数
mytry_and_ntree.sel.class <- function(try, ntree, train){
  # proximity:是否应计算行之间的接近度
  model.try <- randomForest::randomForest(formula = diabetes ~ ., data = train, mytry = try, ntree = ntree, proximity = T) 
  error <- data.frame(ntree = ntree , try = try, error = mean(model.try$err.rate[, 1]))
}

sel.try <- rep(try, length(ntree))
sel.ntree <- rep(ntree, length(try)) %>% sort(); sel.ntree
set.seed(123)
mytry_and_ntree <- mapply(FUN = mytry_and_ntree.sel.class, try = sel.try, ntree = sel.ntree, MoreArgs = list(train = train), SIMPLIFY = F) %>% do.call(what = "rbind", args = .); mytry_and_ntree

mytry_and_ntree.min <- which.min(mytry_and_ntree$error); mytry_and_ntree.min
ntree.min <- mytry_and_ntree$ntree[mytry_and_ntree.min]
mytry.min <- mytry_and_ntree$try[mytry_and_ntree.min]

### 建模
set.seed(123)
model <- randomForest::randomForest(formula = diabetes ~ ., data = train, ntree = ntree.min, mytry = mytry.min, proximity = T)
model
plot(model)

### 变量重要性
randomForest::importance(x = model, type = 2)
randomForest::varImpPlot(x = model, pch = 20, main = "变量重要性")

### 预测
pred <- predict(object = model, newdata = test)
sprintf(fmt = "随机森林模型在测试集上的预测精度为: %f", (which(test$diabetes == pred, T) %>% length()) / nrow(test)) # 1

## 2 随机森林回归
data("biopsy", package = "MASS")
data <- setDT(biopsy) %>% .[, ID := NULL] %>% na.omit()
names(data) <- c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
str(data)
sum(is.na(data)) # 无缺失值

### 设置训练集和测试集
### 划分数据集
set.seed(123)
index <- caret::createDataPartition(y = data$class, times = 5, p = 0.7)
train <- data[index$Resample5]
test <- data[! index$Resample5]
table(train$class) %>% prop.table()
table(test$class) %>% prop.table()
table(data$class) %>% prop.table()

### 初始拟合模型
train <- train[, ! "class"] # 去掉分类变量
test <- test[, ! "class"] # 去掉分类变量

### 粗看ntree参数 
set.seed(123)
model.ntree <- randomForest::randomForest(formula = thick ~ ., data = train, proximity = T)
model.ntree
plot(model.ntree) # 80-120

### 选择最优mgtryt和ntree参数
#### 分类默认 sqrt(8)=3个
set.seed(123)
try <- 2:(ncol(train) - 1)
ntree <- seq(80, 120, 1)

#### 选择自定义函数
mytry_and_ntree.sel.regression <- function(try, ntree, train){
  model.try <- randomForest::randomForest(formula = thick ~ ., data = train, mytry = try, ntree = ntree, proximity = T) # proximity:是否应计算行之间的接近度
  error <- data.frame(ntree = ntree , try = try, error = mean(model.try$mse))
}

sel.try <- rep(try, length(ntree))
sel.ntree <- rep(ntree, length(try)) %>% sort(); sel.ntree
set.seed(123)
mytry_and_ntree <- mapply(FUN = mytry_and_ntree.sel.regression, try = sel.try, ntree = sel.ntree, MoreArgs = list(train = train), SIMPLIFY = F) %>% do.call(what = "rbind", args = .); mytry_and_ntree

mytry_and_ntree.min <- which.min(mytry_and_ntree$error); mytry_and_ntree.min
ntree.min <- mytry_and_ntree$ntree[mytry_and_ntree.min]; ntree.min
mytry.min <- mytry_and_ntree$try[mytry_and_ntree.min]; mytry.min

set.seed(123)
fit <- randomForest::randomForest(formula = thick ~ ., data = train, ntree = ntree.min, mytry = mytry.min, proximity = T)
fit

### 变量重要性图
randomForest::varImpPlot(x = fit, scale = T, main = "huhu")
randomForest::importance(x = fit)

### 预测
predict <- predict(object = fit, newdata = test, type = "response")
predict

### 可视化
data.plot <- data.table(x = 1:nrow(test), truth = test$thick, pred = predict) %>% setorder(truth)
head(data.plot)
ggplot(data = data.plot)+
  geom_line(mapping = aes(x = x, y = truth), color = "red")+
  geom_line(mapping = aes(x = x, y = pred), color = "blue")+
  theme_test()

# 18 end ----------------------------------------------------------------------

# 19 gbm 梯度提升机 ------------------------------------------------------------
## 本原理是串行生成一系列弱学习器（weak learner tree），这些弱学习器直接通过组合到一起构成最终的模型。
## gbm(gradient boosting machine)算法是Boosting(提升)算法的一种,主要思想是，串行地生成多个弱学习器，每个弱学习器的目标是拟合先前累加模型的损失函数的负梯度，(我的理解是每次拟合上次错误的结果) 使加上该弱学习器后的累积模型损失往负梯度的方向减少。 且它用不同的权重将基学习器进行线性组合，使表现优秀的学习器得到重用。 最常用的基学习器为树模型。每轮迭代生成一个弱学习器，这个弱学习器拟合损失函数关于之前累积模型的梯度，然后将这个弱学习器加入累积模型中，逐渐降低累积模型的损失。即参数空间的梯度下降利用梯度信息调整参数，从而降低损失，而函数空间的梯度下降利用梯度，拟合一个新的函数，从而降低损失。
## 它的思想借鉴于梯度下降法，其基本原理是根据当前模型损失函数的负梯度信息来训练新加入的弱分类器，然后将训练好的弱分类器以累加的形式结合到现有模型中。采用决策树作为弱分类器的Gradient Boosting算法被称为GBDT，有时又被称为MART（Multiple Additive Regression Tree）。GBDT中使用的决策树通常为CART。

require(h2o)
## h2o并行初始化
h2o.init(nthreads = 8, max_mem_size = "12G")
data <- h2o.uploadFile(path = "D:/学习/R/R资料/R数据处理数据/wisc_bc_data_knn.csv")
head(data)
data$diagnosis <- as.factor(data$diagnosis)
str(data)


## 划分数据集
data.split <- h2o.splitFrame(data = data, ratios = 0.7, seed = 123) # 出错但运行成功
train <- data.split[[1]]; dim(train)
test <- data.split[[2]]; dim(test)

## 建模
col.names <- names(data)
x <- col.names[-1]
y <- col.names[1]

model.gbm <- h2o.gbm(x = x,
                     y = y,
                     training_frame = train, 
                     distribution = "bernoulli", # 二分类 / "AUTO": 梯度提升机gbm回归
                     ntrees = 100, 
                     learn_rate = 0.01, # 学习率 
                     sample_rate = 0.8, # 每棵树使用80%的样本
                     col_sample_rate = 0.6, # 每次拆分使用60%的特征
                     seed = 123)
summary(model.gbm)

## 模型重要性
h2o.varimp(object = model.gbm)
h2o.varimp_plot(model = model.gbm)

## 模型表现
### 1.MAE（Mean Absolute Error：平均绝对误差）
### 2.MSE（Mean Square Error:均方误差）
### 3.RMSE（Root Mean Square Error:均方根误差）
h2o.performance(model = model.gbm, newdata = test)

## 预测
predict.gbm <- as.data.frame(h2o.predict(object = model.gbm, newdata = test))
head(predict.gbm)
h2o.auc(h2o.performance(model = model.gbm, newdata = test))

## 使用参数网格搜索, 寻找更合适的模型
ntree <- c(20, 50, 100, 200, 500) # 树的数量
max_depth <- seq(2, 10, 2) # 树的最大深度
balance_classes <- c(T, F) # 是否对数据进行类别平衡

hyper <- list(ntrees = ntree,
              max_depth = maxdepth,
              balance_classes = balance)

### 超参数搜索
grid <- h2o.grid(algorithm = "gbm", 
                 grid_id = "grid.gbm",
                 hyper_params = hyper, 
                 x = x,
                 y = y,
                 distribution = "bernoulli", 
                 training_frame = train, 
                 learn_rate = 0.01)

### 查看grid输出
grid.sort <- h2o.getGrid(grid_id = "grid.gbm", sort_by = "accuracy", decreasing = T, verbose = F)
grid.sort 
grid.sort@summary_table %>% head()
grid.sort@summary_table %>% tail()

### 将grid应用于测试集 ==> 使用最好的就行
#### 根据模型id取出模型
grid_models <- lapply(grid@model_ids, function(model_id){model = h2o.getModel(model_id = model_id)})

acc <- vector()
modelid <- vector()
i <- 1
for (i in 1:length(grid_models)) {
  predict.gbm <- as.data.frame(h2o.predict(object = grid_models[[i]], newdata = test))
  modelid[i] <- grid_models[[i]]@model_id
  acc[i] <- (which(as.vector(test$diagnosis) == as.vector(predict.gbm$predict), T) %>% length()) / nrow(test)
}

data.table(modelid = modelid, acc = acc)

## 模型表现
h2o.performance(model = model.gbm, newdata = test)

# 19 end -----------------------------------------------------------------------

# 20 svm 支持向量机 -------------------------------------------------------------
## 支持向量机构建了一个超平面，使得高维特征空间内两个类的边缘间隔最大，定义超平面的向量就被称为支持向量。
## SVM的优势就在于利用了面向工程问题的核函数，能够提供准确率非常高的分类模型，同时借助增则向可以避免模型的过度适应，用户也不用担心诸如局部最优和多重共线性难题。SVM算法最重要的弊端是对模型进行训练和测试的速度很慢，模型处理需要的时间冗长，因而算法不适合应用于规模比较大的数据集，另外SVM的结果很难解释，如何确定合适的核函数也是一个难点，而正则化也是用户需要考虑的问题。
## 通过训练函数SVM，用户可以确定核函数、成本函数以及ganmma函数。对于核函数的选择，默认选择radial（径向函数: 非线性核），用户还可以选择线性核函数形状，默认为数据维度的倒数（1／数据维度），提高gamma值同城会增加支持向量的数量。考虑成本函数，默认通常为1，此时正则向也是常熟，正则向越大，边界越小
## 支持向量机的惩罚因子::支持向量机能够通过最大化边界得到一个优化的超平面以完成对训练数据的分离，不过有时算法也允许被错误分类样本的存在，惩罚因子能实现SVM对分类误差及分离边界的控制，如果控制因子比较小，分离间隔会比较大（软间隔）讲产生比较多的被错分样本；相反当加大惩罚因子时，会缩小分类间隔，从而减小错分样本。

# 1 分类/回归==>由因变量决定
data <- fread("D:/学习/R/R资料/R数据处理数据/wisc_bc_data_knn.csv")
unique(data$diagnosis)
sum(is.na(data))
data$diagnosis <- as.factor(data$diagnosis)

## 变量较多, 先进行主成分分析
### 可视化碎石土, 选择合适的主成分数
#### 碎石图是因子分析里展现因子涵盖变量信息多少的图，一般是先陡后缓，第一个因子涵盖信息最多，到后依次减少，你的图表明提取到第6个因子的时候你这提取出的因子基本上可以很全面的涵盖你原本变量的所有信息了，但最终选取几个因子还要结合方差总解释表，一般认为累计贡献率达到90%以上比较理想。
psych::fa.parallel(x = data[, ! 1], fa = "pc") # 推荐5个

## cor
cor <- cor(data[, ! 1])
dim(cor)
cor.pca5 <- psych::principal(r = cor, nfactors = 5, rotate = "cluster")

## 可视化前5个主成分与各变量的相关性
pheatmap::pheatmap(mat = cor.pca5$loadings, cluster_rows = T, cluster_cols = F)

## 使用pca模型获取数据集的5个主成分
pca.5 <- psych::predict.psych(object = cor.pca5, data = data[, ! 1], impute = "none")
dim(pca.5)
head(pca.5)

## 可视化前两个主成分
plot.data <- data.table(data$diagnosis, pc1 = pca.5[, 1], pc2 = pca.5[, 2])
head(plot.data)

ggplot(data = plot.data, mapping = aes(x = pc1, y = pc2))+
  geom_point(mapping = aes(shape = V1,color = V1))+
  theme_test()+
  theme(legend.position = "top")

## 划分数据集, 方便可视化
data <- cbind(data[, 1], pca.5); head(data)

set.seed(123)
index <- caret::createDataPartition(y = data$diagnosis, times = 5, p = 0.7) # y为分类变量
train <- data[index$Resample5]
test <- data[! index$Resample5]

table(train$diagnosis) %>% prop.table()
table(test$diagnosis) %>% prop.table()

## 建模
model.svm <- e1071::svm(formula = diagnosis ~ ., data = train, kernel = "linear")
summary(model.svm)

## 在训练集上可视化支持向量和分类面(超平)
plot(formula = RC1 ~ RC2, model.svm, train)

## 预测
pred <- predict(object = model.svm, newdata = test) 
sprintf(fmt = "支持向量机+主成分分析的预测精度为: %f", (which(as.vector(pred) == test$diagnosis, T) %>% length()) / nrow(test))
caret::confusionMatrix(pred, test$diagnosis)

## 参数探索 cost参数(违反约束的代价（默认值：1）-它是拉格朗日公式中正则化项的“C”常数)
set.seed(123)
model.svm.opt <- e1071::tune.svm(diagnosis ~ ., data = train, kernel = "linear", cost = seq(1, 20, 1))
### 可视化
plot(model.svm.opt, main = "SVM best cost parameter")
### best.parameters
model.svm.opt$best.parameters # cost=3
### best.model
model.svm.best <- model.svm.opt$best.model
summary(model.svm.best)
### 预测
pred <- predict(object = model.svm.best, newdata = test) 
sprintf(fmt = "model.svm.best的预测精度为: %f", (which(as.vector(pred) == test$diagnosis, T) %>% length()) / nrow(test))
caret::confusionMatrix(pred, test$diagnosis)

## 使用非线性核radial和添加gamma参数
set.seed(123)
model.svm.opt <- e1071::tune.svm(diagnosis ~ ., data = train, kernel = "radial", cost = seq(1, 20, 1), gamma = seq(0, 1, 0.1))
### 可视化
plot(model.svm.opt, main = "SVM best cost parameter") # 颜色代表误差
### best.parameters
model.svm.opt$best.parameters # cost=2; gamma=0.2
### best.model
model.svm.best <- model.svm.opt$best.model
summary(model.svm.best)

###  在训练集上可视化支持向量和分类面(超平)
plot(formula = RC1 ~ RC2, model.svm.best, train)

### 预测
pred <- predict(object = model.svm.best, newdata = test) 
sprintf(fmt = "model.svm.best的预测精度为: %f", (which(as.vector(pred) == test$diagnosis, T) %>% length()) / nrow(test))
caret::confusionMatrix(pred, test$diagnosis)

# 20 end -----------------------------------------------------------------------

# 21 gam 广义相加模型-----------------------------------------------------------
## GAM：Generalized Additive Model,它模型公式如下：有p个自变量，其中X1与y是线性关系，其他变量与y是非线性关系，我们可以对每个变量与y拟合不同关系，对X2可以拟合局部回归，X3采用光滑样条，不必采用统一的关系，而最终结果‘加’在一起就可以了。
## 该函数有两个组成部分，β0+β1X1部分属于参数部分，后半部分属于非参数平滑部分，因此该模型又称为半参数模型。参数部分等同于线性项或广义线性项，非参数平滑部分，则是广义相加模型的关键部分。模型的左侧与广义线性模型一样，可以是因变量本身，也可以是对因变量进行变换后的结果。
## 将自变量的单独模型相加

library(splines)
library(gam)

data("Wage", package = "ISLR")
head(Wage)

gam2 <- gam::gam(wage ~ s(age, 5) + s(year, 4) +  education, data = Wage) # s: 在gam的formala参数中表示平滑项的符号包装, 计算结果为数值向量的单变量预测器或表达式
summary(gam2)
plot(gam2, se = T, col = c('blue'))
gam3 <- gam::gam(wage ~ s(age, 5) + year + education, data = Wage)
plot(gam3, se = T, col = c('blue'))
###
gam5 <- gam(wage ~ s(age, 5) + lo(year, span = 0.7) +  education, data = Wage) # lo: 在gam的formala参数中表示平滑项的符号包装, 根据lo的参数生成的多项式的适当基础; span: 一个邻居的观察次数。
plot(gam5, se = T, col = c('blue'))

## 当Y是分类变量时，GAM同样适用
### 采用I()，将wage变量处理成二分类变量，模型的写法和第一部分稍有变化。
gam4 <- gam(I(wage > 250) ~ splines::ns(age, df = 5) + year + education, data = Wage,family = binomial) # ns: 自然样条回归。; df=knots+4, df=5, 即自动选择一个节点
plot(gam4,se=T,col=c('blue'))

# GAM模型存在的缺点：
# 1.由于模型是可加的，这在很大程度上限制了模型的灵活性，变量间的相互作用常被忽略，虽然模型本身可以考察交互作用。
# 2.由于是基于非线性的模型，因此GAM模型的系数依然难以准确解释和描述。
# 21 end -----------------------------------------------------------------------

# 22 mlp 神经网络 ---------------------------------------------------------------
## (Multi-Layer Perceptron)，即多层感知器，是一种趋向结构的人工神经网络，映射一组输入向量到一组输出向量。MLP可以被看做是一个有向图，由多个节点层组成，每一层全连接到下一层。除了输入节点，每个节点都是一个带有非线性激活函数的神经元(或称处理单元)。一种被称为反向传播算法的监督学习方法常被用来训练MLP。MLP是感知器的推广，克服了感知器无法实现对线性不可分数据识别的缺点。

# 分类
## 单隐藏层
data <- fread("D:/学习/R/R资料/R数据处理数据/wisc_bc_data_knn.csv")
unique(data$diagnosis)
sum(is.na(data))
data$diagnosis <- as.factor(data$diagnosis)

## 变量较多, 先进行主成分分析
### 可视化碎石土, 选择合适的主成分数
#### 碎石图是因子分析里展现因子涵盖变量信息多少的图，一般是先陡后缓，第一个因子涵盖信息最多，到后依次减少，你的图表明提取到第6个因子的时候你这提取出的因子基本上可以很全面的涵盖你原本变量的所有信息了，但最终选取几个因子还要结合方差总解释表，一般认为累计贡献率达到90%以上比较理想。
psych::fa.parallel(x = data[, ! 1], fa = "pc") # 推荐5个

## cor
cor <- cor(data[, ! 1])
dim(cor)
cor.pca5 <- psych::principal(r = cor, nfactors = 5, rotate = "cluster")

## 可视化前5个主成分与各变量的相关性
pheatmap::pheatmap(mat = cor.pca5$loadings, cluster_rows = T, cluster_cols = F)

## 使用pca模型获取数据集的5个主成分
pca.5 <- psych::predict.psych(object = cor.pca5, data = data[, ! 1], impute = "none")
dim(pca.5)
head(pca.5)

## 可视化前两个主成分
plot.data <- data.table(data$diagnosis, pc1 = pca.5[, 1], pc2 = pca.5[, 2])
head(plot.data)

ggplot(data = plot.data, mapping = aes(x = pc1, y = pc2))+
  geom_point(mapping = aes(shape = V1,color = V1))+
  theme_test()+
  theme(legend.position = "top")

## 划分数据集, 方便可视化
data <- cbind(data[, 1], pca.5); head(data)
head(data)

set.seed(123)
index <- caret::createDataPartition(y = data$diagnosis, times = 5, p = 0.7) # y为分类变量
train <- data[index$Resample5]
test <- data[! index$Resample5]

table(train$diagnosis) %>% prop.table()
table(test$diagnosis) %>% prop.table()

# 建模, 隐藏层可优化
mlp.single <- neuralnet::neuralnet(formula = diagnosis ~ .,
                                   data = train,
                                   hidden = 10, # 指定每层中隐藏神经元（顶点）数量的整数向量
                                   linear.output = F, # 非线性
                                   act.fct = "logistic",# 激活函数, “logistic”和“tanh”对于logistic函数和正切双曲线是可能的
                                   algorithm = "rprop+") # “rprop+”和“rprop-”是指具有和不具有权重回溯的弹性反向传播，而“sag”和“slr”则引入了改进的全局收敛算法（grprop）的使用。有关详细信息，请参阅详细信息。
mlp.single
NeuralNetTools::plotnet(mlp.single, pos_col = "red", neg_col = "grey") # 颜色代表正负, 粗细代表权重

## 预测
pred <- neuralnet::compute(x = mlp.single, covariate = test, rep = 1)
pred.class <- as.vector(ifelse(pred$net.result[, 1] > 0.5, "B", "M"))
sprintf(fmt = "单隐藏人工神经网络mlp的预测精度为: %f", (which(test$diagnosis == pred.class, T) %>% length()) / nrow(test))

# 22 end -----------------------------------------------------------------------

# 23 mars 多元自适应样条--------------------------------------------------------
## 样条回归是把数据集划分成k个连续区间，划分的点为节点，每一个连续区间都用单独的模型，线性函数或者低阶多项式函数(如二次或三次多项等)，一般称为分段函数来拟合，很明显，节点越多，模型也越灵活。
## 样条回归可以看成是分段回归，但又不是简单的分段回归，它是加了约束条件的分段回归，要求在节点处连续。上图3展示的是简单的线性样条，有些数据过于复杂，还要考虑多项式样条回归，即每个分段拟合多项式，若是多项式样条，则不仅要求在节点连续，还要求在节点一阶导数相等，这样拟合出来的曲线更光滑，连起来也很自然。

library(splines)
data("Wage", package = "ISLR")
head(Wage)
set.seed(1234)
index <- sample(1:nrow(Wage), 200)
dt <- Wage[index,]   ##加载数据集
head(dt)

# 查看wage和age的关系
ggplot(data = dt, mapping = aes(x = age, y = wage))+
  geom_point()+
  geom_smooth()+
  theme_test()

# 第一部分：分段多项式回归
onefit <- lm(wage ~ poly(age, degree = 3), data = dt, subset = (age <= 40)) # poly: 返回或计算指定点集x上的1到3次正交多项式：这些多项式都与0次的常数多项式正交。或者，计算原始多项式
twofit <- lm(wage ~ poly(age, degree = 3), data = dt, subset = (age > 40))
agelim1 <- range(min(dt$age), 40)
age_grid1 <- seq(agelim1[1], agelim1[2])
preds1 <- predict(onefit, newdata = list(age = age_grid1), se.fit = T)
se_lim1 <- cbind(preds1$fit + 2 * preds1$se.fit, preds1$fit - 2 * preds1$se.fit)

agelim2 <- range(40, max(dt$age))
age_grid2 <- seq(agelim2[1], agelim2[2])
preds2 <- predict(twofit, newdata = list(age = age_grid2), se.fit = T)
se_lim2 <- cbind(preds2$fit + 2*preds2$se.fit, preds2$fit - 2 * preds2$se.fit)

agelim <- range(dt$age)
plot(dt$age, dt$wage, xlim = agelim, cex = 0.5,col = 'gray',
     cex.axis = 0.8, cex.lab = 0.8,
     ylab = "Wage",
     xlab = "age")
lines(age_grid1, preds1$fit, lwd = 2, col = 'purple')
lines(age_grid2, preds2$fit, lwd = 2, col = 'purple')
abline(v = 40, lwd = 1, lty = 3)
# ①上述数据集是从原始工资数据集Wage随机抽取200个样本用来拟合数据。
# ②我们在age=40处，将原数据集一分为二，切成两段，分别拟合3次多项式回归。因此有两个分段回归模型，最终拟合结果见上图。
# ③ 一个很重要的问题，整个模型在x=40处不连续，为了使模型在整个取值区间连续且光滑，我们给分段多项式模型添加一个限制，即：限制性回归样条。

# 第二部分：单节点的回归样条
newwage = dt
## ##注意bs: 生成多项式样条的B样条基矩阵。
fit <- lm(wage ~ splines::bs(age,
                             degree = 2, # 对于三次样条，分段多项式的默认次数为3。
                             knots = c(40)),  # 定义样条线的内部断点
          data = newwage) 
agelim <- range(newwage$age)
age_grid <- seq(agelim[1], agelim[2])
preds <- predict(fit, newdata = list(age = age_grid), se.fit = T)
plot(newwage$age, newwage$wage, col = 'gray',
     cex.axis = 0.8, cex.lab = 0.8,
     ylab = "Wage",
     xlab = "age" )
lines(age_grid, preds$fit, col = rainbow(100)[40], lwd = 2)
abline(v = c(40),lty = 2, lwd = 1)
# 采用splines包中ns，做样条回归，由图我们的拟合的样条回归，光滑且连续。该样条还可称之为B样条，它存在的缺点是在数据的开始和结尾处，由于数据量少，导致模型的预测方差很大，可以看出95%的置信区间比较宽，由下图所示，因此引出自然样条回归。

# 第三部分：B样条vs自然样条

fit <- lm(wage ~ splines::bs(age, knots = c(25, 40, 60)), data = newwage) ##bs样条，ns 是自然样条
agelim <- range(newwage$age)
age_grid <- seq(agelim[1], agelim[2])
preds <- predict(fit, newdata = list(age = age_grid), se.fit = T)
se_lim <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

plot(newwage$age, newwage$wage, col = 'gray',
     cex.axis = 0.8, cex.lab = 0.8,
     ylab = "Wage",
     xlab = "age" )
lines(age_grid, preds$fit, col = 'red')
matlines(age_grid, se_lim, lty = 2, col = 'red')
abline(v = c(25, 40, 60), lty = 2, lwd = 1)

############自然样条ns
fit <- lm(wage ~ splines::ns(age, knots = c(25, 40, 60)), data = newwage)  
agelim <- range(newwage$age)
age_grid <- seq(agelim[1], agelim[2])
preds <- predict(fit, newdata = list(age = age_grid), se.fit = T)
se_lim <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

lines(age_grid, preds$fit,col = 'blue')
matlines(age_grid, se_lim, Ity = 2, col = 'blue')
abline(v = c(25, 40, 60), lty = 2, lwd = 0.8)
legend(x = 20, y = 290, c('cubic spline','natural cubic spline'), col = c('red', 'blue'),
       lty = 1, lwd = 1, cex = 0.8)
# ①函数bs()的使用说明：构建一个多次回归样条的操作是很简单的，将整个模型放到lm()中，bs()函数的右边是预测的变量，knots定义的是将age分成多段的节点。如果不知道选择什么节点，则可以定义df=7，表示定义3个节点，那么模型会自动选择3个节点进行拟合，其中df=节点数+4。里面还有个参数degree，表示的是多次样条回归的次数，默认是3次回归样条，可以传入其他参数，比如4和5。
# 
# ②为了解决B样条回归的边界预测误差大的问题，统计学家们又在B样条回归增加约束，这种回归成为自然样条回归，对应函数是ns()，通过上图的对比，蓝色为自然样条回归，红色为B样条回归，蓝色的虚线间距比红色的虚线间距窄，尤其是在age的两端，表明自然回归在age的边界处得到的结果更加稳健。

# 23 end -----------------------------------------------------------------------

# 24 mlr3 ----------------------------------------------------------------------
require(mlr3verse) # 加载mlr3Verse包通常更方便。Mlr3verse导入大多数mlr3包，并重新导出用于常见机器学习和数据科学任务的函数。
require(mlr3hyperband)
libs <- c("data.table", "magrittr")
purrr::walk(libs, require, character.on = T)

mlr_tasks %>% as.data.table()
mlr_learners
mlr_measures
mlr_resamplings %>% as.data.table()
mlr_filters %>% as.data.table()
mlr_fselectors %>% as.data.table()
mlr_terminators %>% as.data.table()
mlr_tuners %>% as.data.table()
mlr_pipeops %>% as.data.table()

# 超参数
## cp: 控制lrn何时考虑引入另一个分支。 [rpart]
## minsplit: 控制一个分支中必须存在多少个观测值才能尝试另一次拆分。[rpart]
## cost:  。[svm]
## kernel:  。[svm]
## degree:  kernel == "polynomial"。[svm]
## Class.weights:  。[svm]

# 1 quickstart
## 构建任务, 以元数据的形式封装数据
task <- tsk(.key = "iris") # 传递到相应字典以检索对象的键。
task
class(task) # "TaskClassif", "TaskSupervised", "Task", "R6" 

## 构建学习器
learner <- lrn(.key = "classif.rpart")
learner
class(learner) # "LearnerClassifRpart", "LearnerClassif", "Learner", "R6"

## 建模
# model <- learner$train(task = task, row_ids = 1:120) # 不行, 只能学习器本身
learner$train(task = task, row_ids = 1:120)
# model <- learner$model # 不建议
# model
learner$model
learner$param_set

## 预测
pred <- learner$predict(task = task, row_ids = 131:150)
pred

## 计算预测精度
pred$score(measures = msr(.key = "classif.acc")) # 0.95

# 2 任务 Task类
## 从df, dt或matrix创建任务
data("mtcars", package = "datasets")
?mtcars # 查看数据集的信息
data <- mtcars[, 1:3]
head(data)
str(data)

## 构造任务
task.mtcars <- as_task_regr(x = data, target = "mpg", id = "mtcars.id", label = "mtcars.label")
task.mtcars # 推荐

task.mtcars <- TaskRegr$new(id = "mtcars.id", backend = data, target = "mpg", label = "mtcars.label")
task.mtcars

## 可视化
mlr3viz::autoplot(object = task.mtcars, type = "pairs")+
  ggplot2::theme_test()

## 预定义的任务
mlr_tasks
as.data.table(mlr_tasks)

task_penguins <- tsk(.key = "penguins") # 帕尔默企鹅
task_penguins
task_penguins$help()

## 任务函数和属性
task$backend # 后端数据,一般不会变, 包括row_id
task$data(cols = "Species") %>% unique() # Properties: 多分类
task$data(rows = c(1:10, 20:30))
task$nrow
task$head()
task$col_roles
task$col_info
task$target_names
task$feature_names
task$feature_types
task$row_ids
task$data_formats # data.table
task$help()
task$id
task$label
task$levels() # 因子变量level
summary(as.data.table(task))

## 二分类任务, 要设置positive和negativa, 不设置的话函数会默认设置第一个level为positive
data("Sonar", package = "mlbench")
str(Sonar)
task = as_task_classif(Sonar, target = "Class", positive = "R", id = "Sonar.id", label = "Sonar.label")
task
# switch positive class to level 'M'
task$positive = "M"

## role: 行和列的角色, 改变role不会改变底层数据, 但视图发生变化, 即任务也发生变化
### col_roles
task.mtcars$col_roles
task.mtcars$col_roles %>% names() # 列的所有角色名

data <- as.data.table(datasets::mtcars[, 1:3], keep.rownames = T)
head(data)
task.mtcars <- as_task_regr(x = data, target = "mpg", id = "mtcars.id", label = "mtcars.label")
task.mtcars$feature_names # 多了一个新的feature rn
####  设置rn为role: name, task.mtcars$row_names会显示
task.mtcars$set_col_roles(cols = "rn", roles = "name")
task.mtcars$col_roles
task.mtcars$feature_names
task.mtcars$head(n = 3)

### row_roles, 任务创建时因变量列有缺失值的行自动设置为validation(验证)
task.mtcars$row_roles
task.mtcars$row_roles %>% names() # 行的所有角色名, 默认为use
task.mtcars$set_row_roles(rows = 1:10, roles = "test")
task.mtcars$row_roles

## 任务变体, select和filter只会改变显示, 不会改变底层数据(backend), cbind/rbind都会改变
task_penguins <- tsk(.key = "penguins") # 帕尔默企鹅
task_penguins$backend
task_penguins$feature_names
task_penguins$data(cols = c("body_mass", "flipper_length")) %>% head() # 显示
### 选择列
task_penguins$select(cols = c("body_mass", "flipper_length")) # 只保留这两个特征量
task_penguins$feature_names
task_penguins$head()
task_penguins$backend
### 选择行
task_penguins$filter(rows = 1:3)
task_penguins$data()
task_penguins$backend
### 合并其它数据框cbind/rbind
task_penguins$cbind(data = data.table(letters = letters[1:3]))
task_penguins$data()
task_penguins$backend
task_penguins$rbind(data = data.table())

## 画任务
mlr_tasks

### 分类任务
task <- tsk(.key = "pima")
task$select(cols = head(x = task$feature_names, n = 3)) # 保留前3个特征量
task$head()

mlr3viz::autoplot(task)
mlr3viz::autoplot(task, type = "pairs")
mlr3viz::autoplot(task, type = "duo")

### 回归任务
task = tsk("mtcars")
task$head()
task$target_names
task$select(cols = head(x = task$feature_names, n = 3)) # 保留前3个特征量
task$feature_names

mlr3viz::autoplot(task)
mlr3viz::autoplot(task, type = "pairs")

# 3 学习器 Learner类
# remotes::install_github("mlr-org/mlr3extralearners@*release")
# head(mlr3extralearners::list_mlr3learners()) # 额外的机器学习方法
# https://mlr3extralearners.mlr-org.com/articles/learners/test_overview.html # 额外的机器学习方法的测试情况
mlr_learners
learner = lrn("classif.rpart")
learner
## 超参数
learner$param_set # parents字段为该超参数的依赖项，但不具体【Search_space()自动从底层的参数集中提取依赖项，因此，如果内核被调优，那么Degree会自动获得对它的依赖，而不必我们指定它。】
learner$param_set$values <- list(cp = 0.01, xval = 0) # 修改超参数
learner$param_set
learner
### 修改超参数
pv <- learner$param_set$values
pv$cp <- 0.02
learner$param_set$values <- pv
learner
learner <- lrn(.key = "classif.rpart", id = "rp.id", label = "rp.label", cp = 0.001)
learner$id
learner$param_set$values
## 调整阈值
data("Sonar", package = "mlbench")
tsk_Sonar <- as_task_classif(x = Sonar, target = "Class", positive = "M")
tsk_Sonar
lrn_Sonar <- lrn(.key = "classif.rpart", predict_type = "prob")
lrn_Sonar
pred_Sonar <- lrn_Sonar$train(task = tsk_Sonar)$predict(tsk_Sonar)
pred_Sonar
class(pred_Sonar)
mlr_measures
msrs_Sonar <- msrs(.keys = c("classif.tpr", "classif.tnr"))
msrs_Sonar
pred_Sonar$confusion
pred_Sonar$score(measures = msrs_Sonar)
pred_Sonar$set_threshold(threshold = 0.2) # 默认0.5
pred_Sonar$confusion
pred_Sonar$score(measures = msrs_Sonar)

# 4 训练、预测、评估
tsk_penguins <- tsk(.key = "penguins")
tsk_penguins
train_set <- sample(x = tsk_penguins$nrow, size = tsk_penguins$nrow * 0.8)
test_set <- setdiff(x = seq_len(tsk_penguins$nrow), train_set)

lrn_penguins <- lrn(.key = "classif.rpart", id = "Penguins_classif.rpart", label = "Penguins_Classification.Tree")
lrn_penguins
lrn_penguins$model

lrn_penguins$train(task = tsk_penguins, row_ids = train_set)
lrn_penguins$model

## 预测
pred_penguins <- lrn_penguins$predict(task = tsk_penguins, row_ids = test_set)
pred_penguins
class(pred_penguins)
head(as.data.table(pred_penguins))
## 混淆矩阵
pred_penguins$confusion
pred_penguins$score()
## 改变预测类型
lrn_penguins$predict_type <- "prob"
lrn_penguins$train(task = tsk_penguins, row_ids = train_set)
pred_penguins <- lrn_penguins$predict(task = tsk_penguins, row_ids = test_set)
pred_penguins
pred_penguins$confusion
pred_penguins$score()
head(pred_penguins$response) # 预测标签结果
head(pred_penguins$prob) # 预测概率结果
# 画预测结果
autoplot(object = pred_penguins)

## 评价
mlr_measures
msr_penguins <- msr(.key = "classif.acc") # 精度, 默认classif.ce 错误率(与classif.acc相反)，回归任务为regr.mse(均方误差)
msr_penguins
pred_penguins$score(measures = msr_penguins)

# 5 模型评价与比较
## ROC分析适合二分类任务
data("Sonar", package = "mlbench")
tsk_Sonar <-  as_task_classif(Sonar, target = "Class", positive = "R", id = "Sonar.id", label = "Sonar.label")
tsk_Sonar
lrn_Sonar <- lrn(.key = "classif.rpart", predict_type = "prob")
lrn_Sonar
pred_Sonar <- lrn_Sonar$train(task = tsk_Sonar)$predict(tsk_Sonar)
pred_Sonar
pred_Sonar$confusion
### ROC曲线
#### 不同标签的ROC曲线相对于对角线是对称的，所以在对角线下的曲线表明分类器已经切换了正和负类别标签。
#### 横轴FPR: FP/(FP+TN), 1-TNR,1-Specificity，FPR越大，预测正类中实际负类越多。
#### 纵轴TPR: TP/(TP+FN),Sensitivity(正类覆盖率),TPR越大，预测正类中实际正类越多。又是召回率recall
#### PPV: 如果我们预测是正面的，那么真正正面的可能性有多大？
#### NPV: 如果我们的预测是负面的，那么真正负面的可能性有多大？
autoplot(pred_Sonar, type = "roc") # TPR vs. FPR
### PRC曲线，对于不平衡的人群，PRC比ROC曲线更受欢迎
autoplot(pred_Sonar, type = "prc") # PPV vs. TPR

## 6 重采样: 比较相同的任务和模型在不同采样情况下的预测情况
# • cross validation ("cv"),
# • leave-one-out cross validation ("loo"),
# • repeated cross validation ("repeated_cv"),
# • bootstrapping ("bootstrap"),
# • subsampling ("subsampling"),
# • holdout ("holdout"),
# • in-sample resampling ("insample"), and
# • custom resampling ("custom").

tsk <- tsk(.key = "penguins")
lrn <- lrn(.key = "classif.rpart")
lrn$param_set

as.data.table(mlr_resamplings)
rsmp <- rsmp(.key = "holdout")
rsmp
## 修改训练集和测试集比例
rsmp <- rsmp(.key = "holdout", ratio = 0.8)
rsmp
rsmp$param_set$values$ratio <- 0.8
rsmp$param_set$values <- list(ratio = 0.8)
rsmp

## 重采样实例化
rsmp$is_instantiated # F
rsmp$instantiate(task = tsk)
rsmp$train_set(i = 1)
str(rsmp$test_set(i = 1))

## 执行
tsk <- tsk(.key = "penguins")
lrn <- lrn(.key = "classif.rpart", maxdepth = 3, predict_type = "prob")
lrn$param_set
lrn$param_set$values %>% unlist()
rsmp <- rsmp(.key = "cv", folds = 6)
rsmp$param_set
rsmp$param_set$values
### 使用三次交叉验证重采样，即在6个不同的训练和测试集上进行训练和评估
rr <- resample(task = tsk, learner = lrn, resampling = rsmp, store_models = T)
rr
class(rr)
### 根据分类误差计算所有重采样迭代的平均性能：
rr$aggregate(measures = msr("classif.ce"))
### 提取各个重采样迭代的性能
rr$score(measures = msr("classif.ce"))
### 警告和错误
rr$warnings
rr$errors
### 迭代次数
rr$resampling$iters
### 获取某次迭代的数据行数
str(rr$resampling$train_set(i = 6))
str(rr$resampling$test_set(i = 1))
### 获取某次迭代的模型
lrn.3 <- rr$learners[[3]]
lrn.3$model
### 获取预测结果
preds <- rr$prediction() # 所有预测合并到单个预测对象中
preds
pred.3 <- rr$predictions()[[5]] # 单个预测结果
pred.3
### 过滤结果以仅保留指定的重采样迭代次数
rr
rr$filter(iters = c(1, 3))
rr

### Custom resampling 自定义重采样 ==> 例如重现某研究中的案例
rsmp <- rsmp(.key = "custom")
rsmp$instantiate(task = tsk, train_sets = list(c(1:10, 51:60, 101:110)), test_sets =  list(c(11:20, 61:70, 111:120)))
rsmp$train_set(i = 1)
rsmp$iters

### 画重采样
# https://mlr3viz.mlr-org.com/reference/autoplot.ResampleResult.html
tsk_pima <- tsk(.key = "pima")
tsk_pima$select(cols = c("glucose", "mass"))
lrn_pima <- lrn(.key = "classif.rpart", predict_type = "prob")
rsmp_pima <- rsmp(.key = "cv") # 默认10次迭代
rr <- resample(task = tsk_pima, learner = lrn_pima, resampling = rsmp_pima, store_models = T) # 不用手动实例化
class(rr)

autoplot(object = rr, measure = msr(.key = "classif.auc"))
autoplot(object = rr, type = "roc")
autoplot(object = rr, type = "prediction") # 画预测
rr$filter(iters = 1)
autoplot(object = rr, type = "prediction")

# 7 Benchmark: 比较不同模型在多个任务和/或不同重采样方案上的表现
design <- benchmark_grid(tasks = tsks(.keys = c("spam", "german_credit", "sonar")),
                         learners = lrns(.keys = c("classif.ranger", "classif.rpart", "classif.featureless"), predict_type = "prob", predict_sets = c("train", "test")),
                         resamplings = rsmps(.keys = "cv", folds = 3))
design
class(design)

bmr <- benchmark(design = design) # 不用手动实例化
class(bmr)

measures <- list(
  msr(.key = "classif.auc", predict_sets = "train", id = "auc.train"),
  msr(.key = "classif.auc", id = "auc.test")
)

## 计算精度
tab <- bmr$aggregate(measures = measures)
tab
## 精度排序
ranks = tab[, .(learner_id, rank_train = rank(-auc.train), rank_test = rank(-auc.test)), by = task_id]
ranks
ranks = ranks[, .(mrank_train = mean(rank_train), mrank_test = mean(rank_test)), by = learner_id]
ranks[order(mrank_test)]

## 画benchmarking
autoplot(bmr) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

bmr_small = bmr$clone()$filter(task_id = "german_credit") # 用$clone删选不会改变原值
autoplot(bmr_small, type = "roc")

## 提取重采样结果
tab <- bmr$aggregate(measures = measures)
class(tab)
rr <- tab[task_id == "german_credit" & learner_id == "classif.ranger"]$resample_result[[1]]
rr
### 研究该重采样结果
measure <- msr(.key = "classif.auc")
rr$aggregate(measures = measure)
autoplot(rr, type = "roc")
perf <- rr$score(measures = measure)
perf
rr$learners[[1]]
i <- which.min(x = perf$classif.auc)
rr$learners[[i]]
head(rr$resampling$train_set(i))

## 转换和合并 (resample的结果可以转换为benchmarking的结果)
tsk_iris <- tsk(.key = "iris")
resampling <- rsmp(.key = "holdout")$instantiate(tsk_iris)
class(resampling)

rr1 <- resample(task = tsk_iris, learner = lrn(.key = "classif.rpart"), resampling = resampling)
rr2 <- resample(task = tsk_iris, learner = lrn(.key = "classif.featureless"), resampling = resampling)

bmr1 <- as_benchmark_result(rr1)
bmr2 <- as_benchmark_result(rr2)

### 合并bmr1和bmr2
bmr1$combine(bmr2)
bmr1 # 后续需要计算精度 $aggregate / $score

# 8 模型优化（调参） mlr3tuning包
tsk_pima <- tsk(.key = "pima")
tsk_pima
lrn_pima <- lrn(.key = "classif.rpart")
lrn_pima$param_set

## tune space 调整空间
search_space_pima <- ps( # 经验之谈, ParamSet类
  cp = p_dbl(lower = 0.001, upper = 0.1),
  minsplit = p_int(lower = 1, upper = 10)
)
search_space_pima; class(search_space_pima)
rsmp_pima <- rsmp(.key = "holdout")
msr_pima <- msr(.key = "classif.ce")

### 因为不可能穷尽所有可能，所以需要设置预算，即终止器
i_20_pima <- trm(.key = "evals", n_evals = 20) # Terminator: 迭代20次

instance_tune_pima <- TuningInstanceSingleCrit$new(task = tsk_pima,
                                                   learner = lrn_pima,
                                                   resampling = rsmp_pima,
                                                   measure = msr_pima,
                                                   search_space = search_space_pima,
                                                   terminator = i_20_pima)
instance_tune_pima

### 优化算法 https://en.wikipedia.org/wiki/Hyperparameter_optimization
# • Grid Search (TunerGridSearch)
# • Random Search (TunerRandomSearch) (Bergstra and Bengio 2012)
# • Generalized Simulated Annealing (TunerGenSA)
# • Non-Linear Optimization (TunerNLoptr)

### batch size: 表示单次传递给程序用以训练的数据(样本)个数，默认为1
tuner_pima <- tnr(.key = "grid_search", resolution = 5, batch_size = 8) # 分辨率为5的二维网格由5*5=25组态组成。
tuner_pima$param_set; class(tuner_pima)

### 执行调参
tuner_pima$optimize(inst = instance_tune_pima)

#### 查看所有调参的网格参数
as.data.table(instance_tune_pima$archive)

#### benchmark
instance_tune_pima$archive$benchmark_result

#### 计算精度
instance_tune_pima$archive$benchmark_result$score(msr(.key = "classif.acc"))

#### 将最优参数instance$result_learner_param_vals赋值给lrn进行训练
instance_tune_pima$result_y # 最优参数相应的测量性能
lrn_pima$param_set$values <- instance_tune_pima$result_learner_param_vals
lrn_pima$train(task = tsk_pima)
#### 要获得给定任务的统计无偏性能估计，需要嵌套重采样(nested resampling)。

### 多测量标准调参（如最小分类错误(classif.ce)和最小模型训练时间(time_train)）
msrs_pima <- msrs(.keys = c("classif.ce", "time_train"))
evals20_pima <- trm(.key = "evals", n_evals = 20) # 终止器
instance_tune_pima <- TuningInstanceMultiCrit$new(task = tsk_pima,
                                                  learner = lrn_pima,
                                                  resampling = rsmp_pima,
                                                  measures = msrs_pima,
                                                  search_space = search_space_pima,
                                                  terminator = evals20_pima)
instance_tune_pima
#### 在触发调整后，我们将拥有具有最佳分类误差和训练模型的时间的配置。
tuner_pima$optimize(inst = instance_tune_pima)

instance_tune_pima$result_learner_param_vals
instance_tune_pima$result_y

## 自动调参 ,（不用调整好参数后再赋值给lrn），自动调参的结果对象继承自Learner, 所以可以直接训练数据
### 貌似不能multi
auto_tune_pima <- AutoTuner$new(learner = lrn_pima,
                                resampling = rsmp_pima,
                                measure = msr_pima,
                                terminator = evals20_pima,
                                tuner = tnr(.key = "random_search"), # 优化算法
                                search_space = search_space_pima)
auto_tune_pima
auto_tune_pima$train(task = tsk_pima)
auto_tune_pima$model

## 调整查找空间
### 可用的搜索空间参数类型有限：连续、整数、离散和逻辑标量
### 创建空超参数
search_space <- ps()
search_space; class(search_space)

lrn_svm <- lrn(.key = "classif.svm")
lrn_svm$param_set

search_space_svm <- ps(
  cost = p_dbl(lower = 0.1, upper = 10),
  kernel = p_fct(levels = c("polynomial", "radial")) # P_fct参数允许的类别值
)
search_space_svm

### 转换 trafo参数
#### resolution = 3, 只对数值型超参数管用
rbindlist(generate_design_grid(param_set = search_space_svm, resolution = 3)$transpose()) # $transpose()当前无转换
#### 我们注意到，成本参数是在线性范围内取的。然而，我们假设0.1和1之间的成本差异应该与1和10之间的差异具有类似的效果。因此，在对数范围内调整它更有意义。这是通过使用转换(Trafo)来完成的。这是在调谐器对参数进行采样后应用于该参数的函数。我们可以在对数标度上调整成本，方法是在线性标度[-1，1]上采样并从该值计算10^x。

search_space_svm <- ps(
  cost = p_dbl(lower = -1, upper = 1, trafo = function(x) 10^x),
  kernel = p_fct(levels = c("polynomial", "radial"))
)
rbindlist(generate_design_grid(param_set = search_space_svm, resolution = 3)$transpose()) # $transpose() -1, 1转换到0.1, 1

#### 根据参数修改参数
search_space_svm <- ps(
  cost = p_dbl(lower = -1, upper = 1, trafo = function(x) 10^x),
  kernel = p_fct(levels = c("polynomial", "radial")),
  .extra_trafo = function(x, param_set){
    if (x$kernel == "polynomial") {
      x$cost = x$cost * 2
    }
    return(x)
  }
)
rbindlist(generate_design_grid(param_set = search_space_svm, resolution = 3)$transpose())

#### 然而，有许多机器学习算法采用其他类型的参数，例如向量或函数。这些参数不能在搜索空间参数集中定义，通常在学习者的参数集中作为参数Uty给出。尝试调整这些超参数时，需要执行更改参数类型的转换。
##### 一个例子是支持向量机(SVM)的Class.weights参数，它接受一个类权重的命名向量，每个目标类有一个条目。调优tsk(\“spam\”)数据集的Class.Weight的trafo可能是：
search_space = ps(
  class.weights = p_dbl(0.1, 0.9, trafo = function(x) c(spam = x, nonspam = 1 - x))
)
generate_design_grid(search_space, 5)$transpose()

search_space = ps( # 手动
  class.weights = p_fct(
    list(
      candidate_a = c(spam = 0.5, nonspam = 0.5),
      candidate_b = c(spam = 0.3, nonspam = 0.7)
    )
  )
)
generate_design_grid(search_space)$transpose()

#### 因子自动转换
search_space = ps(
  cost = p_fct(c(0.1, 3, 10)), # 只想使用0.1, 3和10
  kernel = p_fct(c("polynomial", "radial"))
)
rbindlist(generate_design_grid(param_set = search_space, resolution = 3)$transpose())
rbindlist(generate_design_grid(param_set = search_space, resolution = 5)$transpose()) # 因为是因子型，写成5也是那三个
rbindlist(generate_design_grid(param_set = search_space)$transpose()) # resolution不指定也行
typeof(search_space$params$cost$levels)

### 参数依赖 depends参数
#### 某些参数仅在另一个参数具有特定值或多个值之一时才相关，例如，支持向量机的degree参数只有在核为polynomial时才有效。
search_space <- ps(
  cost = p_dbl(lower = -1, upper = 1, trafo = function(x) 10^x),
  kernel = p_fct(levels = c("polynomial", "radial")),
  degree = p_int(lower = 1, upper = 3, depends = kernel == "polynomial") # 参数依赖其他参数，即只在在他参数存在时存在
)
rbindlist(generate_design_grid(param_set = search_space, resolution = 3)$transpose(), fill = T)

### 从lrn的超参数生成调整参数: to_tune 可以嵌套p_***()设置ps()
lrn_svm <- lrn(.key = "classif.svm")
lrn_svm$param_set$values$kernel = "polynomial"
lrn_svm$param_set$values
lrn_svm$param_set$values$degree <- to_tune(lower = 1, upper = 3)
lrn_svm$param_set$values
lrn_svm$param_set$search_space() # 查询寻找空间
#### 列出lrn的查找空间
rbindlist(generate_design_grid(param_set = lrn_svm$param_set$search_space(), resolution = 3)$transpose())
#### 这里可以省略下限，因为它可以从Degree参数本身的下限中推断出来。对于已经有界的其他参数，可以根本不给出任何界，因为它们的范围已经有界。逻辑收缩超参数就是一个例子：
lrn_svm$param_set$values$shrinking = to_tune()
lrn_svm$param_set$search_space()
#### to_tune里可以嵌套p_***()函数 
lrn_svm$param_set$values$type <- "C-classification" # 因为paradox中的一个错误而需要设置
lrn_svm$param_set$values$cost <- to_tune(c(val1 = 0.3, val2 = 0.7)) # 调用了p_fct
lrn_svm$param_set$values$shrinking <- to_tune(p_lgl(depends = cost == "val2"))
lrn_svm$param_set$values
lrn_svm$param_set$search_space() 
#### Search_space()自动从底层的参数集中提取依赖项。因此，如果内核被调优，那么Degree会自动获得对它的依赖，而不必我们指定它。
lrn_svm$param_set$values$cost = NULL
lrn_svm$param_set$values$shrinking = NULL
lrn_svm$param_set$values$kernel = to_tune(c("polynomial", "radial"))
lrn_svm$param_set$search_space() 
rbindlist(generate_design_grid(lrn_svm$param_set$search_space(), 3)$transpose(), fill = TRUE) # 证明了自动获取依赖

#### 甚至可以定义针对单个参数进行调整的整个参数集。
lrn_svm$param_set$values$class.weights <- to_tune(
  ps(
    spam = p_dbl(lower = 0.1, upper = 0.9),
    nonspam = p_dbl(lower = 0.1, upper = 0.9),
    .extra_trafo = function(x, param_set) list(c(spam = x$spam, nonspam = x$nonspam))
  )
)
generate_design_grid(param_set = lrn_svm$param_set$search_space(), resolution = 3)$transpose()

## 嵌套重采样
### 当必须选择超参数或特征时，评估机器学习模型通常需要额外的重采样。嵌套重采样将这些模型选择步骤与评估模型性能的过程分开。如果将相同的数据用于模型选择步骤和模型本身的评估，则得到的模型性能估计可能会严重偏差。这种偏差的一个原因是，对测试数据重复评估模型可能会将有关其结构的信息泄露到模型中，这会导致过于乐观的性能估计。
### 请记住，嵌套重采样是一种统计过程，用于估计在完整数据集上训练的模型的预测性能。嵌套重采样不是选择最佳超参数的过程。重采样产生了许多不应用于构建最终模型的超参数配置(Simon 2007)。
### 包括外部重采样训练集（训练集（内部重采样训练集 + 内部重采样训练集）+ 测试集）+ 外部重采样测试集；内部用于超参数调整，外部用于模型拟合
lrn_pima <- lrn(.key = "classif.rpart")
rsmp_inner_pima <- rsmp(.key = "cv", folds = 4) # 内部重采样
msr_pima <- msr(.key = "classif.ce")
search_space_pima <- ps(
  cp = p_dbl(lower = 0.001, upper = 1)
)
trm_pima <- trm(.key = "evals", n_evals = 5)
tnr_pima <- tnr(.key = "grid_search", resolution = 10, batch_size = 8)
auto_tnr_pima <- AutoTuner$new(learner = lrn_pima, # 自动调参是内部重采样, 返回一个lrn对象
                               resampling = rsmp_inner_pima,
                               measure = msr_pima,
                               terminator = trm_pima,
                               tuner = tnr_pima,
                               search_space = search_space_pima)
auto_tnr_pima$param_set$search_space()
#### 参数调整结果
auto_tnr_pima$tuning_result

tsk_pima <- tsk(.key = "pima")
rsmp_outer_pima <- rsmp(.key = "cv", folds = 3)

rr_pima <- resample(task = tsk_pima,
                    learner = auto_tnr_pima,
                    resampling = rsmp_outer_pima,
                    store_models = T)
rr_pima; class(rr_pima)

extract_inner_tuning_archives(x = rr_pima)
### 提取内部调参结果，我们报告嵌套重采样估计的性能作为模型的性能
extract_inner_tuning_results(x = rr_pima)
rr_pima$score()

### 所有外部重采样迭代的聚合性能实质上是网格搜索找到的具有最优超参数的模型的无偏性能
rr_pima$aggregate()
rr_pima$prediction()
rr_pima$predictions()

### 训练模型(应该不用==》resample训练过了)
auto_tnr_pima$train(task = tsk_pima)
auto_tnr_pima$model

## Hyperband调参 ==》是一种面向预算的过程，在部分顺序训练过程中早期剔除性能不佳的配置，从而提高调参效率，为此，使用了增量资源分配和提前停止的组合 mlr3hyperband包
search_space <- ps(
  nrounds = p_int(lower = 1, upper = 6, tags = "budget"),
  booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
)
search_space

set.seed(123)
lrn_new <- po(.obj = "subsample") %>>% lrn(.key = "classif.rpart")
class(lrn_new)
lrn_new$param_set

search_space = ps(
  classif.rpart.cp = p_dbl(lower = 0.001, upper = 0.1), # 可以写全机器学习方法
  classif.rpart.minsplit = p_int(lower = 1, upper = 10),
  subsample.frac = p_dbl(lower = 0.1, upper = 1, tags = "budget")
)

instance = TuningInstanceSingleCrit$new( # 也可TuningInstanceMultiCrit$new
  task = tsk("iris"),
  learner = lrn_new,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("none"), # hyperband terminates itself
  search_space = search_space
)
instance

require(mlr3hyperband)
tuner <- tnr(.key = "hyperband", eta = 3)
tuner; class(tuner)
lgr::get_logger("bbotk")$set_threshold("warn")#减少日志输出
tuner$optimize(inst = instance)
#### 接收每个采样配置的结果
as.data.table(instance$archive)[, c(
  "subsample.frac",
  "classif.rpart.cp",
  "classif.rpart.minsplit",
  "classif.ce"
), with = FALSE]

instance$result_learner_param_vals
instance$result_y

#### 最佳配置
instance$result

## 特征选择或过滤
### Filtering: 外部算法计算特征的等级(例如，基于与响应的相关性)。然后，特征由特定标准子集，例如变量数量的绝对数或百分比。然后，所选特征将用于拟合模型(通过调整选择可选的超参数)。就计算时间而言，这种计算通常比“特征子集选择”更便宜。所有过滤器都通过mlr3过滤器包连接
### Wrapper Methods: 在这里，没有对功能进行排名。取而代之的是，优化算法选择特征的子集，通过计算重采样的预测性能来评估该集合，然后提出新的特征集合(或终止)。一个简单的例子是顺序向前选择。这种方法通常计算量很大，因为需要拟合大量的模型。此外，严格地说，在估计性能之前，所有这些模型都需要进行调整。这将需要在CV设置中附加嵌套级别。在执行所有这些步骤之后，再次拟合所选特征的最终集合(通过调整选择可选的超参数)。包装器方法在mlr3fselect包中实现
### Embedded Methods: 嵌入式方法,许多learns在内部选择他们认为对预测有帮助的特征的子集。通常可以查询这些子集
tsk_iris = tsk("iris")
lrn_iris = lrn("classif.rpart")
# ensure that the learner selects features
stopifnot("selected_features" %in% lrn_iris$properties)
# fit a simple classification tree
lrn_iris$train(tsk_iris)
# extract all features used in the classification tree:
lrn_iris$selected_features()

### Filters
#### 过滤器方法为每个特征量分配一个重要性值。基于这些值，可以对特征进行排序。此后，我们能够选择特征子集。附录中列出了所有已实现的过滤器方法。目前仅支持分类和回归任务。
mlr_filters

flt_iris <- flt(.key = "jmim")
flt_iris; class(flt_iris)
flt_iris$param_set
tsk_iris <- tsk(.key = "iris")
#### 过滤计算
flt_iris$calculate(task = tsk_iris)
flt_iris %>% as.data.table()
#### 某些筛选器支持更改特定的超参数
flt_cor_iris <- flt(.key = "correlation")
flt_cor_iris$param_set
##### 修改超参数
flt_cor_iris$param_set$values <- list(method = "spearman")
flt_cor_iris$param_set
#### 变量重要性过滤
##### 要使用“impurity”方法，需要在构造时设置过滤方法。
lrn_iris = lrn("classif.ranger", importance = "impurity")
tsk_iris <- tsk(.key = "iris")
flt_iris <- flt(.key = "importance", learner = lrn_iris)
flt_iris$calculate(task = tsk_iris)
flt_iris

### Wrapper Methods
tsk_pima <- tsk(.key = "pima")
lrn_pima <- lrn(.key = "classif.rpart")
#### 接下来，我们需要指定如何评估功能子集的性能。为此，我们需要选择重采样策略和业绩衡量标准。
rsmp_pima <- rsmp(.key = "holdout")
msr_pima <- msr(.key = "classif.ce")
#### 最后，必须为功能选择选择可用的预算。这可以通过选择可用的终止器之一来完成：
#####  • Terminate after a given time (TerminatorClockTime)
#####  • Terminate after a given amount of iterations (TerminatorEvals)
#####  • Terminate after a specific performance is reached (TerminatorPerfReached)
#####  • Terminate when feature selection does not improve (TerminatorStagnation)
#####  • A combination of the above in an ALL or ANY fashion (TerminatorCombo)

trm_pima <- trm(.key = "evals", n_evals = 20)
instance_flt_pima <- FSelectInstanceSingleCrit$new(task = tsk_pima,
                                                   learner = lrn_pima,
                                                   resampling = rsmp_pima,
                                                   measure = msr_pima,
                                                   terminator = trm_pima)
instance_flt_pima
#### 要开始要素选择，我们仍然需要选择通过FSelector类定义的算法
##### • Random Search (FSelectorRandomSearch)
##### • Exhaustive Search (FSelectorExhaustiveSearch)
##### • Sequential Search (FSelectorSequential)
##### • Recursive Feature Elimination (FSelectorRFE)
##### • Design Points (FSelectorDesignPoints)
mlr_fselectors

fs_pima <- fs(.key = "random_search", batch_size = 8)
fs_pima; class(fs_pima)
fs_pima$param_set

#### 发动调整
##### 降低日志输出
lgr::get_logger("bbotk")$set_threshold("warn")
##### 优化
fs_pima$optimize(inst = instance_flt_pima)
##### 结果
instance_flt_pima$result_feature_set
instance_flt_pima$result_y
##### 所有运行档案
as.data.table(instance_flt_pima$archive)
##### 访问相关的重采样迭代：
instance_flt_pima$archive$benchmark_result
##### 现在，可以使用优化的特征子集来设置任务子集，并将模型拟合到所有观测数据上
tsk_pima$feature_names
tsk_pima$select(cols = instance_flt_pima$result_feature_set)
lrn_pima$train(tsk_pima)
lrn_pima$model

#### 使用多个性能指标进行过滤
msrs_pima = msrs(c("classif.ce", "time_train"))
evals20 = trm("evals", n_evals = 20)
instance_flts_pima <- FSelectInstanceMultiCrit$new(task = tsk_pima,
                                                   learner = lrn_pima,
                                                   resampling = rsmp_pima,
                                                   measure = msrs_pima,
                                                   terminator = trm_pima)
instance_flts_pima
lgr::get_logger("bbotk")$set_threshold("warn")
fs_pima$optimize(instance_flts_pima)
##### 结果
instance_flts_pima$result_feature_set
instance_flts_pima$result_y
##### 所有运行档案
as.data.table(instance_flts_pima$archive)

#### 自动特征选择
learner = lrn("classif.rpart")
terminator = trm("evals", n_evals = 10)
fselector = fs("random_search")
at = AutoFSelector$new(
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = terminator,
  fselector = fselector
)
at
class(at) # 继承自Learner

grid = benchmark_grid(
  task = tsk("pima"),
  learner = list(at, lrn("classif.rpart")),
  resampling = rsmp("cv", folds = 3)
)
bmr = benchmark(grid, store_models = TRUE)
bmr$aggregate(msrs(c("classif.ce", "time_train")))

# 9 管道
po.pca <- mlr_pipeops$get(key = "pca")
## 推荐语法糖
po.pca <- po(.obj = "pca")
po.pca; class(po.pca)

po.lrn <- po(.obj = "learner")
### Error in as_learner(learner, clone = TRUE) : argument "learner" is missing, with no default
po.lrn <- mlr_pipeops$get(key = "learner", lrn(.key = "classif.rpart"))
po.lrn
## 语法糖
po_lrn <- po(.obj = "learner", lrn(.key = "classif.rpart"))
class(po_lrn)

po_flt <- po(.obj = "filter", flt(.key = "variance"), param_vals = list(filter.frac = 0.5)) # 超参数
po_flt; class(po_flt)
po_flt <- po(.obj = "filter", flt(.key = "variance"), filter.frac = 0.5) # 省略param_vals，直接写超参数
po_flt

## %>>%
require(magrittr)
gr <- po(.obj = "scale") %>>% po(.obj = "pca") 
gr$plot(html = F)
gr$plot(html = T)
gr; class(gr)
gr$ids()

## graph
po_mutate <- po(.obj = "mutate")
po_flt <- po(.obj = "filter", flt(.key = "variance"), param_vals = list(filter.frac = 0.5))
graph <- po_mutate %>>% po_flt 
graph$plot(html = T)
graph$add_pipeop(op = po(.obj = "pca")) # 添加新管道
graph$plot(html = T)
graph$add_pipeop(op = po(.obj = "pca", id = "pca2"))
graph$plot(html = T)
graph$add_edge(src_id = "pca", dst_id = "pca2", src_channel = , dst_channel = ) # 会把pca2链接到pca末端
graph$plot(html = T)

## 建模
po_mutate <- po(.obj = "mutate")
po_flt <- po(.obj = "filter", flt(.key = "variance"), param_vals = list(filter.frac = 0.5))
po_lrn <- po(.obj = "learner", lrn(.key = "classif.rpart"))
graph <- po_mutate %>>% po_flt %>>% po_lrn 
tsk_iris <- tsk(.key = "iris")

graph$train(input = tsk_iris)
graph; class(graph)
graph$predict(input = tsk_iris)
graph$plot(html = T)

### 将graph封装为lrn, 可以进行其它lrn的操作
glrn <- as_learner(x = graph)
glrn; class(glrn)

cv3 <- rsmp(.key = "cv", folds = 3)
resample(task = tsk_iris, learner = glrn, resampling = cv3)

### 设置超参数
glrn$param_set$values$variance.filter.frac <- 0.25
cv3 <- rsmp(.key = "cv", folds = 3)
resample(task = tsk_iris, learner = glrn, resampling = cv3)

### 调参
ps1 <- ps(
  classif.rpart.cp = p_dbl(lower = 0, upper = 0.05),
  variance.filter.frac = p_dbl(lower = 0.25, upper = 1)
)
class(ps1)

instance <- TuningInstanceSingleCrit$new(task = tsk_iris, 
                                         learner = glrn, 
                                         resampling = rsmp(.key = "holdout"), 
                                         search_space = ps1, 
                                         terminator = trm(.key = "evals", n_evals = 20),
                                         measure = msr(.key = "classif.ce"))
instance; class(instance)
tuner <- tnr(.key = "random_search")
tuner$optimize(inst = instance)
tuner; class(tuner)
instance$archive
#### 模型优化结果
instance$result_learner_param_vals
instance$result_y

## 非线性graph
### no unbranch
graph <- po(.obj = "branch", c("nop", "pca", "scale"), id = "branch1") %>>% # "nop": doing nothing
  gunion(list(
    po(.obj = "nop", id = "null1"),
    po(.obj = "pca"),
    po(.obj = "scale")
  ))
graph$plot()

### unbranch
(graph %>>% po(.obj = "unbranch", c("nop", "pca", "scale"), id = "unbranch1"))$plot()
graph

### 合写
graph <- po(.obj = "branch", c("nop", "pca", "scale"), id = "branch1") %>>% # "nop": doing nothing
  gunion(list(
    po(.obj = "nop", id = "null1"),
    po(.obj = "pca"),
    po(.obj = "scale")
  )) %>>%
  po(.obj = "unbranch", c("nop", "pca", "scale"), id = "unbranch1")
graph$plot()

### short
opts <- list(po(.obj = "nop", id = "no_op"), po(.obj = "pca"), po(.obj = "scale")) # 把中间的写成列表
opt_ids <-  mlr3misc::map_chr(.x = opts, `[[`, "id")
opt_ids; class(opt_ids) # character

graph <- po(.obj = "branch", options = opt_ids, id = "branch1") %>>%
  gunion(opts) %>>%
  po(.obj = "unbranch", options = opt_ids, id = "unbranch1")
graph$plot()

## 集成模型
tsk_iris <- tsk(.key = "iris")
train.idx <- sample(x = seq_len(tsk_iris$nrow), 120)
test.idx <- setdiff(x = seq_len(tsk_iris$nrow), y = train.idx)

pred_single <- po(.obj = "subsample", frac = 0.7) %>>%
  po(.obj = "learner", lrn(.key = "classif.rpart"))
pred_single; class(pred_single) # Graph

### 重复10次
pred_set <- ppl(.key = "greplicate", graph = pred_single, 10L)
pred_set; class(pred_set) # Graph

### bagging
bagging <- pred_set %>>%
  po(.obj = "classifavg", innum = 10)
bagging; class(bagging) # Graph
bagging$plot(html = T)

#### 转换为lrn
baglrn <- as_learner(x = bagging)
baglrn; class(baglrn)

baglrn$train(task = tsk_iris, row_ids = train.idx)
baglrn$model
baglrn$predict(task = tsk_iris, row_ids = test.idx)

### stacking










































