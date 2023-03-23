require(mlr3)
require(mlr3verse) # mlr3verse仅安装一方面经常需要执行数据分析的软件包子集，另一方面不引入太多依赖项或系统需求
require(magrittr)
require(data.table)

mlr3verse::mlr3verse_info()
mlr_learners

# 例子 决策树 ----------------------------------------------------------------
# 使用内置的鸢尾花数据(已经被预先定义为任务)集进行测试，创建任务集
task = tsk("iris") 
task

# 创建学习器并使用calssif.rpart算法
learner = lrn("classif.rpart") # lrn()学习器只提供五种基础的算法：classif.debug，classif.featureless，classif.rpart，regr.featureless和regr.rpart，前三种是分类算法，后两种是回归算法。
learner

# 选取前140条数据作为训练集
learner$train(task = task,row_ids = 1:120)

# 查看训练的模型
learner$model

# 对后20条数据进行预测
predictions = learner$predict(task, row_ids= 121:150) 
predictions
predictions.df <- data.table(predictions$truth,predictions$response)

# 计算模型的预测准确性
measure <- msr("classif.acc")
predictions$score(measure)

# -------------------------------------------------------------------------


# 基础 ----------------------------------------------------------------------
# task
# 任务类型
## 1 Classification Task: The target is a label (stored as character()orfactor()) with only relatively few distinct values → TaskClassif.
## 1 分类任务：目标是一个标签（存储为character（）或factor（）），只有相对较少的不同值→ TaskClassif。

## 2 Regression Task: The target is a numeric quantity (stored as integer() or double()) → TaskRegr.
## 2 回归任务：目标是一个数字量（存储为integer（）或double（））→ TaskRegr。

## 3 Survival Task: The target is the (right-censored) time to an event. More censoring types are currently in development → mlr3proba::TaskSurv in add-on package mlr3proba.
## 3 生存任务：目标是事件发生的时间（右删失）。目前正在开发更多的审查类型→ 附加包mlr3proba中的TaskSurv。

## 4 Density Task: An unsupervised task to estimate the density → mlr3proba::TaskDens in add-on package mlr3proba.
## 4 密度任务：估计密度的无监督任务→ mlr3proba：：加载包mlr3proba中的TaskDen。

## 5 Cluster Task: An unsupervised task type; there is no target and the aim is to identify similar groups within the feature space → mlr3cluster::TaskClust in add-on package mlr3cluster.
## 5 集群任务：无监督任务类型；没有目标，目标是在特征空间中识别相似的群体→ mlr3cluster:：加载包mlr3cluster中的TaskClust。

## 6 Spatial Task: Observations in the task have spatio-temporal information (e.g. coordinates) → mlr3spatiotempcv::TaskRegrST or mlr3spatiotempcv::TaskClassifST in add-on package mlr3spatiotempcv.
## 6 空间任务：任务中的观测具有时空信息（例如坐标）→ 附加包mlr3spatiotempcv中的mlr3spatiotempcv:：TaskRegrST或mlr3spatiotempcv:：TaskClassifST。

## 7 Ordinal Regression Task: The target is ordinal → TaskOrdinal in add-on package mlr3ordinal (still in development).
## 7 顺序回归任务：目标是序数→ 附加包mlr3ordinal中的TaskOrdinal（仍在开发中）。

# 手动创建任务
data("mtcars",package = "datasets")
data <- mtcars[,1:3]
str(data)

# 创建一个回归任务
task_mtcars <- mlr3::as_task_regr(x = data, #:用于转换的对象，可以通过扩展包mlr3db连接到SQL服务器
                                  target = "mpg", # 回归任务的目标列
                                  id = "cars", # 任务的随机标识符，用于画图或统计
                                  label = "cars.label") 
task_mtcars
mlr3viz::autoplot(object = task_mtcars, type = "pairs")+
  theme_classic()
autoplot(object = task_mtcars)+
  theme_classic()

# 内置的预先设置的任务
mlr3::mlr_tasks
data.table::as.data.table(mlr_tasks)[,1:4]
help(mlr_tasks_iris) # 特定任务的帮助

# To get a task from the dictionary, use the $get()或tsk() method from the mlr_tasks 
# mlr3data提供了很多回归和分类任务, mlr3proba提供了很多生存和密度任务
task_penguins <- mlr_tasks$get("penguins")
task_penguins <- tsk("penguins")
task_penguins

### 任务函数
task_penguins$help() # 帮助 help(mlr_tasks_penguins)

data.penguins <- as.data.table(task_penguins) # 直接转为data.table调用
data.penguins <- task_penguins$data()
  
task_penguins$data()[1:6] # task_penguins$data()看作是一个data.table
task_penguins$data(rows = 1:6)

task_penguins$nrow
task_penguins$ncol
nrow(task_penguins$data())

task_penguins$row_ids %>% head()
task_penguins$head()

task_penguins$feature_names 
task_penguins$feature_types
task_penguins$target_names 

task_penguins$task_type
summary(task_penguins$data())
summary(as.data.table(task_penguins))
task_penguins$col_roles



## 二分分类，一个为正，一个为负，定义构造函数时可以指定正的类，默认为第一个
mlr_tasks
data("Sonar",package = "mlbench")
task_Sonar <- as_task_classif(x = Sonar, 
                              id = "Sonar",
                              target = "Class",
                              label = "Sonar.label",
                              positive = "R"
                                )
task_Sonar
task_Sonar$col_roles[1:3]
task_Sonar$data() %>% names()
task_Sonar$data() %>% .[, .(Class)] %>% head()
task_Sonar$positive
task_Sonar$positive <- "M" # 转换正类名称

# 任务变体,删除或添加行列，任务直接发生变化
## select 列 ； filter 行
# 设置col_roles
data <- as.data.table(datasets::mtcars[, 1:3], keep.rownames = T)
task_mtcars <- as_task_regr(x = data, target = "mpg", id = "cars",label = "cars.label")
task_mtcars$feature_names
task_mtcars$col_roles %>% names()
task_mtcars$set_col_roles(cols = "rn", roles = "name") # 设置col_roles,只更新显示，不改变底层数据

# 更加方便
task_penguins <- tsk("penguins")
task_penguins$select(c("body_mass","flipper_length")) # 只保留这两个feature 特征列
task_penguins$feature_names

task_penguins$row_ids
task_penguins$filter(1:10) # 只保留前10行
task_penguins$data()
task_penguins$row_ids
task_penguins$nrow

# 添加额外的行列
task_penguins$cbind(data = data.table(letters = letters[1:10])) # 添加列
task_penguins$head()

# 画任务
require(ggplot2)
task_pima <- tsk("pima")
task_pima$feature_names
task_pima$select(head(task_pima$feature_names, 4))
task_pima$feature_names
autoplot(task_pima)+
  theme_classic()
autoplot(task_pima,type = "pairs")+
  theme_classic()
autoplot(task_pima,type = "duo")+
  theme_classic()

# learner
#学习器
# model train  predict
## 预先设置的学习模型

## 学习器模型
mlr_learners  
learner <- mlr_learners$get("classif.rpart")
learner <- lrn("classif.rpart")
learner
## 超参数 hyperparameters
### 查看超参数
learner$param_set
### 设置超参数
learner$param_set$values <- list(cp = .05, xval = 1)
learner$param_set$values # 只显示有值的

pv <- learner$param_set$values # 返回列表 list(cp = .05, xval = 1)
pv
pv$cp <- .02
pv$xval <- 100
learner$param_set$values <- pv
pv
learner$param_set$values
### 直接设置
learner <- lrn(.key = "classif.rpart", id = "cr", label = "q", cp = .05, xval = 1)
learner$param_set$values
learner$id
learner$label
### 设置阈值
data("Sonar", package = "mlbench")
task_Sonar <- as_task_classif(x = Sonar, target = "Class", positive = "M")
learner_Sonar <- lrn(.key = "classif.rpart", predict_type = "prob") # predict_type为普通参数
learner_Sonar
learner_Sonar$param_set
predict_Sonar <- learner_Sonar$train(task = task_Sonar)$predict(task_Sonar)
measures <- msrs(c("classif.tpr", "classif.tnr")) # use msrs() to get a list of multiple measures
predict_Sonar$confusion
predict_Sonar$score(measures = measures) 
predict_Sonar$set_threshold(threshold = .2) # predict_type = "prob" 的可能性
predict_Sonar$confusion
predict_Sonar$score(measures = measures) 

## 训练，预测和评估
mlr_tasks %>% as.data.table()
task <- tsk("penguins")
task
learner <- lrn("classif.rpart")
learner
train_set <- sample(x = task$nrow, size = 0.8 * task$nrow)
test_set <- setdiff(x = seq_len(task$nrow), train_set)

a <- 1:10
b <- 5:15
union(a, b) # 并集
intersect(a, b) # 交集
setdiff(a, b) # a差集
setdiff(b, a) # b差集
setequal(a, b) # 是否相等

learner$model # 训练出的模型，现在为NULL
### 训练
learner$train(task = task, row_ids = train_set)
learner$model
### 预测
prediction <- learner$predict(task = task, row_ids = test_set)
prediction
head(as.data.table(prediction)) # 详细结果列表
prediction$confusion # 结果矩阵，容易看
#### 改变预测类型
learner$predict_type <- "prob" # se : 标准差
learner
learner$train(task = task, row_ids = train_set)
learner$model
prediction <- learner$predict(task = task, row_ids = test_set)
prediction
head(as.data.table(prediction)) # 详细结果列表
prediction$confusion
# head(prediction$response)
# head(prediction$prob)

#### 画预测
autoplot(prediction)+
  theme_classic()
### 结果评估
mlr_measures # 评估方法
measure <- mlr_measures$get("classif.acc")
measure
measure <- msr("classif.acc")
measure
prediction$score(measures = measure)
#################################################################################################################################

#################################################################################################################################
# 结果评估和比较 
#################################################################################################################################
## ROC曲线 ：“受试者工作特征曲线”，或者感受性曲线，主要是用于X对Y的预测准确率情况
## ROC曲线和阈值
data("Sonar", package = "mlbench")
task <- as_task_classif(x = Sonar, target = "Class", positive = "M")
task
# learner <- lrn(.key = "classif.rpart")
# learner
# learner$predict_type # response
# learner$predict_type <- "prob"
learner = lrn("classif.rpart", predict_type = "prob")
learner
prediction <- learner$train(task)$predict(task)
C <- prediction$confusion
C
# install.packages("precrec")
# True Positive Rate (TPR): How many of the true positives did we predict as positive?
# True Negative Rate (TNR): How many of the true negatives did we predict as negative?
# Positive Predictive Value PPV: If we predict positive how likely is it a true positive?
# Negative Predictive Value NPV: If we predict negative how likely is it a true negative?
autoplot(object = prediction, type = "roc") # TPR and FPR
autoplot(object = prediction, type = "prc")+ # PPV vs TPR
  theme_classic()

## 重抽样 resample(task, learner, resampling, store_models = T)
mlr_resamplings %>% as.data.table() # 策略
mlr_resamplings$get()
rsmp()

task <- tsk("penguins")
learner <- lrn("classif.rpart")
resampling <- rsmp("holdout")
resampling
resampling$param_set$values <- list(ratio = 0.8) # 修改比率
resampling <- rsmp("holdout", ratio = 0.667) # 修改比率
resampling$instantiate(task = task)
resampling$train_set(3)
resampling$iters <- 3 # 修改迭代次数
resampling$instantiate(task = task)
resampling
resampling$test_set(2) %>% str()

task <- tsk("penguins")
learner <- lrn("classif.rpart", maxdepth = 3, predict_type = "prob")
learner$param_set
resampling <- rsmp(.key = "cv", folds = 3)
resampling$param_set

rr <- resample(task = task, learner = learner, resampling = resampling, store_models = T)
rr
rr$aggregate(measures = msr("classif.ce")) # 重抽样迭代的平均性能
rr$score(measures = msr("classif.ce")) # 查看每次迭代的性能
rr$warnings
rr$errors
rr$resampling$train_set(i = 1) 
lrn <- rr$learners[[1]] # 查看性能好的迭代次数
lrn$model  # 查看性能好的迭代次数的模型
pre <- rr$predictions()
pre
pre[[1]]
### 保留想要的迭代
rr$filter(iters = c(1, 3))
rr
rr$learners
### 自定义重抽样
mlr_resamplings %>% as.data.table()
resampling <- rsmp("custom")
resampling$instantiate(task = task, 
                       train_sets = list(c(1:10, 51:60, 101:110)), 
                       test_sets = list(c(11:20, 61:70, 111:120))
                      )
resampling$iters
resampling$train_set(i = 1)
### 画重抽样结果
task = tsk("pima")
task$select(c("glucose", "mass"))
learner = lrn("classif.rpart", predict_type = "prob")
rr = resample(task, learner, rsmp("cv"), store_models = TRUE)
autoplot(object = rr,measure = msr(.key = "classif.auc"))
autoplot(object = rr,measure = msr(.key = "classif.auc"), type = "roc")
rr1 <- rr$filter(iters = 1) # 选择特定的迭代次数的预测值
autoplot(object = rr1, type = "prediction")





































