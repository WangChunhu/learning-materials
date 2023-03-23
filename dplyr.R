install.packages("dplyr")

library(dplyr)
library(openxlsx)
hu <- read.xlsx("H:/综合/三师兄/xgfx.xlsx",colNames=TRUE)

#数据集类型转换
hu1 <- tbl_df(hu)
#HU <- as.data.frame(hu1)
names(hu1)
nrow(hu1)
ncol(hu1)

#分组
hu34 <- group_by(hu1,Cultivar)   #分组
groups(hu34)   #获取分组数据集所使用的分组变量
#groups(ungroup(hu34))   #从数据框中移除分组信息
group_indices(hu1,Cultivar)   #返回每条记录所在分组id组成的向量,即排序的第几个
group_size(hu34)   #返回每个分组的数量
n_groups(hu34)   #返回分组数
hu35 <- count(hu1,Cultivar,sort = TRUE)   #看分组，对分组变量进行计数并排序

#筛选行
hu2 <- slice(hu1,5:n())   #切割行 ，到最后一行 (slice() 函数通过行号选取数据)   
hu3 <- slice(hu1,5:30L)      
hu5 <- slice(hu1,5L,7L)
hu6 <- filter(hu1,GY-1 > 6.12 ,GY-2 > 8)  #过滤
hu7 <- filter(hu1,Cultivar == "Y_Lliangyou 900" , GY_1 > 6.12 , GY_4 == 10.98)
hu8 <- filter(hu1,Cultivar %in% c("Y_Lliangyou 900","Shanyou63"))   # %in% "同一变量-或" 
hu9 <- filter(hu1,GY_1 > 6.12 | GY_4 < 0)   # |  "不同变量或" 

#排序（按给定的列名依次对行进行排序）
hu10 <- arrange(hu1,Cultivar,IR_H)   # 正序，可多个变量
hu11 <- arrange(hu1,desc(Cultivar))   #倒序,只可一个变量

#选择列
hu12 <- select(hu1,Cultivar,IR_H,GY_1,IR_W)  
hu13 <- select(hu1,-Cultivar,-IR_H,-GY_1,-IR_W)  #选择除了这几列,用-
hu14 <- select(hu1,-starts_with("GY"))  #前缀包括GY，后缀用ends_with，包含用contains
hu15 <- select(hu1,Cultivar:GY)  #选择多列
hu16 <- select(hu1,-contains("_"))
hu29 <- select(hu1,everything())   #返回所有列
hu17 <- select(hu1,q,GY_8,GY_7,everything())   #返回所有列,并把q列放到最前面,依次

#重命名
hu30 <- rename(hu1,Q = q) #重命名列，返回全部列,前为新名
hu31 <- select(hu1,ir =starts_with("IR"))  #重命名列，只返回重命名的列

#变形
hu18 <- mutate(hu1,Q=q * 11)   #计算并添加新列
hu19 <- transmute(hu1,Q=q * 11)   #计算并只返回新列

#去重
hu32 <- distinct(hu1,Cultivar)   #去除变量重复并返回该变量
hu20 <- nrow(distinct(hu1,e))   #去除变量重复并返回行数和该变量
hu21 <- distinct(hu1,Cultivar,.keep_all = TRUE)   #去除变量重复并返回所有变量

#概括
hu22 <- summarise(hu1,mean(GY-1))   # sd(),max(),min(),n(),first(),last(),sum()
hu23 <- summarise(hu1,n_distinct(GY-1))   #返回变量无重复的个数
hu24 <- summarise_all(hu1,mean)   #计算所有列
hu25 <- summarise_if(hu1,is.numeric,sd)   #按条件计算   
hu26 <- summarise_at(hu1,2:8,mean)   # 按列计算  c(1,3,5)或直接用变量名
hu27 <- summarise_if(select(hu1,c(1,3,4)),is.numeric,funs(min,max,mean,sum,sd))   #多函数计算
hu28 <- summarise(hu1,nth(IR_H,2))   #返回变量中第二个值
hu33 <- summarise(hu1,last(Cultivar))   #返回变量中最后一个个值

#条件语句   #if_else会保留原有数据类型
x <- c(-5:5,NA)
if_else(x < 0,NA_integer_,x)   #真假类型一致
if_else(condition = x < 0,true = "negative",false = "positive",missing = "missing")   #真假类型一致
ifelse(x < 0,"negative",1)   #真假类型不一致

  
  




