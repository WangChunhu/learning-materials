require(stringr)

#字符串拼接
str_c("a","b") 
str_c("a","b",sep = "_") #sep 向量分隔符
str_c(c("a","b"),c("b","c"),sep = "_",collapse = "/") #collapse 向量连接符

#字符计数
a <- c("R","Python","Java.")
str_count(string = a) #不写pattern，默认整个字符串; = str_length
str_length(string = a)
str_count(string = a,pattern = "R")
str_count(string = a,pattern = c("R","P","a"))
str_count(string = a,pattern = "\\.") #或fixed(pattern = ".")

#字符检查
fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(string = fruit,pattern = "a") #检查字符串中是否包含指定字符，返回逻辑向量;negate参数,反选
str_detect(string = fruit,pattern = "^a") # 以a开头 
str_detect(string = fruit,pattern = "a$") # 以a结尾
str_detect(string = fruit,pattern = "[aeiou]") # 或的关系

#字符复制
str_dup(string = a,times = 3)

#字符提取
str_extract(string = fruit,pattern = "[aeiou]")
str_extract_all(string = fruit,pattern = "[aeiou]")
str_extract_all(string = fruit,pattern = "[aeiou]",simplify = T) #返回矩阵
str_match(string = fruit,pattern = "[aeiou]")
str_match_all(string = fruit,pattern = "[aeiou]")

#字符串格式化
name <- "wch"
age <- 18
str_glue("My name is {name},","\n My age is {age}")
mtcars %>% head() %>% str_glue_data("{rownames(.)}的年龄是：{cyl};体重是：{hp}") #组合Dataframe格式数据的列

#字符串位置提取
str_locate(string = fruit,pattern = "a") # 返回第一个匹配到的字符的位置
str_locate_all(string = fruit,pattern = "a") # 返回所有匹配到的字符的位置

#字符补齐
str_pad(string = c("wch","18"),width = 10,side = "both",pad = "=")

#字符删除
str_remove(string = "banana",pattern = "a")
str_remove_all(string = "banana",pattern = "a")

#字符替换
str_replace(string = "banana",pattern = "a",replacement = "*")
str_replace_all(string = "banana",pattern = "a",replacement = "*")
str_replace_na(string = c("banana",NA),replacement = "NA")

#字符排序
a <- c("R","Python","Java.")
##返回字符
str_sort(x = a)
str_sort(x = a,decreasing = T) #降序
##返回索引
str_order(x = a)

#字符分割
str_split(string = "banana",pattern = "a")
str_split(string = "banana",pattern = "",simplify = T) #返回矩阵
str_split_fixed(string = "banana",pattern = "a",n = 4)

#字符过滤
str_sub(string = "banana",start = -2L,end = -1L)
##字符过滤并赋值
x <- "banana"
str_sub(string = x,start = -2,end = -1) <- "**" #必须使用变量
x
##过滤并返回字符串
str_subset(string = fruit,pattern = "^p")
grep(pattern = "^p",x = fruit,value = T)
##字符串过滤并返回位置
str_which(string = fruit,pattern = "^p")
grep(pattern = "^p",x = fruit)

#删除两边的空格
str_trim(string = "  ba     na   n a  ",side = "both")
#删除中间多余的空格
str_squish(string = "  ba     na   n a  ")

#转为大写
str_to_upper("banana")
#转为小写
str_to_lower("BANANA")
#转为标题
dog <- "The quick brown dog"
str_to_title(string = dog)
#转为语句
str_to_sentence(string = str_to_title(string = dog))

#从文本中提取单词
word(string = dog,start = 2,end = 3)

#省略过长的字符串
x <- "ATCGGCACGTTGCTTGCTCGCTCGCTCGAAAVCGAT"
str_trunc(string = x,width = 20,side = "center")





















