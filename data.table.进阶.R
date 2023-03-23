require(data.table)
require(magrittr)

#一个在线数据集，包含2014年纽约机场发出的所有航班信息
mydata <- fread("https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv")
object.size(mydata)
str(mydata)
#行索引
carrier <- unique(mydata$carrier)
carrier
#取样
sample(x = unique(mydata$tailnum),size = 5)

#列索引，使用列表参数而非向量化参数(也可以用)
mydata[,list(carrier,tailnum)]
mydata[,.(carrier,tailnum)] #.()返回数据框，可用于循环
mydata[,carrier] #返回向量，,num返回数据框
mydata[,c("carrier","tailnum")]
mydata[,"carrier"] #返回数据框
##行列索引
mydata[carrier %in% c("AA","AS"),.(year,carrier,tailnum)]
##内建函数列索引
mydata[carrier %in% c("AA","AS"),.(NewName = flight/100,carrier,tailnum)] # =赋名
##添加列
mydata[,delay_all := dep_delay + arr_time]
mydata[,c("delay_all","delay_dif") := .(dep_delay + arr_delay,dep_delay - arr_delay)]
##销毁列
mydata[,delay_all := NULL]
mydata <- mydata[,!18]
mydata[,c("delay_all","delay_dif") := NULL]

#内建函数
mydata[carrier %in% c("AA","AS"),.N,by = carrier] 
mydata[carrier %in% c("AA","AS"),.(sum = sum(dep_delay),mean = mean(arr_delay)),by = carrier]
mydata[,.(sum = sum(dep_delay),num = .N,mean = mean(dep_delay),std = sd(dep_delay)),by = .(carrier,origin)]

#排序
## (a <-  1 + 1)
setorder(mydata,carrier,-arr_delay)[] #后面加[],改变数据框的同时输出内容

#列排序
(order <- sample(names(mydata),length(names(mydata))))
setcolorder(mydata,order)[]

#.SD
mydata[carrier %in% c("AA","AS"), lapply(.SD,mean), .SDcols = c("arr_delay","dep_delay","distance"), by = .(carrier,origin,dest)]
mydata[carrier %in% c("AA","AS"), .(mean.arr = mean(arr_delay),sd.arr = sd(arr_delay),mean.dep = mean(dep_delay),sd.dep = sd(dep_delay)), by = .(carrier,origin,dest)][]
mydata[,lapply(.SD,as.numeric),.SDcols = c("arr_delay","dep_delay")]

#主键
DT <- data.table(x=rep(letters[1:5],each=3), y=runif(15)) %>% setkey(x)
DX <- data.table(z=letters[1:3], c=runif(3)) %>% setkey(z)
DT[DX]
















