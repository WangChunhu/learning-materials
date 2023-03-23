x <- c(1,3,4,2,5,6)
length(x)
y <- c("a","b","c")
mode(y)
z <- cbind(x,y)  
z <- rbind(x,y)
z 
mean(x) 
sum(x)
max(x)
min(x)
var(x)
prod(x)
sd(x)
?prod
help("prod")

format(5e+5, scientific = FALSE)#ȡ????ѧ????

x <- 1:10*2+1
x[x[-5]]
x[-(1:5)]
x[c(1,3,5)]
x[x>5 & x<10]
seq(4,20,3)
letters[1:30]
getwd()
setwd("H:\\Rѧϰ\\R????")
library(ISwR)
plot(rnorm(1000))
2+2
exp(-2)
rnorm(15)
x <- 2
x
x(2)
x*x
weight <- c(60,72,57,90,95,72)
height <- c(1.75,1.80,1.65,1.90,1.74,1.91)
bmi <- weight/height^2
bmi
sd(bmi)
var(bmi)
max(bmi)
min(bmi)
prod(bmi)
mean(bmi)
mean(bmi)
sum(bmi)
xbar <- sum(weight)/length(weight)
weight - xbar
t.test(bmi,mu=22.5)
plot(height,weight,pch=16)
hh <- c(1.65,1.70,1.75,1.80,1.85,1.90)
lines(hh,22.5*hh^2)
args(lines(hh,22.5*hh^2))
c("huey","dewey","wang")
cat(c("huey dewe ywang"),"\n")   #ȥ??????
pi
seq(1.65,1.90,0.05)
oops <- c(7,9,13)
rep(oops,1:3)
rep(c(1,2),c(10,15))
rep(1:2,each=10)
?rep
x <- c(3,4)
dim(x) <- c(3,4)
x <- matrix(1:12,nrow = 3,byrow=T)
rownames(x) <- LETTERS[1:3];colnames(x) <- letters[4:7]
x
t(x)   #????????ת??
months(1:6)
A =1:4;B=5:8;C=9:12
d <- data.frame(A,B,C)
d$C
d[,2]
head(d)
cbind(A,B,C)
A[1:2]
A[B<8]

pain <- c(0,3,2,2,1)
fpain <- factor(pain,levels = 0:3)
levels(fpain) <- c("none","mild","medium","severe")
fpain
as.numeric(fpain)
levels(fpain)
is.na(A)#?鿴ȱʧֵ
energy
exp.len <- energy$expend[energy$stature=="lean"]
exp.len
l <- split(energy$expend,energy$stature)#?Զ?????
l
intake$post
sort(intake$post)
ls()
save.image()
ls()
rm(list=ls())   
ls()
save.image()
sink("myfile")
ls()
getwd()
setwd("H:\\Rѧϰ\\R????\\R???ݷ???")
getwd()
sink("a")
history()
detach()
search()
ls()
runif(50)
memory.limit(102400)
sink()
op=par()
x <- runif(50,0,2)
x
y=x
y
plot(x,y,main = "Main title",sub = "subtitle",xlab = "x-label",ylab = "y-label",type="n",axes=F)
axes=F

dir.create("H:\\Rѧϰ\\R????\\R_files")    #?????ļ???
setwd("H:\\Rѧϰ\\R????\\R_files")     #???ù???·??

#???Ĳ???
.libPaths()   #?鿴???İ?װĿ¼
.libPaths("D:/R-3.6.1/library")   #???ð??İ?װĿ¼
library()   #?鿴?Ѿ???װ?İ?Ŀ¼
is.logical(require(data.table))
require(dplyr)
getOption("defaultPackages")   # ?鿴????Rʱ?Զ??????İ?
sessionInfo()   # ?鿴R???????İ?
(.packages())    # ?г???ǰ?Ѿ????صİ?
installed.packages("plumbr")   # ?鿴?Ѱ?װ????Ϣ
detach(package:processx)   # ???ذ?
remove.packages(processx)   # ж?ذ?
remove.packages(c("pkg1","pkg2"),lib = file.path("path","to","library"))
R.version   # ?鿴R?汾
library(help="plumbr")   # ?鿴??ϸ??Ϣ
packageVersion("plumbr")   # ?鿴?汾??
update.packages()   # ???°?  ask = F
#?鿴????��
devtools::install_github("ikosmidis/cranly")
xts_tree=build_dependence_tree(package_network,"dplyr")
plot(xts_tree)
#######################
methods()

#?жϱ?��?Ƿ?????NA(??????ֵ)
.one <- NA
print(.one)
is.na(.one)
#?жϱ?��?Ƿ?????NULL(???ڱ?��δ??ʼ??ʱ,??֪?��?��?ᱻ??????ֵ)
is.null()
.two <- NULL
print(.two)
is.null(.two)
is_even <- NULL
jishu <- function(x){
  ifelse(x %% 2 == 0,F,T)}
x <- 3
jishu(x)
99 %/% 12   #ȡ??
99 %% 12  #ȡ??
floor(8.9)   #????ȡ??
ceiling(8.9)   #????ȡ??
!TRUE
TRUE & TRUE
TRUE & FALSE
TRUE | TRUE
TRUE | FALSE

#?жϸ???ֵ?Ƿ?Ϊ????
is.factor(sex)
sex <- factor(x = "f",levels = c("m","f"))   #????????(???ڷ???)
nlevels(sex)   #??ȡ????ˮƽ????
levels(sex)[2]   #??ȡ????ˮƽ????(????ֵΪ??��)
ordered()   #????????????
sec <- ordered("a",c("a","b","c"))
nlevels(sec)
levels(sec)

#??��
length()   #?鿴??��????
x <- c(1,2,3)
y <- c(1,2)
NROW(x)   #?????ڲ鿴??��???????ĳ???
nrow(x)   #??????????��
identical(x = x,y = y)   #ȷ???????Ƿ???ͬ
union(x,y)   #?ϼ?
intersect(x,y)   #????
setdiff(x,y)
"a" %in% c("a","b","c")   #?ж?ĳ??ֵ?Ƿ???????ĳ????��
c(1,2,3) == c(1,2,100)   # !=(????��?е?ֵ???бȽ?)
seq(1,100,2)
rep(1:3,each=5,times=3)
head()   #??Ӧtail()
mode(x)   #?ж?????????
na.omit()   #?ų?na

#????R
library(installr)
updateR()


zimu = c("a","b","c","d","e","f","g","h","i","j")
i=1
last <- NULL
for (i in 1:10) {
  RES <- str_replace_all(string = res$.group,pattern = as.character(i),replacement = as.character(zimu[i]))
  last <- cbind(last,RES) %>% as.data.frame()
}
LAST <- paste(last[,1],last[,2],last[,3],last[,4],last[,4],last[,5],last[,6],last[,7],last[,8],last[,9],last[,10],sep = "") %>% str_trim()  #ȥ???ո?
# LSD.test(out,trt="group",p.adj="none",alpha=.05,group = T)
llast <- gsub(pattern = "\\d",replacement = "",x = LAST) %>% str_trim() %>% as.data.table()
LLAST <- rbind(LLAST,llast)


#ȥ????ĸ???ǿո?
i=1
O <- NULL
ALL <- NULL
for (i in 1:nrow(LLAST)) {
  str_replace_all(string = LLAST[i],pattern = " ","")->O
  ALL <- rbind(ALL,O)
}

#?鿴??ҳ??????ʽ
#??ҳ?ڰ?F12,Ȼ????console????̨????document.charset Ȼ???س?
document.charset

#????console
control + L
#?鿴?Ķ?????
str() #?????Ľṹ????
#?ص??༭????
control + 1

#Error: (list) object cannot be coerced to type 'double'
?б���??תΪ??????  #?????취??ȥ?б���תΪ??�� unlist()

?ڽ? factor ???͵ı?��תΪ??ֵ??��??ʱ???мǲ?Ҫֱ??ʹ?? as.numeric(),??ȷ?ķ?ʽ?? as.numeric(as.character(myFactorVar))

ʹ?? file.path() ????(ʹ??)?ļ?·??

sessionInfo() #???Ի?ȡ R ?İ汾????????Ϣ,?Լ????صİ?????Ϣ??

#????R
library(installr)
updateR()

#????RStudio
Help ת  Check for update

#??װRtools
require(installr)
require(stringr)
require(htmltab)
install.Rtools()
#y??֤Rtools?Ƿ?װ??
system('g++ -v')
system('where make')

#BiocManager
if (!"BiocManager" %in% rownames(installed.packages()))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::available()

#读入图片
library(png)
imgpng <- readPNG("E:/data/1/1_dog.png")
r <- nrow(imgpng)/ncol(imgpng) 
plot(c(0,1),c(0,r),type = "n",xlab = "",ylab = "",asp=1)
rasterImage(imgpng,0,0,1,r)

library(imager)
imgjpg <- load.image("E:/data/1/2_dog.jpg")
#imdim <- dim(imgjpg)
plot(imgjpg,xlim = c(1,width(imgjpg)),ylim = c(height(imgjpg),1)) #ylim的值前后变换可颠倒图片

#保存为R文件
save(a,file = "./cottonFGD.RData")

#滑动平均
filter(x = 1:100/4,rep(1,4),sides = 1) #滑动长度为4
#滑动平均函数
filter.length_sel <- function(data,length){
  filter(x = data/length,rep(1,length),sides = 1)
}
#eg
filter.length_sel(data = 1:100,length = 4)

#日期操作
data <- data.table("day" = rep(1:30,2),"month" = rep(1:2,30)) %>% setorder(month) %>% .[,"md" := paste0(day,"-",month)] %>% .[,"md1" := as.Date(md,"%d-%m")] %>% .[,"md2" := format(data$md1,"%d-%b")] %>% .[,"md3" := as.Date(md1) - as.Date("1970-1-1","%Y-%m-%d")]

min(data[!md4 %in% NA]$md4)
%d  #日期 01-31
%a  #缩写星期 mon
%A  #非缩写星期 monday
%m  #数字月份 1-12
%b  #缩写月份 jan
%B  #非缩写月份 january
%y  #两位数字年份 01
%Y  #四位数字年份 2001

#设置英文环境输出
Sys.setlocale(category = "LC_TIME","en_US.UTF8")
#切换环境语言
sys.language_set <- function(English = T){
  ifelse(English == F,Sys.setlocale(),Sys.setlocale(category = "LC_TIME","en_US.UTF8"))
}
#eg
sys.language_set(English = T)
format(as.Date("1-1-01","%d-%m-%y"),format = "%d-%b")

#日历图
require(pacman)
p_load(openair)
data(mydata)
calendarPlot(mydata, pollutant = "wd", year = 2021,month = 3)

#数值化数据框
data <- sapply(data, as.numeric)

#邮件
require(mailR)
#开通smtp服务 ##https://service.mail.qq.com/cgi-bin/help?subtype=1&no=166&id=28
#接收人
receiver <- "2472795414@qq.com"
#发件人
sender <- "1623801078@qq.com"
#主题
emailSubject <- "send the first email with R"
#正文
emailBody <- "王春虎的第一份R邮件"
#附件路径
emailFile <- "/media/huhu/学习/生信/文献/拟南芥GRAS/Lee2008_Article_Large-scaleAnalysisOfTheGRASGe.pdf"

#发送邮件
send.mail(from = sender,
          to = receiver,
          subject = emailSubject,
          body = emailBody,
          smtp = list(host.name="smtp.qq.com", # smtp 服务器主机名
                      port=25, # 默认端口
                      user.name= sender, # 用户名
                      passwd="afybbbawrtqzcgcb", # 密码（授权码）
                      ssl=TRUE),
          authenticate = TRUE,
          send = TRUE,
          #attach.files = emailFile,  #附件路径
          encoding = "utf-8" # 编码
)

#lapply批量读取文件
require(magrittr)
files <- list.files()
lapply(files, function(x){
  openxlsx::read.xlsx(x)
}) %>%  dplyr::bind_rows()-> a




