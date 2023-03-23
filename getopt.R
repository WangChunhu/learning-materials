#!/usr/bin/Rscript

library(getopt,quietly = T)

# 第一列：参数的longname，多个字符。
# 第二列：参数的shortname，一个字符。
# 第三列：参数是必须的，还是可选的，数字：0代表不接参数 ；1代表必须有参数；2代表参数可选。
# 第四列：参数的类型。logical；integer；double；complex；character；numeric
# 第五列：注释信息，可选。

spec <- matrix(c('help', 'h', 0,'loical', 'GSMs插值函数',
                 'times', 't', 1, 'integer', '任务次数, 默认为1, 这次最大为6',
                 'para.two', 'p', 1, 'integer', '随便写的'),
               byrow = T,
               ncol = 5)

args <- getopt(spec = spec) # usage：默认为FALSE,这时参数无实际意义，而是以用法的形式输出。

# 输出用法
if (is.null(args$times) | is.null(args$para.two)) {
  cat(paste(getopt(spec = spec, usage = T), "\n"))
  quit(status = 1) # q()
}

# 设置默认值
if (is.null(args$times)) {args$times <- 1}

times <- args$times
para.two <- args$para.two

print(paste(times, para.two))


"ssh 节点号"
"screen -R huhu.met"
"cd /parastor/home/zzuaga06/yanhl/huhu.met"
"condaup"
"nohup Rscript idw.服务器执行.R -t 1 &"