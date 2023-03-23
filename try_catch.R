require(data.table)

dd <- data.frame(ID = 1:10, y = rnorm(10))
head(dd)

# warning
rel <- dcast(dd, y ~ .) # 虽然会成功执行，但是会警告

# error
a # 环境变量不存在a

# tryCatch
## warning
rel <- tryCatch(expr = {dcast(dd, y ~ .)},
                warning = function(warning){
                  print("warning")
                  2
                },
                error = function(error){
                  print("error")
                  3
                }
                )
rel

## error
rel <- tryCatch(expr = {a},
                warning = function(warning){
                  print("warning")
                  2
                },
                error = function(error){
                  print("error")
                  3
                }
)
rel

## warning & error
melt(dd, yy ~ .)

rel <- tryCatch(expr = {melt(dd, yy ~ .)},
                warning = function(warning){
                  print("warning")
                  2
                },
                error = function(error){
                  print("error")
                  3
                }
)
rel # warning优先级更高

rel <- tryCatch(expr = {plot(1:10, 1:0)},
                warning = function(warning){
                  print("warning")
                  2
                },
                error = function(error){
                  print("error")
                  3
                },
                finally = {print("结束！")}
)


