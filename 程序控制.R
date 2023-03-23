# 控制 ----------------------------------------------------------------------
## if
i <- 10
if (i != 10) {
  i
}

## if-else
i <- "10"
if (is.numeric(i)) {
  print(i)
} else {
  print(NULL)
}

## ifelse
i <- "10"
ifelse(test = is.numeric(i), yes = i, no = "NULL")

## 多重if-else
test <- "B"
if (test == "A") {
  print(3)
} else if (test == "B") {
  6
} else if (test == "C") {
  9
} else if (test == "D") {
  12
}

## switch
test <- "B"
switch (test, # 精确匹配
        A = 3,
        B = 6,
        C = 9,
        D = 12)

# 循环 ----------------------------------------------------------------------
## while
i <- 1
while (i <= 10) {
  print(i)
  i <- i + 1
}

## for
# i <- 1
for (i in c("a", "b", "c")) {
  print(i)
}

a <- c("a", "b", "c")
i <- 1
for (i in 1:length(a)) {
  print(a[i])
  print(paste0(i, " / ", length(a)))
}

## next
i <- 1
for (i in 1:10) {
  if (i %% 2 == 0) {
    print(i)
  } else {
    next # 跳过
  }
}

## break
i <- 1
for (i in 1:10) {
  print(i)
  if (i > 5) {
    break # 结束循环
  }
}

i <- 1
for (i in 1:10) {
  if (i > 5) {
    break
  }
  print(i)
}

## stop
i <- 1
for (i in 1:10) {
  print(i)
  if (i > 5) {
    stop("循环次数将超过", i, "次，程序就此停止") # 结束程序并给出提示
  }
}




