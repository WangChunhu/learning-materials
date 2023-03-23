#1
if (!require(Rcpp)) {
  install.packages("Rcpp")
  require(Rcpp)
}
sourceCpp("C:/Users/Administrator/Desktop/timesTwo.cpp")
timesTwo(99)

#2
cppFunction("int add(int x,int y,int z){
            int num = x + y + z;
            return num;
}")
add
add(x = 98,y = 99,z = 100)

#3
cppFunction("int one(){
            return 1;
}")
one
one()

#4
cppFunction("int signC(int x){
            if(x > 0){
              return 1;
            }else if(x == 0){
              return 0;
            }else{
              return -1;
            }
}")
signC
signC(0)

#5循环
sumR <- function(x){
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  return(total)
}
cppFunction("double sumC(NumericVector x){
            int n = x.size();
            double total = 0;
            for(int i=0;i<n;++i){
              total += x[i];
            }
            return total;
}")
sumC
num <- 1:10
sumC(num)
#比较
if (!require(microbenchmark)) {
  install.packages("microbenchmark")
  require(microbenchmark)
}
x <- runif(1e4)
microbenchmark(
  sum(x),
  sumR(x),
  sumC(x)
)








