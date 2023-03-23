# install.packages('tinsel')

require(tinsel)

# print_start_end <- function(f){
#   wrapper <- function(...){
#     print("Starting function call...")
#     f()
#     print("Finished function call...")
#     }
#   return(wrapper)
#   }

print_start_end <- function(f){
  wrapper <- function(...){
    start <- proc.time()
    f()
    print(proc.time() - start) 
    }
  return(wrapper)
  }

#. print_start_end
todays_date <- function(){ # (): 函数括号可带可不带
  print(Sys.Date()) 
  }

# todays_date() # 在装饰器文件中不管用,必须经过source_decoratees()函数调用

#. print_start_end
yesterday <- function(){
  print(Sys.Date() - 1)
  }

#. print_start_end
tomorrow <- function(){
  print(Sys.Date() + 1)
  }
