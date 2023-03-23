require(R6)

man <- R6Class(classname = "man",
               public = list(
                 name = NA,
                 initialize = function(name,gender){ #初始化
                   self$name = name
                   private$gender = gender
                 },
                 hello = function(){
                   print(paste("hello",self$name))
                   private$myGender()
                 },
                 member = function(){
                   print(self)
                   print(private)
                   print(ls(envir = private))
                 }
               ),
               private = list(
                 gender = NA,
                 myGender = function(){
                   print(paste(self$name,"is",private$gender))
                 }
               )
                  )

wch <- man$new("王春虎","男") #实例化
wch$initialize("lily","女") #修改实例属性
wch$hello()
wch$member()

require(R6)
require(openxlsx)
require(data.table)

data_read = R6Class(classname = "data_read",
                    public = list(
                      type = NA,
                      data =NA,
                      sheet = NA,
                      initialize = function(type){
                        self$type = type
                      },
                      read = function(data,sheet){
                        self$data = data
                        self$sheet = sheet
                        if (self$type == "xlsx") {
                          read.xlsx(xlsxFile = self$data,sheet = self$sheet)
                        }else if(self$type == "txt"){
                          fread(self$data)
                        }
                      }
                    )
)


xlsx <- data_read$new(type = "xlsx")
xlsx$read(data = "/home/huhu/Downloads/1.xlsx",sheet = 1)








