require(rsconnect)

setAccountInfo(name = "springtiger",
               token = "280D22A298CDA4E5CC6482E0300C8D8B",
               secret = "WrFd1Ziq9iLGGdIederoaUTPsQgj7HOmtsOtAWXW")
deployApp("/media/huhu/学习/生信/生信小工具/bioST.4")



deploy_huhu <- function(dir){
  require(rsconnect)
  
  setAccountInfo(name = "springtiger",
                 token = "280D22A298CDA4E5CC6482E0300C8D8B",
                 secret = "WrFd1Ziq9iLGGdIederoaUTPsQgj7HOmtsOtAWXW")
  deployApp(dir)
}
