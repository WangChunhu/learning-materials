require(data.table)
require(ggplot2)
require(openxlsx)
require(magrittr)
require(agricolae)

zhibiao <- c("垩白度","整精米率","千粒重")
danwei <- c("垩白度/%","整精米率/%","千粒重/g")
qian <- c(0,20,15)
hou <- c(22,75,25)
gaodu <- c(4,4,2)
sht <- c(1:3)
i=1
for (i in 1:3) {
  #导入数据
  a <- read.xlsx(xlsxFile = "H:/huhu/2019/2019-all整合 - 副本.xlsx",sheet=sht[i]) %>% setDT() %>% .[!品种 %in% "荃丰丝苗"] ##偷梁换柱
  
  #d短变长
  yuandata <- melt.data.table(data = a,
                              id.vars = c("品种","温度","时期","重复"),
                              variable.name = "部位",value.name = "垩白度")
  
  #计算并合并
  AS <- NULL
  Aebaidu <-  yuandata[,lapply(.SD,mean,na.rm=T),.SDcols="垩白度",by=c("品种","温度","时期","部位")] %>% setnames("垩白度","a_垩白度")
  Sebaidu <-  yuandata[,lapply(.SD,sd,na.rm=T),.SDcols="垩白度",by=c("品种","温度","时期","部位")] %>% setnames("垩白度","sd_垩白度")
  AS <- cbind(Aebaidu,Sebaidu) %>% .[,!6:9]
 
  c <- c("28℃","30℃","33℃")   
  ck<- AS[AS$时期 %in% "CK"] 
  CK <- NULL
  j=1
  for (j in 1:length(c)) {
    ck[,"温度" := c[j]] -> p
    CK <- rbind(CK,p)
  }
  B <- rbind(AS,CK)
  AS <- B[B$温度 %in% c("28℃","30℃","33℃")]
  #方差分析——组内方差
  AS_fCK <- yuandata[!yuandata$时期 %in% "CK"]
  AS_CK <- AS[AS$时期 %in% "CK"] %>% .[,biaoji := ""]
  unq <- unique(AS_fCK[,c(1,2,3,5)])
  LAST <- NULL
  j=1
  for (j in 1:nrow(unq)) {
    AS_fCK[AS_fCK$品种==unq$品种[j] &AS_fCK$温度==unq$温度[j] & AS_fCK$部位==unq$部位[j]] ->o
    o_unq <- unique(o[,c(1:3,5)])
    o.aov <- aov(垩白度~时期,o)
    o.LSD <- LSD.test(o.aov,"时期",p.adj = "none",alpha = 0.05)
    bq <- data.table(row.names(o.LSD$means),o.LSD$means$垩白度) %>% setnames(c("V1","V2"),c("时期","垩白度"))
    LETTER <- setDT(o.LSD[[5]])
    bj <- unique(bq[LETTER,on="垩白度",allow.cartesian=T])
    bj1 <- o_unq[bj,on="时期"] %>% .[,!5]
    LAST <- rbind(LAST,bj1)
  }
  LAST <- setnames(LAST,"groups","biaoji")
  ASS <- AS[LAST,on=c("品种","时期","温度","部位")]
  ASS <- rbind(ASS,AS_CK)
  
  # #方差分析——全方差
  # ##数据整合
  # 
  # ck1 <- yuandata[yuandata$时期 %in% "CK"] 
  # CK1 <- NULL
  # j=1
  # for (j in 1:length(c)) {
  #   ck1[,"温度" := c[j]] -> q
  #   CK1 <- rbind(CK1,q)
  # }
  # A <- rbind(yuandata,CK1) %>% setDT()
  # A <- A[A$温度 %in% c("28℃","30℃","33℃")]
  # yuandata <- A
  # 
  # yuandata <- yuandata[,分组 := paste(时期,温度,sep="-")]
  # unq <- unique(yuandata[,c(1,5)])
  # LAST <- NULL
  # j=1
  # for (j in 1:nrow(unq)) {
  #   yuandata[yuandata$品种==unq$品种[j] & yuandata$部位==unq$部位[j]] ->o
  #   o_unq <- unique(o[,c(1:3,5,7)])
  #   o.aov <- aov(垩白度~分组,o)
  #   o.LSD <- LSD.test(o.aov,"分组",p.adj = "none",alpha = 0.05)
  #   bq <- data.table(row.names(o.LSD$means),o.LSD$means$垩白度) %>% setnames(c("V1","V2"),c("分组","垩白度"))
  #   LETTER <- setDT(o.LSD[[5]])
  #   bj <- unique(bq[LETTER,on="垩白度",allow.cartesian=T])
  #   bj1 <- o_unq[bj,on="分组"] %>% .[,!5]
  #   LAST <- rbind(LAST,bj1)
  # }
  # LAST <- setnames(LAST,"groups","biaoji")
  # ASS <- AS[LAST,on=c("品种","时期","温度","部位")] %>% .[,!7] %>% .[品种 %in% c("象牙香占","R168","中广绿1号")]

  #处理变因子，固定顺序
  ASS$时期 <- factor(x = ASS$时期,levels = c("CK","5-8d","8-11d","11-14d","14-17d"))
  ASS$部位 <- factor(x = ASS$部位,levels = c("总","上","下"))
  ASS$温度 <- factor(x = ASS$温度,levels =c("28℃","30℃","33℃"))
  ASS$品种 <- factor(x = ASS$品种,levels = c("象牙香占","R168","中广绿1号"))
  ASS <- ASS[部位 %in% "总"] #筛选数据

  #作图
  ggplot(data = ASS,aes(x = ASS$温度,y = ASS$a_垩白度,fill=ASS$时期))+
    geom_bar(stat = "identity",position=position_dodge(.9),color="black")+
    facet_wrap(~品种)+     #scales="free_y"
    theme(strip.text = element_text(size = 20,face = "bold"),strip.background = element_rect(fill = "white",color = "black"),axis.text = element_text(size = 15,face = "bold"),axis.title = element_text(size = 25,face = "bold"),legend.text = element_text(size = 15,face = "bold"),legend.title = element_text(size = 20,face = "bold"),panel.background = NULL,legend.position = c(.06,.9),axis.line = element_line())+ 
    scale_fill_discrete(name="处理时期")+
    scale_size(guide="none")+
    labs(x="处理温度",y=danwei[i])+
    coord_cartesian(ylim = c(qian[i],hou[i]))+
    geom_errorbar(data = ASS,aes(ymin=ASS$a_垩白度-ASS$sd_垩白度,ymax=ASS$a_垩白度+ASS$sd_垩白度),position=position_dodge(.9),width=.2)+
    geom_text(aes(x = ASS$温度,y=ASS$a_垩白度+ASS$sd_垩白度+1,label=ASS$biaoji,size=4,fontface = "bold"),position=position_dodge(.9))+
  #保存pdf
  ggsave(filename = paste("H:/huhu/2019/制图/最终/整穗/wrap/",zhibiao[i],"_整穗.jpg",sep = ""),plot =last_plot() ,width = 15,height = 12)}



