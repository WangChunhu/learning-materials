install.packages("dplyr")

library(dplyr)
library(openxlsx)
hu <- read.xlsx("H:/�ۺ�/��ʦ��/xgfx.xlsx",colNames=TRUE)

#���ݼ�����ת��
hu1 <- tbl_df(hu)
#HU <- as.data.frame(hu1)
names(hu1)
nrow(hu1)
ncol(hu1)

#����
hu34 <- group_by(hu1,Cultivar)   #����
groups(hu34)   #��ȡ�������ݼ���ʹ�õķ������
#groups(ungroup(hu34))   #�����ݿ����Ƴ�������Ϣ
group_indices(hu1,Cultivar)   #����ÿ����¼���ڷ���id��ɵ�����,������ĵڼ���
group_size(hu34)   #����ÿ�����������
n_groups(hu34)   #���ط�����
hu35 <- count(hu1,Cultivar,sort = TRUE)   #�����飬�Է���������м���������

#ɸѡ��
hu2 <- slice(hu1,5:n())   #�и��� �������һ�� (slice() ����ͨ���к�ѡȡ����)   
hu3 <- slice(hu1,5:30L)      
hu5 <- slice(hu1,5L,7L)
hu6 <- filter(hu1,GY-1 > 6.12 ,GY-2 > 8)  #����
hu7 <- filter(hu1,Cultivar == "Y_Lliangyou 900" , GY_1 > 6.12 , GY_4 == 10.98)
hu8 <- filter(hu1,Cultivar %in% c("Y_Lliangyou 900","Shanyou63"))   # %in% "ͬһ����-��" 
hu9 <- filter(hu1,GY_1 > 6.12 | GY_4 < 0)   # |  "��ͬ������" 

#���򣨰��������������ζ��н�������
hu10 <- arrange(hu1,Cultivar,IR_H)   # ���򣬿ɶ������
hu11 <- arrange(hu1,desc(Cultivar))   #����,ֻ��һ������

#ѡ����
hu12 <- select(hu1,Cultivar,IR_H,GY_1,IR_W)  
hu13 <- select(hu1,-Cultivar,-IR_H,-GY_1,-IR_W)  #ѡ������⼸��,��-
hu14 <- select(hu1,-starts_with("GY"))  #ǰ׺����GY����׺��ends_with��������contains
hu15 <- select(hu1,Cultivar:GY)  #ѡ�����
hu16 <- select(hu1,-contains("_"))
hu29 <- select(hu1,everything())   #����������
hu17 <- select(hu1,q,GY_8,GY_7,everything())   #����������,����q�зŵ���ǰ��,����

#������
hu30 <- rename(hu1,Q = q) #�������У�����ȫ����,ǰΪ����
hu31 <- select(hu1,ir =starts_with("IR"))  #�������У�ֻ��������������

#����
hu18 <- mutate(hu1,Q=q * 11)   #���㲢��������
hu19 <- transmute(hu1,Q=q * 11)   #���㲢ֻ��������

#ȥ��
hu32 <- distinct(hu1,Cultivar)   #ȥ�������ظ������ظñ���
hu20 <- nrow(distinct(hu1,e))   #ȥ�������ظ������������͸ñ���
hu21 <- distinct(hu1,Cultivar,.keep_all = TRUE)   #ȥ�������ظ����������б���

#����
hu22 <- summarise(hu1,mean(GY-1))   # sd(),max(),min(),n(),first(),last(),sum()
hu23 <- summarise(hu1,n_distinct(GY-1))   #���ر������ظ��ĸ���
hu24 <- summarise_all(hu1,mean)   #����������
hu25 <- summarise_if(hu1,is.numeric,sd)   #����������   
hu26 <- summarise_at(hu1,2:8,mean)   # ���м���  c(1,3,5)��ֱ���ñ�����
hu27 <- summarise_if(select(hu1,c(1,3,4)),is.numeric,funs(min,max,mean,sum,sd))   #�ຯ������
hu28 <- summarise(hu1,nth(IR_H,2))   #���ر����еڶ���ֵ
hu33 <- summarise(hu1,last(Cultivar))   #���ر��������һ����ֵ

#�������   #if_else�ᱣ��ԭ����������
x <- c(-5:5,NA)
if_else(x < 0,NA_integer_,x)   #�������һ��
if_else(condition = x < 0,true = "negative",false = "positive",missing = "missing")   #�������һ��
ifelse(x < 0,"negative",1)   #������Ͳ�һ��

  
  



