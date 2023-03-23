library(wavelets)
library(party)

feature<-NULL
mydata<-
  read.table("http://archive.ics.uci.edu/ml/databases/synthetic_control/synthetic_control.data",header=F,sep="")
for(i in 1:nrow(mydata)){
  a<-t(mydata[i,])
  wt<-dwt(a,filter="haar",boundary="periodic")
  feature<-rbind(feature,unlist(c(wt@W,wt@V[[wt@level]])))
}
feature<-as.data.frame(feature)

classId<-c(rep("1",100),rep("2",100),rep("3",100),rep("4",100),rep("5",100),rep("6",100))
wtSc <- data.frame(cbind(classId,feature))

ct <- ctree(classId~.,data = wtSc,controls = ctree_control(minsplit = 30,minbucket = 10,maxdepth = 5))
pClassId <- predict(ct)
table(classId,pClassId)

(sum(classId==pClassId))/nrow(wtSc)

plot(ct,ip_args=list(pval=F),ep_args=list(digits=0))

f <- NULL
a <- t(mydata[300,])
wt<-dwt(a,filter="haar",boundary="periodic")
f <- data.frame(rbind(f,unlist(c(wt@W,wt@V[[wt@level]]))))
predict(ct,f)







