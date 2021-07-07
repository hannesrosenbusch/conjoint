# set seed in order to replicate

set.seed(362013)
#create structure for choice data
choice<-NULL

#simulates 100 respondents and 9 choice sets
for (i in 1:100) {
  cell1<-NULL
  cell2<-NULL
  cell3<-NULL
  cell4<-NULL
  cell5<-NULL
  cell6<-NULL
  cell7<-NULL
  cell8<-NULL
  cell9<-NULL
  
  cell1<-rbind(cell1,which.max(rmultinom(1, size = 1, prob = c(0.5,0.3,0.2))))
  cell2<-rbind(cell2,which.max(rmultinom(1, size = 1, prob = c(0.5,0.2,0.3))))
  cell3<-rbind(cell3,which.max(rmultinom(1, size = 1, prob = c(0.5,0.1,0.4))))
  cell4<-rbind(cell4,which.max(rmultinom(1, size = 1, prob = c(0.4,0.3,0.3))))
  cell5<-rbind(cell5,which.max(rmultinom(1, size = 1, prob = c(0.4,0.2,0.4))))
  cell6<-rbind(cell6,which.max(rmultinom(1, size = 1, prob = c(0.4,0.1,0.5))))
  cell7<-rbind(cell7,which.max(rmultinom(1, size = 1, prob = c(0.3,0.3,0.4))))
  cell8<-rbind(cell8,which.max(rmultinom(1, size = 1, prob = c(0.3,0.2,0.5))))
  cell9<-rbind(cell9,which.max(rmultinom(1, size = 1, prob = c(0.3,0.1,0.6))))
  
  row<-cbind(cell1,cell2,cell3,cell4,cell5,cell6,cell7,cell8,cell9)
  choice<-rbind(choice,row)
}

choice
apply(choice,2,table)
choice_df<-data.frame(cbind(1:100,choice))
names(choice_df)<-c("id","set1","set2","set3","set4","set5","set6","set7","set8","set9")

choice_df







design1<-matrix(c(
  1,0,1,0,0,0,
  0,1,0,0,1,0,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design1

design2<-matrix(c(
  1,0,1,0,0,0,
  0,1,0,0,0,1,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design2

design3<-matrix(c(
  1,0,1,0,0,0,
  0,1,0,0,0,0,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design3

design4<-matrix(c(
  1,0,0,1,0,0,
  0,1,0,0,1,0,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design4

design5<-matrix(c(
  1,0,0,1,0,0,
  0,1,0,0,0,1,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design5

design6<-matrix(c(
  1,0,0,1,0,0,
  0,1,0,0,0,0,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design6

design7<-matrix(c(
  1,0,0,0,0,0,
  0,1,0,0,1,0,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design7

design8<-matrix(c(
  1,0,0,0,0,0,
  0,1,0,0,0,1,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design8

design9<-matrix(c(
  1,0,0,0,0,0,
  0,1,0,0,0,0,
  0,0,0,0,0,0),
  nrow=3,ncol=6,byrow=TRUE)
design9








id=levels(as.factor(choice_df[,1]))
lgtdata=NULL

for (i in 1:100)
{
  respdata=choice_df[choice_df[,1]==id[i],]
  ty<-NULL
  tdesign<-NULL
  ty=c(ty,respdata$set1)
  tdesign=rbind(tdesign,design1)
  ty=c(ty,respdata$set2)
  tdesign=rbind(tdesign,design2)
  ty=c(ty,respdata$set3)
  tdesign=rbind(tdesign,design3)
  ty=c(ty,respdata$set4)
  tdesign=rbind(tdesign,design4)
  ty=c(ty,respdata$set5)
  tdesign=rbind(tdesign,design5)
  ty=c(ty,respdata$set6)
  tdesign=rbind(tdesign,design6)
  ty=c(ty,respdata$set7)
  tdesign=rbind(tdesign,design7)
  ty=c(ty,respdata$set8)
  tdesign=rbind(tdesign,design8)
  ty=c(ty,respdata$set9)
  tdesign=rbind(tdesign,design9)
  lgtdata[[i]]=list(y=ty,X=as.matrix(tdesign))
}










mcmc=list(R=20000,keep=10)

library(bayesm)
out=rhierMnlRwMixture(Data=list(p=3,lgtdata=lgtdata),Prior=list(ncomp=1),Mcmc=mcmc)
plot(out$loglike,type="l")
trace<-t(apply(out$betadraw,c(2,3),mean))
matplot(trace, type="l")
beta.501_2000<-apply(out$betadraw[,,501:2000], 2, mean)
beta.1001_2000<-apply(out$betadraw[,,1001:2000], 2, mean)
beta.1501_2000<-apply(out$betadraw[,,1501:2000], 2, mean)
cbind(beta.501_2000,beta.1001_2000,beta.1501_2000)
