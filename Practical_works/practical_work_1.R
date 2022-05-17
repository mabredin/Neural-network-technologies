library(nnet)
library(NeuralNetTools)
x1<-c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)
x2<-c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1)
x3<-c(0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1)
x4<-c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)
#матрица значений
x<-matrix(0,nrow=16,ncol=4)
for(i in 1:4){
  if(i==1) for(j in 1:16) x[j,i]<-x1[j]
  if(i==2) for(j in 1:16) x[j,i]<-x2[j]
  if(i==3) for(j in 1:16) x[j,i]<-x3[j]
  if(i==4) for(j in 1:16) x[j,i]<-x4[j]
}
n<-rep(c(0),16)
for (i in 1:16) {
  n1<- (xor(x1[i],x2[i]))
  n2<- (xor(x2[i],x3[i]))
  n3<- (xor(x3[i],x4[i]))
  n[i]<- !(n1|n2|n3)
}
#визуализация результата
plot(n,type='l',col='red',lwd=7)
set.seed(35)
Nnet<-nnet(x,n,size=2,linout=TRUE, rang=0.5)
plotnet(Nnet)
predict(Nnet,x)