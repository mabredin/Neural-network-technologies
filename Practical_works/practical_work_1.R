library(nnet)
x1<-rep(c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1))
x2<-rep(c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1))
x3<-rep(c(0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1))
x4<-rep(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1))
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
  n2<- (xor(n1,x3[i]))
  n3<- (xor(n2,x4[i]))
  n[i]<- !(n3)
}
n <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#визуализация результата
plot(n,type='l',col='red',lwd=7)
set.seed(5)
Nnet<-nnet(x,n,size=0,linout=TRUE, threshold = 0.0006,rep=5)
library(NeuralNetTools)
plotnet(Nnet)
predict(Nnet,x)
