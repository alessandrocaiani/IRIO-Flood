getParams<-function(n){
  combinations=n
  npars=11
  
  Sample<-matrix(data=0,combinations,npars)
  
  
  
  #+-0.01
  Sample[,1]<-runif(combinations,0.034,0.054)
  #+-10
  Sample[,2]<-runif(combinations,48,68)
  #+-20%
  Sample[,3]<-runif(combinations,14,20)
  Sample[,4]<-runif(combinations,16,24)
  Sample[,5]<-runif(combinations,9,13)
  Sample[,6]<-runif(combinations,24,36)
  Sample[,7]<-runif(combinations,18,28)
  Sample[,8]<-runif(combinations,10,14)
  #+-0.25
  Sample[,9]<-runif(combinations,-0.45,0.05)
  Sample[,10]<-runif(combinations,0.15,0.65)
  Sample[,11]<-runif(combinations,2.15,2.65)
  
  Sample[,1]<-round(Sample[,1],digits = 3)
  Sample[,2]<-round(Sample[,2],digits = 0)
  Sample[,3]<-round(Sample[,3],digits = 0)
  Sample[,4]<-round(Sample[,4],digits = 0)
  Sample[,5]<-round(Sample[,5],digits = 0)
  Sample[,6]<-round(Sample[,6],digits = 0)
  Sample[,7]<-round(Sample[,7],digits = 0)
  Sample[,8]<-round(Sample[,8],digits = 0)
  Sample[,9]<-round(Sample[,9],digits = 2)
  Sample[,10]<-round(Sample[,10],digits = 2)
  Sample[,11]<-round(Sample[,11],digits = 2)
  
  as.matrix(Sample)
  
}