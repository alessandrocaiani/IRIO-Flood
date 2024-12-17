getParams_EmiliaFlood<-function(n){
  combinations=n
  npars=8
  
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
  
  
  Sample[,1]<-round(Sample[,1],digits = 3)
  Sample[,2]<-round(Sample[,2],digits = 0)
  Sample[,3]<-round(Sample[,3],digits = 0)
  Sample[,4]<-round(Sample[,4],digits = 0)
  Sample[,5]<-round(Sample[,5],digits = 0)
  Sample[,6]<-round(Sample[,6],digits = 0)
  Sample[,7]<-round(Sample[,7],digits = 0)
  Sample[,8]<-round(Sample[,8],digits = 0)

  
  as.matrix(Sample)
  
}