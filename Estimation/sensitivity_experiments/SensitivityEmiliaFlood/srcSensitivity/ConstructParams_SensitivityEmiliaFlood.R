
  combinations=50000
  npars=8
  
  Sample<-matrix(data=0,combinations,npars)
  
  #WINNING COMBINATION
    Sample_est<-as.data.frame(read.csv("../resources/Sample_FullParamSpace_REV1_DiffCons.csv",header=TRUE),header=FALSE)
    win_comb<-Sample_est[16725,]    #estimated combination for modified Leontief
    #win_comb<-Sample_est[12722,]    #estimated combination for traditional Leontief
    rm(Sample_est)
  
  #alpha: +-25%
    Sample[,1]<-runif(combinations,win_comb[1,1]*(1-0.25),win_comb[1,1]*(1+0.25))
  #beta: +-25%
    Sample[,2]<-runif(combinations,win_comb[1,2]*(1-0.25),win_comb[1,2]*(1+0.25))
  #gammas: +-25%
    Sample[,3]<-runif(combinations,win_comb[1,3]*(1-0.25),win_comb[1,3]*(1+0.25))
    Sample[,4]<-runif(combinations,win_comb[1,5]*(1-0.25),win_comb[1,5]*(1+0.25))
    Sample[,5]<-runif(combinations,win_comb[1,6]*(1-0.25),win_comb[1,6]*(1+0.25))
    Sample[,6]<-runif(combinations,win_comb[1,25]*(1-0.25),win_comb[1,25]*(1+0.25))
    Sample[,7]<-runif(combinations,win_comb[1,28]*(1-0.25),win_comb[1,28]*(1+0.25))
    Sample[,8]<-runif(combinations,win_comb[1,29]*(1-0.25),win_comb[1,29]*(1+0.25))
  
    
  Sample[,1]<-round(Sample[,1],digits = 3)
  Sample[,2]<-round(Sample[,2],digits = 0)
  Sample[,3]<-round(Sample[,3],digits = 0)
  Sample[,4]<-round(Sample[,4],digits = 0)
  Sample[,5]<-round(Sample[,5],digits = 0)
  Sample[,6]<-round(Sample[,6],digits = 0)
  Sample[,7]<-round(Sample[,7],digits = 0)
  Sample[,8]<-round(Sample[,8],digits = 0)
  
  
  as.matrix(Sample)
 
#save the list of parameter configurations in a synthetic way (8 parameters gammas only displayed once)
saveRDS(Sample, file="../Sample_SensitivityEmiliaFlood.rds")
  
#save the list of parameter configurations in the extended format (45 parameters as gammas displayed multiple times, i.e. for each sector)
Sample45 <- matrix(0,combinations,45)
Sample45[,1]=Sample[,1]        #alpha
Sample45[,2]=Sample[,2]        #beta
Sample45[,3:4]=Sample[,3]      #gamma1  (agriculture, forestry and fishing)
Sample45[,5]=Sample[,4]        #gamma2  (mining and quarrying)
Sample45[,6:24]=Sample[,5]     #gamma3  (manufacturing)
Sample45[,25:27]=Sample[,6]    #gamma4  (electricity generation and water supply)
Sample45[,28]=Sample[,7]       #gamma5  (construction)
Sample45[,29:45]=Sample[,8]    #gamma6  (services)


saveRDS(Sample45, file = "../Sample_SensEmiliaFlood_datacenter.rds")