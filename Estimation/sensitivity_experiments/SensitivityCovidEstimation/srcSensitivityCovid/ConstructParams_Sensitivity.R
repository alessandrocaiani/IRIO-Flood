
  combinations=50000
  npars=11
  
  Sample<-matrix(data=0,combinations,npars)
  
  #WINNING COMBINATION
    Sample_est<-as.data.frame(read.csv("../resources/Sample_FullParamSpace_REV1_DiffCons.csv",header=TRUE),header=FALSE)
    win_comb<-Sample_est[16725,]    #estimated combination for modified Leontief
    win_comb<-Sample_est[12722,]    #estimated combination for traditional Leontief
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
  #cons coeff: +-25%  #first combination (w/o cons coeffs)
    #Sample[,9]<-runif(combinations, win_comb[1,46]*(1+0.25),win_comb[1,46]*(1-0.25))
    #Sample[,10]<-runif(combinations, win_comb[1,47]*(1-0.25),win_comb[1,47]*(1+0.25))
    #Sample[,11]<-runif(combinations, win_comb[1,48]*(1-0.25),win_comb[1,48]*(1+0.25))
  #cons coeff: +-0.25  #second combination
    Sample[,9]<-runif(combinations, win_comb[1,46]-0.25,win_comb[1,46]+0.25)
    Sample[,10]<-runif(combinations, win_comb[1,47]-0.25,win_comb[1,47]+0.25)
    Sample[,11]<-runif(combinations, win_comb[1,48]-0.25,win_comb[1,48]+0.25)
    
  
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
 

saveRDS(Sample, file="../Sample_SensitivityCovid.rds")
  

Sample48 <- matrix(0,combinations,48)
Sample48[,1]=Sample[,1]        #alpha
Sample48[,2]=Sample[,2]        #beta
Sample48[,3:4]=Sample[,3]      #gamma1  (agriculture, forestry and fishing)
Sample48[,5]=Sample[,4]        #gamma2  (mining and quarrying)
Sample48[,6:24]=Sample[,5]     #gamma3  (manufacturing)
Sample48[,25:27]=Sample[,6]    #gamma4  (electricity generation and water supply)
Sample48[,28]=Sample[,7]       #gamma5  (construction)
Sample48[,29:45]=Sample[,8]    #gamma6  (services)
Sample48[,46]=Sample[,9] #expenditure essential
Sample48[,47]=Sample[,10] #expenditure intermediate
Sample48[,48]=Sample[,11] #expenditure inessential


saveRDS(Sample48, file = "../Sample_SensitivityCovid_datacenter.rds")