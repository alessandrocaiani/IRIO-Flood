#.libPaths(c("/mnt/beegfs/jlenia.dinoia/pkgR", .libPaths()))

library(readxl)
library(readr)
library(foreach)
library(doParallel)

cl<-makeCluster((15), outfile="")
#numCores <- detectCores()
#cl<-makeCluster(numCores[1]-11, outfile="")
#print(cl)
#clusterEvalQ(cl, .libPaths(c("/mnt/beegfs/jlenia.dinoia/pkgR", .libPaths())))

registerDoParallel(cl)
nstats=26

TradLeontief=0  #0=modified Leontief 1=traditional Leontief


#import the IO table
IO <- readRDS("../resources/IO_ITAregions.rds")

measures <- readRDS("../resources/measures_spese_flood.rds")
#import the H matrix
coefficienti_consumo <- readRDS("../resources/H.rds")
#import the epsilon for the modified Leontief model
epsilon<-readRDS("../resources/matrix_gamma_flood_43sect.rds")

##Labor shocks using Hazus restoration times:
LD_Emilia_hazus <- read_csv("../resources/lab_shocks_EROMgeoloc.csv")

##Labor shocks using insurance claims' business interruption data related to machinery damages:
LD_Emilia_ins_M <- read_csv("../resources/output_shocks_EROMgeoloc_M.csv")
##Shocks to inventories using insurance claims' loss ratio:
LossRatio_Emilia_I <- read_csv("../resources/LR_shocks_EROMgeoloc_I_met2.csv")


# choose scenario to run (1=hazus downtime periods; 2=insurance claims)
scenario=1
nodemand=0
nbRounds=108
Sample<-as.data.frame(readRDS("../resources/Sample_SensEmiliaFlood_datacenter.rds"))
Sample<-Sample[1:50,]


lossesEmilia<-as.data.frame(array(data=NA,dim=c(nrow(Sample),4),dimnames=list(NULL,c("EmiliaGDPloss1y","EmiliaOutputLoss1y","EmiliaGDPlossTot","EmiliaOutputLossTot"))))
source("Model_SensitivityEmiliaFlood.R")
fits<-foreach(j=1:nrow(Sample), .errorhandling = 'remove', .multicombine=TRUE) %dopar% {

  library(matrixStats)
  library(readxl)
  library(readr)
  
  
  params=as.numeric(Sample[j,1:length(Sample[j,])])
  
  result<-runModel(IO,measures,coefficienti_consumo,params,nbRounds,LD_Emilia_hazus,LD_Emilia_ins_M,LossRatio_Emilia_I,scenario,nodemand,epsilon)
  
  c(result)
  
}

for(i in 1:length(fits)){
  lossesEmilia[i,]<-fits[[i]]
}


saveRDS(lossesEmilia,file = "../results/EmiliaGDPLoss_Sensitivity.rds")



stopCluster(cl)


