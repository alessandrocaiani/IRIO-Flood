#This is for the estimation of the model with the traditional Leontief p.f.
#.libPaths(c("/mnt/beegfs/jlenia.dinoia/pkgR", .libPaths()))

library(readxl)
library(foreach)
library(doParallel)

cl<-makeCluster((15), outfile="")
#numCores <- detectCores()
#cl<-makeCluster(numCores[1]-10, outfile="")
#print(cl)
#clusterEvalQ(cl, .libPaths(c("/mnt/beegfs/jlenia.dinoia/pkgR", .libPaths())))

registerDoParallel(cl)
nstats=26

n=903
nsect=43
IO <- readRDS("resources/IO_DiffCons.rds")
IP <- read_excel("resources/IP2020_2021_flood.xlsx")
IP <- IP[,-1]
Service <- read_excel("resources/Services2020_2021_flood.xlsx")
Service <- Service[,-1]
IP <- t(IP)
IP_18 <- IP[-(19:24),]
Service <- t(Service)
Service_18 <- Service[-(7:8),]
Expend <- read_excel("resources/Expenditures_2020_2021_flood_REV1.xlsx")
Expend <- Expend[,-(1:2)]
Expend <- t(Expend)

#for the weighting matrix:
outputIO<-t(as.data.frame(rowSums(IO[1:n,])))
tot_gross_output <- sum(outputIO)
outputIO_sect_3<-sum(outputIO[seq(3,903,by=nsect)])
outputIO_sect_4<-sum(outputIO[seq(4,903,by=nsect)])
outputIO_sect_5<-sum(outputIO[seq(5,903,by=nsect)])
outputIO_sect_6<-sum(outputIO[seq(6,903,by=nsect)])
outputIO_sect_7<-sum(outputIO[seq(7,903,by=nsect)])
outputIO_sect_8<-sum(outputIO[seq(8,903,by=nsect)])
outputIO_sect_9<-sum(outputIO[seq(9,903,by=nsect)])
outputIO_sect_10<-sum(outputIO[seq(10,903,by=nsect)])
outputIO_sect_11<-sum(outputIO[seq(11,903,by=nsect)])
outputIO_sect_12<-sum(outputIO[seq(12,903,by=nsect)])
outputIO_sect_13<-sum(outputIO[seq(13,903,by=nsect)])
outputIO_sect_14<-sum(outputIO[seq(14,903,by=nsect)])
outputIO_sect_15<-sum(outputIO[seq(15,903,by=nsect)])
outputIO_sect_16<-sum(outputIO[seq(16,903,by=nsect)])
outputIO_sect_17<-sum(outputIO[seq(17,903,by=nsect)])
outputIO_sect_18<-sum(outputIO[seq(18,903,by=nsect)])
outputIO_sect_19<-sum(outputIO[seq(19,903,by=nsect)])
outputIO_sect_20<-sum(outputIO[seq(20,903,by=nsect)])
outputIO_sect_21<-sum(outputIO[seq(21,903,by=nsect)])
outputIO_sect_22<-sum(outputIO[seq(22,903,by=nsect)])
outputIO_sect_23<-sum(outputIO[seq(23,903,by=nsect)])
outputIO_sect_26<-sum(outputIO[seq(26,903,by=nsect)])
outputIO_sect_27<-sum(outputIO[seq(27,903,by=nsect)])
outputIO_sect_28<-sum(outputIO[seq(28,903,by=nsect)])
outputIO_sect_29<-sum(outputIO[seq(29,903,by=nsect)])
outputIO_sect_30<-sum(outputIO[seq(30,903,by=nsect)])
outputIO_sect_31<-sum(outputIO[seq(31,903,by=nsect)])
outputIO_sect_32<-sum(outputIO[seq(32,903,by=nsect)])


nItalianRegions=21
nSectors=43
constant=c(1,2,4,6,8)
n_constant=length(constant)
decline=c(3,5,10,12)
n_decline=length(decline)
bad=c(7,9,11)
n_bad=length(bad)
Cons_indices=c(rep(1:12,nItalianRegions))
indices_constant=c(rep(constant,nItalianRegions))
indices_decline=c(rep(decline,nItalianRegions))
indices_bad=c(rep(bad,nItalianRegions))
indices_1=c(rep(c(1),nItalianRegions))
indices_2=c(rep(c(2),nItalianRegions))
indices_3=c(rep(c(3),nItalianRegions))
indices_4=c(rep(c(4),nItalianRegions))
indices_5=c(rep(c(5),nItalianRegions))
indices_6=c(rep(c(6),nItalianRegions))
indices_7=c(rep(c(7),nItalianRegions))
indices_8=c(rep(c(8),nItalianRegions))
indices_9=c(rep(c(9),nItalianRegions))
indices_10=c(rep(c(10),nItalianRegions))
indices_11=c(rep(c(11),nItalianRegions))
indices_12=c(rep(c(12),nItalianRegions))


nFD=16
for(i in 1:nItalianRegions){
  Cons_indices[((i-1)*12+1):((i-1)*12+12)]=Cons_indices[((i-1)*12+1):((i-1)*12+12)]+nFD*(i-1)
  indices_constant[((i-1)*length(constant)+1):((i-1)*length(constant)+length(constant))]=indices_constant[((i-1)*length(constant)+1):((i-1)*length(constant)+length(constant))]+nFD*(i-1)
  indices_decline[((i-1)*length(decline)+1):((i-1)*length(decline)+length(decline))]=indices_decline[((i-1)*length(decline)+1):((i-1)*length(decline)+length(decline))]+nFD*(i-1)
  indices_bad[((i-1)*length(bad)+1):((i-1)*length(bad)+length(bad))]=indices_bad[((i-1)*length(bad)+1):((i-1)*length(bad)+length(bad))]+nFD*(i-1)
  indices_1[(i)]=indices_1[(i)]+nFD*(i-1)
  indices_2[(i)]=indices_2[(i)]+nFD*(i-1)
  indices_3[(i)]=indices_3[(i)]+nFD*(i-1)
  indices_4[(i)]=indices_4[(i)]+nFD*(i-1)
  indices_5[(i)]=indices_5[(i)]+nFD*(i-1)
  indices_6[(i)]=indices_6[(i)]+nFD*(i-1)
  indices_7[(i)]=indices_7[(i)]+nFD*(i-1)
  indices_8[(i)]=indices_8[(i)]+nFD*(i-1)
  indices_9[(i)]=indices_9[(i)]+nFD*(i-1)
  indices_10[(i)]=indices_10[(i)]+nFD*(i-1)
  indices_11[(i)]=indices_11[(i)]+nFD*(i-1)
  indices_12[(i)]=indices_12[(i)]+nFD*(i-1)
}
cons_columns=IO[1:(nItalianRegions*nSectors),n+Cons_indices]
cons_constant=IO[1:(nItalianRegions*nSectors),n+indices_constant]
cons_decline=IO[1:(nItalianRegions*nSectors),n+indices_decline]
cons_bad=IO[1:(nItalianRegions*nSectors),n+indices_bad]

consIO_constant<-sum(rowSums(cons_constant))
consIO_decline<-sum(rowSums(cons_decline))
consIO_bad<-sum(rowSums(cons_bad))
consTot=consIO_constant+consIO_decline+consIO_bad


#weighting matrix (only for sectors of interest):
Wmat <- matrix(0, nrow = nstats+3, ncol = nstats+3)
Wmat[1,1] <- outputIO_sect_3/tot_gross_output
Wmat[2,2] <- outputIO_sect_4/tot_gross_output
Wmat[3,3] <- outputIO_sect_5/tot_gross_output
Wmat[4,4] <- outputIO_sect_6/tot_gross_output
Wmat[5,5] <- outputIO_sect_7/tot_gross_output
Wmat[6,6] <- outputIO_sect_8/tot_gross_output
Wmat[7,7] <- outputIO_sect_9/tot_gross_output
Wmat[8,8] <- outputIO_sect_10/tot_gross_output
Wmat[9,9] <- outputIO_sect_11/tot_gross_output
Wmat[10,10] <- outputIO_sect_12/tot_gross_output
Wmat[11,11] <- outputIO_sect_13/tot_gross_output
Wmat[12,12] <- outputIO_sect_14/tot_gross_output
Wmat[13,13] <- outputIO_sect_15/tot_gross_output
Wmat[14,14] <- outputIO_sect_16/tot_gross_output
Wmat[15,15] <- outputIO_sect_17/tot_gross_output
Wmat[16,16] <- outputIO_sect_18/tot_gross_output
Wmat[17,17] <- outputIO_sect_19/tot_gross_output
Wmat[18,18] <- outputIO_sect_20/tot_gross_output
Wmat[19,19] <- outputIO_sect_21/tot_gross_output
Wmat[20,20] <- outputIO_sect_22/tot_gross_output
Wmat[21,21] <- outputIO_sect_23/tot_gross_output
Wmat[22,22] <- outputIO_sect_26/tot_gross_output
Wmat[23,23] <- outputIO_sect_27/tot_gross_output
Wmat[24,24] <- outputIO_sect_28/tot_gross_output
Wmat[25,25] <- outputIO_sect_29/tot_gross_output
Wmat[26,26] <- outputIO_sect_30/tot_gross_output
Wmat[27,27] <- consIO_constant/tot_gross_output
Wmat[28,28] <- consIO_decline/tot_gross_output
Wmat[29,29] <- consIO_bad/tot_gross_output


# Restricted sample (only for exercise)
#Sample<-as.data.frame(read.csv("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/TradL/Sample_FullParamSpace_REV1_DiffCons_neighb.csv",header=TRUE),header=FALSE)
#Sample<-as.data.frame(read.csv("/home/jlenia.dinoia/jlenia.dinoia/IO_flood/Sample_FullParamSpace_REV1_DiffCons_neighb.csv",header=TRUE),header=FALSE)

# Unrestricted sample
Sample<-as.data.frame(read.csv("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/FINAL_REV1/ESTIMATION/Sample_FullParamSpace_REV1_DiffCons.csv",header=TRUE),header=FALSE)
#Sample<-as.data.frame(read.csv("/home/jlenia.dinoia/jlenia.dinoia/IO_flood/Sample_FullParamSpace_REV1_DiffCons.csv",header=TRUE),header=FALSE)

Sample<-Sample[1:50000,]

fit<-as.data.frame(array(data=NA,dim=c(nrow(Sample),nstats+4),dimnames=list(NULL,c("loss",c(1:(nstats+3))))))

fits<-foreach(j=1:nrow(Sample), .errorhandling = 'remove', .multicombine=TRUE) %dopar% {
#fits<-foreach(j=1:nrow(Sample)) %do% {
  library(matrixStats)
  library(readxl)
  library(readr)
  nstats=26
  nbRounds=80
  source("ModelEstimationTraditionalLeontief.R")
  params=as.numeric(Sample[j,1:length(Sample[j,])])
  IO <- readRDS("resources/IO_DiffCons.rds")
  IO_noCons <- readRDS("resources/IO_noCons.rds")
  measures <- readRDS("resources/measures_spese_flood.rds")

  
  coefficienti_consumo <- readRDS("resources/H.rds")

  ExpService <- read_csv("resources/ExportsServices2020_flood.csv",col_names = FALSE)
  ExpService<-ExpService[-1,-1]
  
  ExpSectoral <- read_csv("resources/ExportsSectoral2020_flood.csv")
  ExpSectoral<-ExpSectoral[,-(1:2)]
  LD <- read_csv("resources/lab_shocks_estimation_80rounds.csv")
  #LD <- read_csv("/home/jlenia.dinoia/jlenia.dinoia/IO_flood/lab_shocks_estimation_80rounds.csv")
  
  matrix_gamma_flood_43sect<-readRDS("resources/matrix_gamma_flood_43sect.rds")
  epsilon <-matrix_gamma_flood_43sect
  
 
  
  # mobility 80 weeks:
  mobility <- as.data.frame(readRDS("resources/weeklyMobility_flood.rds"))
  mobility<-mobility[,3:22]
  
  result<-runModel(IO,measures,coefficienti_consumo,params,nbRounds,LD,ExpService,ExpSectoral,epsilon,IO_noCons,mobility)
  simvec<-result[[1]]
  prod_sectors<-result[[2]]
  Prod_sim<-prod_sectors
  prod_services<-result[[3]]
  Serv_sim<-prod_services
  Expenditure<-result[[4]]
  Expenditure_sim<-Expenditure
  
  
  
  # To calculate the loss function
  vec1=c(rep(0,22))
  vec2=c(rep(0,4))
  vec3=c(rep(0,3))
  
  for(i in 1:22){
    error=0
    for(t in 1:18){
      error=error+(as.numeric(IP_18[t,i])-Prod_sim[t,i])^2
    }
    vec1[i]<-error/18
  }
  
  for(i in 1:4){
    error=0
    for(t in 1:6){
      error=error+(as.numeric(Service_18[t,i])-Serv_sim[t,i])^2
    }
    vec2[i]<-error/6
  }
  
  for(i in 1:3){
    error=0
    for(t in 1:6){
      error=error+(as.numeric(Expend[t,i])-Expenditure_sim[t,i])^2
    }
    vec3[i]<-error/6
  }
  
  vec=c(vec1,vec2,vec3)
  
  objective=(t(vec))%*% Wmat %*% (vec)
  
  rm(Prod_sim,Serv_sim,Expenditure_sim,simvec,vec1,vec2,vec3,result)
  
  c(objective,vec)
  
}

for(i in 1:length(fits)){
  fit[i,]<-fits[[i]]
}

saveRDS(fit,file = "results/MSM_REV1_DiffCons_EXP_TradL_unrstr.rds")


stopCluster(cl)


