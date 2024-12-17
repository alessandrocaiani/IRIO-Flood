#.libPaths(c("/mnt/beegfs/jlenia.dinoia/pkgR", .libPaths()))

library(readxl)
library(foreach)
library(doParallel)

cl<-makeCluster((15), outfile="")
#numCores <- detectCores()
#cl<-makeCluster(numCores[1]-11, outfile="")
#print(cl)
#clusterEvalQ(cl, .libPaths(c("/mnt/beegfs/jlenia.dinoia/pkgR", .libPaths())))

registerDoParallel(cl)
nstats=26

n=903
nsect=43
IO <- readRDS("../resources/IO_DiffCons.rds")
IP <- read_excel("../resources/IP2020_2021_flood.xlsx")
IP <- IP[,-1]
Service <- read_excel("../resources/Services2020_2021_flood.xlsx")
Service <- Service[,-1]
IP <- t(IP)
IP_18 <- IP[-(19:24),]
Service <- t(Service)
Service_18 <- Service[-(7:8),]
Expend <- read_excel("../resources/Expenditures_2020_2021_flood_REV1.xlsx")
Expend <- Expend[,-(1:2)]
Expend <- t(Expend)



Sample<-as.data.frame(readRDS("../resources/Sample_SensitivityCovid_datacenter.rds"))
Sample<-Sample[1:5,]


fit<-as.data.frame(array(data=NA,dim=c(nrow(Sample),1),dimnames=list(NULL,c("GDPloss"))))

fits<-foreach(j=1:nrow(Sample), .errorhandling = 'remove', .multicombine=TRUE) %dopar% {
#fits<-foreach(j=1:nrow(Sample)) %do% {
  library(matrixStats)
  library(readxl)
  library(readr)
  nstats=26
  nbRounds=80
  
  # for Sensitivity Shapley:
  source("ModelEstimation_Sensitivity.R")
  params=as.numeric(Sample[j,1:length(Sample[j,])])
  
  
  IO <- readRDS("../resources/IO_DiffCons.rds")
  IO_noCons <- readRDS("../resources/IO_noCons.rds")
  measures <- readRDS("../resources/measures_spese_flood.rds")

  
  coefficienti_consumo <- readRDS("../resources/H.rds")
  ExpService <- read_csv("../resources/ExportsServices2020_flood.csv",col_names = FALSE)
  ExpService<-ExpService[-1,-1]
  
  ExpSectoral <- read_csv("../resources/ExportsSectoral2020_flood.csv")
  ExpSectoral<-ExpSectoral[,-(1:2)]
  LD <- read_csv("../resources/lab_shocks_estimation_80rounds.csv")

  matrix_gamma_flood_43sect<-readRDS("../resources/matrix_gamma_flood_43sect.rds")
  epsilon <-matrix_gamma_flood_43sect

  
  # mobility 80 weeks:
  mobility <- as.data.frame(readRDS("../resources/weeklyMobility_flood.rds"))
  mobility<-mobility[,3:22]
  
  result<-runModel(IO,measures,coefficienti_consumo,params,nbRounds,LD,ExpService,ExpSectoral,epsilon,IO_noCons,mobility)
  Loss<-result[[1]]

  
  c(Loss)
  
}

for(i in 1:length(fits)){
  fit[i,]<-fits[[i]]
}


saveRDS(fit,file = "../results/GDPLoss_Sensitivity.rds")



stopCluster(cl)


