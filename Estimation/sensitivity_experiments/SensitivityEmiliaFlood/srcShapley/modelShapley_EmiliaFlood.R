modelShapley<-function(params){
  library(dplyr)
  library(readr)
  library(readxl)
  library(foreach)
  library(doParallel)
 
  print(dim(params))
  # print(params[1:30,])
  ncores<-detectCores()
  # print(ncores)
  cl<-makeCluster((32), outfile="")

  registerDoParallel(cl)
  ob=c(rep(0,nrow(params)))
  # params <- readRDS("Shapley_Luigi\\params.rds")
  # print(ob)
  
 obs<-foreach(s=1:3,#nrow(params), #.errorhandling = 'remove', 
              .multicombine=TRUE,.combine='cbind') %do% {

    suppressPackageStartupMessages(library(dplyr))
    library(matrixStats) %>% suppressPackageStartupMessages()
    library(readr) %>% suppressPackageStartupMessages()
    library(readxl) %>% suppressPackageStartupMessages()
    library(foreach) %>% suppressPackageStartupMessages()
    library(doParallel) %>% suppressPackageStartupMessages()

    
    rounds=108
    
    source("functionShapley_EmiliaFlood.R")
    IO <- readRDS("../resources/IO_ITAregions.rds")
    measures <- readRDS("../resources/measures_spese_flood.rds")
    
    coefficienti_consumo <- readRDS("../resources/H.rds")
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
    

    pars=c(rep(0,48))   
    pars[1]=params[s,1]        #alpha
    pars[2]=params[s,2]        #beta
    pars[3:4]=params[s,3]      #gamma1  (agriculture, forestry and fishing)
    pars[5]=params[s,4]        #gamma2  (mining and quarrying)
    pars[6:24]=params[s,5]     #gamma3  (manufacturing)
    pars[25:27]=params[s,6]    #gamma4  (electricity generation and water supply)
    pars[28]=params[s,7]       #gamma5  (construction)
    pars[29:45]=params[s,8]    #gamma6  (services)


    
    Loss<-runModel(IO,measures,coefficienti_consumo,params,nbRounds,LD_Emilia_hazus,LD_Emilia_ins_M,LossRatio_Emilia_I,scenario,nodemand,epsilon)
    Loss
    
    
  }
  
  # print(class(obs))
  # print(length(obs))
  for(i in 1:length(obs)){
    ob[i]<-obs[[i]]
  }
  # print("matrix ob")
  # print(ob)
  ob
  stopCluster(cl)
}

