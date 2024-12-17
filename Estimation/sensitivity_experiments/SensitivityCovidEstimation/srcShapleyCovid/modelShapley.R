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

    
    rounds=80
    
    source("functionShapley.R")
    IO <- readRDS("../resources/IO_DiffCons.rds")
    IO_noCons <- readRDS("../resources/IO_noCons.rds")
    measures <- readRDS("res/rds/measures_spese_flood.rds")
    
    
    coefficienti_consumo <- readRDS("../resources/H.rds")
    
    ExpService <- read_csv("../resources/ExportsServices2020_flood.csv",col_names = FALSE)  %>% suppressMessages()
    ExpService<-ExpService[-1,-1]
    
    ExpSectoral <- read_csv("../resources/ExportsSectoral2020_flood.csv")  %>% suppressMessages()
    ExpSectoral<-ExpSectoral[,-(1:2)]
    LD <- read_csv("../resources/lab_shocks_estimation_80rounds.csv",progress = FALSE)  %>% suppressMessages()
    
    matrix_gamma_flood_43sect<-readRDS("../resources/matrix_gamma_flood_43sect.rds")
    epsilon <-matrix_gamma_flood_43sect
    
    
    # mobility 80 weeks:
    mobility <- as.data.frame(readRDS("../resources/weeklyMobility_flood.rds"))
    mobility<-mobility[,3:22]
    

    pars=c(rep(0,48))   
    pars[1]=params[s,1]        #alpha
    pars[2]=params[s,2]        #beta
    pars[3:4]=params[s,3]      #gamma1  (agriculture, forestry and fishing)
    pars[5]=params[s,4]        #gamma2  (mining and quarrying)
    pars[6:24]=params[s,5]     #gamma3  (manufacturing)
    pars[25:27]=params[s,6]    #gamma4  (electricity generation and water supply)
    pars[28]=params[s,7]       #gamma5  (construction)
    pars[29:45]=params[s,8]    #gamma6  (services)
    pars[46]=params[s,9]
    pars[47]=params[s,10]
    pars[48]=params[s,11]
    
    
    
    
    Loss<-runModel(IO,IO_noCons,measures,coefficienti_consumo,pars,rounds,LD,ExpService,ExpSectoral,epsilon,mobility)
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

