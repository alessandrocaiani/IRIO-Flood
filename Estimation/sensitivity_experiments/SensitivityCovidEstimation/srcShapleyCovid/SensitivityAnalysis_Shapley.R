suppressPackageStartupMessages(library(dplyr))
library('gtools') %>% suppressPackageStartupMessages()
library('doParallel') %>% suppressPackageStartupMessages()
library('sensitivity') %>% suppressPackageStartupMessages()

setwd("/mnt/beegfs/lcesarini/2024_shapley")

# Get command-line arguments (ignoring arguments passed to Rscript itself)
args <- commandArgs(trailingOnly = TRUE)

# Check if the correct number of arguments are provided
if (length(args) < 1) {
  stop("At least one arguments must be provided: <arg1> <arg2>")
}

# Convert the arguments from character to numeric if necessary
arg1 <- args[1]

d <- 11  #parameters

if (arg1=="test") {
    source("getParams.R")
    source("getParams2.R")
    source("modelShapley.R")

    Nv=1e2
    m=1e2


    modelShapley<-function(params){
        library(dplyr)
        library(readr)
        library(readxl)
        library(foreach)
        library(doParallel)

        print(dim(params))

        ncores<-detectCores()
        print(ncores)
        cl<-makeCluster((112), outfile="")

        registerDoParallel(cl)
        ob=c(rep(0,nrow(params)))
    
        obs<-foreach(s=1:320, #.errorhandling = 'remove', 
                    .multicombine=TRUE,
                    .combine = c,
                    .export=ls(envir=globalenv())) %dopar% {

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
        measures <- readRDS("../resources/measures_spese_flood.rds")
        
        
        coefficienti_consumo <- readRDS("../resources/H.rds")
        
        ExpService <- read_csv("../resources/ExportsServices2020_flood.csv",col_names = FALSE)  %>% suppressMessages()
        ExpService<-ExpService[-1,-1]
        
        ExpSectoral <- read_csv("../resources//ExportsSectoral2020_flood.csv")  %>% suppressMessages()
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
        #return list of GDPloss
        list(GDPloss=Loss)        
        
        }


        for(i in 1:length(obs)){
        ob[i]<-obs[[i]]
        }

        return(ob)
        stopCluster(cl)
    }


    x <- shapleyPermRand(
                        model = modelShapley, 
                        Xall=getParams, 
                        Xset=getParams2, 
                        d=d, 
                        Nv=Nv,
                        m=m, 
                        No = 1, 
                        Ni =3
                        )

    TIMESTAMP=format(Sys.time(),format="%Y-%m-%d_%H:%M:%S")

    saveRDS(x,file=paste0("res/rds/Shapley",TIMESTAMP,"_",Nv,m,".rds"))
}else if (arg1=="run") {

    source("getParams.R")
    source("getParams2.R")
    source("modelShapley.R")

    # Nv=1e3
    # m=7000

    Nv=1e3
    m=1e4


    modelShapley<-function(params){
        library(dplyr)
        library(readr)
        library(readxl)
        library(foreach)
        library(doParallel)

        print(dim(params))

        ncores<-detectCores()
        cl<-makeCluster((32), outfile="")

        registerDoParallel(cl)
        ob=c(rep(0,nrow(params)))

        obs<-foreach(s=1:nrow(params), #.errorhandling = 'remove', 
                    .multicombine=TRUE,
                    .combine = c,
                    .export=ls(envir=globalenv())) %dopar% {

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
        measures <- readRDS("../resources/measures_spese_flood.rds")
        
        
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
        #return list of GDPloss
        list(GDPloss=Loss)
        
        
        }

        for(i in 1:length(obs)){
        ob[i]<-obs[[i]]
        }
        return(ob)
        stopCluster(cl)
    }


    x <- shapleyPermRand(
                        model = modelShapley, 
                        Xall=getParams, 
                        Xset=getParams2, 
                        d=d, 
                        Nv=Nv,
                        m=m, 
                        No = 1, 
                        Ni =3
                        )


    TIMESTAMP=format(Sys.time(),format="%Y-%m-%d_%H:%M:%S")

    saveRDS(x,file=paste0("../resources/Shapley",TIMESTAMP,"_",Nv,m,".rds"))

}




