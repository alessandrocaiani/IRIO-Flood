#.libPaths(c("/tmp/Rtmp1rhEwj/downloaded_packages", .libPaths()))
#install.packages('gtools')
library('gtools')
#install.packages("sensitivity")
#library(dplyr, warn.conflicts=FALSE)
# library(sensitivity,lib.loc=c("/mnt/beegfs/jlenia.dinoia/pkgR"))
library('sensitivity')


d <- 11  #parameters
#print("start script 1")
source("getParams_EmiliaFlood.R")
#print("start script 2")
source("getParams2_EmiliaFlood.R")
#print("start script 3")
source("modelShapley_EmiliaFlood.R")


x <- shapleyPermRand(model = modelShapley_EmiliaFlood, Xall=getParams_EmiliaFlood, Xset=getParams2_EmiliaFlood, d=d, Nv=1e3,m=100000, No = 1, Ni =3)

saveRDS(x,file="../results/Shapley_EmiliaFlood.rds")














