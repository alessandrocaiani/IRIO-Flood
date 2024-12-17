#model with expenditure functions and tranditional Leontief p.f.
runModel<-function(IO,measures,coefficienti_consumo,params,nbRounds,LD,ExpService,ExpSectoral,epsilon,IO_noCons,mobility){
  
  library(matrixStats)
  

#  TradLeontief=1
  
  n=measures$n
  nSectors=measures$nSectors
  nFD=16
  nItalianRegions=measures$nItalianRegions
  nbRounds=80
  IO=IO/52
  IO[IO<0]=0
  IO_noCons=IO_noCons/52
  IO_noCons[IO_noCons<0]=0
  production=rowSums(IO[1:n,])
  production[production==0]<-0.00000001
  Z = as.matrix(IO[1:n,1:n])
  Z[Z<0]=0
  x <- diag(1/production)
  A <- Z %*% x
  ## to avoid "ITA external" messing things up
  A[861:903,]<-0
  ArtIndices=c(nSectors*(0:(nItalianRegions-1)))
  ArtIndices=ArtIndices+(nSectors-1)
  
  ## calibrated indexes with new6 files
  ExpService=as.numeric(ExpService$X3)/100                                               #ExpService contains weakly values (53 for 2020 and 27 for 2021) of total export services
  ExpSectoral=ExpSectoral[,1:23]/100                                                     #In ExpSectoral there are 53+27 rows for weeks and 17 columns representing 17 goods' sectors (service sectors (15) are excluded)
  ExpIndex=as.data.frame(array(data=NA,dim=c(nbRounds,nSectors*nItalianRegions)))        #(80x903)ServSectors=c(18:nSectors)
  ServSectors=c(24:nSectors)                                                             #From sector 23 -included- to sector 43 we have services (43-17=26)
  ExpServices<-array(data=NA,dim=c(length(ExpService),length(ServSectors)))              
  ExpServices[,]<-ExpService                                                             #the time series is the same for all service sectors
  ExpSectoral[,22]<-ExpService
  ExpCombined<-cbind(ExpSectoral,ExpServices)                                            #exp for each of the 43 sectors
  for(i in 1:(nItalianRegions-1)){                                                       
    ExpIndex[,((i-1)*nSectors+1):((i-1)*nSectors+nSectors)]<-ExpCombined[1:nbRounds,]    #for each italian region (1:20; reg-1 excludes 21), replicate the values of ExpCombined by sector
  }
  ExpIndex[,(nSectors*nItalianRegions-nSectors+1):(nSectors*nItalianRegions)]<-1         #all sectors of EXT have value=1

  
  constant=c(1,2,4,6,8)
  n_constant=length(constant)
  decline=c(3,5,10,12)
  n_decline=length(decline)
  bad=c(7,9,11)
  n_bad=length(bad)
  
  ## Final demand (pag. 13)
  finalDemandItaTot=rowSums(IO[1:n,(n+1):(n+nFD*nItalianRegions)])           #final demand without changes in inventories and exports (The total demand faced by domestic sectors is the sum of intermediate demand)
  Exports=(IO[1:(nItalianRegions*nSectors),ncol(IO)])                        #ncol(IO) means column=1010
  finalDemandTot=rowSums(IO[1:n,(n+1):ncol(IO)])                             #final demand with exports and changes in inventories
  Inv_indices=c(1:nItalianRegions)                                           #create a vector for investment index
  Inv_indices=Inv_indices*15
  Inv_indices=Inv_indices+c(0:(nItalianRegions-1))
  Investment=rowSums(IO[1:(nItalianRegions*nSectors),(n+Inv_indices)])       #aggregate investment by sector. (n+Inv_indices) returns the exact number of column corresponding to gross investment
  
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
  cons_1=IO[1:(nItalianRegions*nSectors),n+indices_1]
  cons_2=IO[1:(nItalianRegions*nSectors),n+indices_2]
  cons_3=IO[1:(nItalianRegions*nSectors),n+indices_3]
  cons_4=IO[1:(nItalianRegions*nSectors),n+indices_4]
  cons_5=IO[1:(nItalianRegions*nSectors),n+indices_5]
  cons_6=IO[1:(nItalianRegions*nSectors),n+indices_6]
  cons_7=IO[1:(nItalianRegions*nSectors),n+indices_7]
  cons_8=IO[1:(nItalianRegions*nSectors),n+indices_8]
  cons_9=IO[1:(nItalianRegions*nSectors),n+indices_9]
  cons_10=IO[1:(nItalianRegions*nSectors),n+indices_10]
  cons_11=IO[1:(nItalianRegions*nSectors),n+indices_11]
  cons_12=IO[1:(nItalianRegions*nSectors),n+indices_12]
  cons=rowSums(IO[1:(nItalianRegions*nSectors),(n+Cons_indices)])
  
  
  finalDemandExo=finalDemandTot-cons-Exports-Investment              #demand coming from abroad is exogenous but is "added" to final demand (that is why it is here subtracted)           #exogenous demand          
  
  cons_regions<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
  for(i in 1:nItalianRegions){
    cons_regions[,i]<-rowSums(cons_columns[,((i-1)*12+1):((i-1)*12+12)])
  }
  

    cons_regions_endo<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
    for(i in 1:nrow(coefficienti_consumo)){
      coeffs=coefficienti_consumo[i,]
      for(j in 1:nItalianRegions){
        cons_regions_endo[i,j]<-coeffs[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]%*%production[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]
      }
    }
    cons_regions_exo=cons_regions-cons_regions_endo
  
  Inv_shares=Investment/sum(Investment)                            #i^bar / iota' i^bar (pag. 13-14)
  Investment=sum(Investment)
  
  GDPinit=sum(IO[1:n,(n+1):(ncol(IO))])+sum(IO_noCons[905,(n+1):(ncol(IO_noCons))])-sum(IO[906,1:n]) #GDP initial: sum of all inputs + taxes/(subsidies) - imports 
  Taxes=sum(IO_noCons[905,(n+1):(ncol(IO_noCons))])                              #aggregate taxes 
  Tax=(IO_noCons[905,(n+1):(ncol(IO_noCons))])
  Taxes_regions=c(rep(0,20))
  for(i in 1:(nItalianRegions-1)){
    Taxes_regions[i]<-sum(Tax[((i-1)*nFD+1):((i-1)*nFD+nFD)])
  }
  importCoeffs=IO[906,1:n]/production                              #calculate input coefficient concerning imports
  
  outputIO<-rowSums(IO[1:n,])
  GDPShocks=c(rep(0,nbRounds))
  outputTotal=c(rep(0,nbRounds))
  outputShocks=c(rep(0,nbRounds))
  outputShocksSectors=array(0,dim=c(nbRounds,43))
  outputShocksRegions=array(0,dim=c(nbRounds,20))
  ratioShocks=c(rep(0,nbRounds))
  ratioShocksSectors=array(0,dim=c(nbRounds,43))
  ratioShocksRegions=array(0,dim=c(nbRounds,20))
  
  nbParameters=7
  parametersNames=c("speedInv","Inv","timeScale","Exp","c_c","c_d","c_b")
  parameters=array(0,dim=c(n,nbParameters), dimnames=list(c(),parametersNames))
  inventParams<-params[3:(nSectors+2)]
  parameters[,"timeScale"]=rep(t(inventParams),nItalianRegions)
  parameters[,"speedInv"]=1/parameters[,"timeScale"]
  parameters[,"Inv"]=t(params[1])
  parameters[,"Exp"]=t(params[2])
  parameters[,"c_c"]=as.numeric(params[nSectors+3])
  parameters[,"c_d"]=as.numeric(params[nSectors+4])
  parameters[,"c_b"]=as.numeric(params[nSectors+5])
  
  while(nrow(mobility)<nbRounds){
    mobility<-rbind(mobility,c(rep(0,(nItalianRegions-1))))
  }
  
  #we initialize some variables to track the results over time and across the different scenarios
  sales=array(0,dim=c(nbRounds,(nItalianRegions-1)))
  sales_sectors=array(0,dim=c(nbRounds,43))
  capacity=array(0,dim=c(nbRounds,nItalianRegions*nSectors))
  capacitySectors=array(0,dim=c(nbRounds,43))
  capacityRegions=array(0,dim=c(nbRounds,20))
  capacity2=array(0,dim=c(nbRounds,nItalianRegions*nSectors))
  demandTotal=c(rep(0,nbRounds))
  demandSectors=array(0,dim=c(nbRounds,43))
  demandRegions=array(0,dim=c(nbRounds,20))
  expectationsRegions=array(0,dim=c(nbRounds,20))
  producible=array(0,dim=c(nbRounds,nItalianRegions*nSectors))
  producibleTotal=c(rep(0,nbRounds))
  producibleSectors=array(0,dim=c(nbRounds,43))
  producibleRegions=array(0,dim=c(nbRounds,20))
  consTotal=c(rep(0,nbRounds))
  consRegions=array(0,dim=c(nbRounds,20))
  consSectors=array(0,dim=c(nbRounds,43))
  ratio=array(0,dim=c(nbRounds,nSectors*nItalianRegions))
  ratioDemand=array(0,dim=c(nbRounds,nSectors*nItalianRegions))
  ratioTotal=c(rep(0,nbRounds))
  ratioDemandTotal=c(rep(0,nbRounds))
  ratioRegions=array(0,dim=c(nbRounds,20))
  ratioDemandRegions=array(0,dim=c(nbRounds,20))
  ratioSectors=array(0,dim=c(nbRounds,43))
  ratioDemandSectors=array(0,dim=c(nbRounds,43))
  baselineOutput=rowSums(IO[1:903,])
  sectorAvail=c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,26,27,28,29,30)  #see sectors IO in IP2020_flood.xlsx: until sector 26 "production", last 4 sectors "services"
  prod_sectors=array(0,dim=c(nbRounds,length(sectorAvail)))
  prod_sectors_regions=array(0,dim=c(nbRounds,length(sectorAvail)))
  GDP=c(rep(0,nbRounds))
  GDP_regions=array(0,dim=c(nbRounds,(nItalianRegions-1)))
  GDP_mregions=array(0,dim=c(nbRounds,4))
  Inv=c(rep(0,nbRounds))
  Cons=c(rep(0,nbRounds))
  constant_delivered=c(rep(0,nbRounds))
  decline_delivered=c(rep(0,nbRounds))
  bad_delivered=c(rep(0,nbRounds))
  c_1_delivered=c(rep(0,nbRounds))
  c_2_delivered=c(rep(0,nbRounds))
  c_3_delivered=c(rep(0,nbRounds))
  c_4_delivered=c(rep(0,nbRounds))
  c_5_delivered=c(rep(0,nbRounds))
  c_6_delivered=c(rep(0,nbRounds))
  c_7_delivered=c(rep(0,nbRounds))
  c_8_delivered=c(rep(0,nbRounds))
  c_9_delivered=c(rep(0,nbRounds))
  c_10_delivered=c(rep(0,nbRounds))
  c_11_delivered=c(rep(0,nbRounds))
  c_12_delivered=c(rep(0,nbRounds))
  
  #Labor needed for production purposes normalized to 100  but, as for other inputs, also labor in excess is maintained
  #(otherwise it wouldn't be possible to increase production due to a labor constraints)
  laborAvailable1=matrix(100,nrow=1, ncol=n)
  #The leontief coefficient for labor is calculated on workers actually needed for production (i.e. 100)
  laborLeontiefCoefficients=(laborAvailable1)/production     #(rowSums(IO[1:n,]))
  
  #Same logic for the PRODUCTION SHOCKS, expressed as % deviation from initial labor available to each industry, normalized to 100 (plus 20 to keep excess capacity)
  productionShock=matrix(0, nrow=n, ncol=nbRounds)
  
  #labor shocks + propagation + no mobility
  #First lockdown
  productionShock[1:((nItalianRegions-1)*nSectors),11]=-LD$March_1
  productionShock[1:((nItalianRegions-1)*nSectors),13]=-(LD$March_2-LD$March_1)
  productionShock[1:((nItalianRegions-1)*nSectors),16]=-(LD$April-LD$March_2)
  productionShock[1:((nItalianRegions-1)*nSectors),19]=-(LD$May-LD$April)
  productionShock[1:((nItalianRegions-1)*nSectors),21]=-(productionShock[1:((nItalianRegions-1)*nSectors),11]+productionShock[1:((nItalianRegions-1)*nSectors),13]+productionShock[1:((nItalianRegions-1)*nSectors),16]+productionShock[1:((nItalianRegions-1)*nSectors),19])
  
  #Second lockdown
  productionShock[1:((nItalianRegions-1)*nSectors),46]=-LD$Week46
  productionShock[1:((nItalianRegions-1)*nSectors),47]=-(LD$Week47-LD$Week46)
  productionShock[1:((nItalianRegions-1)*nSectors),48]=-(LD$Week48-LD$Week47)
  productionShock[1:((nItalianRegions-1)*nSectors),49]=-(LD$Week49-LD$Week48)
  productionShock[1:((nItalianRegions-1)*nSectors),50]=-(LD$Week50-LD$Week49)
  productionShock[1:((nItalianRegions-1)*nSectors),51]=-(LD$Week51-LD$Week50)
  productionShock[1:((nItalianRegions-1)*nSectors),52]=-(LD$Week52-LD$Week51)
  productionShock[1:((nItalianRegions-1)*nSectors),53]=-(LD$Week53-LD$Week52)
  productionShock[1:((nItalianRegions-1)*nSectors),54]=-(LD$Week1-LD$Week53)
  productionShock[1:((nItalianRegions-1)*nSectors),55]=-(LD$Week2-LD$Week1)
  productionShock[1:((nItalianRegions-1)*nSectors),56]=-(LD$Week3-LD$Week2)
  productionShock[1:((nItalianRegions-1)*nSectors),57]=-(LD$Week4-LD$Week3)
  productionShock[1:((nItalianRegions-1)*nSectors),58]=-(LD$Week5-LD$Week4)
  productionShock[1:((nItalianRegions-1)*nSectors),59]=-(LD$Week6-LD$Week5)
  productionShock[1:((nItalianRegions-1)*nSectors),60]=-(LD$Week7-LD$Week6)
  productionShock[1:((nItalianRegions-1)*nSectors),61]=-(LD$Week8-LD$Week7)
  productionShock[1:((nItalianRegions-1)*nSectors),62]=-(LD$Week9-LD$Week8)
  productionShock[1:((nItalianRegions-1)*nSectors),63]=-(LD$Week10-LD$Week9)
  productionShock[1:((nItalianRegions-1)*nSectors),64]=-(LD$Week11-LD$Week10)
  productionShock[1:((nItalianRegions-1)*nSectors),65]=-(LD$Week12-LD$Week11)
  productionShock[1:((nItalianRegions-1)*nSectors),66]=-(LD$Week13-LD$Week12)
  productionShock[1:((nItalianRegions-1)*nSectors),67]=-(LD$Week14-LD$Week13)
  productionShock[1:((nItalianRegions-1)*nSectors),68]=-(LD$Week15-LD$Week14)
  productionShock[1:((nItalianRegions-1)*nSectors),69]=-(LD$Week16-LD$Week15)
  productionShock[1:((nItalianRegions-1)*nSectors),70]=-(LD$Week17-LD$Week16)
  productionShock[1:((nItalianRegions-1)*nSectors),71]=-(LD$Week18-LD$Week17)
  productionShock[1:((nItalianRegions-1)*nSectors),72]=-(LD$Week19-LD$Week18)
  productionShock[1:((nItalianRegions-1)*nSectors),73]=-(LD$Week20-LD$Week19)
  productionShock[1:((nItalianRegions-1)*nSectors),74]=-(LD$Week21-LD$Week20)
  productionShock[1:((nItalianRegions-1)*nSectors),75]=-(LD$Week22-LD$Week21)
  productionShock[1:((nItalianRegions-1)*nSectors),76]=-(LD$Week23-LD$Week22)
  productionShock[1:((nItalianRegions-1)*nSectors),77]=-(LD$Week24-LD$Week23)
  productionShock[1:((nItalianRegions-1)*nSectors),78]=-(LD$Week25-LD$Week24)
  productionShock[1:((nItalianRegions-1)*nSectors),79]=-(LD$Week26-LD$Week25)
  productionShock[1:((nItalianRegions-1)*nSectors),80]=-(LD$Week27-LD$Week26) 

  
  #Again we translate % in levels (never comment next lines)
  for (i in 1:nbRounds){
    productionShock[,i]=laborAvailable1*productionShock[,i]
  }
  
  #re-initialize variables
  laborAvailable=laborAvailable1
  delivered=matrix(0, nrow=n, ncol=nbRounds)
  availableInputs=Z*rep(parameters[,"timeScale"], rep.int(nrow(Z),length(parameters[,"timeScale"])))
  availableImports=IO[906,1:n]*parameters[,"timeScale"]
  #Production delivered by each sector to each sector (initially equal to Z)
  productionDelivered=Z
  #Goods ordered for delivery next period (has the same format of Z)
  orders=Z
  #The inputs needed to produce the amount corresponding to total expected demand (final and intermediate) (has the same format of Z)
  inputsRequiredGivenDemand=Z
  
  fd=matrix(finalDemandTot,nrow=n, ncol=1)
  productionDemand=matrix(0,nbRounds,n)
  pastDemand=matrix(0,n,max(nbRounds,max(parameters[,"Exp"])))
  bottlenecks=matrix(0,n,nbRounds)
  bottlenecks_inputs=matrix(0,n,nbRounds)
  check1=matrix(0,n,nbRounds)
  check2=matrix(0,n,nbRounds)
  check3=matrix(0,n,nbRounds)
  check4=matrix(0,n,nbRounds)
  check5=matrix(0,n,nbRounds)
  check6=matrix(0,n,nbRounds)
  Num_hiringSectors=matrix(0,nbRounds)
  Num_hiringSectors_EROM=matrix(0,nbRounds)
  pastDemand[1:n,]=rowSums(orders)+fd
  expectations=rowMeans(pastDemand)
  expectations2=expectations
  demand=rowSums(orders)+fd
  pastfd=fd
  demandL=demand
  productionDelivered=matrix(data=as.numeric(c(1:((nItalianRegions)*nSectors))),ncol=((nItalianRegions)*nSectors))
  productionDelivered[1,]=production
  fdexo=finalDemandExo
  epsilon_max=max(epsilon) 
  epsilon<-matrix(rep(t(epsilon),21),ncol=ncol(epsilon),byrow=TRUE)
  epsilon<-matrix(rep(epsilon),n,nrow=nrow(epsilon),byrow=FALSE)
  matrix_demand=t(matrix(rep(demand,n),ncol=n))   #desired output matrix
  maxOutputGivenIntermediateInputsLeontief=availableInputs/A    #what can be produced using the stock of inputs
  maxOutputGivenIntermediateInputsLeontief[is.nan(maxOutputGivenIntermediateInputsLeontief)]=Inf
#  if(TradLeontief==1){
    epsilon[,]<-epsilon_max
    weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*maxOutputGivenIntermediateInputsLeontief
#  }else{
#    weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*pmin(matrix_demand,maxOutputGivenIntermediateInputsLeontief)
#  }
  maxOutputGivenLabour=laborAvailable/laborLeontiefCoefficients
  maxOutputGivenImports=availableImports/importCoeffs
  maxOutputGivenImports[is.nan(maxOutputGivenImports)]=Inf
  maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))
  

  #THE SIMULATION
  for(t in 1:nbRounds){
    laborAvailable=pmin(100,laborAvailable)
    inputsStart=availableInputs
    importsStart=availableImports
    #every sector observes its final demand (fd)
    fd[,1]=fdexo
    inv=Inv_shares*(Investment+parameters[1,"Inv"]*(sum(productionDelivered[1,1:(nItalianRegions*nSectors)])-sum(production[1:(nItalianRegions*nSectors)])))
    Exp=as.numeric(Exports*ExpIndex[t,])
    fd[,1]=fd[,1]+inv
    c_constant=cons_constant
    c_decline=cons_decline
    c_bad=cons_bad
    c_1=cons_1
    c_2=cons_2
    c_3=cons_3
    c_4=cons_4
    c_5=cons_5
    c_6=cons_6
    c_7=cons_7
    c_8=cons_8
    c_9=cons_9
    c_10=cons_10
    c_11=cons_11
    c_12=cons_12
    
    for(i in 1:(nItalianRegions-1)){
      c_constant[,((i-1)*n_constant+1):((i-1)*n_constant+n_constant)]=c_constant[,((i-1)*n_constant+1):((i-1)*n_constant+n_constant)]*c(rep(1+tanh(parameters[1,"c_c"]*mobility[t,i]),n_constant))
      c_decline[,((i-1)*n_decline+1):((i-1)*n_decline+n_decline)]=c_decline[,((i-1)*n_decline+1):((i-1)*n_decline+n_decline)]*c(rep(1+tanh(parameters[1,"c_d"]*mobility[t,i]),n_decline))
      c_bad[,((i-1)*n_bad+1):((i-1)*n_bad+n_bad)]=c_bad[,((i-1)*n_bad+1):((i-1)*n_bad+n_bad)]*c(rep(1+tanh(parameters[1,"c_b"]*mobility[t,i]),n_bad))
      c_1[,i]=c_1[,i]*(1+tanh(parameters[1,"c_c"]*mobility[t,i]))
      c_2[,i]=c_2[,i]*(1+tanh(parameters[1,"c_c"]*mobility[t,i]))
      c_3[,i]=c_3[,i]*(1+tanh(parameters[1,"c_d"]*mobility[t,i]))
      c_4[,i]=c_4[,i]*(1+tanh(parameters[1,"c_c"]*mobility[t,i]))
      c_5[,i]=c_5[,i]*(1+tanh(parameters[1,"c_d"]*mobility[t,i]))
      c_6[,i]=c_6[,i]*(1+tanh(parameters[1,"c_c"]*mobility[t,i]))
      c_7[,i]=c_7[,i]*(1+tanh(parameters[1,"c_b"]*mobility[t,i]))
      c_8[,i]=c_8[,i]*(1+tanh(parameters[1,"c_c"]*mobility[t,i]))
      c_9[,i]=c_9[,i]*(1+tanh(parameters[1,"c_b"]*mobility[t,i]))
      c_10[,i]=c_10[,i]*(1+tanh(parameters[1,"c_d"]*mobility[t,i]))
      c_11[,i]=c_11[,i]*(1+tanh(parameters[1,"c_b"]*mobility[t,i]))
      c_12[,i]=c_12[,i]*(1+tanh(parameters[1,"c_d"]*mobility[t,i]))
    }
    
    

      cons_regions_endo<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
      for(i in 1:nrow(coefficienti_consumo)){
        coeffs=coefficienti_consumo[i,]
        for(j in 1:nItalianRegions){
          cons_regions_endo[i,j]<-coeffs[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]%*%productionDelivered[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]
        }
      }
      cons_regions<-cons_regions_exo+cons_regions_endo

    cons_regions_inf<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
    cons_regions_constant<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
    cons_regions_decline<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
    cons_regions_bad<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
    
    
    for(i in 1:ncol(cons_regions_inf)){
      cons_regions_inf[,i]<-rowSums(c_constant[,((i-1)*n_constant+1):((i-1)*n_constant+n_constant)])+rowSums(c_decline[,((i-1)*n_decline+1):((i-1)*n_decline+n_decline)])+rowSums(c_bad[,((i-1)*n_bad+1):((i-1)*n_bad+n_bad)])
      cons_regions_constant[,i]<-rowSums(c_constant[,((i-1)*n_constant+1):((i-1)*n_constant+n_constant)])
      cons_regions_decline[,i]<-rowSums(c_decline[,((i-1)*n_decline+1):((i-1)*n_decline+n_decline)])
      cons_regions_bad[,i]<-rowSums(c_bad[,((i-1)*n_bad+1):((i-1)*n_bad+n_bad)])
    }
    

    cons_regions_min<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
    for(i in 1:nItalianRegions){
      sum_h=sum(cons_regions[,i])
      sum_f=sum(cons_regions_inf[,i])
      if(sum_f<=sum_h){
        cons_regions_min[,i]<-cons_regions_inf[,i]
      }else{
        cons_regions_min[,i]<-cons_regions_inf[,i]/sum_f*sum_h
        cons_regions_constant[,i]<-cons_regions_constant[,i]/sum_f*sum_h
        cons_regions_decline[,i]<-cons_regions_decline[,i]/sum_f*sum_h
        cons_regions_bad[,i]<-cons_regions_bad[,i]/sum_f*sum_h
        c_1[,i]<-c_1[,i]/sum_f*sum_h
        c_2[,i]<-c_2[,i]/sum_f*sum_h
        c_3[,i]<-c_3[,i]/sum_f*sum_h
        c_4[,i]<-c_4[,i]/sum_f*sum_h
        c_5[,i]<-c_5[,i]/sum_f*sum_h
        c_6[,i]<-c_6[,i]/sum_f*sum_h
        c_7[,i]<-c_7[,i]/sum_f*sum_h
        c_8[,i]<-c_8[,i]/sum_f*sum_h
        c_9[,i]<-c_9[,i]/sum_f*sum_h
        c_10[,i]<-c_10[,i]/sum_f*sum_h
        c_11[,i]<-c_11[,i]/sum_f*sum_h
        c_12[,i]<-c_12[,i]/sum_f*sum_h
      }
    }
    
    cons_regions_constant[is.nan(cons_regions_constant)]<-0
    cons_regions_decline[is.nan(cons_regions_decline)]<-0
    cons_regions_bad[is.nan(cons_regions_bad)]<-0
    c_1[is.nan(c_1)]<-0
    c_2[is.nan(c_2)]<-0
    c_3[is.nan(c_3)]<-0
    c_4[is.nan(c_4)]<-0
    c_5[is.nan(c_5)]<-0
    c_6[is.nan(c_6)]<-0
    c_7[is.nan(c_7)]<-0
    c_8[is.nan(c_8)]<-0
    c_9[is.nan(c_9)]<-0
    c_10[is.nan(c_10)]<-0
    c_11[is.nan(c_11)]<-0
    c_12[is.nan(c_12)]<-0
    
    
    
    
    
    consTotal[t]=sum(cons_regions)
    
    for(i in 1:(nItalianRegions-1)){
      consRegions[t,i]<-sum(cons_regions[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),])
    }
    for(i in 1:nSectors){
      consSectors[t,i]<-sum(cons_regions[((0:(nItalianRegions-1))*nSectors+i),])
    }
    
    
    fd[,1]=fd[,1]+rowSums(cons_regions_min)
    consumption=rowSums(cons_regions_min)
    pastlaborAvailable=laborAvailable
    laborAvailable=laborAvailable+t(productionShock[,t])
    outputVariationLabourShock=(laborAvailable-pastlaborAvailable)/laborLeontiefCoefficients
    
    if(t==1){
      outputIO_t=outputIO
      for(i in 1:(nItalianRegions-1)){
        outputShocksRegions[t,i]<-sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
        ratioShocksRegions[t,i]<-sum(outputIO[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])/sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
      }
      for(i in 1:nSectors){
        outputShocksSectors[t,i]<-sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
        ratioShocksSectors[t,i]<-sum(outputIO[nSectors*(0:(nItalianRegions-1))+i])/sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
      }
      outputShocks[t]<-sum(outputIO_t)
      ratioShocks[t]<-sum(outputIO)/sum(outputIO_t)
    }else{
      outputIO_t=outputIO_t+outputIO*t(productionShock[,t])/100
      for(i in 1:(nItalianRegions-1)){
        outputShocksRegions[t,i]<-sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
        ratioShocksRegions[t,i]<-sum(outputIO[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])/sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
      }
      for(i in 1:nSectors){
        outputShocksSectors[t,i]<-sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
        ratioShocksSectors[t,i]<-sum(outputIO[nSectors*(0:(nItalianRegions-1))+i])/sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
      }
      outputShocks[t]<-sum(outputIO_t)
      ratioShocks[t]<-sum(outputIO)/sum(outputIO_t)
    }
    
    for(i in 1:n){
      orderVariationLabourShock=outputVariationLabourShock*A[i,]
      expectations[i]=pastfd[i]+sum(orders[i,]+orderVariationLabourShock)
      expectations2[i]=mean(pastDemand[i,(nbRounds-parameters[i,"Exp"]+1):nbRounds])
    }
    
    maxOutputGivenLabour=laborAvailable/laborLeontiefCoefficients
    capacity[t,]=maxOutputGivenLabour
    expectations_l=pmin(expectations,maxOutputGivenLabour)     #eq3
    availableInputs2<-availableInputs
    availableInputs2[ArtIndices,]<-Inf
    
    matrix_demand=t(matrix(rep(expectations,n),ncol=n))   #desired output matrix
    maxOutputGivenIntermediateInputsLeontief=availableInputs2/A
    maxOutputGivenIntermediateInputsLeontief[is.nan(maxOutputGivenIntermediateInputsLeontief)]=Inf
#    if(TradLeontief==1){
      epsilon[,]<-epsilon_max
      weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*maxOutputGivenIntermediateInputsLeontief
#    }else{
#      weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*pmin(matrix_demand,maxOutputGivenIntermediateInputsLeontief)
#    } 
    maxOutputGivenImports=availableImports/importCoeffs
    maxOutputGivenImports[is.nan(maxOutputGivenImports)]=Inf
    maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))
    
    producibleTotal[t]<-sum(maxOutputFeasible)
    for(i in 1:(nItalianRegions-1)){
      producibleRegions[t,i]<-sum(maxOutputFeasible[1,((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
    }
    for(i in 1:nSectors){
      producibleSectors[t,i]<-sum(maxOutputFeasible[1,nSectors*(0:(nItalianRegions-1))+i])
    }
    ratio[t,]<-maxOutputFeasible[1,]
    
    #if productioin constrained by labor and demand>production feasible, then allow sectors not subject to labor input exogenous constraints 
    #(i.e. those having >= workers than at the beginning) to hire additional workers
    hiringSectors=which(((laborAvailable[1,]/laborLeontiefCoefficients[1,])<pmin(maxOutputGivenImports,colMins(weightedOutput)))&(laborAvailable[1,]>=laborAvailable1[1,])&(expectations>t(maxOutputFeasible)))
    Num_hiringSectors[t]<-length(hiringSectors)
    laborAvailable[hiringSectors]=expectations[hiringSectors]*laborLeontiefCoefficients[hiringSectors]
    maxOutputGivenLabour=laborAvailable/laborLeontiefCoefficients
    capacity2[t,]=maxOutputGivenLabour
    
    expectations_l=pmin(expectations,maxOutputGivenLabour)
    maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))
    bottlenecks[,t]=colMins(weightedOutput)
    xxx<-round(colMins(maxOutputGivenIntermediateInputsLeontief),digits=2)
    yyy<-round(colMins(weightedOutput),digits=2)
    for (i in 1:(nSectors*nItalianRegions)) {
      if(round(maxOutputFeasible[1,i],digits=2)==round(maxOutputGivenLabour[1,i],digits=2)){
        check1[i,t]<-1   #feasible output is constrained by labor
      } 
      if (round(maxOutputFeasible[1,i],digits=2)==yyy[i] & round(maxOutputFeasible[1,i],digits=2)!=round(maxOutputGivenLabour[1,i],digits=2) & yyy[i]==round(matrix_demand[1,i],digits=2)){
        check2[i,t]<-1   #feasible output not constrained by labor and equal to demand (no connection b/w sectors: epsilon/epsilon_max=0)
      }
      if (round(maxOutputFeasible[1,i],digits=2)==yyy[i] & round(maxOutputFeasible[1,i],digits=2)!=round(maxOutputGivenLabour[1,i],digits=2) & yyy[i]<round(matrix_demand[1,i],digits=2)){
        check3[i,t]<-1   #feasible output not constrained by labor and given by the interconnection b/w demand and stock of inputs (i.e. inputs shortages matter)
      }
      if (xxx[i]==0){
        check4[i,t]<-1
      }
    }
    
    #every sector computes the inputs required to satisfy its expected total demand given by observed current period final demand and past-period orders by opther sectors
    inputsRequiredGivenDemand=A * rep(expectations_l, rep.int(nrow(A),length(expectations_l)))
    inputsRequiredGivenDemand2=A * rep(expectations2, rep.int(nrow(A),length(expectations2)))
    importsRequiredGivenDemand=importCoeffs*expectations_l
    importsRequiredGivenDemand2=importCoeffs*expectations2
    totalExpectedDemand=sum(expectations_l[1:(nItalianRegions*nSectors)])
    
    #every sector demands inputs based on what he needed to produce desired outpout, inventories left from previous period, and targeted inventories given the new expected demand
    #these inputs will be available for next period production. Current production is based on past inventories (i.e. past period orders)
    orderMat=inputsRequiredGivenDemand2 * rep(parameters[,"timeScale"], rep.int(nrow(inputsRequiredGivenDemand2),length(parameters[,"timeScale"])))
    orderMat=orderMat-availableInputs
    futureOrders=orderMat * rep(parameters[,"speedInv"], rep.int(nrow(orderMat),length(parameters[,"speedInv"])))
    orders=pmax(matrix(0,n,n),inputsRequiredGivenDemand+futureOrders)
    
    importsDeviation=importsRequiredGivenDemand2*parameters[,"timeScale"]
    importsDeviation=importsDeviation-availableImports
    futureImports=parameters[,"speedInv"]*importsDeviation
    ordersImports=pmax(0,importsRequiredGivenDemand+futureImports)
    
    #the demand received by each sector in the period is its fd plus orders received from other sectors
    demand=fd+rowSums(orders)
    for (i in 1:(nSectors*nItalianRegions)) {
      matcheck=matrix_demand[1,]
      if((demand[i]+Exp[i])>matcheck[i]){
        check5[i,t]<-1   #actual demand is greater than expected demand (sectors might have produced less)
      }
    }
    pastfd=fd+Exp
    
    demandTotal[t]<-sum(demand+Exp)
    ratio[t,]<-(demand[,1]+Exp)/ratio[t,]
    ratioTotal[t]<-sum(baselineOutput/sum(baselineOutput)*ratio[t,],na.rm = TRUE)
    
    for(i in 1:(nItalianRegions-1)){
      demandRegions[t,i]<-sum(demand[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),1]+Exp[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
      ratioRegions[t,i]<-sum(baselineOutput[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)]/sum(baselineOutput[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])*ratio[t,((i-1)*nSectors+1):((i-1)*nSectors+nSectors)],na.rm = TRUE)
    }
    for(i in 1:nSectors){
      demandSectors[t,i]<-sum(demand[nSectors*(0:(nItalianRegions-1))+i,1]+Exp[nSectors*(0:(nItalianRegions-1))+i])
      ratioSectors[t,i]<-sum(baselineOutput[nSectors*(0:(nItalianRegions-1))+i]/sum(baselineOutput[nSectors*(0:(nItalianRegions-1))+i])*ratio[t,nSectors*(0:(nItalianRegions-1))+i],na.rm = TRUE)
    }
    
    for(i in 1:(nItalianRegions-1)){
      expectationsRegions[t,i]<-sum(expectations[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
    }
    
    #production takes place after sectors observe demand
    Exp=pmin(Exp,maxOutputFeasible)
    matrix_demand=t(matrix(rep(demand+Exp,n),ncol=n))   #desired output matrix
#    if(TradLeontief==1){
      epsilon[,]<-epsilon_max
      weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*maxOutputGivenIntermediateInputsLeontief
#    }else{
#      weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*pmin(matrix_demand,maxOutputGivenIntermediateInputsLeontief)
#    }
    maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))
    
    #the production actually delivered 
#    if(TradLeontief==1){
      productionDelivered=Exp+pmin(maxOutputFeasible-Exp,demand)
#    }else{
#      productionDelivered=maxOutputFeasible
#    }
    bottlenecks[,t]=bottlenecks[,t]==productionDelivered
    for(i in 1:n){
      if(bottlenecks[i,t]==1 & check3[i,t]==1){
        bottlenecks_inputs[i,t]=1
      }
    }
    
    delivered[,t]=t(productionDelivered)
    outputTotal[t]=sum(productionDelivered)
    for(i in 1:(nItalianRegions-1)){
      sales[t,i]=sum(delivered[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),t])
      capacityRegions[t,i]=sum(capacity[t,((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
    }
    for(i in 1:length(sectorAvail)){
      sec=sectorAvail[i]
      for(j in 1:nItalianRegions){
        prod_sectors[t,i]=prod_sectors[t,i]+productionDelivered[1,(sectorAvail[i]+nSectors*(j-1))]
      }
    }
    
    
    for(i in 1:nSectors){
      sec=i
      for(j in 1:nItalianRegions){
        sales_sectors[t,i]=sales_sectors[t,i]+productionDelivered[1,(sec+nSectors*(j-1))]
        capacitySectors[t,i]=capacitySectors[t,i]+capacity[t,(sec+nSectors*(j-1))]
      }
    }
    
    #is there a supply constraint?
    constraint=t(productionDelivered-Exp)/demand
    constraint[demand==0]=1
    #what is the ratio between capacity of production and demand?
    productionDemand[t,]=maxOutputFeasible/t(demand+Exp)
    productionDemandAggregate=sum(maxOutputFeasible[1,1:(nItalianRegions*nSectors)])/sum(demand[1:(nItalianRegions*nSectors)]+Exp[1:(nItalianRegions*nSectors)])
    
    #production consumes inputs of production...
    availableInputs=availableInputs-A * rep(productionDelivered, rep.int(nrow(A),length(productionDelivered)))
    availableInputs[ArtIndices,]<-pmax(0,availableInputs[ArtIndices,])
    availableImports=availableImports-importCoeffs*productionDelivered
    availableImports=as.vector(availableImports)
    
    #at the end of the period/beginning of next period inputs are replenished with new inputs 
    #ordered at the beginning and delivered at the end after production takes place
    ordersDelivered=orders
    ordersDelivered=orders*as.vector(constraint)
    importsDelivered=ordersImports
    
    #Inventories are replenished...
    availableInputs=availableInputs+ordersDelivered
    availableImports=availableImports+importsDelivered
    changeInventories=availableInputs-inputsStart
    changeImports=availableImports-importsStart
    fdDelivered=fd*as.vector(constraint)
    fdDelivered=fdDelivered+Exp
    GDP[t]=sum(fdDelivered)+sum(changeInventories)-sum(importsDelivered)+sum(changeImports)+Taxes
    if(t==1){
      fdBaseline=fdDelivered
      fdShocks=fdBaseline
      restBaseline=sum(changeInventories)-sum(importsDelivered)+sum(changeImports)+Taxes
      GDPShocks[t]=sum(fdDelivered)+sum(changeInventories)-sum(importsDelivered)+sum(changeImports)+Taxes
    }else{
      fdShocks=fdShocks+fdBaseline*productionShock[,t]/100
      GDPShocks[t]=sum(fdShocks)+restBaseline
    }
    for(i in 1:(nItalianRegions-1)){
      GDP_regions[t,i]=sum(fdDelivered[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),1])+sum(changeInventories[,((i-1)*43+1):((i-1)*43+43)])-sum(importsDelivered[((i-1)*43+1):((i-1)*43+43)])+sum(changeImports[((i-1)*43+1):((i-1)*43+43)])+Taxes_regions[i]
    }
    Inv[t]=sum(inv*as.vector(constraint))
    consumption=consumption*as.vector(constraint)
    cons_regions_constant=rowSums(cons_regions_constant)
    cons_regions_decline=rowSums(cons_regions_decline)
    cons_regions_bad=rowSums(cons_regions_bad)
    cons_regions_constant=cons_regions_constant*as.vector(constraint)
    cons_regions_decline=cons_regions_decline*as.vector(constraint)
    cons_regions_bad=cons_regions_bad*as.vector(constraint)
    c_1=rowSums(c_1)
    c_1=c_1*as.vector(constraint)
    c_2=rowSums(c_2)
    c_2=c_2*as.vector(constraint)
    c_3=rowSums(c_3)
    c_3=c_3*as.vector(constraint)
    c_4=rowSums(c_4)
    c_4=c_4*as.vector(constraint)
    c_5=rowSums(c_5)
    c_5=c_5*as.vector(constraint)
    c_6=rowSums(c_6)
    c_6=c_6*as.vector(constraint)
    c_7=rowSums(c_7)
    c_7=c_7*as.vector(constraint)
    c_8=rowSums(c_8)
    c_8=c_8*as.vector(constraint)
    c_9=rowSums(c_9)
    c_9=c_9*as.vector(constraint)
    c_10=rowSums(c_10)
    c_10=c_10*as.vector(constraint)
    c_11=rowSums(c_11)
    c_11=c_11*as.vector(constraint)
    c_12=rowSums(c_12)
    c_12=c_12*as.vector(constraint)
    constant_delivered[t]=sum(cons_regions_constant)
    decline_delivered[t]=sum(cons_regions_decline)
    bad_delivered[t]=sum(cons_regions_bad)
    c_1_delivered[t]=sum(c_1)
    c_2_delivered[t]=sum(c_2)
    c_3_delivered[t]=sum(c_3)
    c_4_delivered[t]=sum(c_4)
    c_5_delivered[t]=sum(c_5)
    c_6_delivered[t]=sum(c_6)
    c_7_delivered[t]=sum(c_7)
    c_8_delivered[t]=sum(c_8)
    c_9_delivered[t]=sum(c_9)
    c_10_delivered[t]=sum(c_10)
    c_11_delivered[t]=sum(c_11)
    c_12_delivered[t]=sum(c_12)
    Cons[t]=sum(consumption)
    demand=demand+Exp
    pastDemand=cbind(pastDemand[,2:nbRounds],demand)
    
    #END OF SIMULATION LOOP
  }
  
  
  # 18 is the number of months we consider 
  prod_sectors_m=array(0,dim=c(18,length(sectorAvail)))
  prod_sectors_m[1,]=colSums(prod_sectors[1:5,])  #30 dic 19 - 2 feb 20
  prod_sectors_m[2,]=colSums(prod_sectors[6:9,])   #3 feb 20 - 1 mar 20
  prod_sectors_m[3,]=colSums(prod_sectors[10:13,])  #2 mar 20 - 29 mar 20
  prod_sectors_m[4,]=colSums(prod_sectors[14:18,])  #30 mar 20 - 3 mag 20
  prod_sectors_m[5,]=colSums(prod_sectors[19:22,])  #4 mag 20 - 31 mag 20
  prod_sectors_m[6,]=colSums(prod_sectors[23:26,])  #1 giu 20 - 28 giu 20
  prod_sectors_m[7,]=colSums(prod_sectors[27:31,])  #29 giu 20 - 2 ago 20
  prod_sectors_m[8,]=colSums(prod_sectors[32:35,])  #3 ago 20 - 30 ago 20
  prod_sectors_m[9,]=colSums(prod_sectors[36:39,])  #31 ago 20 - 27 set 20
  prod_sectors_m[10,]=colSums(prod_sectors[40:44,])  #28 set 20 - 1 nov 20
  prod_sectors_m[11,]=colSums(prod_sectors[45:48,])  #2 nov 20 - 29 nov 20
  prod_sectors_m[12,]=colSums(prod_sectors[49:53,])  #30 nov 20 - 3 gen 21
  prod_sectors_m[13,]=colSums(prod_sectors[54:57,])  #4 gen 21 - 31 gen 21
  prod_sectors_m[14,]=colSums(prod_sectors[58:61,])  #1 feb 21 - 28 feb 21
  prod_sectors_m[15,]=colSums(prod_sectors[62:65,])  #1 mar 21 - 28 mar 21
  prod_sectors_m[16,]=colSums(prod_sectors[66:70,])  #29 mar 21 - 2 mag 21
  prod_sectors_m[17,]=colSums(prod_sectors[71:74,])  #3 mag 21 - 30 mag 21
  prod_sectors_m[18,]=colSums(prod_sectors[75:78,])  #31 mag 21 - 27 giu 21
  
  # 6 is "number of quarters"; the other 4 is "services whose data on revenues are available"
  prod_sectors_q=array(0,dim=c(6,4))
  prod_sectors_q[1,]=colSums(prod_sectors_m[1:3,23:26])
  prod_sectors_q[2,]=colSums(prod_sectors_m[4:6,23:26])
  prod_sectors_q[3,]=colSums(prod_sectors_m[7:9,23:26])
  prod_sectors_q[4,]=colSums(prod_sectors_m[10:12,23:26])/14*13   #because in this quarter we have 5+4+5=14 weeks
  prod_sectors_q[5,]=colSums(prod_sectors_m[13:15,23:26])/12*13   #in this quarter we have 4+4+4=12 weeks
  prod_sectors_q[6,]=colSums(prod_sectors_m[16:18,23:26])
  
  prod_sectors_q[,1]=prod_sectors_q[,1]/(prod_sectors[1,23]*13)*100
  prod_sectors_q[,2]=prod_sectors_q[,2]/(prod_sectors[1,24]*13)*100
  prod_sectors_q[,3]=prod_sectors_q[,3]/(prod_sectors[1,25]*13)*100
  prod_sectors_q[,4]=prod_sectors_q[,4]/(prod_sectors[1,26]*13)*100
  
  
  prod_sectors_m[1,]=prod_sectors_m[1,]/5
  prod_sectors_m[2,]=prod_sectors_m[2,]/4
  prod_sectors_m[3,]=prod_sectors_m[3,]/4
  prod_sectors_m[4,]=prod_sectors_m[4,]/5
  prod_sectors_m[5,]=prod_sectors_m[5,]/4
  prod_sectors_m[6,]=prod_sectors_m[6,]/4
  prod_sectors_m[7,]=prod_sectors_m[7,]/5
  prod_sectors_m[8,]=prod_sectors_m[8,]/4
  prod_sectors_m[9,]=prod_sectors_m[9,]/4
  prod_sectors_m[10,]=prod_sectors_m[10,]/5
  prod_sectors_m[11,]=prod_sectors_m[11,]/4
  prod_sectors_m[12,]=prod_sectors_m[12,]/5
  prod_sectors_m[13,]=prod_sectors_m[13,]/4
  prod_sectors_m[14,]=prod_sectors_m[14,]/4
  prod_sectors_m[15,]=prod_sectors_m[15,]/4
  prod_sectors_m[16,]=prod_sectors_m[16,]/5
  prod_sectors_m[17,]=prod_sectors_m[17,]/4
  prod_sectors_m[18,]=prod_sectors_m[18,]/4
  
  
  declines=rep(0,22)
  for(i in 1:ncol(prod_sectors_m)){
    prod_sectors_m[,i]=prod_sectors_m[,i]/prod_sectors_m[1,i]*100
  }
  
  for(i in 1:length(declines)){
    declines[i]=(min(prod_sectors_m[1:8,i])-100)/100
  }
  
  lossProd=(colSums(prod_sectors_m)-1800)/1800  #1800 è il baseline dato da 100 (baseline, vedi codice sopra) x 18 mesi
  
  
  expenditure_m=array(0,dim=c(18,12))
  expenditure_m[1,]=c(sum(c_1_delivered[1:5]),sum(c_2_delivered[1:5]),sum(c_3_delivered[1:5]),sum(c_4_delivered[1:5]),sum(c_5_delivered[1:5]),sum(c_6_delivered[1:5]),sum(c_7_delivered[1:5]),sum(c_8_delivered[1:5]),sum(c_9_delivered[1:5]),sum(c_10_delivered[1:5]),sum(c_11_delivered[1:5]),sum(c_12_delivered[1:5]))
  expenditure_m[2,]=c(sum(c_1_delivered[6:9]),sum(c_2_delivered[6:9]),sum(c_3_delivered[6:9]),sum(c_4_delivered[6:9]),sum(c_5_delivered[6:9]),sum(c_6_delivered[6:9]),sum(c_7_delivered[6:9]),sum(c_8_delivered[6:9]),sum(c_9_delivered[6:9]),sum(c_10_delivered[6:9]),sum(c_11_delivered[6:9]),sum(c_12_delivered[6:9]))
  expenditure_m[3,]=c(sum(c_1_delivered[10:13]),sum(c_2_delivered[10:13]),sum(c_3_delivered[10:13]),sum(c_4_delivered[10:13]),sum(c_5_delivered[10:13]),sum(c_6_delivered[10:13]),sum(c_7_delivered[10:13]),sum(c_8_delivered[10:13]),sum(c_9_delivered[10:13]),sum(c_10_delivered[10:13]),sum(c_11_delivered[10:13]),sum(c_12_delivered[10:13]))
  expenditure_m[4,]=c(sum(c_1_delivered[14:18]),sum(c_2_delivered[14:18]),sum(c_3_delivered[14:18]),sum(c_4_delivered[14:18]),sum(c_5_delivered[14:18]),sum(c_6_delivered[14:18]),sum(c_7_delivered[14:18]),sum(c_8_delivered[14:18]),sum(c_9_delivered[14:18]),sum(c_10_delivered[14:18]),sum(c_11_delivered[14:18]),sum(c_12_delivered[14:18]))
  expenditure_m[5,]=c(sum(c_1_delivered[19:22]),sum(c_2_delivered[19:22]),sum(c_3_delivered[19:22]),sum(c_4_delivered[19:22]),sum(c_5_delivered[19:22]),sum(c_6_delivered[19:22]),sum(c_7_delivered[19:22]),sum(c_8_delivered[19:22]),sum(c_9_delivered[19:22]),sum(c_10_delivered[19:22]),sum(c_11_delivered[19:22]),sum(c_12_delivered[19:22]))
  expenditure_m[6,]=c(sum(c_1_delivered[23:26]),sum(c_2_delivered[23:26]),sum(c_3_delivered[23:26]),sum(c_4_delivered[23:26]),sum(c_5_delivered[23:26]),sum(c_6_delivered[23:26]),sum(c_7_delivered[23:26]),sum(c_8_delivered[23:26]),sum(c_9_delivered[23:26]),sum(c_10_delivered[23:26]),sum(c_11_delivered[23:26]),sum(c_12_delivered[23:26]))
  expenditure_m[7,]=c(sum(c_1_delivered[27:31]),sum(c_2_delivered[27:31]),sum(c_3_delivered[27:31]),sum(c_4_delivered[27:31]),sum(c_5_delivered[27:31]),sum(c_6_delivered[27:31]),sum(c_7_delivered[27:31]),sum(c_8_delivered[27:31]),sum(c_9_delivered[27:31]),sum(c_10_delivered[27:31]),sum(c_11_delivered[27:31]),sum(c_12_delivered[27:31]))
  expenditure_m[8,]=c(sum(c_1_delivered[32:35]),sum(c_2_delivered[32:35]),sum(c_3_delivered[32:35]),sum(c_4_delivered[32:35]),sum(c_5_delivered[32:35]),sum(c_6_delivered[32:35]),sum(c_7_delivered[32:35]),sum(c_8_delivered[32:35]),sum(c_9_delivered[32:35]),sum(c_10_delivered[32:35]),sum(c_11_delivered[32:35]),sum(c_12_delivered[32:35]))
  expenditure_m[9,]=c(sum(c_1_delivered[36:39]),sum(c_2_delivered[36:39]),sum(c_3_delivered[36:39]),sum(c_4_delivered[36:39]),sum(c_5_delivered[36:39]),sum(c_6_delivered[36:39]),sum(c_7_delivered[36:39]),sum(c_8_delivered[36:39]),sum(c_9_delivered[36:39]),sum(c_10_delivered[36:39]),sum(c_11_delivered[36:39]),sum(c_12_delivered[36:39]))
  expenditure_m[10,]=c(sum(c_1_delivered[40:44]),sum(c_2_delivered[40:44]),sum(c_3_delivered[40:44]),sum(c_4_delivered[40:44]),sum(c_5_delivered[40:44]),sum(c_6_delivered[40:44]),sum(c_7_delivered[40:44]),sum(c_8_delivered[40:44]),sum(c_9_delivered[40:44]),sum(c_10_delivered[40:44]),sum(c_11_delivered[40:44]),sum(c_12_delivered[40:44]))
  expenditure_m[11,]=c(sum(c_1_delivered[45:48]),sum(c_2_delivered[45:48]),sum(c_3_delivered[45:48]),sum(c_4_delivered[45:48]),sum(c_5_delivered[45:48]),sum(c_6_delivered[45:48]),sum(c_7_delivered[45:48]),sum(c_8_delivered[45:48]),sum(c_9_delivered[45:48]),sum(c_10_delivered[45:48]),sum(c_11_delivered[45:48]),sum(c_12_delivered[45:48]))
  expenditure_m[12,]=c(sum(c_1_delivered[49:53]),sum(c_2_delivered[49:53]),sum(c_3_delivered[49:53]),sum(c_4_delivered[49:53]),sum(c_5_delivered[49:53]),sum(c_6_delivered[49:53]),sum(c_7_delivered[49:53]),sum(c_8_delivered[49:53]),sum(c_9_delivered[49:53]),sum(c_10_delivered[49:53]),sum(c_11_delivered[49:53]),sum(c_12_delivered[49:53]))
  expenditure_m[13,]=c(sum(c_1_delivered[54:57]),sum(c_2_delivered[54:57]),sum(c_3_delivered[54:57]),sum(c_4_delivered[54:57]),sum(c_5_delivered[54:57]),sum(c_6_delivered[54:57]),sum(c_7_delivered[54:57]),sum(c_8_delivered[54:57]),sum(c_9_delivered[54:57]),sum(c_10_delivered[54:57]),sum(c_11_delivered[54:57]),sum(c_12_delivered[54:57]))
  expenditure_m[14,]=c(sum(c_1_delivered[58:61]),sum(c_2_delivered[58:61]),sum(c_3_delivered[58:61]),sum(c_4_delivered[58:61]),sum(c_5_delivered[58:61]),sum(c_6_delivered[58:61]),sum(c_7_delivered[58:61]),sum(c_8_delivered[58:61]),sum(c_9_delivered[58:61]),sum(c_10_delivered[58:61]),sum(c_11_delivered[58:61]),sum(c_12_delivered[58:61]))
  expenditure_m[15,]=c(sum(c_1_delivered[62:65]),sum(c_2_delivered[62:65]),sum(c_3_delivered[62:65]),sum(c_4_delivered[62:65]),sum(c_5_delivered[62:65]),sum(c_6_delivered[62:65]),sum(c_7_delivered[62:65]),sum(c_8_delivered[62:65]),sum(c_9_delivered[62:65]),sum(c_10_delivered[62:65]),sum(c_11_delivered[62:65]),sum(c_12_delivered[62:65]))
  expenditure_m[16,]=c(sum(c_1_delivered[66:70]),sum(c_2_delivered[66:70]),sum(c_3_delivered[66:70]),sum(c_4_delivered[66:70]),sum(c_5_delivered[66:70]),sum(c_6_delivered[66:70]),sum(c_7_delivered[66:70]),sum(c_8_delivered[66:70]),sum(c_9_delivered[66:70]),sum(c_10_delivered[66:70]),sum(c_11_delivered[66:70]),sum(c_12_delivered[66:70]))
  expenditure_m[17,]=c(sum(c_1_delivered[71:74]),sum(c_2_delivered[71:74]),sum(c_3_delivered[71:74]),sum(c_4_delivered[71:74]),sum(c_5_delivered[71:74]),sum(c_6_delivered[71:74]),sum(c_7_delivered[71:74]),sum(c_8_delivered[71:74]),sum(c_9_delivered[71:74]),sum(c_10_delivered[71:74]),sum(c_11_delivered[71:74]),sum(c_12_delivered[71:74]))
  expenditure_m[18,]=c(sum(c_1_delivered[75:78]),sum(c_2_delivered[75:78]),sum(c_3_delivered[75:78]),sum(c_4_delivered[75:78]),sum(c_5_delivered[75:78]),sum(c_6_delivered[75:78]),sum(c_7_delivered[75:78]),sum(c_8_delivered[75:78]),sum(c_9_delivered[75:78]),sum(c_10_delivered[75:78]),sum(c_11_delivered[75:78]),sum(c_12_delivered[75:78]))
  
  expenditure_m_3=array(0,dim=c(18,3))
  
  expenditure_m_3[,1]=rowSums(expenditure_m[,constant])
  expenditure_m_3[,2]=rowSums(expenditure_m[,decline])
  expenditure_m_3[,3]=rowSums(expenditure_m[,bad])
  
  expenditure_q=array(0,dim=c(6,12))
  expenditure_q[1,]=sum(expenditure_m[1:3,])
  expenditure_q[2,]=sum(expenditure_m[4:6,])
  expenditure_q[3,]=sum(expenditure_m[7:9,])
  expenditure_q[4,]=sum(expenditure_m[10:12,])/14*13
  expenditure_q[5,]=sum(expenditure_m[13:15,])/12*13
  expenditure_q[6,]=sum(expenditure_m[16:18,])
  
  expenditure_q_3=array(0,dim=c(6,3))
  expenditure_q_3[1,]=colSums(expenditure_m_3[1:3,])
  expenditure_q_3[2,]=colSums(expenditure_m_3[4:6,])
  expenditure_q_3[3,]=colSums(expenditure_m_3[7:9,])
  expenditure_q_3[4,]=colSums(expenditure_m_3[10:12,])/14*13
  expenditure_q_3[5,]=colSums(expenditure_m_3[13:15,])/12*13
  expenditure_q_3[6,]=colSums(expenditure_m_3[16:18,])
  
  
  expenditure_q[,1]=expenditure_q[,1]/(c_1_delivered[1]*13)*100
  expenditure_q[,2]=expenditure_q[,2]/(c_2_delivered[1]*13)*100
  expenditure_q[,3]=expenditure_q[,3]/(c_3_delivered[1]*13)*100
  expenditure_q[,4]=expenditure_q[,4]/(c_4_delivered[1]*13)*100
  expenditure_q[,5]=expenditure_q[,5]/(c_5_delivered[1]*13)*100
  expenditure_q[,6]=expenditure_q[,6]/(c_6_delivered[1]*13)*100
  expenditure_q[,7]=expenditure_q[,7]/(c_7_delivered[1]*13)*100
  expenditure_q[,8]=expenditure_q[,8]/(c_8_delivered[1]*13)*100
  expenditure_q[,9]=expenditure_q[,9]/(c_9_delivered[1]*13)*100
  expenditure_q[,10]=expenditure_q[,10]/(c_10_delivered[1]*13)*100
  expenditure_q[,11]=expenditure_q[,11]/(c_11_delivered[1]*13)*100
  expenditure_q[,12]=expenditure_q[,12]/(c_12_delivered[1]*13)*100
  
  expenditure_3_absolute=expenditure_q_3
  
  expenditure_q_3[,1]=expenditure_q_3[,1]/((c_1_delivered[1]+c_2_delivered[1]+c_4_delivered[1]+c_6_delivered[1]+c_8_delivered[1])*13)*100
  expenditure_q_3[,2]=expenditure_q_3[,2]/((c_3_delivered[1]+c_5_delivered[1]+c_10_delivered[1]+c_12_delivered[1])*13)*100
  expenditure_q_3[,3]=expenditure_q_3[,3]/((c_7_delivered[1]+c_9_delivered[1]+c_11_delivered[1])*13)*100
  
  
  expenditure_m[1,]=expenditure_m[1,]/5
  expenditure_m[2,]=expenditure_m[2,]/4
  expenditure_m[3,]=expenditure_m[3,]/4
  expenditure_m[4,]=expenditure_m[4,]/5
  expenditure_m[5,]=expenditure_m[5,]/4
  expenditure_m[6,]=expenditure_m[6,]/4
  expenditure_m[7,]=expenditure_m[7,]/5
  expenditure_m[8,]=expenditure_m[8,]/4
  expenditure_m[9,]=expenditure_m[9,]/4
  expenditure_m[10,]=expenditure_m[10,]/5
  expenditure_m[11,]=expenditure_m[11,]/4
  expenditure_m[12,]=expenditure_m[12,]/5
  expenditure_m[13,]=expenditure_m[13,]/4
  expenditure_m[14,]=expenditure_m[14,]/4
  expenditure_m[15,]=expenditure_m[15,]/4
  expenditure_m[16,]=expenditure_m[16,]/5
  expenditure_m[17,]=expenditure_m[17,]/4
  expenditure_m[18,]=expenditure_m[18,]/4
  
  expenditure_m_3[1,]=expenditure_m_3[1,]/5
  expenditure_m_3[2,]=expenditure_m_3[2,]/4
  expenditure_m_3[3,]=expenditure_m_3[3,]/4
  expenditure_m_3[4,]=expenditure_m_3[4,]/5
  expenditure_m_3[5,]=expenditure_m_3[5,]/4
  expenditure_m_3[6,]=expenditure_m_3[6,]/4
  expenditure_m_3[7,]=expenditure_m_3[7,]/5
  expenditure_m_3[8,]=expenditure_m_3[8,]/4
  expenditure_m_3[9,]=expenditure_m_3[9,]/4
  expenditure_m_3[10,]=expenditure_m_3[10,]/5
  expenditure_m_3[11,]=expenditure_m_3[11,]/4
  expenditure_m_3[12,]=expenditure_m_3[12,]/5
  expenditure_m_3[13,]=expenditure_m_3[13,]/4
  expenditure_m_3[14,]=expenditure_m_3[14,]/4
  expenditure_m_3[15,]=expenditure_m_3[15,]/4
  expenditure_m_3[16,]=expenditure_m_3[16,]/5
  expenditure_m_3[17,]=expenditure_m_3[17,]/4
  expenditure_m_3[18,]=expenditure_m_3[18,]/4
  
  
  for(i in 1:ncol(expenditure_m)){
    expenditure_m[,i]=expenditure_m[,i]/expenditure_m[1,i]*100
  }
  
  
  lossExpenditure=(colSums(expenditure_q_3)-600)/600
  lossServices=(colSums(prod_sectors_q)-600)/600
  
  GDP_q=c(rep(0,6))
  GDP_q[1]<-sum(GDP[1:13])
  GDP_q[2]<-sum(GDP[14:26])
  GDP_q[3]<-sum(GDP[27:39])
  GDP_q[4]<-sum(GDP[40:53])
  GDP_q[4]<-GDP_q[4]*13/14
  GDP_q[5]<-sum(GDP[54:65])
  GDP_q[5]<-GDP_q[5]*13/12
  GDP_q[6]<-sum(GDP[66:78])
  
  GDP_q=GDP_q/(GDP[1]*13)*100
  
  simvec=c(lossProd[1:22],lossServices,lossExpenditure)
  
  return(list(simvec,prod_sectors_m[,1:22],prod_sectors_q,expenditure_q_3,GDP_q,expenditure_3_absolute))
  
}

