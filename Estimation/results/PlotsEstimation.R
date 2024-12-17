#THIS POLOT ALLOWS TO GENERATE THE FIGURES RELATED TO THE ESTIMATION PHASE
library(readr)
library(readxl)

## EMPIRICAL TIME SERIES FOR INDUSTRIAL PRODUCTION ##
IP <- as.data.frame(read_excel("../resources/IP2020_2021_flood.xlsx"))
rownames(IP)<-IP$`IO sector`
IP <- IP[,-1]
IP <- t(IP)
#IP_18 <- IP[-(19:24),]
IP_18 <- IP[-(13:24),]

## EMPIRICAL TIME SERIES FOR SERVICES ##
Service <- as.data.frame(read_excel("../resources/Services2020_2021_flood.xlsx"))
rownames(Service) <- Service$`IO sector`
Service <- Service[,-1]
Service <- t(Service)
#Service_18 <- Service[-(7:8),]
Service_18 <- Service[-(5:8),]

## EMPIRICAL TIME SERIES FOR EXPENDITURE ##
Expend <- read_excel("../resources/Expenditures_2020_2021_flood_REV1.xlsx")
Expend <- Expend[,-(1:2)]
Expend <- t(Expend)
Expend <- Expend[-(5:6),]


#Rank according to sector's size:
IO_ITAregions <- readRDS("../resources/IO_ITAregions.rds")
IO_ITAregions <- IO_ITAregions[-(904:906),]
x<-as.data.frame(rowSums(IO_ITAregions))
x$sector<-rep(1:43,21)
y<-aggregate(x$`rowSums(IO_ITAregions)`, list(x$sector), FUN=sum)
y=y[order(y$x, decreasing = TRUE),]
#resulting ranking: 27, 34, 28, 26, 41, 39, 4, 33, 18, 29, 38, 35, 15, 23, 5, 40, 19, 14, 32, 43, 1, 10, 9, 12, 42, 21, 37, 17, 31, 25, 13, 20, 11, 30, 7, 16, 36, 22, 6, 24, 8, 3, 2

#load the results of the simulation with the estimated parameters for the model with traditional Leontief p.f.
load("Workspace_EST_TraditionalLeontief.Rdata")
prod_sectors_Leontief=prod_sectors
prod_sectors_m_Leontief=prod_sectors_m
prod_sectors_q_Leontief=prod_sectors_q
expenditure_3_absolute_Leontief=expenditure_3_absolute
expenditure_m_Leontief=expenditure_m
expenditure_m_3_leontief=expenditure_m_3
expenditure_q_Leontief=expenditure_q
expenditure_q_3_Leontief=expenditure_q_3
#load the results of the simulation with the estimated parameters for the model with modified Leontief p.f.
load("Workspace_EST_ModifiedLeontief.Rdata")

#### PLOTS ####


jpeg("Est_IP1.jpeg", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))

#Construction
plot(prod_sectors_m[,22], yaxt="n", xaxt='n', main="Construction", cex.main=1.8, type="l", lwd=2.0, col="brown3", ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,22], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,22], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")


#Food, beverages and tobacco
plot(prod_sectors_m[,2], yaxt="n", xaxt='n', main="Food, beverages & tobacco", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,2], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,2], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Machinery & equipment NEC
plot(prod_sectors_m[,16], yaxt="n", xaxt='n', main="Machinery & equipment NEC", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,16], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,16], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Fabricated metal products
plot(prod_sectors_m[,13], yaxt="n", xaxt='n', main="Fabricated metal products", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,13], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,13], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Electricity, gas, steam & air conditioning supply
plot(prod_sectors_m[,21], yaxt="n", xaxt='n', main="Electricity, gas, steam & air condit.", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,21], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,21], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Textiles, wearing apparel & leather
plot(prod_sectors_m[,3], yaxt="n", xaxt='n', main="Textiles, wearing apparel & leather", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,3], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,3], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Motor vehicles, trailers & semi-trailers
plot(prod_sectors_m[,17], yaxt="n", xaxt='n', main="Motor vehicles, trailers & semi-trailers", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,17], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,17], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Basic metals
plot(prod_sectors_m[,12], yaxt="n", xaxt='n', main="Basic metals", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,12], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,12], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Chemicals 
plot(prod_sectors_m[,8], yaxt="n", xaxt='n', main="Chemicals", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,8], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,8], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Coke & refined petroleum products
plot(prod_sectors_m[,7], yaxt="n", xaxt='n', main="Coke & refined petroleum products", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,7], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,7], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Rubber & plastic products
plot(prod_sectors_m[,10], yaxt="n", xaxt='n', main="Rubber & plastic products", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,10], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,10], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Furniture & other manufactured goods
plot(prod_sectors_m[,19], yaxt="n", xaxt='n', main="Furniture & other manufactured goods", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,19], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,19], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Electrical equipment
plot(prod_sectors_m[,15], yaxt="n", xaxt='n', main="Electrical equipment", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,15], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,15], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Other non-metallic mineral products
plot(prod_sectors_m[,11], yaxt="n", xaxt='n', main="Other non-metallic mineral products", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,11], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,11], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Other transport equipment
plot(prod_sectors_m[,18], yaxt="n", xaxt='n', main="Other transport equipment", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,18], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,18], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Pharmaceuticals  
plot(prod_sectors_m[,9], yaxt="n", xaxt='n', main="Pharmaceuticals", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,9], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,9], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

dev.off()



#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/MSM results/tradL_old param space//Est_IP2.png", width = 2500, height = 1600, res = 150)
# restricted:
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/TradL/figs_TradL12/Est_IP2.png", width = 2500, height = 1600, res = 150)
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_IP2_80TradL.png", width = 2500, height = 1600, res = 150)
# unrestricted:
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_IP2_80TradL_unrstr_1comb.png", width = 2500, height = 1600, res = 150)
jpeg("Est_IP2.jpeg", width = 2500, height = 1600, res = 150)
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_IP2_80_unrstr.png", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))

#Paper & paper products
plot(prod_sectors_m[,5], yaxt="n", xaxt='n',main="Paper & paper products", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,5], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,5], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Computer, electronic & optical products
plot(prod_sectors_m[,14], yaxt="n", xaxt='n', main="Computer, electronic & optical prod.", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,14], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,14], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Manufacturing NEC, repair & inst. of machinery
plot(prod_sectors_m[,20], yaxt="n", xaxt='n', main="Manufacturing NEC, repair & inst. machin.", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,20], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,20], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Wood & products of wood & cork
plot(prod_sectors_m[,4], yaxt="n", xaxt='n', main="Wood & products of wood and cork", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,4], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,4], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Print & rep. of rec. media
plot(prod_sectors_m[,6], yaxt="n", xaxt='n', main="Print & reproduction of rec. media", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_m_Leontief[,6], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(IP_18[,6], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Mining & Quarrying
plot(prod_sectors_m[,1], yaxt="n", xaxt='n', main="Mining & quarrying", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(IP_18[,1], type="l", lty=2, lwd=1.7, col="blue")
lines(prod_sectors_m_Leontief[,1], type="l", lty=1, lwd=2.0, col="chartreuse4")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,4,7,10,13,16), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1.4)
#axis(1, at=c(1,4,7,10), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

# plot.new()
# legend("center", cex=2.55, legend=c("Modified Leontief", "Standard Leontief", "Empirical"), xpd=TRUE,  
#        col = c("brown3","chartreuse4","blue"), lwd=c(1.7,1.7,1.7),lty = c(1,1,2))


dev.off()


#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/MSM results/tradL_old param space//Est_serv.png", width = 2500, height = 1600, res = 150)
# restricted:
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/TradL/figs_TradL12/Est_serv.png", width = 2500, height = 1600, res = 150)
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_serv_80TradL.png", width = 2500, height = 1600, res = 150)
# unrestricted
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_serv_80TradL_unrstr_1comb.png", width = 2500, height = 1600, res = 150)
jpeg("Est_serv.jpeg", width = 2500, height = 1600, res = 150)
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_serv_80_unrstr.png", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))

#Wholesale & retail trade, repair of motor vehicles & motorcycles
plot(prod_sectors_q[,1], yaxt="n", xaxt='n', main="Wholesale & retail trade", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_q_Leontief[,1], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(Service_18[,1], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,2,3,4,5,6), labels=c("Jan 2020","","July 2020","","Jan 2021",""), cex.axis=1.4)
#axis(1, at=c(1,2,3,4), labels=c("Q1 2020","Q2 2020","Q3 2020","Q4 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Transportation & storage
plot(prod_sectors_q[,2], yaxt="n", xaxt='n', main="Transportation & storage", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_q_Leontief[,2], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(Service_18[,2], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,2,3,4,5,6), labels=c("Jan 2020","","July 2020","","Jan 2021",""), cex.axis=1.4)
#axis(1, at=c(1,2,3,4), labels=c("Q1 2020","Q2 2020","Q3 2020","Q4 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Accomodation & food service activities
plot(prod_sectors_q[,3], yaxt="n", xaxt='n', main="Accomodation & food service activities", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_q_Leontief[,3], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(Service_18[,3], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,2,3,4,5,6), labels=c("Jan 2020","","July 2020","","Jan 2021",""), cex.axis=1.4)
#axis(1, at=c(1,2,3,4), labels=c("Q1 2020","Q2 2020","Q3 2020","Q4 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Publishing, motion picture, video, sound & broadcasting activities
plot(prod_sectors_q[,4], yaxt="n", xaxt='n', main="Publishing, video, sound & broadc.", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(prod_sectors_q_Leontief[,4], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(Service_18[,4], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,2,3,4,5,6), labels=c("Jan 2020","","July 2020","","Jan 2021",""), cex.axis=1.4)
#axis(1, at=c(1,2,3,4), labels=c("Q1 2020","Q2 2020","Q3 2020","Q4 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

dev.off()



#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/TradL/figs_TradL12/Est_expend.png", width = 2500, height = 1600, res = 150)
# restricted:
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_expend_80TradL.png", width = 2500, height = 1600, res = 150)
# unrestricted:
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_expend_80TradL_unrstr_1comb.png", width = 2500, height = 1600, res = 150)
jpeg("Est_expend.jpeg", width = 2500, height = 1600, res = 150)
#png("C:/Users/jleni/OneDrive/Desktop/IUSS/PROGETTI/IO FLOOD_REV1/EST_REV1/diff_cons/80rounds/Est_expend_80_unrstr.png", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))

#Expenditure in essentials chartreuse4
plot(expenditure_q_3[,1], xaxt='n', main="Expenditure Essentials", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(expenditure_q_3_Leontief[,1], type="l", lty=1, lwd=1.7, col="chartreuse4")
lines(Expend[,1], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,2,3,4,5,6), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1)
#axis(1, at=c(1,2,3,4), labels=c("Q1 2020","Q2 2020","Q3 2020","Q4 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Expenditure in intermediates
plot(expenditure_q_3[,2], xaxt='n', main="Expenditure Intermediates", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(expenditure_q_3_Leontief[,2], type="l", lty=1, lwd=1.7, col="chartreuse4")
lines(Expend[,2], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,2,3,4,5,6), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1)
#axis(1, at=c(1,2,3,4), labels=c("Q1 2020","Q2 2020","Q3 2020","Q4 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")

#Expenditure in inessentials
plot(expenditure_q_3[,3], xaxt='n', main="Expenditure Inessentials", cex.main=1.8, type="l", lwd=2.0, col=2, ylim=c(10,125), xlab= "", ylab="", cex.axis=1)
lines(expenditure_q_3_Leontief[,3], type="l", lty=1, lwd=1.7, col="chartreuse4")
lines(Expend[,3], type="l", lty=2, lwd=1.7, col="blue")
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, at=c(1,2,3,4,5,6), labels=c("Jan 2020","Apr 2020","July 2020","Oct 2020","Jan 2021","Apr 2021"), cex.axis=1)
#axis(1, at=c(1,2,3,4), labels=c("Q1 2020","Q2 2020","Q3 2020","Q4 2020"), cex.axis=1.4)
axis(2, at=c(20,60,100), labels=c("20","60","100"),cex.axis=1.4)
abline(v=1:18, col="lightgray", lty="dotted")
#abline(v=1:12, col="lightgray", lty="dotted")


plot.new()
legend("center", cex=2.55, legend=c("Modified Leontief", "Standard Leontief", "Empirical"), xpd=TRUE,  
       col = c("brown3","chartreuse4","blue"), lwd=c(1.7,1.7,1.7),lty = c(1,1,2))


dev.off()




