library(readxl)
EROM_histoR <- read_excel("resources/EROM_histoR_hazus.xlsx")
load("resources/modifiedLeontiefWorkspace.Rdata") #load this workspace for the simulation for the Emilia Romagna flood using the revised Leontief p.f.
#load("resources/traditionalLeontiefWorkspace.Rdata") #load this workspace for the simulation for the Emilia Romagna flood using the traditional Leontief p.f.

#Sectors' ranking (from Figures_est2.R): 27, 34, 28, 26, 41, 39, 4, 33, 18, 29, 38, 35, 15, 23, 5, 40, 19, 14, 32, 43, 1, 10, 9, 12, 42, 21, 37, 17, 31, 25, 13, 20, 11, 30, 7, 16, 36, 22, 6, 24, 8, 3, 2
rank_IP<-c(26, 4, 18, 15, 23, 5, 19, 14, 1, 10, 9, 12, 21, 17, 25, 13, 20, 11, 7, 16, 22, 6, 24, 8, 3, 2)
rank_Serv<-c(27, 34, 28, 41, 39, 33, 29, 38, 35, 40, 32, 43, 42, 37,31, 30, 36)
rank_names_IP<-c("Construction","Food, beverages and tobacco","Machinery and equipment NEC","Fabricated metal products","Electricity, gas, steam and air condit. supply","Textiles, wearing apparel and leather","Motor vehicles, trailers and semi-trailers","Basic metals, fabricated metal products","Agriculture, forestry","Chemicals","Coke and refined petroleum products","Rubber and plastic products","Furniture and other manufactured goods","Electrical equipment","Water and waste management and treatment","Other non-metallic mineral products","Other transport equipment","Pharmaceuticals","Paper and paper products","Computer, electronic and optical products","Manufacturing NEC, repair and inst. of machinery","Wood and products of wood and cork","Water supply","Print and reproduction of rec. media","Mining and quarrying","Fishing")
rank_names_Serv<-c("Wholesale and retail trade, repair of motor veh.","Real estate","Transportation and storage","Health","Public administration and defense","Financial services and insurance","Accomodation and food service activities","Other supporting service activities","Accounting and business services","Education","IT services and other information services","Other service activities","Art and Entertainment","Other professional and scientific activities","Telecommunications","Publishing, motion pic., video, sound and broadc.","Research")



## % OF EMPLOYEES INVOLVED ##

EROM_histoR[is.na(EROM_histoR)]<-0

h<-matrix(NA,nrow=(length(rank_IP)+length(rank_Serv)+1),ncol=2)
for (i in 1:length(rank_IP)) {
  x<-as.numeric(rank_IP[i])
  h[i,2]<-as.numeric(EROM_histoR[x,3])
}
for (j in 1:length(rank_Serv)){
  y<-as.numeric(rank_Serv[j])
  pos<-j+(length(rank_IP)+1)
  h[pos,2]<-as.numeric(EROM_histoR[y,3])
}
h[27,2]<-0

h[(1:length(rank_names_IP)),1]<-rank_names_IP
h[((length(rank_names_IP)+2):44),1]<-rank_names_Serv

h_num <- as.data.frame(apply(h, 2, as.numeric))


library(ggplot2)
library(dplyr)
library(forcats)

data <- data.frame(
  name=rev(c("Construction","Food, beverages and tobacco","Machinery and equipment NEC","Fabricated metal products","Electricity, gas, steam and air condit. supply","Textiles, wearing apparel and leather","Motor vehicles, trailers and semi-trailers","Basic metals, fabricated metal products","Agriculture, forestry","Chemicals","Coke and refined petroleum products","Rubber and plastic products","Furniture and other manufactured goods","Electrical equipment","Water and waste management and treatment","Other non-metallic mineral products","Other transport equipment","Pharmaceuticals","Paper and paper products","Computer, electronic and optical products","Manufacturing NEC, repair and inst. of machinery","Wood and products of wood and cork","Water supply","Print and reproduction of rec. media","Mining and quarrying","Fishing"," ","Wholesale and retail trade, repair of motor veh.","Real estate","Transportation and storage","Health","Public administration and defense","Financial services and insurance","Accomodation and food service activities","Other supporting service activities","Accounting and business services","Education","IT services and other information services","Other service activities","Art and Entertainment","Other professional and scientific activities","Telecommunications","Publishing, motion pic., video, sound and broadc.","Research")) ,
  value=rev(c(h_num[(1:44),2]))
)

# make V1 an ordered factor
data$name <- factor(data$name, levels = data$name)

#ordered by share of output
ggplot(data, aes(x=name, y=value)) +
  geom_bar(stat="identity",position="identity", fill="#f68060", alpha=.6, width=.4) +
  theme_bw()+
  xlab("") +
  ylab("%") +
  ggtitle("Percentage of employees affected by the flooding event by sector (Emilia Romagna)") +
  coord_flip() 


#ordered by % empl affected:
# data %>%
#  mutate(name = fct_reorder(name, value)) %>%
#  ggplot( aes(x=name, y=value)) +
#  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
#  coord_flip() +
#  xlab("") +
#  ylab("%") +
#  ggtitle("Percentage of employees involved in the flooded areas")
#  theme_bw()


  ## AVERAGE RESTORATION TIME ##

  EROM_histoR$`Employees involved (%)`<- format(round(EROM_histoR$`Employees involved (%)`, 2), nsmall = 2)

  H<-matrix(NA,nrow=(length(rank_IP)+length(rank_Serv)),ncol=2)
  for (i in 1:length(rank_IP)) {
    x<-as.numeric(rank_IP[i])
    H[i,2]<-as.numeric(EROM_histoR[x,4])
  }
  for (j in 1:length(rank_Serv)){
    y<-as.numeric(rank_Serv[j])
    pos<-j+(length(rank_IP))
    H[pos,2]<-as.numeric(EROM_histoR[y,4])
  }
    
  
  H[(1:length(rank_names_IP)),1]<-rank_names_IP
  H[((length(rank_names_IP)+1):43),1]<-rank_names_Serv
  H_num <- as.data.frame(apply(H, 2, as.numeric))
  H_num$V2<- format(round(H_num$V2, 0), nsmall = 0)
  H_num <- as.data.frame(apply(H_num, 2, as.numeric))
  
  data2 <- data.frame(
    name=rev(c("Construction","Food, beverages & tobacco","Machinery & equipment NEC","Fabricated metal products","Electricity, gas, steam & air condit.","Textiles, wearing apparel & leather","Motor vehicles, trailers & semi-trailers","Basic metals, fabricated metal prod.","Agriculture, forestry","Chemicals","Coke & refined petroleum prod.","Rubber & plastic prod.","Furniture & other manufactured goods","Electrical equipment","Water & waste management-treatment","Other non-metallic mineral prod.","Other transport equipment","Pharmaceuticals","Paper & paper products","Computer, electronic & optical products","Manufacturing NEC, repair & inst. machin.","Wood & prod. of wood and cork","Water supply","Print & reproduction of rec. media","Mining & quarrying","Fishing","Wholesale & retail trade","Real estate","Transportation & storage","Health","Public administration & defense","Financial services & insurance","Accomodation & food services","Other supporting services","Accounting & business services","Education","IT services & other info. serv.","Other service activities","Art & Entertainment","Other professional-scientific act.","Telecommunications","Publishing, video, sound and broadc.","Research")) ,
    value=rev(c(H_num[(1:43),2]))
  )
  
  # make V1 an ordered factor
  data2$name <- factor(data2$name, levels = data2$name)
  
  
  jpeg("avDowntime_hazus.jpeg", width = 2500, height = 3000, res = 270)
  #png("avDowntime_hazus.png", width = 2500, height = 2750, res = 270)
  #ordered by share of output
  ggplot(data2, aes(x=name, y=value)) + 
    geom_bar(stat="identity",position="identity", fill="blue", alpha=.6, width=.5) +
    theme_bw()+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x = element_text(size = 12))+
    theme(plot.title = element_text(size = 12, face = "bold"))+
    theme(axis.title.x =element_text(size=12, face = "bold"))+
    xlab("") +
    ylab("Number of weeks") +
    ggtitle("Average sectoral restoration time (weeks)") +
    coord_flip()
  
  dev.off()
  
  #ordered by % empl affected:
  #  data2 %>%
  #  mutate(name = fct_reorder(name, value)) %>%
  #  ggplot( aes(x=name, y=value)) +
  #  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +
  #  coord_flip() +
  #  xlab("") +
  #  ylab("Number of weeks") +
  #  ggtitle("Average restoration time (weeks)")
  #  theme_bw()
  


#### After running the model:


## TOTAL OUTPUT OF EMILIA ROMAGNA AND ITALIAN GDP ##

# Emilia (values)
plot.ts(GDP_regions[,8], main="Total output (Emilia Romagna)", cex.main=1, type="l", xlab= "Weeks", ylab="", cex.axis=1)
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, cex.axis=1)
abline(col="lightgray", lty="dotted")  
  
 # Emilia (norm)
 plot.ts(GDP_regions2[,8], main="Total output (Emilia Romagna)", cex.main=1, type="l", xlab= "Weeks", ylab="", cex.axis=1)
 grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
 axis(1, cex.axis=1)
 abline(col="lightgray", lty="dotted") 
  
  
# Italy (values)
plot.ts(GDP, main="Italian GDP", cex.main=1, type="l", xlab= "Weeks", ylab="", cex.axis=1)
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
axis(1, cex.axis=1)
abline(col="lightgray", lty="dotted")

 # Italy (norm)
 plot.ts(GDP2, main="Italian GDP", cex.main=1, type="l", xlab= "Weeks", ylab="", cex.axis=1)
 grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
 axis(1, cex.axis=1)
 abline(col="lightgray", lty="dotted")


 # Plot GDP for Emilia Romagna and Italy in the same graph 
 load("funzEpsilon workspace.RData")
 
 library(ggplot2)
 library(ggeasy)
 
 
 data1 <- data.frame(
   weeks = c(1:108),
   val = GDP_regions2[,8]
 )
 
 data2 <- data.frame(
   weeks = c(1:108),
   val = GDP2
 )
 load("tradL workspace.RData")
 
 data1_Leontief <- data.frame(
   weeks = c(1:108),
   val = GDP_regions2[,8]
 )
 
 data2_Leontief <- data.frame(
   weeks = c(1:108),
   val = GDP2
 )
 #For funzEpsilon:
 #png("GDP_funzEpsilon.png", width = 2000, height = 1000, res = 250)
 png("GDP.png", width = 2000, height = 1000, res = 250)
 
 theme_update(plot.title = element_text(hjust = 0.5))
 ggplot() + 
   geom_line(data = data1, aes(x = weeks, y = val, color = "Emilia Romagna Modified Leontief", linetype="Emilia Romagna Modified Leontief"), lwd=0.8) +
   geom_line(data = data2, aes(x = weeks, y = val, color = "Italy Modified Leontief",linetype="Italy Modified Leontief") , lwd=0.8) +
   geom_line(data = data1_Leontief, aes(x = weeks, y = val, color = "Emilia Romagna Traditional Leontief", linetype="Emilia Romagna Traditional Leontief"), lwd=0.8) +
   geom_line(data = data2_Leontief, aes(x = weeks, y = val, color = "Italy Traditional Leontief", linetype="Italy Traditional Leontief"),lwd=0.8) +
   xlab('') +
   ylab('') +
   theme_bw() +
   scale_colour_manual("", 
                       breaks = c("Emilia Romagna Modified Leontief", "Italy Modified Leontief","Emilia Romagna Traditional Leontief", "Italy Traditional Leontief"),
                       values = c("brown3", "brown3", "chartreuse4", "chartreuse4")) +    #FOR funzEpsilon
   scale_linetype_manual("",
                         breaks = c("Emilia Romagna Modified Leontief", "Italy Modified Leontief","Emilia Romagna Traditional Leontief", "Italy Traditional Leontief"),
 values = c(1, 2, 1, 2)) +
   xlab("Weeks") +
   ylab("") +
#   ggtitle("GDP (modified production function)") +
   ggtitle("GDP") +
   ggeasy::easy_center_title()
 
 dev.off()
 
 
 #For tradL:
 png("GDP_tradL.png", width = 2000, height = 1000, res = 250)
 
 theme_update(plot.title = element_text(hjust = 0.5))
 ggplot() + 
   geom_line(data = data1, aes(x = weeks, y = val, color = "Emilia Romagna"), lwd=0.8,) +
   geom_line(data = data2, aes(x = weeks, y = val, color = "Italy"), lwd=0.8) +
   xlab('') +
   ylab('') +
   theme_bw() +
   scale_colour_manual("", 
                       breaks = c("Emilia Romagna", "Italy"),
                       values = c("chartreuse4", "cornsilk3")) +    #FOR tradL
   xlab("Weeks") +
   ylab("") +
   ggtitle("GDP (traditional Leontiesf production function)") +
   ggeasy::easy_center_title()
 
 dev.off()
 
 
 

 
#### PLOT PRODUCTION BY SECTOR (EMILIA ROMAGNA) ####
prod_plot_funzEpsilon <- readRDS("resources/prod_plot_funzEpsilon.rds")
prod_plot_tradL <- readRDS("resources/prod_plot_tradL.rds")
cap_plot_funzEpsilon <- readRDS("resources/cap_plot_funzEpsilon.rds")
cap_plot_tradL <- readRDS("resources/cap_plot_tradL.rds")
 
wks=c(seq(from=10,to=100,by=10))
prod_plot<-prod_plot_funzEpsilon
prod_plot2<-prod_plot_tradL
cap_plot<-cap_plot_funzEpsilon
cap_plot2<-cap_plot_tradL
 
## RIPRENDERE DA QUA


jpeg("Emilia_IP1_hazus.jpeg", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))


# 26. Construction
plot(prod_plot[,26], yaxt="n", xaxt='n', main="Construction", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,26],prod_plot2[,26]),max(prod_plot[,26],prod_plot2[,26])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,26], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,26], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,26]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,26]), labels=l)
l<-formatC(c(min(prod_plot[,26],prod_plot2[,26]), (min(prod_plot[,26],prod_plot2[,26])+((max(prod_plot[,26],prod_plot2[,26])-min(prod_plot[,26],prod_plot2[,26]))/2)), max(prod_plot[,26],prod_plot2[,26])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,26],prod_plot2[,26]), (min(prod_plot[,26],prod_plot2[,26])+((max(prod_plot[,26],prod_plot2[,26])-min(prod_plot[,26],prod_plot2[,26]))/2)), max(prod_plot[,26],prod_plot2[,26])), labels=l, cex.axis=1.4)
abline(v=wks,col="lightgray", lty="dotted")

# 4. Food, beverages and tobacco
plot(prod_plot[,4], yaxt="n", xaxt='n', main="Food, beverages & tobacco", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,4],prod_plot2[,4]),max(prod_plot[,4],prod_plot2[,4])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,4], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,4], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,4]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,4]), labels=l)
l<-formatC(c(min(prod_plot[,4],prod_plot2[,4]), (min(prod_plot[,4],prod_plot2[,4])+((max(prod_plot[,4],prod_plot2[,4])-min(prod_plot[,4],prod_plot2[,4]))/2)), max(prod_plot[,4],prod_plot2[,4])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,4],prod_plot2[,4]), (min(prod_plot[,4],prod_plot2[,4])+((max(prod_plot[,4],prod_plot2[,4])-min(prod_plot[,4],prod_plot2[,4]))/2)), max(prod_plot[,4],prod_plot2[,4])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 18. Machinery and equipment NEC
plot(prod_plot[,18], yaxt="n", xaxt='n', main="Machinery & equipment NEC", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,18],prod_plot2[,18]),max(prod_plot[,18],prod_plot2[,18])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,18], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,18], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,18]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,18]), labels=l)
l<-formatC(c(min(prod_plot[,18],prod_plot2[,18]), (min(prod_plot[,18],prod_plot2[,18])+((max(prod_plot[,18],prod_plot2[,18])-min(prod_plot[,18],prod_plot2[,18]))/2)), max(prod_plot[,18],prod_plot2[,18])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,18],prod_plot2[,18]), (min(prod_plot[,18],prod_plot2[,18])+((max(prod_plot[,18],prod_plot2[,18])-min(prod_plot[,18],prod_plot2[,18]))/2)), max(prod_plot[,18],prod_plot2[,18])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 15. Fabricated metal products
plot(prod_plot[,15], yaxt="n", xaxt='n', main="Fabricated metal products", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,15],prod_plot2[,15]),max(prod_plot[,15],prod_plot2[,15])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,15], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,15], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,15]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,15]), labels=l)
l<-formatC(c(min(prod_plot[,15],prod_plot2[,15]), (min(prod_plot[,15],prod_plot2[,15])+((max(prod_plot[,15],prod_plot2[,15])-min(prod_plot[,15],prod_plot2[,15]))/2)), max(prod_plot[,15],prod_plot2[,15])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,15],prod_plot2[,15]), (min(prod_plot[,15],prod_plot2[,15])+((max(prod_plot[,15],prod_plot2[,15])-min(prod_plot[,15],prod_plot2[,15]))/2)), max(prod_plot[,15],prod_plot2[,15])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 23. Electricity, gas, steam and air conditioning supply
plot(prod_plot[,23], yaxt="n", xaxt='n', main="Electricity, gas, steam & air condit.", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,23],prod_plot2[,23]),max(prod_plot[,23],prod_plot2[,23])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,23], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,23], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,23]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,23]), labels=l)
l<-formatC(c(min(prod_plot[,23],prod_plot2[,23]), (min(prod_plot[,23],prod_plot2[,23])+((max(prod_plot[,23],prod_plot2[,23])-min(prod_plot[,23],prod_plot2[,23]))/2)), max(prod_plot[,23],prod_plot2[,23])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,23],prod_plot2[,23]), (min(prod_plot[,23],prod_plot2[,23])+((max(prod_plot[,23],prod_plot2[,23])-min(prod_plot[,23],prod_plot2[,23]))/2)), max(prod_plot[,23],prod_plot2[,23])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 5. Textiles, wearing apparel and leather
plot(prod_plot[,5], yaxt="n", xaxt='n', main="Textiles, wearing apparel & leather", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,5],prod_plot2[,5]),max(prod_plot[,5],prod_plot2[,5])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,5], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,5], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,5]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,5]), labels=l)
l<-formatC(c(min(prod_plot[,5],prod_plot2[,5]), (min(prod_plot[,5],prod_plot2[,5])+((max(prod_plot[,5],prod_plot2[,5])-min(prod_plot[,5],prod_plot2[,5]))/2)), max(prod_plot[,5],prod_plot2[,5])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,5],prod_plot2[,5]), (min(prod_plot[,5],prod_plot2[,5])+((max(prod_plot[,5],prod_plot2[,5])-min(prod_plot[,5],prod_plot2[,5]))/2)), max(prod_plot[,5],prod_plot2[,5])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")


# 19. Motor vehicles, trailers and semi-trailers
plot(prod_plot[,19], yaxt="n", xaxt='n', main="Motor vehicles, trailers & semi-trailers", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,19],prod_plot2[,19]),max(prod_plot[,19],prod_plot2[,19])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,19], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,19], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,19]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,19]), labels=l)
l<-formatC(c(min(prod_plot[,19],prod_plot2[,19]), (min(prod_plot[,19],prod_plot2[,19])+((max(prod_plot[,19],prod_plot2[,19])-min(prod_plot[,19],prod_plot2[,19]))/2)), max(prod_plot[,19],prod_plot2[,19])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,19],prod_plot2[,19]), (min(prod_plot[,19],prod_plot2[,19])+((max(prod_plot[,19],prod_plot2[,19])-min(prod_plot[,19],prod_plot2[,19]))/2)), max(prod_plot[,19],prod_plot2[,19])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 14. Basic metals
plot(prod_plot[,14], yaxt="n", xaxt='n', main="Basic metals", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,14],prod_plot2[,14]),max(prod_plot[,14],prod_plot2[,14])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,14], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,14], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,14]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,14]), labels=l)
l<-formatC(c(min(prod_plot[,14],prod_plot2[,14]), (min(prod_plot[,14],prod_plot2[,14])+((max(prod_plot[,14],prod_plot2[,14])-min(prod_plot[,14],prod_plot2[,14]))/2)), max(prod_plot[,14],prod_plot2[,14])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,14],prod_plot2[,14]), (min(prod_plot[,14],prod_plot2[,14])+((max(prod_plot[,14],prod_plot2[,14])-min(prod_plot[,14],prod_plot2[,14]))/2)), max(prod_plot[,14],prod_plot2[,14])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 1. Agriculture, forestry
plot(prod_plot[,1], yaxt="n", xaxt='n', main="Agriculture, forestry", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,1],prod_plot2[,1]),max(prod_plot[,1],prod_plot2[,1])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,1], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,1], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,1]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,1]), labels=l)
l<-formatC(c(min(prod_plot[,1],prod_plot2[,1]), (min(prod_plot[,1],prod_plot2[,1])+((max(prod_plot[,1],prod_plot2[,1])-min(prod_plot[,1],prod_plot2[,1]))/2)), max(prod_plot[,1],prod_plot2[,1])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,1],prod_plot2[,1]), (min(prod_plot[,1],prod_plot2[,1])+((max(prod_plot[,1],prod_plot2[,1])-min(prod_plot[,1],prod_plot2[,1]))/2)), max(prod_plot[,1],prod_plot2[,1])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 10. Chemicals 
plot(prod_plot[,10], yaxt="n", xaxt='n', main="Chemicals", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,10],prod_plot2[,10]),max(prod_plot[,10],prod_plot2[,10])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,10], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,10], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,10]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,10]), labels=l)
l<-formatC(c(min(prod_plot[,10],prod_plot2[,10]), (min(prod_plot[,10],prod_plot2[,10])+((max(prod_plot[,10],prod_plot2[,10])-min(prod_plot[,10],prod_plot2[,10]))/2)), max(prod_plot[,10],prod_plot2[,10])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,10],prod_plot2[,10]), (min(prod_plot[,10],prod_plot2[,10])+((max(prod_plot[,10],prod_plot2[,10])-min(prod_plot[,10],prod_plot2[,10]))/2)), max(prod_plot[,10],prod_plot2[,10])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 9. Coke and refined petroleum products
plot(prod_plot[,9], yaxt="n", xaxt='n', main="Coke & refined petroleum products", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,9],prod_plot2[,9]),max(prod_plot[,9],prod_plot2[,9])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,9], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,9], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,9]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,9]), labels=l)
l<-formatC(c(min(prod_plot[,9],prod_plot2[,9]), (min(prod_plot[,9],prod_plot2[,9])+((max(prod_plot[,9],prod_plot2[,9])-min(prod_plot[,9],prod_plot2[,9]))/2)), max(prod_plot[,9],prod_plot2[,9])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,9],prod_plot2[,9]), (min(prod_plot[,9],prod_plot2[,9])+((max(prod_plot[,9],prod_plot2[,9])-min(prod_plot[,9],prod_plot2[,9]))/2)), max(prod_plot[,9],prod_plot2[,9])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 12. Rubber and plastic products
plot(prod_plot[,12], yaxt="n", xaxt='n', main="Rubber & plastic products", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,12],prod_plot2[,12]),max(prod_plot[,12],prod_plot2[,12])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,12], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,12], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,12]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,12]), labels=l)
l<-formatC(c(min(prod_plot[,12],prod_plot2[,12]), (min(prod_plot[,12],prod_plot2[,12])+((max(prod_plot[,12],prod_plot2[,12])-min(prod_plot[,12],prod_plot2[,12]))/2)), max(prod_plot[,12],prod_plot2[,12])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,12],prod_plot2[,12]), (min(prod_plot[,12],prod_plot2[,12])+((max(prod_plot[,12],prod_plot2[,12])-min(prod_plot[,12],prod_plot2[,12]))/2)), max(prod_plot[,12],prod_plot2[,12])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 21. Furniture and other manufactured goods
plot(prod_plot[,21], yaxt="n", xaxt='n', main="Furniture & other manufactured goods", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,21],prod_plot2[,21]),max(prod_plot[,21],prod_plot2[,21])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,21], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,21], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,21]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,21]), labels=l)
l<-formatC(c(min(prod_plot[,21],prod_plot2[,21]), (min(prod_plot[,21],prod_plot2[,21])+((max(prod_plot[,21],prod_plot2[,21])-min(prod_plot[,21],prod_plot2[,21]))/2)), max(prod_plot[,21],prod_plot2[,21])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,21],prod_plot2[,21]), (min(prod_plot[,21],prod_plot2[,21])+((max(prod_plot[,21],prod_plot2[,21])-min(prod_plot[,21],prod_plot2[,21]))/2)), max(prod_plot[,21],prod_plot2[,21])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 17. Electrical equipment
plot(prod_plot[,17], yaxt="n", xaxt='n', main="Electrical equipment", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,17],prod_plot2[,17]),max(prod_plot[,17],prod_plot2[,17])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,17], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,17], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,17]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,17]), labels=l)
l<-formatC(c(min(prod_plot[,17],prod_plot2[,17]), (min(prod_plot[,17],prod_plot2[,17])+((max(prod_plot[,17],prod_plot2[,17])-min(prod_plot[,17],prod_plot2[,17]))/2)), max(prod_plot[,17],prod_plot2[,17])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,17],prod_plot2[,17]), (min(prod_plot[,17],prod_plot2[,17])+((max(prod_plot[,17],prod_plot2[,17])-min(prod_plot[,17],prod_plot2[,17]))/2)), max(prod_plot[,17],prod_plot2[,17])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 25. Water and waste management and treatment
plot(prod_plot[,25], yaxt="n", xaxt='n', main="Water & waste management-treatment", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,25],prod_plot2[,25]),max(prod_plot[,25],prod_plot2[,25])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,25], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,25], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,25]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,25]), labels=l)
l<-formatC(c(min(prod_plot[,25],prod_plot2[,25]), (min(prod_plot[,25],prod_plot2[,25])+((max(prod_plot[,25],prod_plot2[,25])-min(prod_plot[,25],prod_plot2[,25]))/2)), max(prod_plot[,25],prod_plot2[,25])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,25],prod_plot2[,25]), (min(prod_plot[,25],prod_plot2[,25])+((max(prod_plot[,25],prod_plot2[,25])-min(prod_plot[,25],prod_plot2[,25]))/2)), max(prod_plot[,25],prod_plot2[,25])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 13. Other non-metallic mineral products
plot(prod_plot[,13], yaxt="n", xaxt='n', main="Other non-metallic mineral products", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,13],prod_plot2[,13]),max(prod_plot[,13],prod_plot2[,13])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,13], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,13], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,13]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,13]), labels=l)
l<-formatC(c(min(prod_plot[,13],prod_plot2[,13]), (min(prod_plot[,13],prod_plot2[,13])+((max(prod_plot[,13],prod_plot2[,13])-min(prod_plot[,13],prod_plot2[,13]))/2)), max(prod_plot[,13],prod_plot2[,13])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,13],prod_plot2[,13]), (min(prod_plot[,13],prod_plot2[,13])+((max(prod_plot[,13],prod_plot2[,13])-min(prod_plot[,13],prod_plot2[,13]))/2)), max(prod_plot[,13],prod_plot2[,13])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

dev.off()



jpeg("Emilia_IP2_hazus.jpeg", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))

# 20. Other transport equipment
plot(prod_plot[,20], yaxt="n", xaxt='n', main="Other transport equipment", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,20],prod_plot2[,20]),max(prod_plot[,20],prod_plot2[,20])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,20], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,20], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,20]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,20]), labels=l)
l<-formatC(c(min(prod_plot[,20],prod_plot2[,20]), (min(prod_plot[,20],prod_plot2[,20])+((max(prod_plot[,20],prod_plot2[,20])-min(prod_plot[,20],prod_plot2[,20]))/2)), max(prod_plot[,20],prod_plot2[,20])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,20],prod_plot2[,20]), (min(prod_plot[,20],prod_plot2[,20])+((max(prod_plot[,20],prod_plot2[,20])-min(prod_plot[,20],prod_plot2[,20]))/2)), max(prod_plot[,20],prod_plot2[,20])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 11. Pharmaceuticals
plot(prod_plot[,11], yaxt="n", xaxt='n', main="Pharmaceuticals", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,11],prod_plot2[,11]),max(prod_plot[,11],prod_plot2[,11])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,11], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,11], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,11]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,11]), labels=l)
l<-formatC(c(min(prod_plot[,11],prod_plot2[,11]), (min(prod_plot[,11],prod_plot2[,11])+((max(prod_plot[,11],prod_plot2[,11])-min(prod_plot[,11],prod_plot2[,11]))/2)), max(prod_plot[,11],prod_plot2[,11])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,11],prod_plot2[,11]), (min(prod_plot[,11],prod_plot2[,11])+((max(prod_plot[,11],prod_plot2[,11])-min(prod_plot[,11],prod_plot2[,11]))/2)), max(prod_plot[,11],prod_plot2[,11])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 7. Paper and paper products
plot(prod_plot[,7], yaxt="n", xaxt='n',main="Paper & paper products", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,7],prod_plot2[,7]),max(prod_plot[,7],prod_plot2[,7])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,7], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,7], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,7]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,7]), labels=l)
l<-formatC(c(min(prod_plot[,7],prod_plot2[,7]), (min(prod_plot[,7],prod_plot2[,7])+((max(prod_plot[,7],prod_plot2[,7])-min(prod_plot[,7],prod_plot2[,7]))/2)), max(prod_plot[,7],prod_plot2[,7])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,7],prod_plot2[,7]), (min(prod_plot[,7],prod_plot2[,7])+((max(prod_plot[,7],prod_plot2[,7])-min(prod_plot[,7],prod_plot2[,7]))/2)), max(prod_plot[,7],prod_plot2[,7])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 16. Computer, electronic and optical products
plot(prod_plot[,16], yaxt="n", xaxt='n', main="Computer, electronic & optical prod.", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,16],prod_plot2[,16]),max(prod_plot[,16],prod_plot2[,16])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,16], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,16], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,16]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,16]), labels=l)
l<-formatC(c(min(prod_plot[,16],prod_plot2[,16]), (min(prod_plot[,16],prod_plot2[,16])+((max(prod_plot[,16],prod_plot2[,16])-min(prod_plot[,16],prod_plot2[,16]))/2)), max(prod_plot[,16],prod_plot2[,16])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,16],prod_plot2[,16]), (min(prod_plot[,16],prod_plot2[,16])+((max(prod_plot[,16],prod_plot2[,16])-min(prod_plot[,16],prod_plot2[,16]))/2)), max(prod_plot[,16],prod_plot2[,16])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 22. Manufacturing NEC, repair and inst. of machinery
plot(prod_plot[,22], yaxt="n", xaxt='n', main="Manufacturing NEC, repair & inst. machin.", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,22],prod_plot2[,22]),max(prod_plot[,22],prod_plot2[,22])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,22], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,22], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,22]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,22]), labels=l)
l<-formatC(c(min(prod_plot[,22],prod_plot2[,22]), (min(prod_plot[,22],prod_plot2[,22])+((max(prod_plot[,22],prod_plot2[,22])-min(prod_plot[,22],prod_plot2[,22]))/2)), max(prod_plot[,22],prod_plot2[,22])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,22],prod_plot2[,22]), (min(prod_plot[,22],prod_plot2[,22])+((max(prod_plot[,22],prod_plot2[,22])-min(prod_plot[,22],prod_plot2[,22]))/2)), max(prod_plot[,22],prod_plot2[,22])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 6. Wood and products of wood and cork
plot(prod_plot[,6], yaxt="n", xaxt='n', main="Wood & products of wood and cork", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,6],prod_plot2[,6]),max(prod_plot[,6],prod_plot2[,6])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,6], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,6], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,6]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,6]), labels=l)
l<-formatC(c(min(prod_plot[,6],prod_plot2[,6]), (min(prod_plot[,6],prod_plot2[,6])+((max(prod_plot[,6],prod_plot2[,6])-min(prod_plot[,6],prod_plot2[,6]))/2)), max(prod_plot[,6],prod_plot2[,6])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,6],prod_plot2[,6]), (min(prod_plot[,6],prod_plot2[,6])+((max(prod_plot[,6],prod_plot2[,6])-min(prod_plot[,6],prod_plot2[,6]))/2)), max(prod_plot[,6],prod_plot2[,6])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 24. Water supply
plot(prod_plot[,24], yaxt="n", xaxt='n', main="Water supply", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,24],prod_plot2[,24]),max(prod_plot[,24],prod_plot2[,24])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,24], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,24], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,24]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,24]), labels=l)
l<-formatC(c(min(prod_plot[,24],prod_plot2[,24]), (min(prod_plot[,24],prod_plot2[,24])+((max(prod_plot[,24],prod_plot2[,24])-min(prod_plot[,24],prod_plot2[,24]))/2)), max(prod_plot[,24],prod_plot2[,24])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,24],prod_plot2[,24]), (min(prod_plot[,24],prod_plot2[,24])+((max(prod_plot[,24],prod_plot2[,24])-min(prod_plot[,24],prod_plot2[,24]))/2)), max(prod_plot[,24],prod_plot2[,24])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 8. Print and reproduction of rec. media
plot(prod_plot[,8], yaxt="n", xaxt='n', main="Print & reproduction of rec. media", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,8],prod_plot2[,8]),max(prod_plot[,8],prod_plot2[,8])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,8], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,8], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,8]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,8]), labels=l)
l<-formatC(c(min(prod_plot[,8],prod_plot2[,8]), (min(prod_plot[,8],prod_plot2[,8])+((max(prod_plot[,8],prod_plot2[,8])-min(prod_plot[,8],prod_plot2[,8]))/2)), max(prod_plot[,8],prod_plot2[,8])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,8],prod_plot2[,8]), (min(prod_plot[,8],prod_plot2[,8])+((max(prod_plot[,8],prod_plot2[,8])-min(prod_plot[,8],prod_plot2[,8]))/2)), max(prod_plot[,8],prod_plot2[,8])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 3. Mining and quarrying
plot(prod_plot[,3], yaxt="n", xaxt='n', main="Mining & quarrying", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,3],prod_plot2[,3]),max(prod_plot[,3],prod_plot2[,3])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,3], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,3], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,3]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,3]), labels=l)
l<-formatC(c(min(prod_plot[,3],prod_plot2[,3]), (min(prod_plot[,3],prod_plot2[,3])+((max(prod_plot[,3],prod_plot2[,3])-min(prod_plot[,3],prod_plot2[,3]))/2)), max(prod_plot[,3],prod_plot2[,3])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,3],prod_plot2[,3]), (min(prod_plot[,3],prod_plot2[,3])+((max(prod_plot[,3],prod_plot2[,3])-min(prod_plot[,3],prod_plot2[,3]))/2)), max(prod_plot[,3],prod_plot2[,3])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 2. Fishing
plot(prod_plot[,2], yaxt="n", xaxt='n', main="Fishing", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,2],prod_plot2[,2]),max(prod_plot[,2],prod_plot2[,2])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,2], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,2], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,2]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,2]), labels=l)
l<-formatC(c(min(prod_plot[,2],prod_plot2[,2]), (min(prod_plot[,2],prod_plot2[,2])+((max(prod_plot[,2],prod_plot2[,2])-min(prod_plot[,2],prod_plot2[,2]))/2)), max(prod_plot[,2],prod_plot2[,2])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,2],prod_plot2[,2]), (min(prod_plot[,2],prod_plot2[,2])+((max(prod_plot[,2],prod_plot2[,2])-min(prod_plot[,2],prod_plot2[,2]))/2)), max(prod_plot[,2],prod_plot2[,2])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")
#legend("bottomright", inset= c(-.05,.05), cex=1.1, legend=c("modified Leontief", "Standard Leontief", "Shock Impact"), xpd=TRUE,  
       #col = c("brown3","chartreuse4","black"), lty = c(1,1,5))

plot.new()
legend("center", cex=2.55, legend=c("Modified Leontief", "Standard Leontief", "Shock Impact"), xpd=TRUE,  
       col = c("brown3","chartreuse4","black"), lty = c(1,1,5))


dev.off()



jpeg("Emilia_serv1_hazus.jpeg", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))

# 27. Wholesale and retail trade, repair of motor vehicles and motorcycles
plot(prod_plot[,27], yaxt="n", xaxt='n', main="Wholesale & retail trade", cex.main=1.8, type="l", lwd=2.0, col="brown3", xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,27],prod_plot2[,27]),max(prod_plot[,27],prod_plot2[,27])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,27], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,27], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,27]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,27]), labels=l)
l<-formatC(c(min(prod_plot[,27],prod_plot2[,27]), (min(prod_plot[,27],prod_plot2[,27])+((max(prod_plot[,27],prod_plot2[,27])-min(prod_plot[,27],prod_plot2[,27]))/2)), max(prod_plot[,27],prod_plot2[,27])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,27],prod_plot2[,27]), (min(prod_plot[,27],prod_plot2[,27])+((max(prod_plot[,27],prod_plot2[,27])-min(prod_plot[,27],prod_plot2[,27]))/2)), max(prod_plot[,27],prod_plot2[,27])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 34. Real estate
plot(prod_plot[,34], yaxt="n", xaxt='n', main="Real estate", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,34],prod_plot2[,34]),max(prod_plot[,34],prod_plot2[,34])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,34], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,34], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,34]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,34]), labels=l)
l<-formatC(c(min(prod_plot[,34],prod_plot2[,34]), (min(prod_plot[,34],prod_plot2[,34])+((max(prod_plot[,34],prod_plot2[,34])-min(prod_plot[,34],prod_plot2[,34]))/2)), max(prod_plot[,34],prod_plot2[,34])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,34],prod_plot2[,34]), (min(prod_plot[,34],prod_plot2[,34])+((max(prod_plot[,34],prod_plot2[,34])-min(prod_plot[,34],prod_plot2[,34]))/2)), max(prod_plot[,34],prod_plot2[,34])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 28. Transportation and storage
plot(prod_plot[,28], yaxt="n", xaxt='n', main="Transportation & storage", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,28],prod_plot2[,28]),max(prod_plot[,28],prod_plot2[,28])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,28], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,28], type="l", lty=5, lwd=1.7, col="black")
lines(cap_plot2[,28], type="l", lty=3, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,28]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,28]), labels=l)
l<-formatC(c(min(prod_plot[,28],prod_plot2[,28]), (min(prod_plot[,28],prod_plot2[,28])+((max(prod_plot[,28],prod_plot2[,28])-min(prod_plot[,28],prod_plot2[,28]))/2)), max(prod_plot[,28],prod_plot2[,28])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,28],prod_plot2[,28]), (min(prod_plot[,28],prod_plot2[,28])+((max(prod_plot[,28],prod_plot2[,28])-min(prod_plot[,28],prod_plot2[,28]))/2)), max(prod_plot[,28],prod_plot2[,28])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 41. Health
plot(prod_plot[,41], yaxt="n", xaxt='n', main="Health", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,41],prod_plot2[,41]),max(prod_plot[,41],prod_plot2[,41])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,41], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,41], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,41]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,41]), labels=l)
l<-formatC(c(min(prod_plot[,41],prod_plot2[,41]), (min(prod_plot[,41],prod_plot2[,41])+((max(prod_plot[,41],prod_plot2[,41])-min(prod_plot[,41],prod_plot2[,41]))/2)), max(prod_plot[,41],prod_plot2[,41])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,41],prod_plot2[,41]), (min(prod_plot[,41],prod_plot2[,41])+((max(prod_plot[,41],prod_plot2[,41])-min(prod_plot[,41],prod_plot2[,41]))/2)), max(prod_plot[,41],prod_plot2[,41])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 39. Public administration and defense
plot(prod_plot[,39], yaxt="n", xaxt='n', main="Public administration & defense", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,39],prod_plot2[,39]),max(prod_plot[,39],prod_plot2[,39])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,39], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,39], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,39]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,39]), labels=l)
l<-formatC(c(min(prod_plot[,39],prod_plot2[,39]), (min(prod_plot[,39],prod_plot2[,39])+((max(prod_plot[,39],prod_plot2[,39])-min(prod_plot[,39],prod_plot2[,39]))/2)), max(prod_plot[,39],prod_plot2[,39])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,39],prod_plot2[,39]), (min(prod_plot[,39],prod_plot2[,39])+((max(prod_plot[,39],prod_plot2[,39])-min(prod_plot[,39],prod_plot2[,39]))/2)), max(prod_plot[,39],prod_plot2[,39])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 33. Financial services and insurance
plot(prod_plot[,33], yaxt="n", xaxt='n', main="Financial services & insurance", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,33],prod_plot2[,33]),max(prod_plot[,33],prod_plot2[,33])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,33], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,33], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,33]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,33]), labels=l)
l<-formatC(c(min(prod_plot[,33],prod_plot2[,33]), (min(prod_plot[,33],prod_plot2[,33])+((max(prod_plot[,33],prod_plot2[,33])-min(prod_plot[,33],prod_plot2[,33]))/2)), max(prod_plot[,33],prod_plot2[,33])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,33],prod_plot2[,33]), (min(prod_plot[,33],prod_plot2[,33])+((max(prod_plot[,33],prod_plot2[,33])-min(prod_plot[,33],prod_plot2[,33]))/2)), max(prod_plot[,33],prod_plot2[,33])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 29. Accomodation and food service activities
plot(prod_plot[,29], yaxt="n", xaxt='n', main="Accomodation & food services", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,29],prod_plot2[,29]),max(prod_plot[,29],prod_plot2[,29])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,29], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,29], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,29]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,29]), labels=l)
l<-formatC(c(min(prod_plot[,29],prod_plot2[,29]), (min(prod_plot[,29],prod_plot2[,29])+((max(prod_plot[,29],prod_plot2[,29])-min(prod_plot[,29],prod_plot2[,29]))/2)), max(prod_plot[,29],prod_plot2[,29])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,29],prod_plot2[,29]), (min(prod_plot[,29],prod_plot2[,29])+((max(prod_plot[,29],prod_plot2[,29])-min(prod_plot[,29],prod_plot2[,29]))/2)), max(prod_plot[,29],prod_plot2[,29])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 38. Other supporting service activities
plot(prod_plot[,38], yaxt="n", xaxt='n', main="Other supporting services", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,38],prod_plot2[,38]),max(prod_plot[,38],prod_plot2[,38])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,38], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,38], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,38]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,38]), labels=l)
l<-formatC(c(min(prod_plot[,38],prod_plot2[,38]), (min(prod_plot[,38],prod_plot2[,38])+((max(prod_plot[,38],prod_plot2[,38])-min(prod_plot[,38],prod_plot2[,38]))/2)), max(prod_plot[,38],prod_plot2[,38])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,38],prod_plot2[,38]), (min(prod_plot[,38],prod_plot2[,38])+((max(prod_plot[,38],prod_plot2[,38])-min(prod_plot[,38],prod_plot2[,38]))/2)), max(prod_plot[,38],prod_plot2[,38])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 35. Accounting and business services
plot(prod_plot[,35], yaxt="n", xaxt='n', main="Accounting & business services", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,35],prod_plot2[,35]),max(prod_plot[,35],prod_plot2[,35])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,35], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,35], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,35]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,35]), labels=l)
l<-formatC(c(min(prod_plot[,35],prod_plot2[,35]), (min(prod_plot[,35],prod_plot2[,35])+((max(prod_plot[,35],prod_plot2[,35])-min(prod_plot[,35],prod_plot2[,35]))/2)), max(prod_plot[,35],prod_plot2[,35])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,35],prod_plot2[,35]), (min(prod_plot[,35],prod_plot2[,35])+((max(prod_plot[,35],prod_plot2[,35])-min(prod_plot[,35],prod_plot2[,35]))/2)), max(prod_plot[,35],prod_plot2[,35])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 40. Education
plot(prod_plot[,40], yaxt="n", xaxt='n', main="Education", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,40],prod_plot2[,40]),max(prod_plot[,40],prod_plot2[,40])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,40], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,40], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,40]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,40]), labels=l)
l<-formatC(c(min(prod_plot[,40],prod_plot2[,40]), (min(prod_plot[,40],prod_plot2[,40])+((max(prod_plot[,40],prod_plot2[,40])-min(prod_plot[,40],prod_plot2[,40]))/2)), max(prod_plot[,40],prod_plot2[,40])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,40],prod_plot2[,40]), (min(prod_plot[,40],prod_plot2[,40])+((max(prod_plot[,40],prod_plot2[,40])-min(prod_plot[,40],prod_plot2[,40]))/2)), max(prod_plot[,40],prod_plot2[,40])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 32. IT services and other information services
plot(prod_plot[,32], yaxt="n", xaxt='n', main="IT services & other info. serv.", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,32],prod_plot2[,32]),max(prod_plot[,32],prod_plot2[,32])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,32], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,32], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,32]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,32]), labels=l)
l<-formatC(c(min(prod_plot[,32],prod_plot2[,32]), (min(prod_plot[,32],prod_plot2[,32])+((max(prod_plot[,32],prod_plot2[,32])-min(prod_plot[,32],prod_plot2[,32]))/2)), max(prod_plot[,32],prod_plot2[,32])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,32],prod_plot2[,32]), (min(prod_plot[,32],prod_plot2[,32])+((max(prod_plot[,32],prod_plot2[,32])-min(prod_plot[,32],prod_plot2[,32]))/2)), max(prod_plot[,32],prod_plot2[,32])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 43. Other service activities
plot(prod_plot[,43], yaxt="n", xaxt='n', main="Other service activities", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,43],prod_plot2[,43]),max(prod_plot[,43],prod_plot2[,43])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,43], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,43], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,43]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,43]), labels=l)
l<-formatC(c(min(prod_plot[,43],prod_plot2[,43]), (min(prod_plot[,43],prod_plot2[,43])+((max(prod_plot[,43],prod_plot2[,43])-min(prod_plot[,43],prod_plot2[,43]))/2)), max(prod_plot[,43],prod_plot2[,43])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,43],prod_plot2[,43]), (min(prod_plot[,43],prod_plot2[,43])+((max(prod_plot[,43],prod_plot2[,43])-min(prod_plot[,43],prod_plot2[,43]))/2)), max(prod_plot[,43],prod_plot2[,43])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 42. Art and entertainment
plot(prod_plot[,42], yaxt="n", xaxt='n', main="Art & entertainment", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,42],prod_plot2[,42]),max(prod_plot[,42],prod_plot2[,42])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,42], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,42], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,42]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,42]), labels=l)
l<-formatC(c(min(prod_plot[,42],prod_plot2[,42]), (min(prod_plot[,42],prod_plot2[,42])+((max(prod_plot[,42],prod_plot2[,42])-min(prod_plot[,42],prod_plot2[,42]))/2)), max(prod_plot[,42],prod_plot2[,42])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,42],prod_plot2[,42]), (min(prod_plot[,42],prod_plot2[,42])+((max(prod_plot[,42],prod_plot2[,42])-min(prod_plot[,42],prod_plot2[,42]))/2)), max(prod_plot[,42],prod_plot2[,42])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 37. Other professional and scientific activities
plot(prod_plot[,37], yaxt="n", xaxt='n', main="Other professional-scientific act.", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,37],prod_plot2[,37]),max(prod_plot[,37],prod_plot2[,37])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,37], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,37], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,37]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,37]), labels=l)
l<-formatC(c(min(prod_plot[,37],prod_plot2[,37]), (min(prod_plot[,37],prod_plot2[,37])+((max(prod_plot[,37],prod_plot2[,37])-min(prod_plot[,37],prod_plot2[,37]))/2)), max(prod_plot[,37],prod_plot2[,37])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,37],prod_plot2[,37]), (min(prod_plot[,37],prod_plot2[,37])+((max(prod_plot[,37],prod_plot2[,37])-min(prod_plot[,37],prod_plot2[,37]))/2)), max(prod_plot[,37],prod_plot2[,37])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 31. Telecommunications
plot(prod_plot[,31], yaxt="n", xaxt='n', main="Telecommunications", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,31],prod_plot2[,31]),max(prod_plot[,31],prod_plot2[,31])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,31], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,31], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,31]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,31]), labels=l)
l<-formatC(c(min(prod_plot[,31],prod_plot2[,31]), (min(prod_plot[,31],prod_plot2[,31])+((max(prod_plot[,31],prod_plot2[,31])-min(prod_plot[,31],prod_plot2[,31]))/2)), max(prod_plot[,31],prod_plot2[,31])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,31],prod_plot2[,31]), (min(prod_plot[,31],prod_plot2[,31])+((max(prod_plot[,31],prod_plot2[,31])-min(prod_plot[,31],prod_plot2[,31]))/2)), max(prod_plot[,31],prod_plot2[,31])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

# 30. Publishing, motion picture, video, sound and broadcasting activities
plot(prod_plot[,30], yaxt="n", xaxt='n', main="Publishing, video, sound & broadc.", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,30],prod_plot2[,30]),max(prod_plot[,30],prod_plot2[,30])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,30], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,30], type="l", lty=5, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,30]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,30]), labels=l)
l<-formatC(c(min(prod_plot[,30],prod_plot2[,30]), (min(prod_plot[,30],prod_plot2[,30])+((max(prod_plot[,30],prod_plot2[,30])-min(prod_plot[,30],prod_plot2[,30]))/2)), max(prod_plot[,30],prod_plot2[,30])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,30],prod_plot2[,30]), (min(prod_plot[,30],prod_plot2[,30])+((max(prod_plot[,30],prod_plot2[,30])-min(prod_plot[,30],prod_plot2[,30]))/2)), max(prod_plot[,30],prod_plot2[,30])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")

dev.off()





jpeg("Emilia_serv2_hazus.jpeg", width = 2500, height = 1600, res = 150)
par(mfrow = c(4, 4))
par(mar = c(4, 4, 2, 2))


# 36. Research
plot(prod_plot[,36], yaxt="n", xaxt='n', main="Research", cex.main=1.8, type="l", lwd=2.0, col="brown3",  xlab= "", ylab="", cex.axis=1, ylim=c(min(prod_plot[,36],prod_plot2[,36]),max(prod_plot[,36],prod_plot2[,36])))
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted")
lines(prod_plot2[,36], type="l", lty=1, lwd=2.0, col="chartreuse4")
lines(cap_plot[,36], type="l", lty=5, lwd=1.7, col="black")
lines(cap_plot2[,36], type="l", lty=3, lwd=1.7, col="black")
axis(1, cex.axis=1.4)
#l<-formatC(pretty(prod_plot[,36]),format="f",digits = 1)
#axis(2, at=pretty(prod_plot[,36]), labels=l)
l<-formatC(c(min(prod_plot[,36],prod_plot2[,36]), (min(prod_plot[,36],prod_plot2[,36])+((max(prod_plot[,36],prod_plot2[,36])-min(prod_plot[,36],prod_plot2[,36]))/2)), max(prod_plot[,36],prod_plot2[,36])),format="f",digits = 1)
axis(2, at=c(min(prod_plot[,36],prod_plot2[,36]), (min(prod_plot[,36],prod_plot2[,36])+((max(prod_plot[,36],prod_plot2[,36])-min(prod_plot[,36],prod_plot2[,36]))/2)), max(prod_plot[,36],prod_plot2[,36])), labels=l, cex.axis=1.4)
abline(v=wks, col="lightgray", lty="dotted")
#legend("bottomright", inset= c(-.05,.05), cex=1.1, legend=c("modified prod. function (MPD)", "traditional Leontief (TL)", "Capacity_MPD", "Capacity_TL"), xpd=TRUE,  
       #col = c("brown3","chartreuse4","black","black"), lty = c(1,1,5,3))

plot.new()
legend("center", cex=2.55, legend=c("Modified Leontief", "Standard Leontief", "Shock Impact"), xpd=TRUE,  
       col = c("brown3","chartreuse4","black"), lty = c(1,1,5))

dev.off()



#### PLOT LOSSES ####

# FOR SECTORS IN EMILIA ROMAGNA

#plotloss=lossProd_EROM_minDT2
#plotloss=lossProd_EROM_1y2
plotloss=lossProd_EROM2


L<-matrix(NA,nrow=(length(rank_IP)+length(rank_Serv)+1),ncol=2)
for (i in 1:length(rank_IP)) {
  x<-as.numeric(rank_IP[i])
  L[i,2]<-as.numeric(plotloss[x])
}
for (j in 1:length(rank_Serv)){
  y<-as.numeric(rank_Serv[j])
  pos<-j+(length(rank_IP)+1)
  L[pos,2]<-as.numeric(plotloss[y])
}
L[27,2]<-0   #line to set a space between IP and Serv

L[(1:length(rank_names_IP)),1]<-rank_names_IP
L[((length(rank_names_IP)+2):44),1]<-rank_names_Serv
L <- as.data.frame(apply(L, 2, as.numeric))

L$V2<- format(round(L$V2, 4), nsmall = 4)
L <- as.data.frame(apply(L, 2, as.numeric))

data3 <- data.frame(
  name=c("Construction","Food, beverages and tobacco","Machinery and equipment NEC","Fabricated metal products","Electricity, gas, steam and air condit. supply","Textiles, wearing apparel and leather","Motor vehicles, trailers and semi-trailers","Basic metals, fabricated metal products","Agriculture, forestry","Chemicals","Coke and refined petroleum products","Rubber and plastic products","Furniture and other manufactured goods","Electrical equipment","Water and waste management and treatment","Other non-metallic mineral products","Other transport equipment","Pharmaceuticals","Paper and paper products","Computer, electronic and optical products","Manufacturing NEC, repair and inst. of machinery","Wood and products of wood and cork","Water supply","Print and reproduction of rec. media","Mining and quarrying","Fishing"," ","Wholesale and retail trade, repair of motor veh.","Real estate","Transportation and storage","Health","Public administration and defense","Financial services and insurance","Accomodation and food service activities","Other supporting service activities","Accounting and business services","Education","IT services and other information services","Other service activities","Art and Entertainment","Other professional and scientific activities","Telecommunications","Publishing, motion pic., video, sound and broadc.","Research"),
  value=c(L[(1:44),2])
)

# make V1 an ordered factor
data3$name <- factor(data3$name, levels = data3$name)

scaleFUN <- function(x) sprintf("%.2f", x)


#png("EMILIAdecline_minDT_2.png", width = 2000, height = 1200, res = 200)
#png("EMILIAdecline_1y_2.png", width = 2000, height = 1200, res = 200)
jpeg("EMILIAdecline_2y_2.jpeg", width = 2000, height = 1200, res = 200)


ggplot(data3, aes(x=name, y=value, label=sprintf("%0.2f", round(value, digits = 2)))) +
  geom_bar(stat="identity",position="identity") +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1)) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"
                                    ),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey90")
  )+
  scale_y_continuous(labels=scaleFUN) +
  #ggtitle("Simulated loss of gross output for the length of the minimum restoration time (Emilia-Romagna)") #for minDT
  #ggtitle("Simulated loss of gross output after one year (Emilia-Romagna)") #for 1y
  ggtitle("Simulated loss of gross output over the after two years (Emilia-Romagna)") #for 2 years

dev.off()


## PLOT LOSSES IN SAME GRAPH ##

plotloss1=lossProd_EROM_minDT2
plotloss2=lossProd_EROM_1y2
plotloss3=lossProd_EROM2


L1<-matrix(NA,nrow=(length(rank_IP)+length(rank_Serv)),ncol=2)
L2<-matrix(NA,nrow=(length(rank_IP)+length(rank_Serv)),ncol=2)
L3<-matrix(NA,nrow=(length(rank_IP)+length(rank_Serv)),ncol=2)
for (i in 1:length(rank_IP)) {
  x<-as.numeric(rank_IP[i])
  L1[i,2]<-as.numeric(plotloss1[x])
  L2[i,2]<-as.numeric(plotloss2[x])
  L3[i,2]<-as.numeric(plotloss3[x])
}
for (j in 1:length(rank_Serv)){
  y<-as.numeric(rank_Serv[j])
  pos<-j+(length(rank_IP))
  L1[pos,2]<-as.numeric(plotloss1[y])
  L2[pos,2]<-as.numeric(plotloss2[y])
  L3[pos,2]<-as.numeric(plotloss3[y])
}



L1[(1:length(rank_names_IP)),1]<-rank_names_IP
L2[(1:length(rank_names_IP)),1]<-rank_names_IP
L3[(1:length(rank_names_IP)),1]<-rank_names_IP

L1[((length(rank_names_IP)+1):43),1]<-rank_names_Serv
L2[((length(rank_names_IP)+1):43),1]<-rank_names_Serv
L3[((length(rank_names_IP)+1):43),1]<-rank_names_Serv

L2<-L2[,-1]
L3<-L3[,-1]
df <- data.frame(L1,L2,L3)
colnames(df)<-c("sector","minDT","oney","twoy")

df$minDT<- format(round(as.numeric(df$minDT), digits=4), nsmall = 4)
df$oney<- format(round(as.numeric(df$oney), digits=4), nsmall = 4)
df$twoy<- format(round(as.numeric(df$twoy), digits=4), nsmall = 4)
df <- as.data.frame(apply(df, 2, as.numeric))

data3 <- data.frame(
  name=c("Construction","Food, beverages and tobacco","Machinery and equipment NEC","Fabricated metal products","Electricity, gas, steam and air condit. supply","Textiles, wearing apparel and leather","Motor vehicles, trailers and semi-trailers","Basic metals, fabricated metal products","Agriculture, forestry","Chemicals","Coke and refined petroleum products","Rubber and plastic products","Furniture and other manufactured goods","Electrical equipment","Water and waste management and treatment","Other non-metallic mineral products","Other transport equipment","Pharmaceuticals","Paper and paper products","Computer, electronic and optical products","Manufacturing NEC, repair and inst. of machinery","Wood and products of wood and cork","Water supply","Print and reproduction of rec. media","Mining and quarrying","Fishing","Wholesale and retail trade, repair of motor veh.","Real estate","Transportation and storage","Health","Public administration and defense","Financial services and insurance","Accomodation and food service activities","Other supporting service activities","Accounting and business services","Education","IT services and other information services","Other service activities","Art and Entertainment","Other professional and scientific activities","Telecommunications","Publishing, motion pic., video, sound and broadc.","Research"),
  value=c(df[(1:43),(2:4)])
)

# make V1 an ordered factor
data3$name <- factor(data3$name, levels = data3$name)

scaleFUN <- function(x) sprintf("%.2f", x)

library(tidyr)
df_long <- pivot_longer(data3, cols = c("value.minDT", "value.oney", "value.twoy"), 
                        names_to = "variable", 
                        values_to = "value")
jpeg("EMILIA_decline_rev2.jpeg", width = 3000, height = 2500, res = 270)

p<-ggplot(df_long, aes(x = name, y = value, fill = variable, label=sprintf("%0.2f", round(value, digits = 2)))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("") +
  ylab("%") +
  theme(axis.text.y = element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(angle = 65, vjust=1, hjust=1, size=14)) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"
    ),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey90")
  )+
  scale_y_continuous(labels=scaleFUN) +
  scale_fill_manual(values = c("value.minDT" = "darkslategrey", "value.oney" = "darkslategray4", "value.twoy" = "darkslategray3"),
                    labels = c("over one month", "over one year", "over two years")) +  # Cambia i nomi nella legenda
  guides(fill = guide_legend(title = NULL)) +  # Rimuove il titolo della legenda
  ggtitle("Simulated loss of gross output (Emilia-Romagna)") +
  theme(plot.title = element_text(size = 16))
print(p)
dev.off()
#ggsave("EMILIA_decline_rev2.jpeg", plot = p, width = 13.5, height = 9)







# FOR ITALIAN REGIONS

plotloss_reg=lossGDPreg_1y2

library(ggrepel)
library(scales)
df <- data.frame(reg=c(1:19),
                 loss=plotloss_reg[-8],
                 name=c('Piemonte', "Valle D'Aosta", 'Lombardia', 'Trentino Alto Adige', 'Veneto', 'Friuli Venezia Giulia', 'Liguria', 'Toscana', 'Umbria', 'Marche', 'Lazio', 'Abruzzo', 'Molise', 'Campania', 'Puglia', 'Basilicata', 'Calabria', 'Sicilia', 'Sardegna'))

png("Loss_reg2.png", width = 2000, height = 1000, res = 200)

#create scatterplot with a label on every point
ggplot(df, aes(reg,loss)) +
  #ggplot(data=df, aes(x=reorder(reg, loss), y=loss))+
  geom_point() +
  geom_text_repel(aes(label = name)) +
  #scale_y_continuous(labels = percent) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"
    ),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey90")
  )+
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

dev.off()





# FOR ITALIAN REGIONS

plotloss_reg=lossGDPreg_1y2

library(ggrepel)
library(scales)
df <- data.frame(reg=c(1:19),
                 loss=plotloss_reg[-8],
                 # name=c('Piemonte', "Valle D'Aosta", 'Lombardia', 'Trentino Alto Adige', 'Veneto', 'Friuli Venezia Giulia', 'Liguria', 'Toscana', 'Umbria', 'Marche', 'Lazio', 'Abruzzo', 'Molise', 'Campania', 'Puglia', 'Basilicata', 'Calabria', 'Sicilia', 'Sardegna'))
                 name=c("Friuli-Venezia Giulia","Liguria","Lombardia","Piemonte","Trentino-Alto Adige","Valle d'Aosta","Veneto","Lazio","Marche","Toscana","Umbria","Abruzzo","Basilicata","Calabria","Campania","Molise","Puglia","Sardegna","Sicilia"))



png("Loss_reg2.png", width = 2000, height = 1000, res = 200)

#create scatterplot with a label on every point
ggplot(df, aes(reg,loss)) +
  #ggplot(data=df, aes(x=reorder(reg, loss), y=loss))+
  geom_point() +
  geom_text_repel(aes(label = name)) +
  #scale_y_continuous(labels = percent) +
  theme(
    text=element_text(size=21),
    panel.background = element_rect(fill = "white", colour = "black"
    ),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey90")
  )+
  xlab("Italian regions") +
  ylab(" % Loss ") +
  geom_ribbon(aes(xmin=0, xmax=7.5), alpha=0.2) +
  geom_ribbon(aes(xmin=7.6, xmax=11.5), alpha=0.4) +
  geom_ribbon(aes(xmin=11.6, xmax=21), alpha=0.6) +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

dev.off()






#### PLOT MAX DECLINES ####

#plotdecline=declinesEROM_minDT
plotdecline=declinesEROM_1y
#plotdecline=declinesEROM


L<-matrix(NA,nrow=(length(rank_IP)+length(rank_Serv)+1),ncol=2)
for (i in 1:length(rank_IP)) {
  x<-as.numeric(rank_IP[i])
  L[i,2]<-as.numeric(plotdecline[x])
}
for (j in 1:length(rank_Serv)){
  y<-as.numeric(rank_Serv[j])
  pos<-j+(length(rank_IP)+1)
  L[pos,2]<-as.numeric(plotdecline[y])
}
L[27,2]<-0   #line to set a space between IP and Serv

L[(1:length(rank_names_IP)),1]<-rank_names_IP
L[((length(rank_names_IP)+2):44),1]<-rank_names_Serv
L <- as.data.frame(apply(L, 2, as.numeric))

L$V2<- format(round(L$V2, 4), nsmall = 4)
L <- as.data.frame(apply(L, 2, as.numeric))

data3 <- data.frame(
  name=c("Construction","Food, beverages and tobacco","Machinery and equipment NEC","Fabricated metal products","Electricity, gas, steam and air conditioning supply","Textiles, wearing apparel and leather","Motor vehicles, trailers and semi-trailers","Basic metals, fabricated metal products","Agriculture, forestry","Chemicals","Coke and refined petroleum products","Rubber and plastic products","Furniture and other manufactured goods","Electrical equipment","Water and waste management and treatment","Other non-metallic mineral products","Other transport equipment","Pharmaceuticals","Paper and paper products","Computer, electronic and optical products","Manufacturing NEC, repair and inst. of machinery","Wood and products of wood and cork","Water supply","Print and reproduction of rec. media","Mining and quarrying","Fishing"," ","Wholesale and retail trade, repair of motor vehicles and motorcycles","Real estate","Transportation and storage","Health","Public administration and defense","Financial services and insurance","Accomodation and food service activities","Other supporting service activities","Accounting and business services","Education","IT services and other information services","Other service activities","Art and Entertainment","Other professional and scientific activities","Telecommunications","Publishing, motion picture, video, sound and broadcasting activities","Research"),
  value=c(L[(1:44),2])
)

# make V1 an ordered factor
data3$name <- factor(data3$name, levels = data3$name)

ggplot(data3, aes(x=name, y=value)) +
  geom_bar(stat="identity",position="identity") +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1)) +
  ggtitle("Maximum percentage decline by sector (Emilia Romagna)") 





#### BOXPLOTS ####
library(sf)
library(dplyr)
library(ggplot2)

df <- readRDS("resources/floodedUnits.rds")


#immaginando che il settore sia dato dalla colonna Sectors
# find the number of addetti for each sectors
df %>%
  group_by(irpet_n) %>% 
  summarise(n_addetti=sum(addetti_ul)) -> add_ul_tot


df %>%
  filter(!is.na(wh_m)) %>% 
  group_by(irpet_n) %>% 
  summarise(n_addetti_flooded=sum(addetti_ul)) -> add_ul_flooded


left_join(add_ul_tot,add_ul_flooded,by='irpet_n') %>% 
  mutate(percentage=n_addetti_flooded/n_addetti * 100,
         whole=100,
         group=rep(1:4,each=10)) -> add_ul_flooded_perc

#ggplot(df,aes(x=as.factor(irpet_n),y=wh_m))+
#  geom_boxplot()+
#  theme_bw()+
#  theme(axis.text = element_text(angle=60,hjust = 1))+
#  scale_x_discrete(labels=Sectors)+
#  ylab("water depth (mt)")+
#  xlab("") 

png("boxplot.png", width = 2000, height = 1200, res = 200)

ggbox<-left_join(df,add_ul_flooded_perc,by='irpet_n') %>% 
       ggplot(.,aes(x=as.factor(irpet_n),y=wh_m))+
       geom_boxplot(aes(fill=percentage),col='grey20',outlier.size = 0.8)+
       theme_bw()+
       scale_fill_continuous(
       low = 'blue',
       high = "red"
       )+
       labs(fill="% employees")+
       theme(axis.text.x = element_text(angle=60,hjust = 1))+
       ylab("water depth (m)")+
       xlab("") 

Sectors <- c("Mining and quarrying", "Food, beverages and tobacco", "Textiles, wearing apparel and leather", "Wood and products of wood and cork", "Paper and paper products", "Print and reproduction of rec. media", "Coke and refined petroleum products","Chemicals","Pharmaceuticals","Rubber and plastic products", "Other non-metallic mineral products", "Basic metals", "Fabricated metal products", "Computer, electronic and optical products", "Electrical equipment", "Machinery and equipment NEC", "Motor vehicles, trailers and semi-trailers", "Other transport equipment", "Furniture and other manufactured goods", 
             "Manufacturing NEC, repair and inst. of machinery", "Electricity, gas, steam and air condit. supply", "Water supply", "Water and waste management and treatment", "Construction", "Wholesale and retail trade, repair of motor veh.", "Transportation and storage", "Accomodation and food service activities", "Publishing, motion pic., video, sound and broadc.", "Telecommunications", "IT services and other information services",
             "Financial services and insurance", "Real estate", "Accounting and business services", "Research", "Other professional and scientific activities", "Other supporting service activities","Education", "Health", "Art and entertainment", "Other service activities")

ggbox<-ggbox+scale_x_discrete(labels=Sectors)
plot(ggbox)

dev.off()



#Other tentatives with boxplots
ggplot(df,aes(x=as.factor(irpet_n),y=wh_m))+
  geom_boxplot(outlier.size = 0.4)+
  theme_bw()+
  theme(axis.text = element_text(angle=60,hjust = 1))+
  ylab("water depth (mt)")+
  xlab("") 


left_join(df,add_ul_flooded_perc,by='Sectors') %>% 
  ggplot(.,aes(x=as.factor(Sectors),y=wh_m))+
  geom_boxplot(aes(fill=percentage),col='grey70')+
  scale_fill_continuous(
    low = 'blue',
    high = "red"
  )+
  labs(fill="")+
  theme(axis.text = element_text(angle=0,hjust = 0.5))


df %>% 
  mutate(n_addetti_flooded=case_when(
    !is.na(wh_m) ~ addetti_ul,
    TRUE ~ NA
  )) %>% 
  ggplot(.,aes(x=as.factor(Sectors),y=n_addetti_flooded))+
  geom_boxplot(col='grey70')+
  labs(fill="")+
  theme(axis.text = element_text(angle=0,hjust = 0.5))






### EXTRA FIGURE

library(ggplot2)
library(ggeasy)

data1 <- data.frame(
  weeks = c(1:108),
  val = capacityRegions[,8]
)

data2 <- data.frame(
  weeks = c(1:108),
  val = demandRegions[,8]
)

data3 <-  data.frame(
  weeks = c(1:108),
  val = producibleRegions[,8]
)

data4 <-  data.frame(
  weeks = c(1:108),
  val = sales[,8]
)

data5 <-  data.frame(
  weeks = c(1:108),
  val = expectationsRegions[,8]
)


theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + 
  geom_line(data = data1, aes(x = weeks, y = val, color = "capacity")) +
  geom_line(data = data2, aes(x = weeks, y = val, color = "demand")) +
  geom_line(data = data3, aes(x = weeks, y = val, color = "producible")) +
  geom_line(data = data4, aes(x = weeks, y = val, color = "sales")) +
  geom_line(data = data5, aes(x = weeks, y = val, color = "exp demand")) +
  xlab('') +
  ylab('') +
  theme_bw() +
  scale_colour_manual("", 
                      breaks = c("capacity", "demand","producible","sales","exp demand"),
                      values = c("brown", "darkgoldenrod3","green","black","blue")) +
  xlab("Weeks") +
  ylab("") +
  ggtitle("") +
  ggeasy::easy_center_title()




## FOR SOME ANALYSIS

which(producible[57,302:344]<capacity[57,302:344])

which(producible[80,302:344]>delivered[302:344,80])

length(which(producible[82,]==capacity[82,]))

length(which(producible[82,]>delivered[,82]))






