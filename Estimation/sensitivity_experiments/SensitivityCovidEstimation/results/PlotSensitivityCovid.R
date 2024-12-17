library(readr)
library(readxl)


## SENSITIVITY ANALYSIS ##
library(ggplot2)
library(dplyr)
library(latex2exp)
Losses <- readRDS("GDPLoss_Sensitivity.rds")
Sample <- readRDS("../resources/Sample_SensitivityCovid.rds")
GDPLoss=Losses[,1]
OutputLoss=Losses[,2]
min=0.01
max=0.011


# alpha
#discretize: divide the range into 10 equally sized intervals
alpha <- data.frame(Sample[,1])
alpha_disc <- cut(unlist(alpha), breaks = 10, labels = FALSE)
int<-levels(cut(unlist(alpha), breaks = 10))
last <- as.numeric(gsub(".*,([^]]*)\\]", "\\1", int))

data <- data.frame(alpha_disc,GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(alpha_disc) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()
gdp_summary$last<-last


ggsave("alphaShap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = last)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(alpha),
    x = "",
    y = ""
  ) +
  #ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min,max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left)
  theme_minimal()




# beta
data <- data.frame(beta=Sample[,2],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(beta) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()
ggsave("betaShap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = beta)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    #title = "Median GDP Loss and Quartile Ranges for Parameter Beta",
    title = expression(beta),
    #x = expression(beta),
    x = "",
    y = ""
  ) +
  #lim(min(GDPLoss), max(GDPLoss))+
  ylim(min, max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left)
  theme_minimal()

  
# gamma1 
  data <- data.frame(gamma1=Sample[,3],GDPloss = GDPLoss)
  gdp_summary <- data %>%
    group_by(gamma1) %>%
    summarize(median_gdp = median(GDPloss),
              q1_gdp = quantile(GDPloss, 0.25),
              q3_gdp = quantile(GDPloss, 0.75)) %>%
    ungroup()
  
  ggsave("gamma1Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
  ggplot(gdp_summary, aes(x = gamma1)) +
    geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
    geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
    labs(
      title = expression(gamma[1]),
      x = "",
      y = ""
    ) +
    #ylim(min(GDPLoss), max(GDPLoss))+
    ylim(min, max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left)
  theme_minimal()  


  
  # gamma2 
  data <- data.frame(gamma2=Sample[,4],GDPloss = GDPLoss)
  gdp_summary <- data %>%
    group_by(gamma2) %>%
    summarize(median_gdp = median(GDPloss),
              q1_gdp = quantile(GDPloss, 0.25),
              q3_gdp = quantile(GDPloss, 0.75)) %>%
    ungroup()
  
  ggsave("gamma2Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
  ggplot(gdp_summary, aes(x = gamma2)) +
    geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
    geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
    labs(
      title = expression(gamma[2]),
      x = "",
      y = ""
    ) +
   # ylim(min(GDPLoss), max(GDPLoss))+
    ylim(min, max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
  theme_minimal()
  
  
  
# gamma3 
data <- data.frame(gamma3=Sample[,5],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(gamma3) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()

ggsave("gamma3Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = gamma3)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(gamma[3]),
    x = "",
    y = ""
  ) +
  #ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min, max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
theme_minimal()



# gamma4 
data <- data.frame(gamma4=Sample[,6],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(gamma4) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()

ggsave("gamma4Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = gamma4)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(gamma[4]),
    x = "",
    y = ""
  ) +
  #ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min, max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
theme_minimal()



# gamma5 
data <- data.frame(gamma5=Sample[,7],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(gamma5) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()

ggsave("gamma5Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = gamma5)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(gamma[5]),
    x = "",
    y = ""
  ) +
#  ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min, max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
theme_minimal()



# gamma6 
data <- data.frame(gamma6=Sample[,8],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(gamma6) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()

ggsave("gamma6Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = gamma6)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(gamma[6]),
    x = "",
    y = ""
  ) +
  #ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min, max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
theme_minimal()


# rho_e 
data <- data.frame(rhoe=Sample[,9],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(rhoe) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()

ggsave("rhoeShap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = rhoe)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(rho[e]),
    x = "",
    y = ""
  ) +
  #ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min, 0.092)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
theme_minimal()



# rho_m 
data <- data.frame(rhom=Sample[,10],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(rhom) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()

ggsave("rhomShap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = rhom)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(rho[m]),
    x = "",
    y = ""
  ) +
  #ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min, max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
theme_minimal()



# rho_i
data <- data.frame(rhoi=Sample[,11],GDPloss = GDPLoss)
gdp_summary <- data %>%
  group_by(rhoi) %>%
  summarize(median_gdp = median(GDPloss),
            q1_gdp = quantile(GDPloss, 0.25),
            q3_gdp = quantile(GDPloss, 0.75)) %>%
  ungroup()

ggsave("rhoiShap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
ggplot(gdp_summary, aes(x = rhoi)) +
  geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
  geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
  labs(
    title = expression(rho[i]),
    x = "",
    y = ""
  ) +
  #ylim(min(GDPLoss), max(GDPLoss))+
  ylim(min, max)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase the margin (top, right, bottom, left
theme_minimal()





