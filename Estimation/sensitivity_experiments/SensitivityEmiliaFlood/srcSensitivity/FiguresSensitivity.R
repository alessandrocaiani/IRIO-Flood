library(readr)
library(readxl)
## SENSITIVITY ANALYSIS ##
library(ggplot2)
library(dplyr)
library(latex2exp)

Losses <- readRDS("../resources/2024_11_19_00_07_47_EmiliaGDPLoss_Sensitivity.rds")
#Probably running with the sample from the datacenter the results are different
DATACENTER=TRUE
if (DATACENTER) {
  Sample <- readRDS("../resources/Sample_SensEmiliaFlood_datacenter.rds")
  #select columns that are not repeated
  Sample <- Sample[,c(1,2,3,5,6,25,28,29)]
}else{
  Sample <- readRDS("../resources/Sample_SensitivityEmiliaFlood.rds")
}


for (name_col in colnames(Losses)) {
  
  GDPLoss=Losses[,name_col]
  # OutputLoss=Losses[,3]
  # min=0.01
  # max=0.011
  
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
  
  
  # ggsave("../FIGURES/alphaShap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
  ggplot(gdp_summary, aes(x = last)) +
    geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
    geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
    labs(
      title = expression(alpha),
      x = "",
      y = ""
    ) +
    #ylim(min(GDPLoss), max(GDPLoss))+
    # ylim(min,max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  -> alpha# Increase the margin (top, right, bottom, left)
    # theme_minimal()
  
  
  
  
  # beta
  data <- data.frame(beta=Sample[,2],GDPloss = GDPLoss)
  gdp_summary <- data %>%
    group_by(beta) %>%
    summarize(median_gdp = median(GDPloss),
              q1_gdp = quantile(GDPloss, 0.25),
              q3_gdp = quantile(GDPloss, 0.75)) %>%
    ungroup()
  # ggsave("../FIGURES/betaShap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
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
    # ylim(min, max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) -> beta  # Increase the margin (top, right, bottom, left)
    # theme_minimal()
  
    
  # gamma1 
    data <- data.frame(gamma1=Sample[,3],GDPloss = GDPLoss)
    gdp_summary <- data %>%
      group_by(gamma1) %>%
      summarize(median_gdp = median(GDPloss),
                q1_gdp = quantile(GDPloss, 0.25),
                q3_gdp = quantile(GDPloss, 0.75)) %>%
      ungroup()
    
    # ggsave("../FIGURES/gamma1Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
    ggplot(gdp_summary, aes(x = gamma1)) +
      geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
      geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
      labs(
        title = expression(gamma[1]),
        x = "",
        y = ""
      ) +
      #ylim(min(GDPLoss), max(GDPLoss))+
      # ylim(min, max)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) -> gamma_1 # Increase the margin (top, right, bottom, left)
    # theme_minimal()  
  
  
    
    # gamma2 
    data <- data.frame(gamma2=Sample[,4],GDPloss = GDPLoss)
    gdp_summary <- data %>%
      group_by(gamma2) %>%
      summarize(median_gdp = median(GDPloss),
                q1_gdp = quantile(GDPloss, 0.25),
                q3_gdp = quantile(GDPloss, 0.75)) %>%
      ungroup()
    
    # ggsave("../FIGURES/gamma2Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
    ggplot(gdp_summary, aes(x = gamma2)) +
      geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
      geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
      labs(
        title = expression(gamma[2]),
        x = "",
        y = ""
      ) +
     # ylim(min(GDPLoss), max(GDPLoss))+
      # ylim(min, max)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  -> gamma_2# Increase the margin (top, right, bottom, left
    # theme_minimal()
    
    
    
  # gamma3 
  data <- data.frame(gamma3=Sample[,5],GDPloss = GDPLoss)
  gdp_summary <- data %>%
    group_by(gamma3) %>%
    summarize(median_gdp = median(GDPloss),
              q1_gdp = quantile(GDPloss, 0.25),
              q3_gdp = quantile(GDPloss, 0.75)) %>%
    ungroup()
  
  # ggsave("../FIGURES/gamma3Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
  ggplot(gdp_summary, aes(x = gamma3)) +
    geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
    geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
    labs(
      title = expression(gamma[3]),
      x = "",
      y = ""
    ) +
    #ylim(min(GDPLoss), max(GDPLoss))+
    # ylim(min, max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) -> gamma_3  # Increase the margin (top, right, bottom, left
  # theme_minimal()
  
  
  
  # gamma4 
  data <- data.frame(gamma4=Sample[,6],GDPloss = GDPLoss)
  gdp_summary <- data %>%
    group_by(gamma4) %>%
    summarize(median_gdp = median(GDPloss),
              q1_gdp = quantile(GDPloss, 0.25),
              q3_gdp = quantile(GDPloss, 0.75)) %>%
    ungroup()
  
  # ggsave("../FIGURES/gamma4Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
  ggplot(gdp_summary, aes(x = gamma4)) +
    geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
    geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
    labs(
      title = expression(gamma[4]),
      x = "",
      y = ""
    ) +
    #ylim(min(GDPLoss), max(GDPLoss))+
    # ylim(min, max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) -> gamma_4  # Increase the margin (top, right, bottom, left
  # theme_minimal()
  
  
  
  # gamma5 
  data <- data.frame(gamma5=Sample[,7],GDPloss = GDPLoss)
  gdp_summary <- data %>%
    group_by(gamma5) %>%
    summarize(median_gdp = median(GDPloss),
              q1_gdp = quantile(GDPloss, 0.25),
              q3_gdp = quantile(GDPloss, 0.75)) %>%
    ungroup()
  
  # ggsave("../FIGURES/gamma5Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
  ggplot(gdp_summary, aes(x = gamma5)) +
    geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
    geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
    labs(
      title = expression(gamma[5]),
      x = "",
      y = ""
    ) +
  #  ylim(min(GDPLoss), max(GDPLoss))+
    # ylim(min, max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) -> gamma_5 # Increase the margin (top, right, bottom, left
  # theme_minimal()
  
  
  
  # gamma6 
  data <- data.frame(gamma6=Sample[,8],GDPloss = GDPLoss)
  gdp_summary <- data %>%
    group_by(gamma6) %>%
    summarize(median_gdp = median(GDPloss),
              q1_gdp = quantile(GDPloss, 0.25),
              q3_gdp = quantile(GDPloss, 0.75)) %>%
    ungroup()
  
  # ggsave("../FIGURES/gamma6Shap2_smallINT.jpeg", width = 10, height = 10, units = "cm") #
  ggplot(gdp_summary, aes(x = gamma6)) +
    geom_ribbon(aes(ymin = q1_gdp, ymax = q3_gdp), fill = "lightblue", alpha = 0.5) +  # Light blue ribbons
    geom_line(aes(y = median_gdp), color = "red", size = 1.2) +  # Red line for median
    labs(
      title = expression(gamma[6]),
      x = "",
      y = ""
    ) +
    #ylim(min(GDPLoss), max(GDPLoss))+
    # ylim(min, max)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) -> gamma_6  # Increase the margin (top, right, bottom, left
  # theme_minimal()
  
  
  
  library(patchwork)
  
  
  
  (alpha | beta ) /
  (gamma_1 | gamma_2 ) /
  (gamma_3 | gamma_4 ) /
  (gamma_5 | gamma_6 ) 
  
  if(DATACENTER){
    ggsave(paste0("../results/panel_sensitivity_",name_col,"_datacenter.jpeg"), width = 20, height = 40, units = "cm")
  }else{
    ggsave(paste0("../results/panel_sensitivity_",name_col,".jpeg"), width = 20, height = 40, units = "cm")
  }
  

}




