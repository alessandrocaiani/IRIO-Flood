library(ggplot2)
library(latex2exp)

x <- readRDS("../results/Shapley_EmiliaFlood_2024-11-24_15_32_04_10007000.rds")

greek_names <- c(
    TeX( r'($\alpha$)' ),
    expression(beta),
    expression(gamma[1]),
    expression(gamma[2]),
    expression(gamma[3]),
    expression(gamma[4]),
    expression(gamma[5]),
    expression(gamma[6]),
    expression(rho["e"]),
    expression(rho["m"],
    expression(rho["i"]))
    )

df <- data.frame(
  param=factor(colnames(x$X),levels=c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11")),
  shapVal=x$Shapley$original,
  shapsd=x$Shapley$`std. error`
)

# write.csv(df, "out/Shapley_REV1_2024-09-23_10k.csv")
ggplot(df, aes(x=param, y=shapVal)) + 
  # geom_line() +
  geom_point(col='red')+
  geom_errorbar(aes(ymin=shapVal-shapsd, ymax=shapVal+shapsd), width=.2,
                position=position_dodge(0.05),col='red')+
#   coord_cartesian(ylim=c(0,1))+
    labs(title="Shapley values", x="", y="")+
    scale_x_discrete(labels = c(
        TeX( r'(\textit{$\alpha$})' ),
        TeX( r'($\beta$)' ),
        TeX( r'($\gamma_{1}$)' ),
        TeX( r'($\gamma_{2}$)' ),
        TeX( r'($\gamma_{3}$)' ),
        TeX( r'($\gamma_{4}$)' ),
        TeX( r'($\gamma_{5}$)' ),
        TeX( r'($\gamma_{6}$)' ),
        TeX( r'($\rho_{e}$)' ),
        TeX( r'($\rho_{m}$)' ),
        # Use raw strings, no need to escape backslashes.
# TeX(r"(\textit{Euler's identity} is $e^{i\pi} + 1 = 0$.)")
        TeX( r'($\rho_{i}$)' )
    ))+
  theme_bw()+
  theme(
    axis.text=element_text(size=24),
    title=element_text(size=20),    
  )

ggsave(
    "../results/Shapley_REV1_2024-11-24_15_32_04_10007000.png", 
    width = 35, height = 25, units = "cm",
    dpi=300
    )

