
setwd("D:/Study/Cardiovescular_thoracic_anesthesia/tracheal_tube_length")

library(ggplot2)

data <- read.csv("tube_depth_1125.csv", header=TRUE)
data <- data[ , c(1:4)]

ggplot(data,                ###The data frame to use.
       aes(x = model,
           y = P)) +
    geom_errorbar(aes(ymin = lower,
                      ymax= upper),
                  width= 0.05, 
                  size  = 0.5) +
    geom_point(shape = 15, 
               size  = 2) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(limits=c("RF", "ElasticNet", "SVM", "ANN", "age", "ID", "ht")) + 
    theme_bw() +
    theme(axis.title   =element_text(face  = "bold")) +
    ylab("Proportions")
    
