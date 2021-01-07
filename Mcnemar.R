setwd("C:/Users/jaege/Desktop/Tracheal tube depth")

library(tidyverse)

data <- read.csv("Mcnemar.csv", header=TRUE)

RF_SVM <- select(data, RF, SVM)
tab <- xtabs(data = RF_SVM)
mc.result <- mcnemar.test(tab)
print(mc.result$p.value)

RF_ElasticNet <- select(data, RF, ElasticNet)
tab <- xtabs(data = RF_ElasticNet)
mc.result <- mcnemar.test(tab)
print(mc.result$p.value)

RF_ANN <- select(data, RF, ANN)
tab <- xtabs(data = RF_ANN)
mc.result <- mcnemar.test(tab)
print(mc.result$p.value)

RF_age <- select(data, RF, age)
tab <- xtabs(data = RF_age)
mc.result <- mcnemar.test(tab)
print(mc.result$p.value)

RF_ht <- select(data, RF, ht)
tab <- xtabs(data = RF_ht)
mc.result <- mcnemar.test(tab)
print(mc.result$p.value)

RF_tube <- select(data, RF, tube)
tab <- xtabs(data = RF_tube)
mc.result <- mcnemar.test(tab)
print(mc.result$p.value)

