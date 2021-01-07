setwd("C:/Users/jaege/Desktop/Tracheal tube depth")

data <- read.csv("data_red.csv", header = TRUE)

library(tidyverse)
library(caret)

set.seed(42)
training.samples <- data$tube_depth_G %>%
    createDataPartition(p = 0.7, list = FALSE)
train <- data[training.samples, ]
test <- data[-training.samples, ]
# 834Έν, train 586Έν, test 248Έν

# age-based formula
test$age_formula <- ifelse(test$age_c == 0, 9,
                           ifelse(test$age_c == 0.5, 10,
                                  ifelse(test$age_c == 1, 11,
                                         test$age_c/2 + 12)))
summary(test$age_formula)

# height-based formula (using Morgan and Steward formula)
test$ht_formula <- test$ht/10 + 5
summary(test$ht_formula)

# ETT ID-based formula
test$tube_formula <- test$tube*3
summary(test$tube_formula)

test$upper_margin <- test$fix - (test$T1 - test$CE)
test$lower_margin <- test$fix + (test$CE - test$T3)

CI_age <- ifelse(test$age_formula < test$lower_margin & test$age_formula > test$upper_margin, 1, 0)
table(CI_age)

CI_ht <- ifelse(test$ht_formula < test$lower_margin & test$ht_formula > test$upper_margin, 1, 0)
table(CI_ht)

CI_tube <- ifelse(test$tube_formula < test$lower_margin & test$tube_formula > test$upper_margin, 1, 0)
table(CI_tube)

result <- data.frame(x = CI_age, y = CI_tube, z = CI_ht)
write.csv(result, file = "result_age_ht_tube.csv")

library(Hmisc)
binconf(166, 248, alpha = 0.05) # age, 0.6693548 0.6086118 0.7249313
binconf(110, 248, alpha = 0.05) # ht, 0.4435484 0.3830458 0.5057732
binconf(145, 248, alpha = 0.05) # wt, 0.5846774 0.5225116 0.64426
