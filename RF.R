setwd("C:/Users/jaege/Desktop/Tracheal tube depth")

# Loading required R packages

# tidyverse for easy data manipulation and visualization
# caret for easy machine learning workflow
# randomForest for computing random forest algorithm

library(tidyverse)
library(caret)
library(randomForest)

# Load the data
data <- read.csv("data_p1.csv", header=TRUE)
data <- data[ , c(2:11)]

# Inspect the data
sample_n(data, 3)

# Split the data into training and test set
set.seed(42)
training.samples <- data$tube_depth_G %>%
    createDataPartition(p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

# Computing random forest regression trees

# Here the prediction error is measured by the RMSE, which corresponds to the average difference between the observed known values of the outcome and the predicted value by the model. RMSE is computed as RMSE = mean((observeds - predicteds)^2) %>% sqrt(). The lower the RMSE, the better the model

# Fit the model on the training set

set.seed(42)
model <- train(
    tube_depth_G ~ age + sex + ht + wt, data = train, method = "rf",
    trControl = trainControl("cv", number = 5)
)

# Best tuning parameter mtry
model$bestTune

# Make predictions on the test data
predictions <- model %>% predict(test)
head(predictions)

# Compute the average prediction error RMSE
RMSE(predictions, test$tube_depth_G)

test$prediction <- predictions

test$upper_margin <- test$fix - (test$T1 - test$CE)
test$lower_margin <- test$fix + (test$CE - test$T3)

CI <- ifelse(test$prediction < test$lower_margin & test$prediction > test$upper_margin, 1, 0)
table(CI)

results <- data.frame(x = CI, y = predictions)
write.csv(results, file = "result_RF.csv")

library(Hmisc)
binconf(196, 248, alpha = 0.05) # RF, 0.7903226 0.7354233 0.836365


