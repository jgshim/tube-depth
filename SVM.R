setwd("C:/Users/jaege/Desktop/Dr_Ahn")

# Loading required R packages

# tidyverse for easy data manipulation and visualization
# caret for easy machine learning workflow
# randomForest for computing random forest algorithm

library(tidyverse)
library(caret)
library(e1071)
library(MLmetrics)

# Load the data
data <- read.csv("data_p1.csv", header=TRUE)
data <- data[ , c(2:11)]

# Inspect the data
sample_n(data, 3)

# Split the data into training and test set
set.seed(42)
training.samples <- data$tube_depth_G %>%
    createDataPartition(p = 0.7, list = FALSE)
train <- data[training.samples, ]
test <- data[-training.samples, ]

# Fitting the model and predicting test data

# Train and test data are ready. Now, we can define the svm model with default parameters and fit it with train data. Here, we can change the kernel type into 'linear', 'polynomial', and 'sigmoid' for training and predicting. The default is a 'radial' kernel.

model_reg = svm(tube_depth_G ~ age + sex + ht + wt, data = train)
print(model_reg)

# Next, we'll predict the test data and plot the results to compare visually.

pred = predict(model_reg, test)
 
x = 1:length(test$tube_depth_G)
plot(x, test$tube_depth_G, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

# Accuracy checking

# Finally, we'll check the prediction accuracy with the MSE, MAE, RMSE, and R-squared metrics.

mse = MSE(test$tube_depth_G, pred)
mae = MAE(test$tube_depth_G, pred)
rmse = RMSE(test$tube_depth_G, pred)
r2 = R2(test$tube_depth_G, pred, form = "traditional")
 
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
     "RMSE:", rmse, "\n", "R-squared:", r2)

test$upper_margin <- test$fix - (test$T1 - test$CE)
test$lower_margin <- test$fix + (test$CE - test$T3)

test$pred <- pred
CI <- ifelse(test$pred < test$lower_margin & test$pred > test$upper_margin, 1, 0)
table(CI)

result <- data.frame(x = CI, y = pred)
write.csv(result, file = "result_SVM.csv")

library(Hmisc)
binconf(191, 248, alpha = 0.05) # SVM, 0.7701613 0.7139151 0.8181657

