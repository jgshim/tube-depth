# Elastic Net produces a regression model that is penalized with both the L1-norm and L2-norm. The consequence of this is to effectively shrink coefficients (like in ridge regression) and to set some coefficients to zero (as in LASSO).

setwd("C:/Users/jaege/Desktop/Dr_Ahn")

# The following R packages are required for this chapter:
# tidyverse for data manipulation and visualization
# caret for easy machine learning workflow
# glmnet for computing penalized regression
# Using caret package

library(tidyverse)
library(caret)
library(glmnet)

# Load the data
data <- read.csv("data_p1.csv", header=TRUE)
data <- data[ , c(2:11)]

# Inspect the data
sample_n(data, 3)

# Split the data into training and test set
set.seed(42)
training.samples <- data$tube_depth_G %>%
    createDataPartition(p = 0.7, list = FALSE)
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]

# 0. Setup a grid range of lambda values:
lambda <- 10^seq(-3, 3, length = 100)

# 1. Compute ridge regression:
# Build the model
set.seed(42)
ridge <- train(
    tube_depth_G ~ age + sex + ht + wt, data = train.data, method = "glmnet",
    trControl = trainControl("cv", number = 5),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)

# Make predictions
predictions <- ridge %>% predict(test.data)

# Model prediction performance
data.frame(
    RMSE = RMSE(predictions, test.data$tube_depth_G),
    Rsquare = R2(predictions, test.data$tube_depth_G)
)

# 2. Compute lasso regression:
# Build the model
set.seed(42)
lasso <- train(
    tube_depth_G ~ age + sex + ht + wt, data = train.data, method = "glmnet",
    trControl = trainControl("cv", number = 5),
    tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)

# Make predictions
predictions <- lasso %>% predict(test.data)

# Model prediction performance
data.frame(
    RMSE = RMSE(predictions, test.data$tube_depth_G),
    Rsquare = R2(predictions, test.data$tube_depth_G))

# 3. Elastic net regression:
# Build the model
set.seed(42)
elastic <- train(
    tube_depth_G ~ age + sex + ht + wt, data = train.data, method = "glmnet",
    trControl = trainControl("cv", number = 5),
    tuneLength = 10
)

# Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)

# Make predictions
predictions <- elastic %>% predict(test.data)

# Model prediction performance
data.frame(
    RMSE = RMSE(predictions, test.data$tube_depth_G),
    Rsquare = R2(predictions, test.data$tube_depth_G)
)

# 4. Comparing models performance:
# The performance of the different models - ridge, lasso and elastic net - can be easily compared using caret. The best model is defined as the one that minimizes the prediction error.

models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")

result <- data.frame(x = predictions, y = test.data$tube_depth_G)

test.data$upper_margin <- test.data$fix - (test.data$T1 - test.data$CE)
test.data$lower_margin <- test.data$fix + (test.data$CE - test.data$T3)

test.data$prediction <- predictions

CI <- ifelse(test.data$prediction < test.data$lower_margin & test.data$prediction > test.data$upper_margin, 1, 0)
table(CI)

results <- data.frame(x = CI, y = predictions)
write.csv(results, file = "result_ElasticNet.csv")

library(Hmisc)
binconf(193, 248, alpha = 0.05) # ElasticNet, 0.7741935 0.7182031 0.8218192



