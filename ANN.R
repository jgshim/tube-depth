setwd("C:/Users/jaege/Desktop/Dr_Ahn")

# Library 불러오기 ----

library(keras)
library(caret)

########## Regression ########### 

# total 834명, train 명, test 명 

# 1. 데이터 불러오기 
data <- read.csv("data_p1.csv", header=TRUE)
data <- data[ , c(2:11)] 
head(data) 
str(data)

# Split the data into training and test set
set.seed(42)
training.samples <- data$tube_depth_G %>%
  createDataPartition(p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

train_data <- as.matrix(train[6:9], ncol = 4)
train_targets <- as.matrix(train[10], ncol = 1)
test_data <- as.matrix(test[6:9], ncol = 4)
test_targets <- as.matrix(test[10], ncol = 1)

# Building our network
# Because we will need to instantiate the same model multiple times,
# we use a function to construct it.

build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 30, activation = "relu", 
                input_shape = dim(train_data)[[2]]) %>% 
    layer_dense(units = 20, activation = "relu") %>% 
    layer_dense(units = 1) 
  
  model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mae")
  )
}

# Validating our approach using K-fold validation

k <- 5
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
num_epochs <- 100
all_scores <- c()

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  # Build the Keras model (already compiled)
  model <- build_model()
  
  # Train the model (in silent mode, verbose=0)
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 1, verbose = 0)
  
  # Evaluate the model on the validation data
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results$mae)
} 

all_scores

mean(all_scores)

# Some memory clean-up
k_clear_session()

num_epochs <- 500
all_mae_histories <- NULL
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  # Build the Keras model (already compiled)
  model <- build_model()
  
  # Train the model (in silent mode, verbose=0)
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

library(ggplot2)
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_line()
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_smooth()

# 최종 모델 훈련하기
model <- build_model()
model %>% fit(train_data, train_targets,
              epochs = 100, batch_size = 20, verbose = 0)
results <- model %>% evaluate(test_data, test_targets)
results

model %>% predict(test_data) -> pred_test
result <- data.frame(x = pred_test, y = test_targets)

test$prediction <- result$x

test$upper_margin <- test$fix - (test$T1 - test$CE)
test$lower_margin <- test$fix + (test$CE - test$T3)

CI <- ifelse(test$prediction < test$lower_margin & test$prediction > test$upper_margin, 1, 0)
table(CI)

results <- data.frame(x = CI, y = test$prediction)
write.csv(results, file = "result_ANN.csv")

library(Hmisc)
binconf(195, 248, alpha = 0.05) # ANN, 0.7862903 0.7311077 0.8327391



