setwd("C:/Users/jaege/Desktop/Tracheal tube depth")

data <- read.csv("data_red.csv", header = TRUE)

library(tidyverse)
library(caret)

set.seed(42)
training.samples <- data$tube_depth_G %>%
  createDataPartition(p = 0.7, list = FALSE)
train <- data[training.samples, ]
test <- data[-training.samples, ]

data$group[training.samples]=0
data$group[-training.samples]=1

# 834명, train 586명, test 248명

# 1. age

table(data$age)
table(is.na(data$age))
hist(data$age)
summary(data$age)

table(data$age_c)
table(is.na(data$age_c))
hist(data$age_c)
summary(data$age_c)

table(train$age_c)
table(is.na(train$age_c))
hist(train$age_c)
summary(train$age_c)

table(test$age_c)
table(is.na(test$age_c))
hist(test$age_c)
summary(test$age_c)

# 2. ht

table(is.na(data$ht))
hist(data$ht)
summary(data$ht)
shapiro.test(data$ht)

table(is.na(data$ht))
hist(data$ht)
summary(data$ht)

table(is.na(train$ht))
hist(train$ht)
summary(train$ht)
shapiro.test(train$ht)

table(is.na(test$ht))
hist(test$ht)
summary(test$ht)
shapiro.test(test$ht)

# 3. wt

table(is.na(data$wt))
hist(data$wt)
summary(data$wt)
shapiro.test(test$wt)

table(is.na(train$wt))
hist(train$wt)
summary(train$wt)
shapiro.test(train$wt)

table(is.na(test$wt))
hist(test$wt)
summary(test$wt)
shapiro.test(test$wt)

# 4. sex
# 0. 여자 1. 남자

table(data$sex)
qplot(data$sex, bins = 20)
df_sex <- data %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

table(train$sex)
qplot(train$sex, bins = 20)
df_sex <- train %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

table(test$sex)
qplot(test$sex, bins = 20)
df_sex<- test %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

sex__cross <- xtabs(~ sex + group, data = data)
sex__cross
chisq.test(sex__cross)

# 5. tube size

table(data$tube)
table(is.na(data$tube))
hist(data$tube)
summary(data$tube)
shapiro.test(test$tube)

table(train$tube)
table(is.na(train$tube))
hist(train$tube)
summary(train$tube)
shapiro.test(train$tube)

table(test$tube)
table(is.na(test$tube))
hist(test$tube)
summary(test$tube)
shapiro.test(test$tube)

# 6. Carina-T1 distance, cm

table(is.na(data$T1))
hist(data$T1)
summary(data$T1)
shapiro.test(test$T1)

table(is.na(train$T1))
hist(train$T1)
summary(train$T1)
shapiro.test(train$T1)

table(is.na(test$T1))
hist(test$T1)
summary(test$T1)
shapiro.test(test$T1)

# 7. Carina-T3 distance, cm

table(is.na(data$T3))
hist(data$T3)
summary(data$T3)
shapiro.test(test$T3)

table(is.na(train$T3))
hist(train$T3)
summary(train$T3)
shapiro.test(train$T3)

table(is.na(test$T3))
hist(test$T3)
summary(test$T3)
shapiro.test(test$T3)

# 8. Carina-ETT tip distance, cm

table(is.na(data$CE))
hist(data$CE)
summary(data$CE)
shapiro.test(test$CE)

table(is.na(train$CE))
hist(train$CE)
summary(train$CE)
shapiro.test(train$CE)

table(is.na(test$CE))
hist(test$CE)
summary(test$CE)
shapiro.test(test$CE)

