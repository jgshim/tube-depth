setwd("D:/Study/Cardiovescular_thoracic_anesthesia/tracheal_tube_length")

data_raw <- read.csv("raw_data.csv", header = TRUE)
dim(data_raw) # 982 cases

library(tidyverse)
library(DMwR)
library(caret)

data2 <- select(data_raw, ID, date, tube, fix, CE, T1, T3, sex, age, age_c, wt, ht, tube_depth_G)

data_knn <- select(data_raw, tube, fix, CE, T1, T3, sex, age, wt, ht)
knnOutput <- knnImputation(data_knn, k = 10, scale = T, meth = "median", distData = NULL)

data2$ht <- knnOutput$ht
data_knn <- data2

write.csv(data_knn, file="data_knn.csv", row.names = TRUE)

# 3. ID 변수가 중복 확인하자
data2$ID[duplicated(data2$ID)] # 148개의 cases가 중복
filter(data2, ID == "37339648") # 케이스 추출(filter)

# 날짜(date) 변수 설정하기
data2$date = as.Date(data2$date,"%Y-%m-%d")
summary(data2$date) # 2015년 1월 5일부터 2018년 12월 5일까지의 data 추출

class(data2$date)
table(is.na(data2$date))

# date 변수 기준으로 정렬
data_sort <- data2[order(data2[,'date'],decreasing = TRUE), ]

# ID가 같은 변수들 중복 제거
data_sort2 <- data_sort[-which(duplicated(data_sort$ID)),]
data <- data_sort2 # 834명의 data

write.csv(data, file="data_red.csv", row.names = TRUE)

# ID 변수
class(data$ID)
summary(data$ID)
table(is.na(data$ID))

# tube 변수
class(data$tube)
summary(data$tube)
table(is.na(data$tube))
hist(data$tube)

# fix 변수
class(data$fix)
summary(data$fix)
table(is.na(data$fix))

# CE 변수
class(data$CE)
summary(data$CE)
table(is.na(data$CE))

# T1 변수
class(data$T1)
summary(data$T1)
table(is.na(data$T1))

# T3 변수
class(data$T3)
summary(data$T3)
table(is.na(data$T3))

# sex 변수, F = 0, M = 1로 coding
class(data$sex)
qplot(data$sex)
table(data$sex)
table(is.na(data$sex))

# age 변수
class(data$age)
summary(data$age)
table(is.na(data$age))
qplot(data$age)

# age_c 변수
# 0 for 0 to 6 months, 0.5 for 6 to 12 months, and 1 for 1 to 2 years로 coding
class(data$age_c)
summary(data$age_c)
table(is.na(data$age_c))
qplot(data$age_c)

# wt 변수
class(data$wt)
summary(data$wt)
table(is.na(data$wt))
hist(data$wt)

# ht 변수, 8명 missing
class(data$ht)
summary(data$ht)
table(is.na(data$ht))
data %>% filter(is.na(ht))
hist(data$ht)

########################################### 
## !!! Data scaling & 범주특성의 변환 ## ---- 
########################################### 

# 1. 데이터 범주(categorical data)/연속(continuous data)/label 분류

data_cat <- data %>%
    select(sex)
data_num <- data %>% 
    select(age, ht, wt)
data_class <- data %>% 
    select(tube_depth_G)

# 2. 연속형 특성의 Scaling

# 2-1. 표준화(평균 0, 표준편차 1) scaling 

StandardScale <- preProcess(data_num, method=c("center", "scale")) 
print(StandardScale) 
data_standard <- predict(StandardScale, data_num) 
head(data_standard) 

# 2-2. min-max scaling 

MinMaxScale <- preProcess(data_num, method=c("range")) 
print(MinMaxScale) 
data_minmax <- predict(MinMaxScale, data_num) 
head(data_minmax) 

# 3. 데이터 통합 및 저장 

# cbind로 column 데이터를 추가해준다. cbind 외에도 여러가지 방법으로 같은 작업이 가능하다.

data_etc <- data %>% 
    select(ID, fix, CE, T1, T3)

# min-max scaling에 대해서는 p1, 표준화 scaling에 대해서는 p2
data_p1 = cbind(data_etc, data_cat, data_minmax, data_class) 
data_p2 = cbind(data_etc, data_cat, data_standard, data_class) 

write.csv(data_p1, file="data_p1.csv", row.names = TRUE)
write.csv(data_p2, file="data_p2.csv", row.names = TRUE) 


  









