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

# 3. ID ������ �ߺ� Ȯ������
data2$ID[duplicated(data2$ID)] # 148���� cases�� �ߺ�
filter(data2, ID == "37339648") # ���̽� ����(filter)

# ��¥(date) ���� �����ϱ�
data2$date = as.Date(data2$date,"%Y-%m-%d")
summary(data2$date) # 2015�� 1�� 5�Ϻ��� 2018�� 12�� 5�ϱ����� data ����

class(data2$date)
table(is.na(data2$date))

# date ���� �������� ����
data_sort <- data2[order(data2[,'date'],decreasing = TRUE), ]

# ID�� ���� ������ �ߺ� ����
data_sort2 <- data_sort[-which(duplicated(data_sort$ID)),]
data <- data_sort2 # 834���� data

write.csv(data, file="data_red.csv", row.names = TRUE)

# ID ����
class(data$ID)
summary(data$ID)
table(is.na(data$ID))

# tube ����
class(data$tube)
summary(data$tube)
table(is.na(data$tube))
hist(data$tube)

# fix ����
class(data$fix)
summary(data$fix)
table(is.na(data$fix))

# CE ����
class(data$CE)
summary(data$CE)
table(is.na(data$CE))

# T1 ����
class(data$T1)
summary(data$T1)
table(is.na(data$T1))

# T3 ����
class(data$T3)
summary(data$T3)
table(is.na(data$T3))

# sex ����, F = 0, M = 1�� coding
class(data$sex)
qplot(data$sex)
table(data$sex)
table(is.na(data$sex))

# age ����
class(data$age)
summary(data$age)
table(is.na(data$age))
qplot(data$age)

# age_c ����
# 0 for 0 to 6 months, 0.5 for 6 to 12 months, and 1 for 1 to 2 years�� coding
class(data$age_c)
summary(data$age_c)
table(is.na(data$age_c))
qplot(data$age_c)

# wt ����
class(data$wt)
summary(data$wt)
table(is.na(data$wt))
hist(data$wt)

# ht ����, 8�� missing
class(data$ht)
summary(data$ht)
table(is.na(data$ht))
data %>% filter(is.na(ht))
hist(data$ht)

########################################### 
## !!! Data scaling & ����Ư���� ��ȯ ## ---- 
########################################### 

# 1. ������ ����(categorical data)/����(continuous data)/label �з�

data_cat <- data %>%
    select(sex)
data_num <- data %>% 
    select(age, ht, wt)
data_class <- data %>% 
    select(tube_depth_G)

# 2. ������ Ư���� Scaling

# 2-1. ǥ��ȭ(��� 0, ǥ������ 1) scaling 

StandardScale <- preProcess(data_num, method=c("center", "scale")) 
print(StandardScale) 
data_standard <- predict(StandardScale, data_num) 
head(data_standard) 

# 2-2. min-max scaling 

MinMaxScale <- preProcess(data_num, method=c("range")) 
print(MinMaxScale) 
data_minmax <- predict(MinMaxScale, data_num) 
head(data_minmax) 

# 3. ������ ���� �� ���� 

# cbind�� column �����͸� �߰����ش�. cbind �ܿ��� �������� ������� ���� �۾��� �����ϴ�.

data_etc <- data %>% 
    select(ID, fix, CE, T1, T3)

# min-max scaling�� ���ؼ��� p1, ǥ��ȭ scaling�� ���ؼ��� p2
data_p1 = cbind(data_etc, data_cat, data_minmax, data_class) 
data_p2 = cbind(data_etc, data_cat, data_standard, data_class) 

write.csv(data_p1, file="data_p1.csv", row.names = TRUE)
write.csv(data_p2, file="data_p2.csv", row.names = TRUE) 


  








