#Jingjing Yang
# 09-10-2015

#Task1
library(ISLR)
library(magrittr)
library(dplyr)
data(College, package="ISLR")

#Task2
College <- College %>% mutate(Yield=Enroll/Accept)

#Task3
all_indeces <- c(1:777)
train_indeces <- sample(all_indeces,200)
test_indeces <- setdiff(all_indeces,train_indeces)
Train_data <- College[train_indeces,]
Test_data <- College[test_indeces,]

#Task4
Yield_mod1 <- lm(Yield~Top10perc + Outstate + Expend, data = Train_data)

#Task5
Y_train <- Train_data$Yield
fhat_train <- predict(Yield_mod1, newdata = Train_data)
MSE_train <- mean((Y_train - fhat_train)^2)

#Task6
Y_test <- Test_data$Yield
fhat_test <- predict(Yield_mod1, newdata = Test_data)
MSE_test <- mean((Y_test - fhat_test)^2)

#test
require(scoreActivity, quietly = TRUE )
score253(day = 3)
