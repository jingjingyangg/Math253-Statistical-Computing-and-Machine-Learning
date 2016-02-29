#2015-11-24 in class activity
#Jingjing Yang
#Support Vector Machine

#Generate some simulated data
##1
set.seed(101)
n_cases <- 100
n_predictors <- 2
##2
X <- matrix(data = rnorm(n = 200), nrow = n_cases, ncol = n_predictors)
##3
y <- rep(c(-1,1), length.out = n_cases)
##4
Movement <- matrix(y, ncol = n_predictors, nrow = length(y), byrow = FALSE)
##5
offset <- 1.4
D <- data.frame(x = X + offset*Movement, y = as.factor(y))

plot(x.1 ~ x.2, data = D, col = D$y)

#==========================================================
#Fit the support vector machine
library(e1071)
classifier_1 <- svm(y~., data = D, kernel = "linear", cost = 1, scale = FALSE)

#==========================================================
#Performance and visualization
vals <- predict(classifier_1)
table(D$y, vals)
plot(classifier_1, x.1~x.2, data = D)

#========================================================
#lower the cost
classifier_2 <- svm(y~., data = D, kernel = "linear", cost = 0.0000001, scale = FALSE)
vals_2 <- predict(classifier_2)
table(D$y, vals_2)
plot(classifier_2, x.1~x.2, data = D)

#=====================================================
library(scoreActivity)
Test_Statements <- score_set(
  check_exists(n_cases),
  check_exists(n_predictors),
  check(TRUE, "package:e1071" %in% search(), pts = 8),
  leave_out_names = c("wage", "age", "sex", "mosaicData")
)
print_test_results(Test_Statements)
