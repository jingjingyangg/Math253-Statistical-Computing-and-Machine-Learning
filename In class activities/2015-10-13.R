# Jingjing Yang
# 10-13-2015 LDA & QDA

library(magrittr)

#Generate simulated data
n_cases <- 10000
red_mean <- c(1,0)
green_mean <- c(0,-1)
blue_mean <- c(-1,1)
covar_1 <- matrix(c(3, -1.7,-1.7, 1), nrow = 2, ncol = 2, byrow = TRUE)
covar_2 <- matrix(c(2, 1.5,1.5, 3), nrow = 2, ncol = 2, byrow = TRUE)
one <- matrix(c(rnorm(20000)),nrow = 10000, ncol = 2)
two <- matrix(c(rnorm(20000)),nrow = 10000, ncol = 2)
three <- matrix(c(rnorm(20000)),nrow = 10000, ncol = 2)

red <- one %*% chol(covar_1) 
green <- two %*% chol(covar_1)
blue <- three %*% chol(covar_2)

red <- matrix(outer(red, red_mean, "+"), nrow = 10000, ncol = 2, byrow = TRUE)
green <- matrix(outer(green, green_mean, "+"), nrow = 10000, ncol = 2, byrow = TRUE)
blue <- matrix(outer(blue, blue_mean, "+"), nrow = 10000, ncol = 2, byrow = TRUE)

Red <- data.frame(red[,1],red[,2], "red", stringsAsFactors = FALSE)
colnames(Red)<- (c("x","y", "class"))

Blue <- data.frame(blue[,1],blue[,2], "blue", stringsAsFactors = FALSE)
colnames(Blue)<- (c("x","y", "class"))

Green <- data.frame(green[,1],green[,2], "green", stringsAsFactors = FALSE)
colnames(Green)<- (c("x","y", "class"))

#LDA & QDA
Sim_one <- rbind(Red, Green)
Sim_two <- rbind(Red, Blue)

##LDA on Sim_one
mod_LDA_one <- MASS::lda(class ~ x + y, data = Sim_one)
test_LDA_one <- predict(mod_LDA_one, newdata = Sim_one)
table(Sim_one$class,test_LDA_one$class)

#QDA on Sim_one
mod_QDA_one <- MASS::qda(class ~ x + y, data = Sim_one)
test_QDA_one <- predict(mod_QDA_one, newdata = Sim_one)
table(Sim_one$class,test_QDA_one$class)

#For Sim_one, QDA shows better performance


##LDA on Sim_two
mod_LDA_two <- MASS::lda(class ~ x + y, data = Sim_two)
test_LDA_two <- predict(mod_LDA_two, newdata = Sim_two)
table(Sim_two$class,test_LDA_two$class)

#QDA on Sim_two
mod_QDA_two <- MASS::qda(class ~ x + y, data = Sim_two)
test_QDA_two <- predict(mod_QDA_two, newdata = Sim_two)
table(Sim_two$class,test_QDA_two$class)

#For Sim_two, LDA shows slightly better performance

#test
require(scoreActivity, quietly = TRUE )
score253(day = 12)
