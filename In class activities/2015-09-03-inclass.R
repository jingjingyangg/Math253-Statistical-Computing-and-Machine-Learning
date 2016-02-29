library(mosaicData)

# Task 2
task2 <- paste("Today is", date())


# Task 3
task3a <- names(Galton)
task3b <- nrow(Galton)
task3c <- mean(Galton$height)


# Task 4
task4 <- matrix(1:6,2,3, byrow = TRUE)


# Task 5
task5x <- runif(1000)
task5y <- runif(1000)
task5pi <- ((sum(ifelse((sqrt(task5x ^ 2 + task5y ^ 2))<=1,1,0)))/1000) * 4
# another way to calculate task5pi: mean((sqrt(task5x ^ 2 + task5y ^ 2))<=1)*4

#test
require(scoreActivity, quietly = TRUE )
score253(day = 1)