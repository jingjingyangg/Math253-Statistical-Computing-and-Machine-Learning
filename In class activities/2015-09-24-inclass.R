# Jingjing Yang
# Sept 24, 2015

#Task1
vals <- rexp(10, 1/100)
test200 <- dexp(vals, 1/200)
sum(log(test200))

LL_exp <- function(rate){
  test <- dexp(vals, rate)
  sum(log(test))
}

rates <- runif(100,1/200, 1/50)
results <- sapply(rates, LL_exp)
plot(results ~ 1/rates)


#Task2
exp_results <- optimize(LL_exp, lower = 1/200, upper = 1/50, maximum = TRUE)
exp_results

#Task3
x <- runif(100,20,70)
y <- 5+3*x + 2*rnorm(100)
My_data <- data.frame(x, y)
plot(My_data)

LL_line <- function(m,b,sigma){
  sum(log(dnorm(y-(m*x + b), sd = sigma)))
}

A <- LL_line(3,5,2)
B <- LL_line(4,1,10)

optim(c(1,1,1), LL_line, control = list(fnscale = -1))

#test
require(scoreActivity, quietly = TRUE )
score253(day = 7)
