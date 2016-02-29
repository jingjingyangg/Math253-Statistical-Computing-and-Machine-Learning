# Jingjing Yang
# Sept. 29, 2015 in class

#Task1
x <- runif(10000,20,70)
y <- 5+3*x + 2*rnorm(10000)
My_data <- data.frame(x, y)
plot(My_data)

LL_line <- function(line){
  m <- line[1]
  b <- line[2]
  sigma <- line[3]
  sum(log(dnorm(y - (m * x + b), sd = sigma)))
}

A <- LL_line(c(3,5,2))
B <- LL_line(c(4,1,10))

optim(c(3,5,2), LL_line, control = list(fnscale = -1))

#Task2
load(url("http://tiny.cc/dcf/Taxi_trips.rda"))
dist <- Taxi_trips$trip_distance
fare <- Taxi_trips$fare_amount

taxi <- function(trip){
  base_fare <- trip[1]
  per_mile <- trip[2]
  params <- trip[3]
  sum(log(dexp(fare - base_fare - per_mile*dist, rate=params) + 0.00002))
}

taxi(c(4,2,1/5))

max_values <- optim(c(2, 2, 1/5), taxi,control = list(fnscale = -1))
plot(fare~dist)
abline(max_values$par[1],max_values$par[2], col = "red")
stand_still <- mean((fare - dist * max_values$par[2] - max_values$par[1])/ fare)

#test
require(scoreActivity, quietly = TRUE )
score253(day = 8)
