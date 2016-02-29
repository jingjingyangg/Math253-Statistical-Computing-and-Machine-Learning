#Oct 6, 2015 in class 

require(ISLR)

logistic <- function(x){
  return(exp(x)/(1+exp(x)))
}

linear_combine <- function(data,coefs){
  result <- 0
  for (nm in names(coefs)){
    if (nm == "intercept"){
      result <- result + coefs[[nm]]
    } else if (nm %in% names(data)){
      result <- result + coefs[[nm]] * data[[nm]]
    } else {
      stop("Invalid variable in coefficient: ", nm)
    }
  } 
  return(result)
}

linear_combine(data=Default, coefs = c(intercept = 3, balance = 1, income = -2))

LL_logistic <- function(data,coefs, outcome){
  result <- log(1)
  linear_combins <- linear_combine(data, coefs)
  prob <- logistic(linear_combins)
  for (i in seq_along(prob)){
    if (outcome[i]){
      likelihood <- prob[i]
    } else {
      likelihood <- 1 - prob[i]
    }
    result <- result + log(likelihood)
  }
  return(result)
}

LL_logistic(data = Default, coefs = c(intercept = 1, income = -0.0001),outcome = Default$default == "Yes")

#Optimize
fun_to_minimize <- function(coefs){
  LL_logistic(data=Default, coefs = coefs, outcome = Default$default=="Yes")
}

best_coefs <- optim(c(intercept =1, income = -0.0001), fun_to_minimize)
glm(default == "Yes" ~ income, data = Default, family = "binomial")

#test
require(scoreActivity, quietly = TRUE )
score253(day = 10)
