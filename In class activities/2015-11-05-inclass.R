# Build my own cubic spline fitter
# Jingjing Yang

library(ISLR)
data(Wage)

#the knots
my_knots <- function(x,k){
  quantile(x,probs = (1:k)/(k+1))
}

#the basis set

knots <- my_knots(Wage$age,k = 5)

spline_model_matrix <- function(x, knot_locations){
  MM <- cbind(1,x,x^2,x^3)
  for (k in knot_locations){
    MM <- cbind(MM, ifelse(x<k,0,(x-k)^3))
  }
  return(MM)
}

MM <- spline_matrix(Wage$age, knots)

# finding the best linear combination
spline_coefs <- coef(lm(Wage$age ~ MM, data = Wage))

fit_spline <- function(formula, k =2, data = parent.frame()){
  y <- eval(formula[[2]], envir = data)
  x <- eval(formula[[3]], envir = data)
  knot_locations <- my_knots(x,k)
  MM <- spline_model_matrix(x, knot_locations)
  mod <- lm(y ~ MM - 1)
  df <- nrow(data)- ncol(MM)
  rse <- sqrt(sum(resid(mod)^2)/df)
  res <- list(coef = coef(mod), knots = knot_locations, cov = vcov(mod), rse=rse, rdf=df)
  class(res) <- "my_spline"
  
  return(res)
}

# The predict function
predict.my_spline <- function(mod, newx, level = 0.95, intervals = c("none", "confidence", "prediction")){
  intervals <- match.arg(intervals)
  MM <- spline_model_matrix(newx,mod$knots)
  vals <- MM %*% mod$coef
  se <- sqrt(rowSums(MM %*% mod$cov * MM))

  if (intervals == "none") return(vals)
  else if (intervals == "confidence"){
    res <- data.frame(vals=vals, lower=vals-2*se, upper=vals+2*se)
    return(res)
  } 
  else if (intervals == "prediction"){
    pred_se <- sqrt(se^2+mod$rse^2)
    res <- data.frame(vals = vals, lower = vals-2*pred_se, upper = vals+2*pred_se)
    return(res)
  }
}


plot(Wage$age, Wage$wage, xlim = c(0,100), ylim = c(-400, 400))

mod <- fit_spline(wage~age, data = Wage)

#find the prediction values
spline_pred <- predict.my_spline(mod,20:80)
lines(20:80, spline_pred, col = "blue", lwd = 2)

# find the confidence intervals
spline_pred_conf <- predict.my_spline(mod, 20:80, intervals = "confidence")
lines(20:80, spline_pred_conf$upper, col="red", lwd=2)
lines(20:80, spline_pred_conf$lower, col="red", lwd=2)

# find the prediction intervals
spline_pred_pred <- predict.my_spline(mod, 20:80, intervals = "prediction")
lines(20:80, spline_pred_pred$upper, col="green", lwd=2)
lines(20:80, spline_pred_pred$lower, col="green", lwd=2)
