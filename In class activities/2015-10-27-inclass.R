# Jingjing Yang
# 10-27-2015
# k-fold cross validation

k_fold1<- function(formular, mothod = lm, data = mtcars, predfun = predict, k = 10){
  mspe <- numeric(k)
  sets <- rep(1:k, each = nrow(data)/k, length.out = nrow(data))
  for (i in 1:k){
    For_Testing <- data[which(sets == i),]
    For_Training <- data[which(sets != i),]
    mod <- lm(mpg~hp + wt + am, data = For_Training)
    pred_vals <- predict(mod, newdata = For_Testing)
    MSPE <- mean((For_Testing[["mpg"]] - pred_vals)^2)
    mspe[i] <- MSPE
  }
  return(mean(mspe))
}
