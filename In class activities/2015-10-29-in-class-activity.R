# Jingjing Yang
# 10-29-2015
# Comparing Ordinary Least Squares(OLS) to ridge

require(ridge)
data("GenCont",package = "ridge")

compare_ols_ridge <- function(responses = GenCont[,1],
                              predictors = GenCont[,-1],
                              lambda = 1){
  responses = as.data.frame(GenCont[,1])
  predictors = as.data.frame(GenCont[,-1])
  training_index = sample(x = c(1:500), size = 250, replace = FALSE)
  training_resp = responses[training_index,]
  training_pred = predictors[training_index,]
  testing_resp = responses[-training_index,]
  testing_pred = predictors[-training_index,]
  mod_lm_training <- lm(training_resp~as.matrix(training_pred))
  mod_ridge_training <- glmnet(as.matrix(training_pred),training_resp, lambda = 1, alpha = 0)
  in_error_ols <- mean((training_resp - predict(mod_lm_training, training_pred))^2)
  in_error_ridge <- mean((training_resp - predict(mod_ridge_training, as.matrix(training_pred)))^2)
  
  out_erro_ols <- mean((testing_resp - predict(mod_lm_training, testing_pred))^2)
  out_error_ridge <- mean((testing_resp - predict(mod_ridge_training, as.matrix(testing_pred)))^2)
  
  return (c("lambda" = lambda, "ols_in" = in_error_ols, "ridge_in" = in_error_ridge, 
            "ols_out" = out_erro_ols, "ridge_out" = out_error_ridge))
}