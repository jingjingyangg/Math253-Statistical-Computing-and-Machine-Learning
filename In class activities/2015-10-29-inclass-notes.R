
data(GenCont, package = "ridge")
response <- GenCont[,1]
predictors <- GenCont[,-1]

mod_lm <- lm(response~predictors)
coef(mod_lm)

mod_ridge <- glmnet(predictors,response, lambda = 2, alpha = 0)
coef(mod_ridge)

mod_lasso_0.2 <- glmnet(predictors,response, lambda = .2, alpha = 1)
coef(mod_lasso_0.2)

mod_lasso_20 <- glmnet(predictors,response, lambda = 20, alpha = 1)

mean((response - predict(mod_lasso_20, predictors))^2)
mean((response - predict(mod_lasso_0.2, predictors))^2)
