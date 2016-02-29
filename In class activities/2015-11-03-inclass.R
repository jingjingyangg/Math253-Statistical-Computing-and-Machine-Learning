download.file("http://tiny.cc/dcf/mona.rda", destfile = "mona.rda")

load("mona.rda")

X <- t(mona) - mean(mona[])
X_rand <- matrix(data = rnorm(n = 250*191), nrow = 250, ncol = 191)
X_corr <- X_rand %*% chol(var(X))

#sparse beta
num <- c(rep(0,175), rep(2,4), rep(5,4), rep(-3,4), rep(-4,4))
beta <- sample(num,replace=FALSE)

# the output
Y_pure <- X %*% beta
Y_real <- Y_pure + rnorm(250, mean = 0, sd = sqrt(0.1*var(Y_pure)))


# least squares
beta_hat_pure <- coef(lm(Y_pure ~ X))[-1]
plot(beta_hat_pure,beta)

beta_hat_real <- coef(lm(Y_real ~ X))[-1]
plot(beta_hat_real,beta)


# the lasso estimator
library(glmnet)
lasso_mod<- cv.glmnet(X, Y_real, alpha = 1)
beta_lasso <- predict(lasso_mod, type = "coefficients", s = lasso_mod$lambda.min)

# principal components
sing_vals <- svd(X)$d
sing_vals_X_rand <- svd(X_rand)$d
sing_vals_X_corr <- svd(X_corr)$d
R2 <- (cumsum(sing_vals^2))/sum(sing_vals^2)
R2_rand <- (cumsum(sing_vals_X_rand^2))/sum(sing_vals_X_rand^2)
R2_corr <- (cumsum(sing_vals_X_corr^2))/sum(sing_vals_X_corr^2)  

k <- seq(191)
plot(R2, k, col = "green")
points(R2_rand, k, col = "blue")
points(R2_corr,k, col = "tomato")

# how many principal components for R^2 > 0.99
n99 <- which(R2>0.99)[1]
n99_rand <- which(R2_rand>0.99)[1]
n99_corr <- which(R2_corr>0.99)[1]


# use principal components to mode Y_real against X
library(pls)
pcr.fit <- pcr(Y_real ~ X, scale = TRUE,validation = "CV")
R2(pcr.fit)
