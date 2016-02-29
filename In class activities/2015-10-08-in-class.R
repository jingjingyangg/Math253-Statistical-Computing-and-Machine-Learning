# Jingjing Yang
# 2015-10-08 in class activity


#Matrix Compositions
A <- matrix(c(1.0,-0.7,-0.7,1.0), nrow = 2, ncol = 2, byrow = TRUE)
A_inv <- solve(A)
test_inverse <- A %*% A_inv
A_chol <- chol(A)

A_chol_trans <- t(A_chol)
A_chol_trans %*% A_chol



#Orthogonal vectors and matrices
x1 <- cbind(rnorm(10))
x2 <- cbind(rnorm(10))
t(x1) %*% x2
X <- cbind(x1,x2)
t(X) %*% X

##covariance matrix
t(X) %*% X / nrow(X)

##length = 10,000
w1 <- cbind(rnorm(10000))
w2 <- cbind(rnorm(10000))
t(w1) %*% w2
W <- cbind(w1,w2)
t(W) %*% W / nrow(W)



#Generating correlated random vectors
A_inv_chol <- chol(A_inv)
Y <- X %*% A_chol
Y <- W %*% A_chol
##covariance matrix
t(Y) %*% Y / nrow(Y)

##plot
plot(Y[,1], Y[,2], col = rgb(0,0,0,0.05))


#test
require(scoreActivity, quietly = TRUE )
score253(day = 11)
