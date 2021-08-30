rm(list = ls())

library(MASS)
library(ggplot2)
library(Matrix)

###### 2 groups, uncorrelated samples, low dimensional
n <- 100
p <- 50
X <- rbind(matrix(rnorm(n * p / 2, mean = 1), nrow = n, ncol = p/2),
           matrix(rnorm(n * p / 2, mean = 0), nrow = n, ncol = p/2))
kmeans.out <- kmeans(X, centers = 2)
qplot(svd(X)$u[,1], svd(X)$u[,2],
      col = factor(kmeans.out$cluster)) +
  theme_classic()

##### 2 groups, uncorrelated, high dim
n <- 100
p <- 5000
X <- rbind(matrix(rnorm(n * p / 2, mean = 1), nrow = n, ncol = p/2),
           matrix(rnorm(n * p / 2, mean = 0), nrow = n, ncol = p/2))
kmeans.out <- kmeans(X, centers = 2)
qplot(svd(X)$u[,1], svd(X)$u[,2],
      col = factor(kmeans.out$cluster)) +
  theme_classic()

##### 2 groups, "sibling" correlated samples, low dim
n <- 100
p <- 50
rho <- 0.9
Sigma = bdiag(lapply(1:(n/2), function(i){
 matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
}))
X <- t(mvrnorm(n = p, mu = rep(c(0.35, 0), n/2), ## one sibling in each group
        Sigma = Sigma))
kmeans.out <- kmeans(X, centers = 2)
qplot(svd(X)$u[,1], svd(X)$u[,2],
      col = factor(kmeans.out$cluster)) +
  theme_classic()

##### 2 groups, "family" correlated samples, low dim
n <- 100
p <- 50
rho <- 0.9
Sigma = bdiag(lapply(1:(n/5), function(i){
 I <- (1-rho) * diag(5)
 ones <- rho * rep(1 ,5) %*% t(rep(1, 5))
 I + ones
}))
X <- t(mvrnorm(n = p, mu = rep(c(0.25, 0), n/2),
        Sigma = Sigma))
kmeans.out <- kmeans(X, centers = 2)
qplot(svd(X)$u[,1], svd(X)$u[,2],
      col = factor(kmeans.out$cluster)) +
  theme_classic()

#### 2 groups, two correlated blocks, low dim
n <- 100
p <- 50
rho <- 0.9
Sigma <- bdiag(lapply(1:2, function(i){
 I <- (1-rho) * diag(n/2)
 ones <- rho * rep(1 ,n/2) %*% t(rep(1, n/2))
 I + ones
}))
X <- t(mvrnorm(n = p, mu = c(rep(0.1, n/2), rep(0,n/2)),
        Sigma = Sigma))
kmeans.out <- kmeans(X, centers = 2)
qplot(svd(X)$u[,1], svd(X)$u[,2],
      col = factor(kmeans.out$cluster)) +
  theme_classic()
