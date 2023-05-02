### separable component decomposition

# calculates the separable component decomposition
# for R=1, it calculates the least squares separable estimator
scd_est <- function(X,R=1,maxiter=10,B=NULL){
  N <- dim(X)[1]
  K1 <- dim(X)[2]
  K2 <- dim(X)[3]
  if(length(B) == 0){
    B <- array(0, c(R,K2,K2))
    for(r in 1:R){
      B[r,,] <- diag(K2)
    }
  }
  A <- array(0,c(R,K1,K1))
  sigma <- rep(0,R)
  for(r in 1:R){
    iter <- 0
    while(iter < maxiter){
      A[r,,] <- T1(X,A,B,sigma,r,N,K1,K2)
      A[r,,] <- A[r,,]/norm(A[r,,],type="F")
      B[r,,] <- T2(X,A,B,sigma,r,N,K1,K2)
      sigma[r] <- norm(B[r,,],type="F")
      B[r,,] <- B[r,,]/sigma[r]
      iter <- iter +1
    }
  }
  return(list(A=A,B=B,sigma=sigma))
}

# partial inner product w.r.t. the first argument
T1 <- function (X,A,B,sigma,r,N,K1,K2){
  Res <- array(0,c(K1,K1))
  for(n in 1:N){
    Res <- Res + X[n,,] %*% B[r,,] %*% t(X[n,,])
  }
  Res <- Res/N
  if(r > 1){
    for(j in 1:(r-1)){
      Res <- Res - sigma[j] * sum(B[j,,]*B[r,,]) * A[j,,]
    }
  }
  return(Res)
}

# partial inner product w.r.t. the second argument
T2 <- function (X,A,B,sigma,r,N,K1,K2){
  Res <- array(0,c(K2,K2))
  for(n in 1:N){
    Res <- Res + t(X[n,,]) %*% A[r,,] %*% X[n,,]
  }
  Res <- Res/N
  if(r>1){
    for(j in 1:(r-1)){
      Res <- Res - sigma[j] * sum(A[j,,]*A[r,,]) * B[j,,]
    }
  }
  return(Res)
}

### separable MLE

library(MASS) # for calculating pseudoinverse by ginv() below

# calculates the separable MLE
sep_MLE <- function(X,maxiter=10,B=NULL){
  N <- dim(X)[1]
  K1 <- dim(X)[2]
  K2 <- dim(X)[3]
  if(length(B) == 0){
    B <- diag(K2)
  }
  A <- array(0,c(K1,K1))
  sigma <- 0
  iter <- 0
  while(iter < maxiter){
    A <- A/norm(A,type="F") #
    A <- T1_inv(X,B,sigma,N,K1,K2)
    B <- T2_inv(X,A,sigma,N,K1,K2)
    iter <- iter +1
  }
  return(list(A=A,B=B))
}

T1_inv <- function (X,B,sigma,N,K1,K2){
  Res <- array(0,c(K1,K1))
  B_inv <- ginv(B)
  for(n in 1:N){
    Res <- Res + X[n,,] %*% B_inv %*% t(X[n,,])
  }
  Res <- Res/N/K2
  return(Res)
}

T2_inv <- function (X,A,sigma,N,K1,K2){
  Res <- array(0,c(K2,K2))
  A_inv <- ginv(A)
  for(n in 1:N){
    Res <- Res + t(X[n,,]) %*% A_inv %*% X[n,,]
  }
  Res <- Res/N/K1
  return(Res)
}

### utils

# square-root of a matrix for simulation purposes
mat_root <- function(X){
  EIG <- eigen(X)
  lambda <- EIG$values
  V <- EIG$vectors
  lambda <- (lambda + abs(lambda))/2
  return(V %*% diag(sqrt(lambda)) %*% t(V))
}

sample_cov <- function(X){
  N <- dim(X)[1]
  C <- array(0,rep(dim(X)[2:3],2))
  for(n in 1:N){
    C <- C + outer(X[n,,],X[n,,])
  }
  return(C/N)
}

tensor2matrix <- function(C){
  # transforms a covariance tensor into the proper covariance matrix
  K1 <- dim(C)[1]
  K2 <- dim(C)[2]
  C_mat <- matrix(c(C),ncol=K1*K2)
  return(C_mat)
}

matrix2tensor <- function(C_mat,K1,K2){
  # transforms a covariance matrix into the proper covariance tensor, dimensions must be provided
  C <- array(c(C_mat),c(K1,K2,K1,K2))
  return(C)
}

frobenius <- function(X){
  return(sqrt(sum(X^2)))
}

####################
### demo example ###
####################

A <- matrix(c(1,0.5,0.5,1), ncol=2)
B <- matrix(c(1,-0.5,0,-0.5,1,0.2,0,0.2,1), ncol=3) 
I(min(eigen(B)$values)>0) # check

A_half <- mat_root(A)
B_half <- mat_root(B)

N <- 100
K1 <- dim(A)[1]
K2 <- dim(B)[2]
X <- array(0,c(N,K1,K2))
for(n in 1:N) X[n,,] <- A_half %*% matrix(rnorm(K1*K2),ncol=K2) %*% B_half

# check empirical cov
C <- aperm(outer(A,B),c(1,3,2,4))
Chat <- sample_cov(X)
frobenius(Chat-C)/frobenius(C) # increase N to and re-run to see whether this error decreases

# check least squares separable estimator
Res <- scd_est(X)
Chat <- Res$sigma[1] * aperm(outer(Res$A[1,,],Res$B[1,,]),c(1,3,2,4))
frobenius(Chat-C)/frobenius(C)

# check the separable MLE
Res <- sep_MLE(X)
Chat <- aperm(outer(Res$A,Res$B),c(1,3,2,4))
frobenius(Chat-C)/frobenius(C)






