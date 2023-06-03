### separable component decomposition and LDA algorithm
#in this file there are all the functions used for the experimentation present in the github:
#  https://github.com/DavideLaManna/Covariance-Estimation-for-Matrix-variate-Data
library(covKCD) #This library provide Covariance Shrinkage estimator developed
#by Hoff et. Al in their article, allows to implement the Core Shrinkage estimation 
#algorithm, i.e. find the best convex combination between MLE and separable MLE
library(MASS)


#The main functions included in this code are:
#my_lda: compute Discriminant analysis for both QDA and LDA cases, it requires
#that the inverse problem C_i*w_i=mean_i be solved first
#scd_est: This function calculates the separable component decomposition of a
#given tensor. For R=1, it calculates the least squares separable estimator. 
#The function takes as input a tensor X, a positive integer R (the number of 
#components), a maximum number of iterations (maxiter), and an optional initial 
#estimate for the tensor B.
#sep_MLE: This function calculates the separable maximum likelihood estimator.
#Similar to scd_est, the function takes as input a tensor X, a maximum number of
#iterations (maxiter), and an optional initial estimate for the tensor B.
#T1 and T2: These functions calculate partial inner products with respect to the
#first and second arguments, respectively, which are used in the main functions 
#scd_est and sep_MLE.
#scdR and sMLE: These are wrapper functions for scd_est and sep_MLE, respectively,
#which perform the decomposition and return the estimated covariance tensor.
#logD, cov1, tensor2matrix, matrix2tensor,mat_root, cCSE, plot_eigenvalues and frobenius: 
#These are utility functions used for the calculation and manipulation of tensors
#and matrices.






# Discriminant analysis algorithm once the inverse problem is solved.
my_lda <- function(M, u,w) {
  # Get the row of array
  k <- dim(M)[1]
  #get the number of levels
  h<-dim(w)[3]
  # Create an empty vector to store the indicator values
  indicator_values <- vector("numeric", k)
  
  # Calculate the label for each row in M
  # the label function calculates the sum of k for the set indicator 
  # "s_j(M[i,,])z>s_l(M[i,,] \forall l\neq j \in \{1,...,k\}"
  for (i in 1:k) {
    label<-0
    for(j in 1:h){
      product<-1
      for(l in 1:h){
        if(l!=j){
          product<-product*indicator(M[i,,], u[,,j], u[,,l],w[,,j],w[,,l])
        }
      }
      label<-label+j*product
    }
    indicator_values[i]<-label
  }
  
  return(indicator_values)
}


# Define the indicator function
indicator <- function(x, u1, u2,w1,w2) {
  
  
  # Calculate the difference of two score
  dot_product <- sum(u1* w1) -2*sum(x * w1) -sum(u2*w2)+2*sum(x * w2) 
  
  # Calculate the indicator value
  if (dot_product > 0) {
    indicator_value <- 0
  } else {
    indicator_value <- 1
  }
  
  return(indicator_value)
}



# calculates the separable component decomposition
# for R=1, it calculates the least squares separable estimator
scd_est <- function(X,R,maxiter=10,B=NULL){
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



#Calculation of the covariance matrix for an array of 3 dimensions along the first dimension
cov1 <- function(M) {
  n2 <- dim(M)[2]
  n3 <- dim(M)[3]
  
  cov1 <- array(0, dim = c(n2, n3, n2, n3))
  
  for (i in 1:n3) {
    for (j in 1:n3) {
      cov1[, i, , j] <- cov(M[,,i],M[,,j])
    }
  }
  
  return(cov1)
}


### utils

# partition in train and test set 
#alpha is percentage of data allocated to the training
partition <- function(M,alpha) {
  nrow<-dim(M)[1]
  index <- sample(1:nrow, size = nrow, replace = FALSE)
  train_index <- index[1:(alpha*nrow)]
  test_index <- index[(alpha*nrow+1):nrow]
  train <- M[train_index,,]
  test <- M[test_index,,]
  return(list(train=train, test=test))
}




# R separable estimator lse
scdR <- function (X,R){
  K1 <- dim(X)[2]
  K2 <- dim(X)[3]
Chat=array(0,rep(c(K1,K2),2))
Res <- scd_est(X,R)
for (i in 1:R) {
  Chat <- Chat+Res$sigma[i] * aperm(outer(Res$A[i,,],Res$B[i,,]),c(1,3,2,4))}
  return(Chat)
}

# 1 separable estimator MLE
sMLE <- function (X){
Res <- sep_MLE(X)
return(aperm(outer(Res$A,Res$B),c(1,3,2,4)))
}



# square-root of a matrix for simulation purposes
mat_root <- function(X){
  EIG <- eigen(X)
  lambda <- EIG$values
  V <- EIG$vectors
  lambda <- (lambda + abs(lambda))/2
  return(V %*% diag(sqrt(lambda)) %*% t(V))
}


logD <-function(mat){
  if(!is.matrix(mat)) {
    mat=ca2cm(mat)
  }
return(sum(log(abs(eigen(mat)$values))))
}

tensor2matrix <- function(C){
  # transforms a covariance tensor into the proper covariance matrix
  K1 <- dim(C)[1]
  K2 <- dim(C)[2]
  C_mat <- matrix(c(C),ncol=K1*K2)
  return(C_mat)
}

matrix2tensor <- function(C_mat,K1,K2){
  # transforms a covariance matrix into the proper covariance tensor, 
  #dimensions must be provided
  C <- array(c(C_mat),c(K1,K2,K1,K2))
  return(C)
}


#in this code we use the covCSE developed by Hoff et. al to output an array
#instead of a matrix
cCSE <- function(K){
  n<-dim(K)[1]
  p1<-dim(K)[2]
  p2<-dim(K)[3]

  return(matrix2tensor(covCSE(K,n,p1,p2),p1,p2))
}
  
#Function to generate the eigenvalue plot

plotEigenvalues <- function(mat) {
  if(!is.matrix(mat)) {
    mat=tensor2matrix(mat)
  }
  
  # Calculation of the eigenvalues
  eigenvalues <- eigen(mat)$values
  
  # creation of a dataframe for the plot
  df <- data.frame(index = 1:length(eigenvalues), value = eigenvalues)
  
  # Creation of the plot
  ggplot(df, aes(x = index, y = value)) +
    geom_point() +
    scale_y_log10()+
    labs(title = "Eigenvalues of the matrix",
         x = "Index",
         y = "Eigenvalue") +
    theme_minimal()
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

i<-7
N <- 2^i
K1 <- dim(A)[1]
K2 <- dim(B)[2]
X <- array(0,c(N,K1,K2))
for(n in 1:N) X[n,,] <- A_half %*% matrix(rnorm(K1*K2),ncol=K2) %*% B_half

# check empirical cov
C <- aperm(outer(A,B),c(1,3,2,4))
Chat <- cov1(X)
frobenius(Chat-C)/frobenius(C) 
# increase N to and re-run to see whether this error decreases

# check least squares separable estimator
Chat <- scdR(X,1)
frobenius(Chat-C)/frobenius(C)

# check the separable MLE
Chat <- sMLE(X)
frobenius(Chat-C)/frobenius(C)

# check the CSE
Chat <-cCSE(X)
frobenius(Chat-C)/frobenius(C)





