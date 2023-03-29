# Define the indicator_matrix function
my_lda1 <- function(M, u0, u1,w) {
  # Get the row of array
  k <- dim(M)[1]
  # Create an empty vector to store the indicator values
  indicator_values <- vector("numeric", k)
  
  # Calculate the indicator for each row in M
  for (i in 1:k) {
    indicator_values[i] <- indicator1(M[i,,], u0, u1, w)
  }
  
  return(indicator_values)
}


# Define the indicator function
indicator1 <- function(x, u0, u1,w) {

  
  # Calculate the dot product between (x - u0, w) and (x - u1, w)
  dot_product <- sum((x - u0) * w) + sum((x - u1) * w)
  
  # Calculate the indicator value
  if (dot_product > 0) {
    indicator_value <- 0
  } else {
    indicator_value <- 1
  }
  
  return(indicator_value)
}

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

#Calculation of the covariance matrix for an array of 3 dimensions along the first dimension
cov1 <- function(M) {
  n2 <- dim(M)[2]
  n3 <- dim(M)[3]
  
  cov_mat <- array(0, dim = c(n2, n3, n2, n3))
  
  for (i in 1:n3) {
    for (j in 1:n3) {
      cov_mat[, i, , j] <- cov(M[,,i],M[,,j])
    }
  }
  
  return(cov_mat)
}
