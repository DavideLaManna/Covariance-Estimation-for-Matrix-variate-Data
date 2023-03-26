# Define the indicator_matrix function
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

