# Define the indicator_matrix function
matrix1 <- function(M, u0, u1, C) {
  # Get the number of rows k
  k <- nrow(M)
  # Get w
  w <- solve(cov, mean_dog - mean_cat)
  # Create an empty vector to store the indicator values
  indicator_values <- vector("numeric", k)
  
  # Calculate the indicator for each row in M
  for (i in 1:k) {
    indicator_values[i] <- indicator(M[i,], u0, u1, C,w)
  }
  
  return(indicator_values)
}