# Define the indicator_matrix function
matrix1 <- function(M, u0, u1,w) {
  # Get the number of rows k
  k <- nrow(M)
  # Create an empty vector to store the indicator values
  indicator_values <- vector("numeric", k)
  
  # Calculate the indicator for each row in M
  for (i in 1:k) {
    indicator_values[i] <- indicator(M[i,], u0, u1, w)
  }
  
  return(indicator_values)
}