
# Define the indicator_matrix function
#the indicization of the class will begin by 1.
matrix_new <- function(M, w, u) {
  # Get the number of rows k
  k <- nrow(M)
  # Get the number of class h
  h<- length(u)
  # Create an empty vector to store the indicator values
  indicator_values <- vector("numeric", k)
  
  # Calculate the indicator for each row in M
  for (i in 1:k) {
    # Initialize the sum for the current row
    row_sum <- 0
    
    # Iterate over all instances of the vector u
    for (j in 1:h) {
      product <-0
      for (l in 1:h){
          # Skip the case when j is equal to i
          if (j != l) {
            # Calculate the product of the indicators for the i-th and j-th instances of the vector u
            product <- product * indicator(M[i,], u[j], u[l], C, w[j, l])
            # Add the product to the sum
            row_sum <- row_sum + j*product
          }
      }
    }
    # Store the sum of the products in the indicator_values vector
    indicator_values[i] <- row_sum}
  
  return(indicator_values)
}