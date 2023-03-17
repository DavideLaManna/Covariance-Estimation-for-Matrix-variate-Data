
# Define the indicator function
indicator <- function(x, u0, u1, C,w) {

  
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