

# transforms an array of size n1 x n2 x n3 into an array of size n3 x (n1*n2)

trasa <- function(M) {
n_row <- dim(M)[3]
n_col <- dim(M)[1] * dim(M)[2]
M_t <- matrix(rep(0,n_row*n_col), nrow=n_row)
for (i in 1:nrow) {
  M_t[i,] <- c(M[,,i])
}
return(M_t)
}