source("~/Desktop/semester project/SemesterProject/functions/functions.R") #Our estimator are store in this R files.
library(ggplot2)

#In this first simulation we provide a separable covariance.

K=1 #Number of independent run
A <- matrix(c(1,0.5,0.5,1), ncol=2)
B <- matrix(c(1,-0.5,0,-0.5,1,0.2,0,0.2,1), ncol=3) 
I(min(eigen(A)$values)>0) # check that A and B are positive definite.
I(min(eigen(B)$values)>0)


A_half <- mat_root(A)
B_half <- mat_root(B)
C <- aperm(outer(A,B),c(1,3,2,4))
K1 <- dim(A)[1]
K2 <- dim(B)[2]

# Initialize vectors to store results, we store 11 different value of N.
LSSE1 <- rep(0,11)
LSSE2<- rep(0,11)
LSSE3<- rep(0,11)
MLE<- rep(0,11)
CSE<- rep(0,11)
for(i in 1:K){
  N_values <- c()
  LSSE1_values <- c()
  LSSE2_values <- c()
  LSSE3_values <- c()
  MLE_values <- c()
  CSE_values <- c()
for (i in 5:15) {
  N <- 2^i
  X <- array(0,c(N,K1,K2))
  for(n in 1:N) X[n,,] <- A_half %*% matrix(rnorm(K1*K2),ncol=K2) %*% B_half
  
  C <- aperm(outer(A,B),c(1,3,2,4))
  
  
  # check empirical cov
  Chat <- cov1(X)
  MLE_values <- c(MLE_values,frobenius(Chat-C)/frobenius(C))
  
  
  # Least squares separable estimator
  Chat <- scdR(X,1)
  LSSE1_values <- c(LSSE1_values, frobenius(Chat-C)/frobenius(C))

  # Least squares separable estimator
  Chat <- scdR(X,2)
  LSSE2_values <- c(LSSE2_values, frobenius(Chat-C)/frobenius(C))
  
  # Least squares separable estimator
  Chat <- scdR(X,3)
  LSSE3_values <- c(LSSE3_values, frobenius(Chat-C)/frobenius(C))
  
  # The CSE
  Chat <-cCSE(X)
  CSE_values <- c(CSE_values, frobenius(Chat-C)/frobenius(C))
  
  N_values <- c(N_values, N)
}
  LSSE1 <- LSSE1+LSSE1_values
  LSSE2<- LSSE2+LSSE2_values
  LSSE3<- LSSE3+LSSE3_values
  MLE<- MLE+MLE_values
  CSE<- CSE+CSE_values
  }

  LSSE1 <-LSSE1/K
  LSSE2 <-LSSE2/K
  LSSE3 <-LSSE3/K
  MLE <-MLE/K
  CSE <-CSE/K
# Create data frame for plotting
df <- data.frame(
  N = N_values,
  Separable1 = LSSE1,
  Separable2 = LSSE2,
  Separable3 = LSSE3,
  Empirical = MLE,
  CoreShrinkage = CSE
)

# Reshape data for plotting
df_melted <- reshape2::melt(df, id.vars = "N")

# Create plot
ggplot(df_melted, aes(x = N, y = value, color = variable)) +
  geom_line() +
  geom_point(aes(shape = variable), size = 3) +
  scale_x_continuous(trans = "log2") +
  labs(x = "N", y = "Frobenius Norm", color = "Estimator", shape = "Estimator") +
  theme_minimal()
theme(
  panel.grid.major = element_line(colour = "grey80"),
  panel.grid.minor = element_line(colour = "grey90")
)

#Now we provide a not separable covariance matrix  for R<4

A1 <- matrix(c(1, 0.2, 0.2, 1), nrow = 2)
A2 <- matrix(c(1, 0.4, 0.4, 1), nrow = 2)
A3 <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)
A4 <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
B1 <- matrix(c(1,0.2,0,0.2,1,0.8,0,0.8,1), nrow=3)
B2 <- matrix(c(1,0.4,0,0.4,1,0.6,0,0.6,1), nrow=3)
B3 <- matrix(c(1,0.6,0,0.6,1,0.4,0,0.4,1), nrow=3)
B4 <- matrix(c(1,0.8,0,0.8,1,0.2,0,0.2,1), nrow=3)
C <- kronecker(A1, B1)+kronecker(A2, B2)+kronecker(A3, B3)+kronecker(A4, B4)

# Show that C is positive definite
is_positive_definite <- I(min(eigen(C)$values) > 0)
C<-matrix2tensor(C,2,3)
# Initialize vectors to store results
LSSE1 <- rep(0,11)
LSSE2<- rep(0,11)
LSSE3<- rep(0,11)
MLE<- rep(0,11)
CSE<- rep(0,11)
for(i in 1:K){
  N_values <- c()
  LSSE1_values <- c()
  LSSE2_values <- c()
  LSSE3_values <- c()
  MLE_values <- c()
  CSE_values <- c()
for (i in 5:15) {
  N <- 2^i
X<- array(mvrnorm(n=N,mu=rep(0,6),tensor2matrix(C)),dim=c(N,2,3))
# check empirical cov
Chat <- cov1(X)
MLE_values <- c(MLE_values,frobenius(Chat-C)/frobenius(C))


# Least squares separable estimator
Chat <- scdR(X,1)
LSSE1_values <- c(LSSE1_values, frobenius(Chat-C)/frobenius(C))

# Least squares separable estimator
Chat <- scdR(X,2)
LSSE2_values <- c(LSSE2_values, frobenius(Chat-C)/frobenius(C))

# Least squares separable estimator
Chat <- scdR(X,3)
LSSE3_values <- c(LSSE3_values, frobenius(Chat-C)/frobenius(C))

# The CSE
Chat <-cCSE(X)
CSE_values <- c(CSE_values, frobenius(Chat-C)/frobenius(C))

N_values <- c(N_values, N)
}
LSSE1 <- LSSE1+LSSE1_values
LSSE2<- LSSE2+LSSE2_values
LSSE3<- LSSE3+LSSE3_values
MLE<- MLE+MLE_values
CSE<- CSE+CSE_values
}

LSSE1 <-LSSE1/K
LSSE2 <-LSSE2/K
LSSE3 <-LSSE3/K
MLE <-MLE/K
CSE <-CSE/K
# Create data frame for plotting
df <- data.frame(
  N = N_values,
  Separable1 = LSSE1,
  Separable2 = LSSE2,
  Separable3 = LSSE3,
  empirical = MLE,
  CoreShrinkage = CSE
)

# Reshape data for plotting
df_melted <- reshape2::melt(df, id.vars = "N")

# Create plot
ggplot(df_melted, aes(x = N, y = value, color = variable)) +
  geom_line() +
  geom_point(aes(shape = variable), size = 3) +
  scale_x_continuous(trans = "log2") +
  labs(x = "N", y = "Frobenius Norm", color = "Estimator", shape = "Estimator") +
  theme_minimal()
theme(
  panel.grid.major = element_line(colour = "grey80"),
  panel.grid.minor = element_line(colour = "grey90")
)
