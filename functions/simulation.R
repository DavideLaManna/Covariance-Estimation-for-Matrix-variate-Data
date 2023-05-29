source("~/Desktop/semester project/SemesterProject/functions/separable_estimators.R")
library(ggplot2)
library(MASS)

# random seed for the reproducibility 
K=25
A <- matrix(c(1,0.5,0.5,1), ncol=2)
B <- matrix(c(1,-0.5,0,-0.5,1,0.2,0,0.2,1), ncol=3) 
I(min(eigen(B)$values)>0) # check

A_half <- mat_root(A)
B_half <- mat_root(B)
C <- aperm(outer(A,B),c(1,3,2,4))
K1 <- dim(A)[1]
K2 <- dim(B)[2]

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
  X <- array(0,c(N,K1,K2))
  for(n in 1:N) X[n,,] <- A_half %*% matrix(rnorm(K1*K2),ncol=K2) %*% B_half
  
  C <- aperm(outer(A,B),c(1,3,2,4))
  
  
  # check empirical cov
  Chat <- sample_cov(X)
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

# Creation of a sample 6x6 positive definite matrix
C <- matrix(c(4, 1, 2, 1, 0, 3,
              1, 5, 0, 0, 2, 1,
              2, 0, 3, 1, 1, 1,
              1, 0, 1, 6, 0, 2,
              0, 2, 1, 0, 5, 0,
              3, 1, 1, 2, 0, 4), nrow = 6, ncol = 6, byrow = TRUE)

# Show that C is positive definite
is_positive_definite <- I(min(eigen(C)$values) > 0)
C<-cm2ca(C,2,3)
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
X<- array(mvrnorm(n=N,mu=rep(0,6),ca2cm(C)),dim=c(N,2,3))
# check empirical cov
Chat <- sample_cov(X)
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