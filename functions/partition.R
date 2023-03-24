# partition in train and test set 
#alpha is percentage of data allocated to the training
partition <- function(M,alpha) {
  nrow<-nrow(M)
index <- sample(1:nrow, size = nrow, replace = FALSE)
train_index <- index[1:(alpha*nrow)]
test_index <- index[(alpha*nrow+1):nrow]
train <- M[train_index,]
test <- M[test_index,]
return(list(train=train, test=test))
}