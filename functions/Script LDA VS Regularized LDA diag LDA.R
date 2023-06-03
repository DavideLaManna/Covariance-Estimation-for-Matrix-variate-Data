#load the necessary
library(abind) #for concatenate the data
library(caret) #for confusion matrix
library(reshape2) #for melt function
#load the functions
source("~/SemesterProject/functions/functions.R")

#load the data
data<-get(load("./MFCCs.RData"))

# random seed for the reproducibility 
set.seed(123)
#prepare the data
alpha<-0.8
i<-1
train_set<-NULL
test_set<-NULL
test_labels<-NULL
attr <- (attributes(data))$names
mean<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3], length(attr)))
#create test set and train set array, mean array
for (name in attr) {
  val<-partition(data[[name]],alpha)
  train_set<-abind(train_set,val$train,along=1)
  test_set<-abind(test_set,val$test,along=1)
  test_labels<-factor(c(test_labels,rep(i,dim(val$test)[1])),level=c(1:10))
  mean[,,i]<-apply(val$train, c(2,3), mean)
  i<-i+1
}


#set the value of lambda for the regularization:
L=1/2



#MLE of the data
CMLE=cov1(train_set)
#remove the comment symbol below if you want the result of the regularized problem
#CMLE<-matrix2tensor(L*tensor2matrix(CMLE)+(1-L)*sum(diag(tensor2matrix(CMLE)))/(dim(CMLE)[1]*dim(CMLE)[2])*diag(rep(1,1287)),99,13)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CMLE[,i,,i],mean[,i,j])
  
}

#predict the value
predictions<-my_lda(test_set,mean,w)

# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(labels = attr,limits=attr) + 
  scale_y_discrete(labels = attr,limits=attr) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix LDA", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")



# check the separable MLE
CKMLE<- sMLE(train_set)
#remove the comment symbol below if you want the result of the regularized problem
#CKMLE<-matrix2tensor(L*tensor2matrix(CKMLE)+(1-L)*sum(diag(tensor2matrix(CKMLE)))/1287*diag(rep(1,1287)),99,13)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CKMLE[,i,,i],mean[,i,j])
}

#predict the value
predictions<-my_lda(test_set,mean,w)

# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(labels = attr,limits=attr) + 
  scale_y_discrete(labels = attr,limits=attr) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix LDAsepMLE", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")



# check the CSE
CCSE<- cCSE(train_set)
#remove the comment symbol below if you want the result of the regularized problem
#CCSE<-matrix2tensor(L*tensor2matrix(CCSE)+(1-L)*sum(diag(tensor2matrix(CCSE)))/1287*diag(rep(1,1287)),99,13)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CCSE[,i,,i],mean[,i,j])
}

#predict the value
predictions<-my_lda(test_set,mean,w)

# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(labels = attr,limits=attr) + 
  scale_y_discrete(labels = attr,limits=attr) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix LDA CSE", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")



# check least squares separable estimator R=1
CKLSE1<- scdR(train_set,1)
#remove the comment symbol below if you want the result of the regularized problem
#CKLSE1<-matrix2tensor(L*tensor2matrix(CKLSE1)+(1-L)*sum(diag(tensor2matrix(CKLSE1)))/1287*diag(rep(1,1287)),99,13)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CKLSE1[,i,,i],mean[,i,j])
}

#predict the value
predictions<-my_lda(test_set,mean,w)

# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(labels = attr,limits=attr) + 
  scale_y_discrete(labels = attr,limits=attr) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix LDA sepLSE R=1", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")



# check least squares separable estimator R=2
CKLSE2<- scdR(train_set,2)
#remove the comment symbol below if you want the result of the regularized problem
#CKLSE2<-matrix2tensor(L*tensor2matrix(CKLSE2)+(1-L)*sum(diag(tensor2matrix(CKLSE2)))/1287*diag(rep(1,1287)),99,13)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CKLSE2[,i,,i],mean[,i,j])
}

#predict the value
predictions<-my_lda(test_set,mean,w)

# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(labels = attr,limits=attr) + 
  scale_y_discrete(labels = attr,limits=attr) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix LDA sepLSE R=2", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")


# check least squares separable estimator R=3
CKLSE3<- scdR(train_set,3)
#remove the comment symbol below if you want the result of the regularized problem
#CKLSE3<-matrix2tensor(L*tensor2matrix(CKLSE3)+(1-L)*sum(diag(tensor2matrix(CKLSE3)))/1287*diag(rep(1,1287)),99,13)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CKLSE3[,i,,i],mean[,i,j])
}

#predict the value
predictions<-my_lda(test_set,mean,w)

# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(labels = attr,limits=attr) + 
  scale_y_discrete(labels = attr,limits=attr) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix LDA sepLSE R=3", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")








#Diagonal LDA
A<-rep(0,n*m)
for(i in 1:1287){
  A[i]<-var(matrix(train_set,ncol=n*m)[,i])
}
#we transform the matrix into an array just to make the data congruent with our own classification algorithm
B<-array(diag(A),c(n,m,n,m))
#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(B[,i,,i],mean[,i,j])}

#predict the value
predictions<-my_lda(test_set,mean,w)

# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(labels = attr,limits=attr) + 
  scale_y_discrete(labels = attr,limits=attr) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix diag LDA", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")

