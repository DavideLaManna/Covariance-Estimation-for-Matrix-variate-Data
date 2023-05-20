#load the necessary
library(abind) #for concatenate the data
library(fgm)  # Partial Separability 
library(caret) #for confusion matrix
library(reshape2) #for melt function
library(covKCD)  #for CSE estimation
source("~/Desktop/semester project/SemesterProject/functions/LDA.R")
source("~/Desktop/semester project/SemesterProject/functions/separable_estimators.R")

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






#MLE of the data
CMLE=cov1(train_set)

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


#prepare covariance for QDA
i<-1
CCMLE<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3],dim(data$yes)[2], dim(data$yes)[3], length(attr)))
for (name in attr) {
  val<-partition(data[[name]],alpha)
  CCMLE[,,,,i]<-cov1(val$train)
  i<-i+1
}

#solve inverse problem for QDA
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CCMLE[,i,,i,j],mean[,i,j])
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
  labs(title = "Confusion Matrix QDA", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")




# check the separable MLE
CKMLE<- sMLE(train_set)

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


#prepare covariance for QDA
i<-1
CCSMLE<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3],dim(data$yes)[2], dim(data$yes)[3], length(attr)))
for (name in attr) {
  val<-partition(data[[name]],alpha)
  CCSMLE[,,,,i]<-sMLE(val$train)
  i<-i+1
}

#solve inverse problem for QDA
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CCSMLE[,i,,i,j],mean[,i,j])
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
  labs(title = "Confusion Matrix QDA sep MLE", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")






# check the CSE
CCSE<- cCSE(train_set)

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


#prepare covariance for QDA
i<-1
CCCSE<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3],dim(data$yes)[2], dim(data$yes)[3], length(attr)))
for (name in attr) {
  val<-partition(data[[name]],alpha)
  CCCSE[,,,,i]<-cCSE(val$train)
  i<-i+1
}



#solve inverse problem for QDA
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CCCSE[,i,,i,j],mean[,i,j])
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
  labs(title = "Confusion Matrix QDA CSE", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")

















# check least squares separable estimator R=3
CKLSE<- scdR(train_set,3)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CKLSE[,i,,i],mean[,i,j])
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

#prepare covariance for QDA
i<-1
CCLSE3<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3],dim(data$yes)[2], dim(data$yes)[3], length(attr)))
for (name in attr) {
  val<-partition(data[[name]],alpha)
  CCLSE3[,,,,i]<-scdR(val$train,3)
  i<-i+1
}



#solve inverse problem for QDA
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(CCLSE3[,i,,i,j],mean[,i,j])
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
  labs(title = "Confusion Matrix QDA sep LSE R=3", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")












