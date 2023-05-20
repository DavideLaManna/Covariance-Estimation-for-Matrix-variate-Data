
#load the necessary
library(abind) #for concatenate the data
library(caret) #for confusion matrix
library(reshape2) #for melt function
source("./functions/LDA.R")
data<-get(load("./MFCCs.RData"))

#parameters for sepcov
maxrep=100

#prepare the data
alpha<-0.8
i<-1
train_set<-NULL
test_set<-NULL
train_labels<-NULL
test_labels<-NULL
attr <- (attributes(data))$names
mean<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3], length(attr)))
classcov<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3],dim(data$yes)[2], dim(data$yes)[3], length(attr)))
classsepcov<- array(NA, dim=c(dim(data$yes)[2], dim(data$yes)[3],dim(data$yes)[2], dim(data$yes)[3], length(attr)))
for (name in attr) {
  val<-partition(data[[name]],alpha)
  train_set<-abind(train_set,val$train,along=1)
  test_set<-abind(test_set,val$test,along=1)
  train_labels<-factor(c(train_labels,rep(i,dim(val$train)[1])),level=c(1:10)) #actually it doesn't need
  test_labels<-factor(c(test_labels,rep(i,dim(val$test)[1])),level=c(1:10))
  mean[,,i]<-apply(val$train, c(2,3), mean)
  classcov[,,,,i]=cov1(val$train) #this is necessary for QDA
  classsepcov[,,,,i]=sepcov(val$train,maxrep)
  i<-i+1
  
}

#MLE of the data
C=cov1(train_set)

#solve inverse problem for LDA 
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(C[,i,,i],mean[,i,j])
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


#solve inverse problem for QDA
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(classcov[,i,,i,j],mean[,i,j])
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



#SEP MLE for the data
C<-sepcov(train_set,maxrep)

#solve inverse problem for LDA in sep cov case
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(C[,i,,i],mean[,i,j])
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
  labs(title = "Confusion Matrix LDA sep", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")


#solve inverse problem for QDA in sepcov case
w<-array(NA,dim=c(dim(mean)[1],dim(mean)[2],length(attr)))
for (i in 1:dim(mean)[2]) {
  for(j in 1:dim(mean)[3])
    w[,i,j]=solve(classsepcov[,i,,i,j],mean[,i,j])
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
  labs(title = "Confusion Matrix QDA sep", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(1:10)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")
