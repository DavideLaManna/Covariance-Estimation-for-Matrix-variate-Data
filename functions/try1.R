#group=c('yes', 'no', 'up', 'down', 'left', 'right', 'on', 'off','stop', 'go')
group=c('off','left')
setwd("C:/Users/david/OneDrive/Desktop/semester project/git/SemesterProject/speech_commands_v0.02.tar/preprocessed data")
source("C:/Users/david/OneDrive/Desktop/semester project/git/SemesterProject/functions/partition.R")
library(MASS) #we will be use for QDA algorithm
library(caret) #we will be use this package for confusion matrix
library(reshape2) #we will use for melt function

#percentage of data allocated to the training
alpha<-0.9
i<-0

#preparation of the data
for (string in group) {

  file <- paste0(string, "P.RData")
  M<-get(load(file))
  nrow <- dim(M)[3]
  ncol <- dim(M)[1] * dim(M)[2]
  mfccs_t <- array(data = NA, dim = c(nrow, ncol))
  for (i in 1:nrow) {
    M_t[i,] <- c(M[,,i])
  }
  data<-partition(M_t,alpha)  #we partition the data in M in train and test data
  
  if (i==0)
  {
    train_data <- data$train
    test_data <- data$test
    train_labels <- rep(i, nrow(data$train))
    test_labels <- rep(i, nrow(data$test))
  }
  else
  {
    train_data <- rbind(train_data, data$train)
    test_data <- rbind(test_data, data$test)
    train_labels <- c(train_labels, rep(i, nrow(data$train)))
    test_labels <- c(test_labels, rep(i, nrow(data$test)))
  }
  i<-i+1
}

source("C:/Users/david/OneDrive/Desktop/semester project/git/SemesterProject/functions/indicator.R")
source("C:/Users/david/OneDrive/Desktop/semester project/git/SemesterProject/functions/matrix1.R")
rows_with_label_0 <- train_labels == 0
mean0<-colMeans(train_data[rows_with_label_0,])
rows_with_label_1 <- train_labels == 1
mean1<-colMeans(train_data[rows_with_label_1,])
cov <- cov(train_data)
 #Get w
w <- solve(cov, mean0 - mean1)
vec<-matrix1(test_data,mean0,mean1,w)
accuracy <- sum(test_labels == vec)/length(vec)
accuracy





qda.fit <- qda(train_data, train_labels)
predictions<-predict(qda.fit, test_data)
accuracy <- sum(diag(confusionMatrix(predictions$class, as.factor(test_labels))$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")

# Plot the confusion matrix

cm <- as.matrix(confusionMatrix(predictions$class, as.factor(test_labels))$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.title = element_blank()) +
  labs(title = "confusion matrix")
