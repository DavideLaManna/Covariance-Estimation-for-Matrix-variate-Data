
#load the necessary
library(abind)
library(caret) #for confusion matrix
library(reshape2) #for melt function
source("./functions/LDARightLeft.R")
data<-get(load("./MFCCs.RData"))

#prepare the data
alpha<-0.8
right<-partition(data$right,0.8)
left<-partition(data$left,0.8)
train_set<-abind(right$train,left$train,along=1)
test_set<-abind(right$test,left$test,along=1)
test_labels<-factor(c(rep(0,dim(right$test)[1]),rep(1,dim(left$test)[1])))

#MLE of the data
meanR<-apply(right$train, c(2,3), mean)
meanL<-apply(left$train, c(2,3), mean)
C=cov1(train_set)

#solve inverse problem
w<-matrix(nrow=dim(meanR)[1],ncol=dim(meanR)[2])
for (i in 1:dim(meanR)[2]) {
  w[,i]=solve(C[,i,,i],meanR[,i]-meanL[,i])
}

#predict the value
predictions<-my_lda(test_set,meanR,meanL,w)


# Plot the confusion matrix
cm <- as.matrix(confusionMatrix(factor(predictions,level=c(0,1)), test_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  labs(title = "Confusion Matrix", x = "True", y = "Predicted")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(factor(predictions,level=c(0,1)), test_labels)$table)) / length(test_labels)
cat("Accuracy:", accuracy * 100, "%\n")

