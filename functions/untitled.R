# Load necessary library
library(caret)
library(reshape2)
library(ggplot2)
library(MASS)

source("C:/Users/david/OneDrive/Desktop/semester project/git/SemesterProject/functions/indicator.R")
source("C:/Users/david/OneDrive/Desktop/semester project/git/SemesterProject/functions/matrix.R")
# loading of the data dog
load("dogP.RData")
# Array transform
nrow <- dim(mfccs)[3]
ncol <- dim(mfccs)[1] * dim(mfccs)[2]
mfccs_t <- array(data = NA, dim = c(nrow, ncol))
for (i in 1:nrow) {
  mfccs_t[i,] <- c(mfccs[,,i])
}

# partition in train and test set 
index <- sample(1:nrow, size = nrow, replace = FALSE)
train_index <- index[1:(0.9*nrow)]
test_index <- index[(0.9*nrow+1):nrow]
train_dog <- mfccs_t[train_index,]
test_dog <- mfccs_t[test_index,]

#repeat for cat
load("catP.RData")
# Array transform
nrow <- dim(mfccs)[3]
ncol <- dim(mfccs)[1] * dim(mfccs)[2]
mfccs_t <- array(data = NA, dim = c(nrow, ncol))
for (i in 1:nrow) {
  mfccs_t[i,] <- c(mfccs[,,i])
}

# partition in train and test set 
index <- sample(1:nrow, size = nrow, replace = FALSE)
train_index <- index[1:(0.9*nrow)]
test_index <- index[(0.9*nrow+1):nrow]
train_cat <- mfccs_t[train_index,]
test_cat <- mfccs_t[test_index,]


#preparation of the data
x <- rbind(train_dog, train_cat)
y <- factor(c(rep(0, each = dim(train_dog)[1]),rep(1, each = dim(train_cat)[1])))

#MLE estimation
mean_dog <- colMeans(train_dog)
cov <- cov(x)
mean_cat <- colMeans(train_cat)


#LDA by hand
x_new <- rbind(test_dog, test_cat)
y_new <- factor(c(rep(0, each = dim(test_dog)[1]),rep(1, each = dim(test_cat)[1])))
vec<-matrix1(x_new,mean_dog,mean_cat,cov)
accuracy <- sum(y_new == vec)/length(vec)
vec <- factor(vec, levels = unique(c(0,1)))

#LDA
qda.fit <- lda(x, y)

# Predizione dei nuovi dati
predictions<-predict(qda.fit, rbind(test_dog, test_cat))

#Calculation of the confusion matrix and graphical representation



# Create the confusion matrix
true_labels <- factor(c(rep(0, each = dim(test_dog)[1]),rep(1, each = dim(test_cat)[1])))
confusionMatrix(predictions$class, true_labels)

  # Plot the confusion matrix

cm <- as.matrix(confusionMatrix(predictions$class, true_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.title = element_blank()) +
  labs(title = "confusion matrix")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(predictions$class, true_labels)$table)) / length(true_labels)
cat("Accuracy:", accuracy * 100, "%\n")

#QDA
x <- rbind(train_dog, train_cat)
y <- factor(c(rep("0", each = dim(train_dog)[1]),rep("1", each = dim(train_cat)[1])))
qda.fit <- qda(x, y)

# Prediction new data
predictions<-predict(qda.fit, rbind(test_dog, test_cat))

#Calculation of the confusion matrix and graphical representation

# Create the confusion matrix
true_labels <- factor(c(rep(0, each = dim(test_dog)[1]),rep(1, each = dim(test_cat)[1])))
confusionMatrix(predictions$class, true_labels)

# Plot the confusion matrix

cm <- as.matrix(confusionMatrix(predictions$class, true_labels)$table)
cm_melted <- melt(cm)
colnames(cm_melted) <- c("True", "Predicted", "value")
ggplot(data = cm_melted, aes(x = True, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.title = element_blank()) +
  labs(title = "confusion matrix")

# Calculate the accuracy of the algorithm
accuracy <- sum(diag(confusionMatrix(predictions$class, true_labels)$table)) / length(true_labels)
cat("Accuracy:", accuracy * 100, "%\n")



