# Importa il pacchetto necessario per l'analisi discriminante
library(MASS)
# Installa e carica il pacchetto "shrink"
install.packages("shrink")
library(shrink)

# Funzione per il calcolo della QDA con regolarizzazione
qda_shrink_algorithm <- function(X, Y) {
  # Stima delle matrici di covarianza con regolarizzazione
  cov_matrices <- cov.shrink(X, method = "oas")  # "oas" per la stima di covarianza shrinkage
  
  # Addestramento del modello QDA
  model <- qda.shrink(X, Y, Sigma = cov_matrices)
  
  # Calcolo delle predizioni sul training set
  predictions <- predict(model, X)
  
  # Calcolo dell'accuratezza del modello
  accuracy <- sum(predictions$class == Y) / length(Y)
  
  # Restituisce il modello e l'accuratezza
  return(list(model = model, accuracy = accuracy))
}

# Applicazione della funzione qda_shrink_algorithm
result <- qda_shrink_algorithm(X, Y)

# Stampa del modello e dell'accuratezza
print(result$model)
print("Accuratezza:")
print(result$accuracy)

# Funzione per il calcolo della QDA
qda_algorithm <- function(X, Y) {
  # Addestramento del modello QDA
  model <- qda(X, Y)
  
  # Calcolo delle predizioni sul training set
  predictions <- predict(model, matrix(test_set,nrow=dim(test_set)[1]))
  
  # Calcolo dell'accuratezza del modello
  accuracy <- sum(predictions$class == test_labels) / length(test_labels)
  
  # Restituisce il modello e l'accuratezza
  return(list(model = model, accuracy = accuracy))
}

# Esempio di utilizzo della funzione qda_algorithm
# Generazione di dati casuali
X <- matrix(train_set,nrow=dim(train_set)[1])
Y <-train_labels[c(1:dim(train_set)[1])]

# Stima della matrice di covarianza C (esempio con covarianza diagonale)
C <- matrix(CKMLE,nrow=1287,ncol=1287)

# Applicazione della funzione qda_algorithm
result <- qda_algorithm(X, Y)

# Stampa del modello e dell'accuratezza
print(result$model)
print("Accuratezza:")
print(result$accuracy)
