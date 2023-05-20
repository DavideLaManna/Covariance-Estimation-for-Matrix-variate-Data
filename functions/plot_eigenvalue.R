# Iniziamo installando ed importando il pacchetto necessario per i grafici
if(!require(ggplot2)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Funzione per generare il plot degli autovalori
plotEigenvalues <- function(mat) {
  if(!is.matrix(mat)) {
    mat=ca2cm(mat)
  }
  
  # Calcolo degli autovalori
  eigenvalues <- abs(eigen(mat)$values)
  # Ordinamento degli autovalori
  sortedEigenvalues <- sort(eigenvalues)
  
  # Creazione di un dataframe per il plot
  df <- data.frame(index = 1:length(sortedEigenvalues), value = sortedEigenvalues)
  
  # Creazione del plot
  ggplot(df, aes(x = index, y = value)) +
    geom_point() +
    scale_y_log10()+
    labs(title = "Eigenvalues of the matrix",
         x = "Index",
         y = "Eigenvalue") +
    theme_minimal()
}

# Test della funzione la funzioen deve avere autovalori reali
mat <- matrix(rnorm(25), nrow = 5, ncol = 5)
plotEigenvalues(mat)