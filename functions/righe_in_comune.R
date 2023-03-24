# Funzione per confrontare le righe di due matrici e trovare quelle in comune
righe_in_comune <- function(matrice1, matrice2) {
  # Trasforma le righe delle matrici in stringhe
  matrice1_str <- apply(matrice1, 1, paste, collapse = "_")
  matrice2_str <- apply(matrice2, 1, paste, collapse = "_")
  
  # Trova le righe in comune
  righe_comuni <- intersect(matrice1_str, matrice2_str)
  
  # Indici delle righe in comune per ciascuna matrice
  indici_matrice1 <- which(matrice1_str %in% righe_comuni)
  indici_matrice2 <- which(matrice2_str %in% righe_comuni)
  
  # Restituisci gli indici delle righe in comune e le righe stesse
  return(list(indici_matrice1 = indici_matrice1, indici_matrice2 = indici_matrice2, righe_comuni = righe_comuni))
}