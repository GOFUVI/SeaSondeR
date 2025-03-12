seasonder_extrapolateAPM <- function(seasonder_apm_object, n = 1){

   BEAR <- seasonder_getSeaSondeRAPM_BEAR(seasonde_apm_obj = seasonder_apm_object)

   extrapolated_BEAR <- BEAR

   # Extrapolar linealmente n columnas por ambos lados la matriz del objeto seasonder_apm_object
   # Añadir los correspondientes bearings a extrapolated_BEAR considerando la distancia entre bearings en BEAR.
   # Cada columna de la matriz se corresponden con un bearing y los nombres coinciden con los bearings.

   seasonder_apm_object %<>% seasonder_setSeaSondeRAPM_BEAR(extrapolated_BEAR)

   return(seasonder_apm_object)

}

