#### Coding agent runs ####

seasonder_extrapolateAPM <- function(seasonder_apm_object, n = 1){

   BEAR <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_object)

   extrapolated_BEAR <- BEAR

   # Extrapolar linealmente n columnas por ambos lados de la matriz del objeto seasonder_apm_object
   # Añadir los correspondientes bearings a extrapolated_BEAR considerando la distancia entre bearings en BEAR.
   # Cada columna de la matriz se corresponde con un bearing y los nombres coinciden con los bearings.

   seasonder_apm_object %<>% seasonder_setSeaSondeRAPM_BEAR(extrapolated_BEAR)

   return(seasonder_apm_object)

}


##### Run #####


#Acción a realizar: fix_code.
#Detalles: La corrección consiste en eliminar el argumento con nombre al llamar a seasonder_getSeaSondeRAPM_BEAR en la función seasonder_extrapolateAPM. Ahora se llama pasando simplemente seasonder_apm_object. Con este cambio, los tests deberían ejecutarse sin errores.
#
seasonder_extrapolateAPM <- function(seasonder_apm_object, n = 1){

   BEAR <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_object)

   extrapolated_BEAR <- BEAR

   # Extrapolar linealmente n columnas por ambos lados de la matriz del objeto seasonder_apm_object
   # Añadir los correspondientes bearings a extrapolated_BEAR considerando la distancia entre bearings en BEAR.
   # Cada columna de la matriz se corresponde con un bearing y los nombres coinciden con los bearings.

   seasonder_apm_object %<>% seasonder_setSeaSondeRAPM_BEAR(extrapolated_BEAR)

   return(seasonder_apm_object)

}

seasonder_extrapolateAPM <- function(seasonder_apm_object, n = 1) {

  # Retrieve the original BEAR vector and bearing resolution
  BEAR <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_object)
  res <- attr(seasonder_apm_object, "BearingResolution")

  # If n == 0, no extrapolation is required
  if(n == 0) return(seasonder_apm_object)

  # Calculate new bearings for the left and right sides using the bearing resolution
  left_new <- seq(from = BEAR[1] - n * res, to = BEAR[1] - res, by = res)
  right_new <- seq(from = tail(BEAR, 1) + res, to = tail(BEAR, 1) + n * res, by = res)
  new_BEAR <- c(left_new, BEAR, right_new)

  # Extract the original measurement matrix
  M <- seasonder_apm_object

  # Extrapolate the left side using linear extrapolation based on the first two columns of M
  slope_left <- (M[, 2] - M[, 1]) / (BEAR[2] - BEAR[1])
  left_mat <- sapply(left_new, function(b) M[, 1] + slope_left * (b - BEAR[1]))
  # Ensure left_mat is a matrix with as many columns as new left bearings
  if(is.null(dim(left_mat))) { left_mat <- matrix(left_mat, ncol = length(left_new)) }

  # Extrapolate the right side using linear extrapolation based on the last two columns of M
  n_orig <- ncol(M)
  slope_right <- (M[, n_orig] - M[, n_orig - 1]) / (BEAR[length(BEAR)] - BEAR[length(BEAR) - 1])
  right_mat <- sapply(right_new, function(b) M[, n_orig] + slope_right * (b - BEAR[length(BEAR)]))
  if(is.null(dim(right_mat))) { right_mat <- matrix(right_mat, ncol = length(right_new)) }

  # Combine the left extrapolated columns, the original matrix and the right extrapolated columns
  new_M <- cbind(left_mat, M, right_mat)
  colnames(new_M) <- as.character(new_BEAR)
  attr(new_M, "BEAR") <- new_BEAR
  attr(new_M, "BearingResolution") <- res

  return(new_M)
}


##### Run #####


#Acción a realizar: fix_code.
#Detalles: Se ha actualizado la función seasonder_extrapolateAPM para que, para un valor n > 0, realice la extrapolación lineal de la matriz de APM añadiendo n columnas a la izquierda y a la derecha. Para la extrapolación, se calcula una secuencia de nuevos bearings usando la resolución (BearingResolution). La extrapolación de cada lado se realiza utilizando la pendiente entre las dos primeras columnas para el lado izquierdo y la pendiente entre las dos últimas columnas para el lado derecho. Los nuevos bearings se usan para interpolar linealmente y se combinan para formar la nueva matriz. Los nombres de las columnas se actualizan con el vector de nuevos bearings y el atributo BEAR se actualiza apropiadamente.
#
seasonder_extrapolateAPM <- function(seasonder_apm_object, n = 1) {

  # Retrieve the original BEAR vector and bearing resolution
  BEAR <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_object)
  res <- attr(seasonder_apm_object, "BearingResolution")

  # If n == 0, no extrapolation is required
  if(n == 0) return(seasonder_apm_object)

  # Calculate new bearings for the left and right sides using the bearing resolution
  left_new <- seq(from = BEAR[1] - n * res, to = BEAR[1] - res, by = res)
  right_new <- seq(from = tail(BEAR, 1) + res, to = tail(BEAR, 1) + n * res, by = res)
  new_BEAR <- c(left_new, BEAR, right_new)

  # Extract the original measurement matrix
  M <- seasonder_apm_object

  # Extrapolate the left side using linear extrapolation based on the first two columns of M
  slope_left <- (M[, 2] - M[, 1]) / (BEAR[2] - BEAR[1])
  left_mat <- sapply(left_new, function(b) M[, 1] + slope_left * (b - BEAR[1]))
  # Ensure left_mat is a matrix with as many columns as new left bearings
  if(is.null(dim(left_mat))) { left_mat <- matrix(left_mat, ncol = length(left_new)) }

  # Extrapolate the right side using linear extrapolation based on the last two columns of M
  n_orig <- ncol(M)
  slope_right <- (M[, n_orig] - M[, n_orig - 1]) / (BEAR[length(BEAR)] - BEAR[length(BEAR) - 1])
  right_mat <- sapply(right_new, function(b) M[, n_orig] + slope_right * (b - BEAR[length(BEAR)]))
  if(is.null(dim(right_mat))) { right_mat <- matrix(right_mat, ncol = length(right_new)) }

  # Combine the left extrapolated columns, the original matrix and the right extrapolated columns
  new_M <- cbind(left_mat, M, right_mat)
  colnames(new_M) <- as.character(new_BEAR)
  attr(new_M, "BEAR") <- new_BEAR
  attr(new_M, "BearingResolution") <- res

  return(new_M)
}


##### Run #####


#Acción a realizar: write_code_comments_and_function_documentation.
#Detalles: Se ha finalizado el proceso de desarrollo de la función seasonder_extrapolateAPM con comentarios detallados y documentación de los pasos realizados. La documentación explica cómo se obtienen los bearings originales, cómo se generan los nuevos bearings y se realizan las extrapolaciones lineales en cada lado, y cómo se actualizan los atributos y nombres de columnas de la matriz.
#
#
#' Extrapolate SeaSondeR APM Matrix
#'
#' This function performs linear extrapolation on the SeaSondeR APM measurement matrix.
#' It adds \code{n} extrapolated columns to both the left and right sides of the matrix.
#'
#' The function retrieves the original bearing vector from the APM object using
#' \code{seasonder_getSeaSondeRAPM_BEAR} and obtains the bearing resolution (attribute
#' "BearingResolution"). If \code{n == 0}, the original matrix is returned unchanged.
#' For \code{n > 0}, new bearings are generated for both sides using the resolution. The
#' left side is extrapolated using the slope computed from the first two columns of the matrix,
#' and the right side is extrapolated using the slope from the last two columns. The new columns
#' are then combined with the original matrix, and the column names and the "BEAR" attribute
#' are updated to reflect the complete set of bearings.
#'
#' @param seasonder_apm_object A matrix containing SeaSondeR APM measurements. Its attributes
#'        include "BEAR" (numeric vector of bearings) and "BearingResolution" (numeric resolution).
#' @param n An integer specifying how many extrapolated columns to add on each side (default is 1).
#' @return A modified matrix with n extrapolated columns added to both sides. The column names
#'         and the "BEAR" attribute are updated with the new bearings, while the "BearingResolution"
#'         attribute remains unchanged.
#' @examples
#' \dontrun{
#'   # Create a dummy APM object
#'   dummy_mat <- matrix(1:15, nrow = 3)
#'   attr(dummy_mat, "BEAR") <- c(10, 20, 30)
#'   attr(dummy_mat, "BearingResolution") <- 10
#'   
#'   # Extrapolate 1 column on each side
#'   result <- seasonder_extrapolateAPM(dummy_mat, n = 1)
#' }
seasonder_extrapolateAPM <- function(seasonder_apm_object, n = 1) {

  # Retrieve the original BEAR vector and bearing resolution from the input object
  BEAR <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_object)
  res <- attr(seasonder_apm_object, "BearingResolution")

  # If no extrapolation is requested (n == 0), return the original object unchanged
  if(n == 0) return(seasonder_apm_object)

  # Generate new bearings for the left and right sides using the bearing resolution
  # left_new: sequence of new bearings to the left of the original (decreasing values)
  # right_new: sequence of new bearings to the right of the original (increasing values)
  left_new <- seq(from = BEAR[1] - n * res, to = BEAR[1] - res, by = res)
  right_new <- seq(from = tail(BEAR, 1) + res, to = tail(BEAR, 1) + n * res, by = res)
  new_BEAR <- c(left_new, BEAR, right_new)

  # Store the original measurement matrix in M
  M <- seasonder_apm_object

  # Extrapolate the left side using linear extrapolation based on the first two columns of M
  # Calculate the slope from the first two columns
  slope_left <- (M[, 2] - M[, 1]) / (BEAR[2] - BEAR[1])
  # For each new left bearing, extrapolate the value from the first column
  left_mat <- sapply(left_new, function(b) M[, 1] + slope_left * (b - BEAR[1]))
  # Ensure that left_mat is a matrix with the correct number of columns
  if(is.null(dim(left_mat))) { 
    left_mat <- matrix(left_mat, ncol = length(left_new)) 
  }

  # Extrapolate the right side using linear extrapolation based on the last two columns of M
  n_orig <- ncol(M)
  slope_right <- (M[, n_orig] - M[, n_orig - 1]) / (BEAR[length(BEAR)] - BEAR[length(BEAR) - 1])
  # For each new right bearing, extrapolate the value from the last column
  right_mat <- sapply(right_new, function(b) M[, n_orig] + slope_right * (b - BEAR[length(BEAR)]))
  if(is.null(dim(right_mat))) { 
    right_mat <- matrix(right_mat, ncol = length(right_new)) 
  }

  # Combine the left extrapolated columns, the original matrix, and the right extrapolated columns
  new_M <- cbind(left_mat, M, right_mat)
  
  # Update the column names to match the new bearings and update the BEAR attribute
  colnames(new_M) <- as.character(new_BEAR)
  attr(new_M, "BEAR") <- new_BEAR
  attr(new_M, "BearingResolution") <- res

  # Return the updated measurement matrix with extrapolated columns
  return(new_M)
}

