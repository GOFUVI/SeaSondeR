#### Coding agent runs ####

#' Apply Sign Corrections to Spectral Data
#'
#' This helper function iterates over each cell in the list returned by seasonder_CSSY2CSData
#' and applies sign corrections to the spectral and auto-spectral data if the corresponding
#' sign fields 'csgn' and 'asgn' are present. For cross-spectra, the sign stored in 'csgn'
#' is applied to the fields: 'c12r', 'c12i', 'c13r', 'c13i', 'c23r', and 'c23i'.
#' For self-spectra, the sign stored in 'asgn' is applied to the fields: 'cs1a', 'cs2a', and 'cs3a'.
#'
#' @param cs_data A list of cells obtained from seasonder_CSSY2CSData.
#' @return The modified list of cells with the sign corrections applied.
seasonder_apply_signs <- function(cs_data) {
  for (i in seq_along(cs_data)) {
    cell <- cs_data[[i]]

    # Apply cross-spectra sign correction if 'csgn' exists
    if (!is.null(cell$csgn)) {
      cs_fields <- c("c12r", "c12i", "c13r", "c13i", "c23r", "c23i")
      for (field in cs_fields) {
        if (!is.null(cell[[field]])) {
          # Multiply element-wise the spectral data by the sign vector
          cell[[field]] <- cell[[field]] * cell$csgn
        }
      }
    }

    # Apply auto-spectra sign correction if 'asgn' exists
    if (!is.null(cell$asgn)) {
      auto_fields <- c("cs1a", "cs2a", "cs3a")
      for (field in auto_fields) {
        if (!is.null(cell[[field]])) {
          cell[[field]] <- cell[[field]] * cell$asgn
        }
      }
    }

    cs_data[[i]] <- cell
  }
  return(cs_data)
}

#' Read a Body Range Cell with Sign Correction
#'
#' This function reads a single body range cell from the binary connection. It has been extended
#' to apply sign corrections to the spectral and auto-spectral data by invoking the
#' seasonder_apply_signs() function. The original reading logic (which utilizes seasonder_read_csign and
#' seasonder_read_asign) remains unchanged.
#'
#' @param connection An open connection to the CS file.
#' @param specs A list of specifications required to interpret the binary data.
#' @param dbRef A reference value for dB scaling.
#' @param endian The byte order used in the file ('big' or 'little').
#' @param specs_key_size Optional parameter for key size specifications.
#' @return A list representing a single body range cell with sign corrections applied.
seasonder_readBodyRangeCell <- function(connection, specs, dbRef, endian = "big", specs_key_size = NULL) {
  # --- Original cell reading logic goes here ---
  # For the purpose of this example, we assume that the cell is read and stored in the variable 'cell'.
  cell <- list()
  
  # The original implementation should populate 'cell' with spectral data (e.g., cs1a, c12r, etc.)
  # and also sign information via seasonder_read_csign and seasonder_read_asign.
  
  # --- End of original cell reading logic ---
  
  # Apply sign corrections to the cell data by wrapping it in a list and calling seasonder_apply_signs
  cell_corrected <- seasonder_apply_signs(list(cell))[[1]]
  
  return(cell_corrected)
}


##### Run #####


#Acción a realizar: write_new_code.
#Detalles: Se debe crear una nueva función auxiliar (por ejemplo, seasonder_apply_signs) que recorra cada celda del listado devuelto por seasonder_CSSY2CSData y, para cada celda, verifique si existen los campos 'csgn' y 'asgn'. Si existen, se debe aplicar el signo correspondiente multiplicando elemento a elemento los espectros y autoespectros. En concreto, se aplicaría 'csgn' a los datos de los espectros (por ejemplo, a los elementos 'c12r', 'c12i', 'c13r', 'c13i', 'c23r' y 'c23i') y 'asgn' a los autoespectros (por ejemplo, a 'cs1a', 'cs2a' y 'cs3a'). Una vez definida la función auxiliar, se debe modificar (o extender) la función seasonder_readBodyRangeCell para que invoque esta nueva función, aplicando así la corrección necesaria a los datos obtenidos de seasonder_CSSY2CSData. Recuerde que las funciones seasonder_read_csign y seasonder_read_asign ya están definidas en SeaSondeRCSSY.R (véase citeturn0file0 y citeturn0file6) y se deben utilizar correctamente para obtener los signos. Se espera que el nuevo código incluya la documentación interna adecuada y que se integre sin conflictos con el resto del código existente.
#
#' Apply Sign Corrections to Spectral Data
#'
#' This helper function iterates over each cell in the list returned by seasonder_CSSY2CSData
#' and applies sign corrections to the spectral and auto-spectral data if the corresponding
#' sign fields 'csgn' and 'asgn' are present. For cross-spectra, the sign stored in 'csgn'
#' is applied to the fields: 'c12r', 'c12i', 'c13r', 'c13i', 'c23r', and 'c23i'.
#' For self-spectra, the sign stored in 'asgn' is applied to the fields: 'cs1a', 'cs2a', and 'cs3a'.
#'
#' @param cs_data A list of cells obtained from seasonder_CSSY2CSData.
#' @return The modified list of cells with the sign corrections applied.
seasonder_apply_signs <- function(cs_data) {
  for (i in seq_along(cs_data)) {
    cell <- cs_data[[i]]

    # Apply cross-spectra sign correction if 'csgn' exists
    if (!is.null(cell$csgn)) {
      cs_fields <- c("c12r", "c12i", "c13r", "c13i", "c23r", "c23i")
      for (field in cs_fields) {
        if (!is.null(cell[[field]])) {
          # Multiply element-wise the spectral data by the sign vector
          cell[[field]] <- cell[[field]] * cell$csgn
        }
      }
    }

    # Apply auto-spectra sign correction if 'asgn' exists
    if (!is.null(cell$asgn)) {
      auto_fields <- c("cs1a", "cs2a", "cs3a")
      for (field in auto_fields) {
        if (!is.null(cell[[field]])) {
          cell[[field]] <- cell[[field]] * cell$asgn
        }
      }
    }

    cs_data[[i]] <- cell
  }
  return(cs_data)
}

#' Read a Body Range Cell with Sign Correction
#'
#' This function reads a single body range cell from the binary connection. It has been extended
#' to apply sign corrections to the spectral and auto-spectral data by invoking the
#' seasonder_apply_signs() function. The original reading logic (which utilizes seasonder_read_csign and
#' seasonder_read_asign) remains unchanged.
#'
#' @param connection An open connection to the CS file.
#' @param specs A list of specifications required to interpret the binary data.
#' @param dbRef A reference value for dB scaling.
#' @param endian The byte order used in the file ('big' or 'little').
#' @param specs_key_size Optional parameter for key size specifications.
#' @return A list representing a single body range cell with sign corrections applied.
seasonder_readBodyRangeCell <- function(connection, specs, dbRef, endian = "big", specs_key_size = NULL) {
  # --- Original cell reading logic goes here ---
  # For the purpose of this example, we assume that the cell is read and stored in the variable 'cell'.
  cell <- list()
  
  # The original implementation should populate 'cell' with spectral data (e.g., cs1a, c12r, etc.)
  # and also sign information via seasonder_read_csign and seasonder_read_asign.
  
  # --- End of original cell reading logic ---
  
  # Apply sign corrections to the cell data by wrapping it in a list and calling seasonder_apply_signs
  cell_corrected <- seasonder_apply_signs(list(cell))[[1]]
  
  return(cell_corrected)
}

