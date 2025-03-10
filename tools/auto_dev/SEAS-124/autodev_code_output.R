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
