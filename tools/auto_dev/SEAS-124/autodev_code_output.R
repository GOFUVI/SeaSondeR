# Auxiliary function: Applies the sign factors to the spectra and autospectra data
seasonder_apply_signs <- function(spectra, autospectra, cSign, aSign) {
  # Apply the sign to the spectra and autospectra data.
  # It multiplies the spectra with the cSign and the autospectra with the aSign.
  # It assumes that the sign values (cSign and aSign) are either scalars or conformable vectors/matrices.
  modifiedSpectra <- spectra * cSign
  modifiedAutospectra <- autospectra * aSign
  
  # Return the modified data as a list
  return(list(spectra = modifiedSpectra, autospectra = modifiedAutospectra))
}

# Main function: Reads the SeaSonde RCSSY file, processes the data and applies the sign corrections
seasonder_readSeaSondeRCSSYFile <- function(file_path) {
  # Read spectra and autospectra data using seasonder_CSSY2CSData
  data <- seasonder_CSSY2CSData(file_path)
  
  # Read the sign values using the corresponding functions
  cSign <- seasonder_read_csign(file_path)
  aSign <- seasonder_read_asign(file_path)
  
  # Apply the sign corrections to the data using the auxiliary function
  modifiedData <- seasonder_apply_signs(data$spectra, data$autospectra, cSign, aSign)
  
  # Return the sign-corrected data
  return(modifiedData)
}

