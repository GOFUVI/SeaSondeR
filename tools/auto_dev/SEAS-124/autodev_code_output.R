# Helper function to apply sign corrections to spectra and autospectra
applySignsToData <- function(data, cellSign, autoSign) {
  # Check and apply the cell sign to spectra
  if (!is.null(data$spectra)) {
    data$spectra <- data$spectra * cellSign
  } else {
    warning("Spectra data not found in the input object.")
  }
  
  # Check and apply the auto sign to autospectra
  if (!is.null(data$autospectra)) {
    data$autospectra <- data$autospectra * autoSign
  } else {
    warning("Autospectra data not found in the input object.")
  }
  
  return(data)
}

# Main function to read SeaSonde RCSSY File, which integrates sign corrections
seasonder_readSeaSondeRCSSYFile <- function(filePath) {
  # Read raw data (spectra and autospectra) from the SeaSonde RCSSY file
  rawData <- seasonder_CSSY2CSData(filePath)
  
  # Read the sign information for spectra and autospectra
  cellSign <- seasonder_read_csign(filePath)
  autoSign <- seasonder_read_asign(filePath)
  
  # Apply sign corrections to the raw data using the helper function
  correctedData <- applySignsToData(rawData, cellSign, autoSign)
  
  return(correctedData)
}

