#### Coding agent runs ####

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


##### Run #####


#Acción a realizar: write_new_code.
#Detalles: Se solicita implementar la función auxiliar en R que aplique los signos (leídos por seasonder_read_csign y seasonder_read_asign) a los espectros y autoespectros devueltos por seasonder_CSSY2CSData, y luego llamar a dicha función desde seasonder_readSeaSondeRCSSYFile. Se deberá definir claramente la firma de la función auxiliar, su comportamiento y las expectativas de entrada y salida, permitiendo así la integración en seasonder_readSeaSondeRCSSYFile.
#
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

