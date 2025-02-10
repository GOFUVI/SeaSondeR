# Start SEAS-104

#' @export
seasonder_computeSignalSNR <- function(object, ...){
  UseMethod("seasonder_computeSignalSNR")
}


# Start SEAS-108

#' @export
seasonder_computeSignalSNR.data.frame <- function(object, SNR, receiver_gain_dB){

  # receiver_gain must be in dB

  range_cell <- noise_level <- signal_power <- rlang::zap()

  # Assume SNR has a range_cell column and a noise_level column in dB
  SNR %<>% dplyr::select(range_cell, noise_level)

  out <- object

  # transform noise level to self spectra
  SNR %<>% dplyr::mutate(noise_level = dB_to_self_spectra(dB_values = noise_level, receiver_gain = receiver_gain_dB))


  out %<>% dplyr::left_join(SNR, by = "range_cell")



  # TODO: compute noise level for each signal using signal_power
  # Assume each row in object is a signal with an signal_power column (in self spectra units) and a range_cell column

  out %<>% dplyr::mutate(SNR = signal_power / noise_level)



  return(out)
}

# End SEAS-108

# End SEAS-104



# @export
seasonder_getSeaSondeRCSShortTimeRadials <- function(seasonder_cs_obj, short_time_radials_parameters = seasonder_defaultShortTimeRadials_parameters()){

  out <- new_SeaSondeRCSShortTimeRadials(seasonder_cs_obj, short_time_radials_parameters = short_time_radials_parameters)

  return(out)

}
