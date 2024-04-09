##### Validation #####



seasonder_validateFORMethod <- function(method) {

  method %in% c("SeaSonde") || seasonder_logAndAbort(glue::glue("Method '{method}' not implemented."), calling_function = "seasonder_validateFORMethod")

  invisible(method)

}

seasonder_defaultFOR_parameters <- function() {

  out <- list(nsm = 11,
              fdown = 7.5,
              flim = 4,
              noisefact = 15,
              currmax = 2,
              reject_distant_bragg = TRUE, #  Default is to apply this test
              reject_noise_ionospheric = TRUE, #  Default is to apply this test (except for 42 MHz)
              # TODO: implement default reject_noise_ionospheric = FALSE for 42 MHz
              reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
  )


  return(out)
}

seasonder_validateFOR_parameters <- function(seasonder_cs_obj, FOR_parameters, method = "SeaSonde") {

  seasonder_validateFORMethod(method)

  if (method == "SeaSonde") {

    FOR_parameters$nsm <- FOR_parameters$nsm %||% seasonder_defaultFOR_parameters()$nsm

    FOR_parameters$reference_noise_normalized_limits <- FOR_parameters$reference_noise_normalized_limits %||% seasonder_estimateReferenceNoiseNormalizedLimits(seasonder_cs_obj)


    FOR_parameters$fdown <- FOR_parameters$fdown %||% seasonder_defaultFOR_parameters()$fdown
    FOR_parameters$flim <- FOR_parameters$flim %||% seasonder_defaultFOR_parameters()$flim
    FOR_parameters$noisefact <- FOR_parameters$noisefact %||% seasonder_defaultFOR_parameters()$noisefact
    FOR_parameters$currmax <- FOR_parameters$currmax %||% seasonder_defaultFOR_parameters()$currmax
    FOR_parameters$reject_distant_bragg <- FOR_parameters$reject_distant_bragg %||% seasonder_defaultFOR_parameters()$reject_distant_bragg
    FOR_parameters$reject_noise_ionospheric <- FOR_parameters$reject_noise_ionospheric %||% seasonder_defaultFOR_parameters()$reject_noise_ionospheric
    FOR_parameters$reject_noise_ionospheric_threshold <- FOR_parameters$reject_noise_ionospheric_threshold %||% seasonder_defaultFOR_parameters()$reject_noise_ionospheric_threshold

  }

  invisible(FOR_parameters)




}

##### Setters #####



seasonder_setSeaSondeRCS_FOR_parameters <- function(seasonder_cs_obj, FOR_parameters) {



  FOR_parameters <- seasonder_validateFOR_parameters(seasonder_cs_obj, FOR_parameters)
  attr(seasonder_cs_obj, "FOR_data")$FOR_parameters  <- FOR_parameters

  return(seasonder_cs_obj)

}

seasonder_setSeaSondeRCS_FOR <- function(seasonder_cs_obj, FOR) {

  # TODO: validate FOR



  attr(seasonder_cs_obj, "FOR_data")$FOR <- FOR


  return(seasonder_cs_obj)


}

seasonder_setSeaSondeRCS_FOR_SS_Smoothed <- function(seasonder_cs_obj, FOR_SS_Smoothed) {

  # TODO: validate



  attr(seasonder_cs_obj, "FOR_data")$FOR_SS_Smoothed <- FOR_SS_Smoothed


  return(seasonder_cs_obj)


}

seasonder_setSeaSondeRCS_FOR_method <- function(seasonder_cs_obj, FOR_method) {

  FOR_method <- seasonder_validateFORMethod(FOR_method)



  attr(seasonder_cs_obj, "FOR_data")$FOR_method <- FOR_method


  return(seasonder_cs_obj)


}

seasonder_setSeaSondeRCS_FOR_MAXP <- function(seasonder_cs_obj, FOR_MAXP) {

  # TODO: Validate



  attr(seasonder_cs_obj, "FOR_data")$FOR_MAXP <- FOR_MAXP


  return(seasonder_cs_obj)


}

seasonder_setSeaSondeRCS_FOR_MAXP.bin <- function(seasonder_cs_obj, FOR_MAXP.bin) {

  # TODO: Validate



  attr(seasonder_cs_obj, "FOR_data")$FOR_MAXP.bin <- FOR_MAXP.bin


  return(seasonder_cs_obj)


}


seasonder_setSeaSondeRCS_NoiseLevel <- function(seasonder_cs_obj, NoiseLevel) {

  # TODO: validate
  attr(seasonder_cs_obj, "NoiseLevel") <- NoiseLevel


  return(seasonder_cs_obj)
}

##### Getters #####

# TODO: implement access to individual parameters:
# seasonder_getSeaSondeRCS_FOR_parameter <- function(seasonder_cs_object, FOR_parameter)

seasonder_getSeaSondeRCS_FOR_parameters <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "FOR_data", exact = TRUE)$FOR_parameters %||% seasonder_validateFOR_parameters(seasonder_cs_obj, list())


  return(out)


}

seasonder_getSeaSondeRCS_FOR_reference_noise_normalized_limits <- function(seasonder_cs_obj) {

  out <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$reference_noise_normalized_limits
  return(out)
}

seasonder_getSeaSondeRCS_FOR <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "FOR_data", exact = TRUE)$FOR %||% seasonder_initSeaSondeRCS_FOR(seasonder_cs_obj)


  return(out)


}


seasonder_getSeaSondeRCS_FOR_SS_Smoothed  <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "FOR_data", exact = TRUE)$FOR_SS_Smoothed


  return(out)


}


seasonder_getSeaSondeRCS_FOR_method  <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "FOR_data", exact = TRUE)$FOR_method %||% "SeaSonde"


  return(out)


}

seasonder_getSeaSondeRCS_NoiseLevel <- function(seasonder_cs_obj, dB = TRUE) {


  out <- attr(seasonder_cs_obj, "NoiseLevel", exact = TRUE) %||% numeric(0)
  if (length(out) > 0 && dB) {

    out <- seasonder_SelfSpectra2dB(seasonder_cs_obj = seasonder_cs_obj, out)
  }

  return(out)
}

seasonder_getSeaSondeRCS_FOR_reject_distant_bragg <- function(seasonder_cs_obj) {

  out <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$reject_distant_bragg %||% seasonder_defaultFOR_parameters()$reject_distant_bragg

  return(out)
}


seasonder_getSeaSondeRCS_FOR_reject_noise_ionospheric <- function(seasonder_cs_obj) {

  out <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$reject_noise_ionospheric %||% seasonder_defaultFOR_parameters()$reject_noise_ionospheric

  return(out)
}


seasonder_getSeaSondeRCS_FOR_reject_noise_ionospheric_threshold <- function(seasonder_cs_obj) {

  out <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$reject_noise_ionospheric_threshold %||% seasonder_defaultFOR_parameters()$reject_noise_ionospheric_threshold

  return(out)
}



##### FOR #####



seasonder_estimateReferenceNoiseNormalizedLimits <- function(seasonder_cs_obj) {


  freq <- seasonder_getDopplerBinsFrequency(seasonder_cs_obj, normalized = T)

  out <- max(freq)*c(0.565,1)

  return(out)

}




seasonder_computeNoiseLevel <- function(seasonder_cs_obj) {

  normalized_doppler_range <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$reference_noise_normalized_limits

  positive_doppler_range <- seasonder_SwapDopplerUnits(seasonder_cs_obj, sort(normalized_doppler_range), in_units = "normalized doppler frequency", out_units = "bins")

if(is.na(positive_doppler_range[2])){
  positive_doppler_range[2] <- seasonder_getnDopplerCells(seasonder_cs_obj)
}

  negative_doppler_range <- seasonder_SwapDopplerUnits(seasonder_cs_obj, sort(-1 * normalized_doppler_range), in_units = "normalized doppler frequency", out_units = "bins")

  if(is.na(negative_doppler_range[1])){
    negative_doppler_range[1] <- 1
  }

  SS3 <- seasonder_getSeaSondeRCS_SelfSpectra(seasonder_cs_obj, antennae = 3, doppler_ranges = list(negative = negative_doppler_range, positive = positive_doppler_range), collapse = TRUE)

  avg_noise <- cbind(SS3[[1]],SS3[[2]]) %>% rowMeans()



  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_NoiseLevel(avg_noise)


  return(seasonder_cs_obj)


}

seasonder_SmoothFORSS <- function(seasonder_cs_obj) {

  nsm <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$nsm

  SmoothSS <- seasonder_SmoothSS(seasonder_cs_obj, antenna = 3, smoothing = nsm)

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_SS_Smoothed(SmoothSS)

  return(seasonder_cs_obj)

}

seasonder_SmoothSS <- function(seasonder_cs_obj, antenna, smoothing = NULL) {

  nsm <- smoothing %||% seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$nsm

  SS <- seasonder_getSeaSondeRCS_antenna_SSdata(seasonder_cs_obj, antenna = antenna)


  if (nsm %% 2 != 0) { #odd
    after_bins <- before_bins <- (nsm - 1)/2
  }else{ # even

    after_bins  <- nsm/2
    before_bins <- nsm/2 - 1
  }

  out <- purrr::map(1:nrow(SS),\(i) {

    slider::slide_mean(abs(SS[i,,drop = TRUE]),after = after_bins,before = before_bins) %>% matrix(nrow = 1,byrow = T) %>% magrittr::set_rownames(rownames(SS)[i])

  }) %>% purrr::reduce(\(x,y) rbind(x,y)) %>% magrittr::set_colnames(colnames(SS))


  return(out)

}



seasonder_findFORNullsInFOR <- function(FOR, start_point_P, doppler_bins, left_region = FALSE) {



  if (left_region) {
    FOR %<>% rev()
    doppler_bins %<>% rev()
  }




  FOR %<>% abs() %>% magrittr::multiply_by(-1)
  start_point_P <- -1 * abs(start_point_P)


  FOR_index <- pracma::findpeaks(FOR,nups = 1, ndowns = 1, zero = "0",minpeakheight = start_point_P, npeaks = 1, sortstr = FALSE)[,2,drop = TRUE]

  FOR_bins <- doppler_bins[FOR_index]

  return(FOR_bins)



}

seasonder_findFORNullsInSpectrum <- function(seasonder_cs_obj, spectrum, doppler_bins, negative_Bragg_region = FALSE) {

  fdown <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$fdown

  # TODO: better center region removing
  sp <- -1 * abs(spectrum)
  dop_b <- doppler_bins
  if (negative_Bragg_region) {
    sp %<>% rev()
    dop_b %<>% rev()
  }



  center_region_index <- pracma::findpeaks(sp,nups = 2, ndowns = 2, zero = "0", npeaks = 1, sortstr = FALSE)[,2,drop = TRUE]

  dop_b <- dop_b[center_region_index:length(dop_b)]

  sp <- sp[center_region_index:length(sp)]

  sp <- abs(sp)

  if (negative_Bragg_region) {
    sp %<>% rev()
    dop_b %<>% rev()
  }


  MAXP <- max(sp, na.rm = TRUE)
  MAXP.bin <- which.max(sp)
  start_point_P <- MAXP / fdown

  right_FOR_index <- (MAXP.bin + 1):length(sp)
  right_FOR_sp <- sp[right_FOR_index]
  right_FOR_bins <- dop_b[right_FOR_index]

  left_FOR_index <- 1:(MAXP.bin - 1)
  left_FOR_sp <- sp[left_FOR_index]
  left_FOR_bins <- dop_b[left_FOR_index]

  right_FOL <- seasonder_findFORNullsInFOR(right_FOR_sp, start_point_P, doppler_bins = right_FOR_bins, left_region = FALSE)

  left_FOL <- seasonder_findFORNullsInFOR(left_FOR_sp, start_point_P, doppler_bins = left_FOR_bins, left_region = TRUE)

  has_left_FOL <- length(left_FOL) == 1 && !is.na(left_FOL)
  has_right_FOL <- length(right_FOL) == 1 && !is.na(right_FOL)

  out <- list(FOR = integer(0),MAXP = MAXP, MAXP.bin = MAXP.bin)

  if (has_left_FOL && has_right_FOL) {

    out$FOR <- seq(left_FOL, right_FOL)
  }
  return(out)

}

seasonder_findFORNullsInSSMatrix <- function(seasonder_cs_obj, SS, doppler_bins,  negative_Bragg_region = FALSE) {



  out <- purrr::map(seq_len(nrow(SS)), \(i) {
    spectrum <- SS[i,,drop = TRUE]

    result <- seasonder_findFORNullsInSpectrum(seasonder_cs_obj, spectrum, doppler_bins, negative_Bragg_region = negative_Bragg_region)

    return(result)

  }) %>% magrittr::set_names(rownames(SS))


  return(out)
}

seasonder_findFORNulls <- function(seasonder_cs_obj) {

  seasonder_cs_obj %<>% seasonder_SmoothFORSS()

  SS3_smoothed <- seasonder_getSeaSondeRCS_FOR_SS_Smoothed(seasonder_cs_obj)

  Center_Bin <- seasonder_getCenterDopplerBin(seasonder_cs_obj)

  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  SS3_smoothed_positive <- SS3_smoothed[,(Center_Bin + 1):nDoppler]

  SS3_smoothed_negative <- SS3_smoothed[,1:(Center_Bin - 1)]

  FOR_negative <- seasonder_findFORNullsInSSMatrix(seasonder_cs_obj, SS3_smoothed_negative, doppler_bins = 1:(Center_Bin - 1),  negative_Bragg_region = TRUE)

  FOR_positive <- seasonder_findFORNullsInSSMatrix(seasonder_cs_obj, SS3_smoothed_positive, doppler_bins = (Center_Bin + 1):nDoppler,  negative_Bragg_region = FALSE)




  FOR <- list(negative_FOR = FOR_negative, positive_FOR = FOR_positive)
  FOR <- purrr::transpose(FOR)


  MAXP <- FOR %>% purrr::map(\(x) x %>% purrr::map( \(peak) purrr::pluck(peak,"MAXP")) )

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_MAXP(MAXP)



  MAXP.bin <- FOR %>% purrr::map(\(x) x %>% purrr::map( \(peak) purrr::pluck(peak,"MAXP.bin")) )

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_MAXP.bin(MAXP.bin)

  FOR %<>% purrr::map(\(x) x %>% purrr::map( \(peak) purrr::pluck(peak,"FOR")) )

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR(FOR)

  return(seasonder_cs_obj)

}


seasonder_extractFOR <- function(seasonder_cs_obj, spectrum, FOR) {



  negative_FOR <- matrix(numeric(0), byrow = T)

  if (length(FOR$negative_FOR) > 0) {
    negative_FOR <- seasonder_extractSeaSondeRCS_dopplerRanges_from_SSdata(spectrum, FOR$negative_FOR)

  }

  positive_FOR <- matrix(numeric(0), byrow = T)

  if (length(FOR$positive_FOR) > 0) {
    positive_FOR <- seasonder_extractSeaSondeRCS_dopplerRanges_from_SSdata(spectrum, FOR$positive_FOR)

  }


  out <- list(negative_FOR = negative_FOR, positive_FOR = positive_FOR)

  return(out)

}

seasonder_filterFORAmplitudes <- function(seasonder_cs_obj) {

  FORs <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_FOR()

  flim <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$flim
  noisefact <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$noisefact

  seasonder_cs_obj %<>%  seasonder_computeNoiseLevel()

  noise_levels <- seasonder_getSeaSondeRCS_NoiseLevel(seasonder_cs_obj, dB = FALSE)

  noise_limit <- noise_levels * noisefact
  SS3 <- seasonder_getSeaSondeRCS_FOR_SS_Smoothed(seasonder_cs_obj) %>% abs()

  FORs_sp <- 1:length(FORs) %>% purrr::map(\(i) seasonder_extractFOR(seasonder_cs_obj,SS3[i,,drop = FALSE], FORs[[i]]) ) %>% magrittr::set_names(names(FORs))

  FOR_bins <- FORs %>% purrr::map(\(FOR) {





    positive_FOR <- FOR$positive_FOR



    negative_FOR <- FOR$negative_FOR


    list(negative_FOR = negative_FOR, positive_FOR = positive_FOR)

  })

  p_limit <- FORs_sp %>% purrr::map(\(FOR) FOR %>% purrr::map(\(x) if (all(dim(x) > 0)) max(x,na.rm = T)/flim else NA_real_ ))


  filtered_FORs <- purrr::pmap(list(FORs_sp, FOR_bins, p_limit, noise_limit), \(FOR, bins, p, noise) {



    if (length(bins$positive_FOR) > 0) {
      bins$positive_FOR <- bins$positive_FOR[FOR$positive_FOR > noise & FOR$positive_FOR > p$positive_FOR]
    }

    if (length(bins$negative_FOR) > 0) {
      bins$negative_FOR <- bins$negative_FOR[FOR$negative_FOR > noise & FOR$negative_FOR > p$negative_FOR]
    }


    bins
  })

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR(filtered_FORs)

  return(seasonder_cs_obj)
}


seasonder_limitFORCurrentRange <- function(seasonder_cs_obj) {

  currmax <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$currmax

  FOR <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_obj)

  rad_vel <- seasonder_getBinsRadialVelocity(seasonder_cs_obj)

  drop_rad_vel <- which(abs(rad_vel) >= currmax)

  FOR %<>% purrr::map(\(x) x %>% purrr::map(\(Bragg_peak) dplyr::setdiff(Bragg_peak,drop_rad_vel)))

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR(FOR)

  return(seasonder_cs_obj)

}

#' @details
#' Reject the First Order when the limits are farther away from the Bragg index than the width of the first order. This is implemented by setting the peak to integer(0).
seasonder_rejectDistantBraggPeakTest <- function(seasonder_cs_obj, peak, range = NA, peak_name = ""){

  # If there is a peak
  if (length(peak) > 0) {

    bragg_lines <- seasonder_getBraggLineBins(seasonder_cs_obj)

    # Calculates the width of the peak
    peak_width <- diff(range(peak))

    # Left boundary

    left_limit <- min(peak)

    # Right boundary

    right_limit <- max(peak)

    # Calculates the distance from the left boundary to all bragg_lines

    right_limit_distances <- abs(bragg_lines - right_limit)

    # Calculates the distance from the right boundary to all bragg_lines

    left_limit_distances <- abs(bragg_lines - left_limit)

    # Are all boundary distances to bragg peaks greater than the width?
    if (all(left_limit_distances > peak_width) && all(right_limit_distances > peak_width)) { # Yes: we reject the peak

      seasonder_logAndMessage(glue::glue("First Order Rejected at range {range}, peak {peak_name}. Distance Bragg test failed."), log_level = "info", calling_function = "seasonder_rejectDistantBraggPeakTest")

      # TODO: include speed range rejected in message
      # TODO: Include rejected index in FOR_data
      peak <- integer(0)

    }


  }
  return(peak)
}

#' Implements SpectraPlotterMap First Order Setting: Reject Bragg when First Order is far from index
#' @details
#' When applied, reject the First Order when the limits are farther away from the Bragg index than the width of the first order.
#' This should be turned off only when the site’s field of view over water is limited such that it sees only string positive current for any given time.
seasonder_rejectDistantBragg <- function(seasonder_cs_obj){


  FORs <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_obj)

  # This test is applied for each range and each negative and positive Bragg.
  FORs %<>% purrr::map2(1:length(FORs), \(FOR, FOR_range) FOR %>% purrr::map2(names(.), \(peak, peak_name) seasonder_rejectDistantBraggPeakTest(seasonder_cs_obj, peak, FOR_range, peak_name)))

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR(FORs)

  return(seasonder_cs_obj)

}


seasonder_rejectNoiseIonosphericTest <- function(seasonder_cs_obj, peak, range = NA, peak_name = ""){

  # Si hay pico

  if (length(peak) > 0) {



    reject_noise_ionospheric_threshold <- seasonder_getSeaSondeRCS_FOR_reject_noise_ionospheric_threshold(seasonder_cs_obj)

    # Obtener el bin central
    center_bin <- seasonder_getCenterDopplerBin(seasonder_cs_obj)

    # Obtener los límites izquierdo y derecho del FOR
    peak_limits <- range(peak)

    # comprobamos si peak está por encima o por debajo del centro. Y extraemos el espectro suavizado del lado correspondiente.



    ss_smoothed <- seasonder_getSeaSondeRCS_FOR_SS_Smoothed(seasonder_cs_obj)[range,,drop = TRUE]

    half_spectrum <- seq(1,center_bin - 1)

    if (all(peak_limits > center_bin)) {
      half_spectrum <- seq(center_bin + 1, length(ss_smoothed))
    }

    # Obtener la región NonBragg eliminando de la región positive/negativa los bins fuera del FOR

    non_bragg <- dplyr::setdiff(half_spectrum, peak)

    # calcular potencia total NonBragg

    ## sumando los valores de potencia

    non_bragg_total <- sum(ss_smoothed[non_bragg], na.rm = T)

    ## pasamos a dB.

    non_bragg_power <- seasonder_SelfSpectra2dB(seasonder_cs_obj,non_bragg_total)


    # calcular potencia total Bragg sumando los valores del espectro dentro de la FOR y pasando a dB

    bragg_total <- sum(ss_smoothed[seq(peak_limits[1],peak_limits[2])], na.rm = T)

    bragg_power <- seasonder_SelfSpectra2dB(seasonder_cs_obj,bragg_total)

    # ¿Es la potencia Bragg más el threshold menor que la potencia NonBragg?
    if ((bragg_power + reject_noise_ionospheric_threshold) < non_bragg_power) {# Si: rechazamos el peak

      seasonder_logAndMessage(glue::glue("First Order Rejected at range {range}, peak {peak_name}. Noise/Ionospheric test failed."), log_level = "info", calling_function = "seasonder_rejectNoiseIonosphericTest")


      # TODO: Include rejected index in FOR_data
      peak <- integer(0)

    }

  }


  return(peak)

}


#' @details
#' When applied, reject the Negative and/or Positive Bragg when the total external (Non Bragg) is greater then the Bragg power by the dB amount entered.
#'
seasonder_rejectNoiseIonospheric <- function(seasonder_cs_obj){

  FORs <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_obj)

  # This test is applied for each range and each negative and positive Bragg.
  FORs %<>% purrr::map2(1:length(FORs), \(FOR, FOR_range) FOR %>% purrr::map2(names(.), \(peak, peak_name) seasonder_rejectNoiseIonosphericTest(seasonder_cs_obj, peak, FOR_range, peak_name)))

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR(FORs)

  return(seasonder_cs_obj)

}

seasonder_computeFORsSeaSondeMethod <- function(seasonder_cs_obj) {


  seasonder_cs_obj %<>% seasonder_findFORNulls()

  seasonder_cs_obj %<>% seasonder_filterFORAmplitudes()

  seasonder_cs_obj %<>% seasonder_limitFORCurrentRange()

  # Reject distant Bragg peaks.
  if (seasonder_getSeaSondeRCS_FOR_reject_distant_bragg(seasonder_cs_obj)) {
    seasonder_cs_obj %<>% seasonder_rejectDistantBragg()
  }

  # Reject Noise/Ionospheric Test.

  if (seasonder_getSeaSondeRCS_FOR_reject_noise_ionospheric(seasonder_cs_obj)) {

    seasonder_cs_obj %<>% seasonder_rejectNoiseIonospheric()
  }

  return(seasonder_cs_obj)

}

#' @export
seasonder_computeFORs <- function(seasonder_cs_obj, method = NULL, FOR_control = NULL) {

  if (!is.null(method)) {
    seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_method(method)
  }

  method <- seasonder_getSeaSondeRCS_FOR_method(seasonder_cs_obj)

  if (!is.null(FOR_control)) {
    seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_parameters(FOR_control)
  }

  if (method == "SeaSonde") {
    seasonder_cs_obj %<>%  seasonder_computeFORsSeaSondeMethod()
  }

  return(seasonder_cs_obj)
}
