##### Validation #####



#' Validate First Order Region (FOR) Processing Method
#'
#' This function checks whether the specified method for First Order Region (FOR) detection
#' is supported. If an unsupported method is provided, it logs an error and aborts execution.
#'
#' @param method A character string specifying the FOR processing method. Currently, only `"SeaSonde"` is supported.
#'
#' @details
#' The function verifies that the `method` argument is valid. If the method is not recognized,
#' an error is raised using \code{\link{seasonder_logAndAbort}}.
#'
#' **Supported Methods:**
#' - `"SeaSonde"`: Implements first-order region detection based on CODAR's SeaSonde methodology.
#'
#' @return The function returns the input \code{method} invisibly if it is valid.
#'
#' @seealso
#' - \code{\link{seasonder_validateFOR_parameters}} for FOR parameter validation.
#' - \code{\link{seasonder_logAndAbort}} for error handling and logging.
#'
#' @examples
#' \dontrun{
#' # Validate a correct method
#' seasonder_validateFORMethod("SeaSonde")
#'
#' # Attempting to use an invalid method will trigger an error
#' seasonder_validateFORMethod("InvalidMethod") # This will abort execution
#' }
seasonder_validateFORMethod <- function(method) {

  # Check if the provided method is in the list of supported methods
  method %in% c("SeaSonde") ||
    seasonder_logAndAbort(
      glue::glue("Method '{method}' not implemented."),
      calling_function = "seasonder_validateFORMethod"
    )

  # Return the method invisibly if it is valid
  invisible(method)
}


#' Default First-Order Radial Processing Parameters
#'
#' This function returns a list of default parameters for first-order
#' radial processing in CODAR's Radial Suite R7. Each parameter has an equivalent
#' R8 version, where applicable, often expressed in decibels (dB).
#'
#' @details
#' **Parameter Descriptions:**
#'
#' 1. **nsm (R8: Doppler Smoothing)**
#'
#'    - **Default value:** 2
#'    - **Usage:** Sets how many Doppler bins (points) are smoothed. Smoothing helps
#'      remove jagged edges in the sea echo spectrum, aiding in locating the null
#'      between the first and second order (or noise floor).
#'    - **Recommended values:** Typically 2 to 6. Default in Radial Suite R8 is 2
#'    - **Effects of over-/under-smoothing:**
#'      - Too high: May smear out the real null, causing the first order to appear
#'        wider.
#'      - Too low: Jagged minima may cause the null to be detected inside the
#'        first-order region, making it appear too narrow.
#'
#' 2. **fdown (R8: Null Below Peak Power)**
#'
#'    - **Default value (R7):** 10
#'    - **Equivalent in dB (R8):** 10 dB
#'    - **Usage:** Defines how far below the peak power the algorithm must descend
#'      (in dB) before searching for the null that separates the first and second
#'      order. This helps avoid including second-order energy as part of the
#'      first-order.
#'    - **Recommended range:** 3.981072 to 31.62278 (6 to 15 dB in R8). Default in Radial Suite R8 is 10 dB
#'    - **Effects of misconfiguration:**
#'      - Too large: The null search may be bypassed entirely, causing
#'        second-order content to be included in the first order.
#'      - Too small: The null may be found inside the first-order region,
#'        excluding valid Bragg energy.
#'
#' 3. **flim (R8: Peak Power Dropoff)**
#'
#'    - **Default value (R7):** 100
#'    - **Equivalent in dB (R8):** 20 dB
#'    - **Usage:** Once a peak is located, any spectral bins that are more than
#'      `flim` below the peak (in linear scale) or `20 dB` below the peak (in dB
#'      scale) are excluded from the first-order region.
#'    - **Recommended range:** 15.84893 to 316.2278 (12 to 25 dB in R8). Default in Radial Suite R8 is 20 dB.
#'    - **Effects of misconfiguration:**
#'      - Too high: May include non-Bragg signal and yield spurious high
#'        velocity estimates.
#'      - Too low: May cut out part of the actual Bragg signal, underestimating
#'        maximum velocities.
#'
#' 4. **noisefact (R8: Signal to Noise)**
#'
#'    - **Default value (R7):** 3.981072
#'    - **Equivalent in dB (R8):** 6 dB
#'    - **Usage:** Sets the threshold above the noise floor that must be exceeded
#'      for the algorithm to accept Doppler bins as potential first-order.
#'    - **Recommended range:** 3.981072 to 7.943282 (6 to 9 dB in R8). Default in Radial Suite R8 is 6 dB
#'    - **Effects of misconfiguration:**
#'      - Too high: Useful Bragg data could be excluded.
#'      - Too low: Noise or spurious signals may be included as Bragg.
#'
#' 5. **currmax (R8: Maximum Velocity)**
#'
#'    - **Default value:** 2 m/s
#'    - **Usage:** Sets a maximum radial velocity,
#'      preventing first-order limits from extending beyond realistic current
#'      speeds for the site.
#'    - **Effects of misconfiguration:**
#'      - Too high: May include non-Bragg data, producing overestimated
#'        velocities.
#'      - Too low: May exclude valid Bragg data, underestimating velocities.
#'
#' 6. **reject_distant_bragg (Reject Distant Bragg)**
#'
#'    - **Default value:** TRUE
#'    - **Usage:** Rejects a first-order region if its limits are farther from
#'      the Bragg index (central Doppler bin for zero current) than the width of
#'      the region itself. Helps avoid misclassifying strong but isolated signals
#'      (e.g., ships) as Bragg.
#'    - **Recommendation:** Usually keep this enabled unless operating at a site
#'      where only strongly biased positive or negative currents are expected.
#'
#' 7. **reject_noise_ionospheric (Reject Noise/Ionospheric)**
#'
#'    - **Default value:** TRUE
#'    - **Usage:** Rejects Bragg if the total non-Bragg power in a range cell
#'      exceeds the Bragg power by at least the threshold set in
#'      `reject_noise_ionospheric_threshold`. Recommended to set as FALSE for 42 MHz systems.
#'    - **Recommendation:** Enable if the site experiences significant noise.
#'
#' 8. **reject_noise_ionospheric_threshold (Reject Noise/Ionospheric Threshold)**
#'
#'    - **Default value:** 0
#'    - **Equivalent in dB:** 0 dB
#'    - **Usage:** Difference threshold (in dB) for comparing non-Bragg power to
#'      Bragg power. If non-Bragg power is higher by this threshold, the Bragg is
#'      rejected.
#'    - **Recommended setting:** Typically 0 dB. Increase only if needed to be
#'      less sensitive to noise contamination.
#'
#' @return A named list containing the default parameter values.
#'
#' @references
#'
#' COS. SeaSonde Radial Suite Release 7; CODAR Ocean Sensors (COS): Mountain View, CA, USA, 2013.
#' COS. SeaSonde Radial Suite Release 8; CODAR Ocean Sensors (COS): Mountain View, CA, USA, 2016.
#'
#' @examples
#' params <- seasonder_defaultFOR_parameters()
#' print(params)
#'
#' @export
seasonder_defaultFOR_parameters <- function() {

  out <- list(
    nsm = 2,    # R8: Doppler Smoothing
    fdown = 10, # R8: Null Below Peak Power (10 dB)
    flim = 100, # R8: Peak Power Dropoff (20 dB)
    noisefact = 3.981072, # R8: Signal to Noise (6 dB)
    currmax = 2,          # R8: Maximum Velocity
    reject_distant_bragg = TRUE,    # Reject Distant Bragg
    reject_noise_ionospheric = TRUE, # Reject Noise/Ionospheric
    # TODO: implement default reject_noise_ionospheric = FALSE for 42 MHz
    reject_noise_ionospheric_threshold = 0 # 0 dB threshold
  )

  return(out)
}

#' Validate First Order Region (FOR) Parameters
#'
#' This function validates and assigns default values to the parameters used in defining
#' the First Order Region (FOR) in a SeaSondeR cross-spectral object. It ensures that all
#' necessary parameters are present and assigns appropriate defaults where values are missing.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing metadata about the Doppler spectrum.
#' @param FOR_parameters A named list containing the parameters for first-order region detection.
#' @param method A character string specifying the validation method. Default is \code{"SeaSonde"}.
#'        Currently, only "SeaSonde" is supported.
#'
#' @details
#' The function validates FOR parameters and assigns default values where necessary.
#' If the selected method is "SeaSonde", the function ensures that each parameter is
#' defined and, if missing, assigns it a default value based on \code{\link{seasonder_defaultFOR_parameters}}.
#'
#' The parameters validated include:
#'
#' - **nsm** (Doppler Smoothing): Number of points used for spectral smoothing.
#' - **fdown** (Peak Power Dropoff): Defines how far below peak power the algorithm descends before searching for the null.
#' - **flim** (Null Below Peak Power): Specifies a power threshold for identifying the first-order region.
#' - **noisefact** (Signal to Noise): Threshold above the noise floor that a spectral bin must exceed to be considered first-order.
#' - **currmax** (Maximum Velocity): Maximum radial velocity allowed in the first-order region.
#' - **reject_distant_bragg**: Logical flag indicating whether to reject Bragg regions that are too distant from the central Bragg frequency.
#' - **reject_noise_ionospheric**: Logical flag indicating whether to reject Bragg regions affected by ionospheric noise.
#' - **reject_noise_ionospheric_threshold**: Threshold (in dB) for rejecting first-order regions based on noise contamination.
#' - **reference_noise_normalized_limits**: Estimated reference noise range in normalized Doppler frequency,
#'   computed using \code{\link{seasonder_estimateReferenceNoiseNormalizedLimits}}.
#'
#' @return A named list containing validated and completed FOR parameters.
#'
#' @seealso
#' - \code{\link{seasonder_defaultFOR_parameters}} for default FOR settings.
#' - \code{\link{seasonder_estimateReferenceNoiseNormalizedLimits}} for computing reference noise limits.
#' - \code{\link{seasonder_validateFORMethod}} for validating the processing method.
#'
#' @examples
#' \dontrun{
#' # Validate and complete FOR parameters for a SeaSondeRCS object
#' validated_params <- seasonder_validateFOR_parameters(cs_obj, list(fdown = 12))
#' print(validated_params)
#' }
seasonder_validateFOR_parameters <- function(seasonder_cs_obj, FOR_parameters, method = "SeaSonde") {

  # Validate that the provided method is supported
  seasonder_validateFORMethod(method)

  # Ensure that the method is "SeaSonde" before proceeding with parameter validation
  if (method == "SeaSonde") {

    # Assign default value for Doppler Smoothing if not provided
    FOR_parameters$nsm <- FOR_parameters$nsm %||% seasonder_defaultFOR_parameters()$nsm

    # Assign default reference noise limits if not provided
    FOR_parameters$reference_noise_normalized_limits <- FOR_parameters$reference_noise_normalized_limits %||%
      seasonder_estimateReferenceNoiseNormalizedLimits(seasonder_cs_obj)

    # Assign default values for first-order detection parameters if missing
    FOR_parameters$fdown <- FOR_parameters$fdown %||% seasonder_defaultFOR_parameters()$fdown
    FOR_parameters$flim <- FOR_parameters$flim %||% seasonder_defaultFOR_parameters()$flim
    FOR_parameters$noisefact <- FOR_parameters$noisefact %||% seasonder_defaultFOR_parameters()$noisefact
    FOR_parameters$currmax <- FOR_parameters$currmax %||% seasonder_defaultFOR_parameters()$currmax
    FOR_parameters$reject_distant_bragg <- FOR_parameters$reject_distant_bragg %||% seasonder_defaultFOR_parameters()$reject_distant_bragg
    FOR_parameters$reject_noise_ionospheric <- FOR_parameters$reject_noise_ionospheric %||% seasonder_defaultFOR_parameters()$reject_noise_ionospheric
    FOR_parameters$reject_noise_ionospheric_threshold <- FOR_parameters$reject_noise_ionospheric_threshold %||%
      seasonder_defaultFOR_parameters()$reject_noise_ionospheric_threshold
  }

  # Return the validated FOR parameters without printing them explicitly (invisible)
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

#' Set Smoothed Self-Spectra for First Order Region (FOR)
#'
#' This function assigns a smoothed self-spectra (SS) matrix to the First Order Region (FOR) data
#' within a \code{SeaSondeRCS} object. This smoothed matrix is used in FOR processing to improve
#' the detection of the first-order region.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object to which the smoothed FOR self-spectra will be assigned.
#' @param FOR_SS_Smoothed A matrix containing the smoothed self-spectra data.
#'
#' @details
#' The function assigns the provided smoothed self-spectra matrix to the \code{FOR_data} attribute
#' of the \code{SeaSondeRCS} object. This matrix is typically generated using
#' \code{\link{seasonder_SmoothSS}} and applied to antenna 3.
#'
#' **Validation Considerations:**
#' - The function currently lacks explicit validation for \code{FOR_SS_Smoothed}.
#' - Future improvements should include checking whether \code{FOR_SS_Smoothed} is a matrix and
#'   ensuring its dimensions match the original self-spectra structure.
#'
#' @return The updated \code{SeaSondeRCS} object with the smoothed self-spectra stored in the \code{FOR_data} attribute.
#'
#' @seealso
#' - \code{\link{seasonder_SmoothSS}} for generating the smoothed self-spectra.
#' - \code{\link{seasonder_SmoothFORSS}} for applying smoothing and setting the result.
#'
#' @examples
#' \dontrun{
#' # Smooth and assign self-spectra for FOR processing
#' smoothed_SS <- seasonder_SmoothSS(cs_obj, antenna = 3)
#' cs_obj <- seasonder_setSeaSondeRCS_FOR_SS_Smoothed(cs_obj, smoothed_SS)
#' }
seasonder_setSeaSondeRCS_FOR_SS_Smoothed <- function(seasonder_cs_obj, FOR_SS_Smoothed) {

  # TODO: Implement validation to check that FOR_SS_Smoothed is a valid matrix

  # Assign the smoothed self-spectra matrix to the FOR_data attribute of the SeaSondeRCS object
  attr(seasonder_cs_obj, "FOR_data")$FOR_SS_Smoothed <- FOR_SS_Smoothed

  # Return the updated object
  return(seasonder_cs_obj)
}


seasonder_setSeaSondeRCS_FOR_method <- function(seasonder_cs_obj, FOR_method) {

  FOR_method <- seasonder_validateFORMethod(FOR_method)



  attr(seasonder_cs_obj, "FOR_data")$FOR_method <- FOR_method


  return(seasonder_cs_obj)


}

#' Set Maximum Power (MAXP) for First Order Region (FOR)
#'
#' This function assigns the computed maximum power values (\code{MAXP}) for each range cell
#' in the First Order Region (FOR) to the \code{SeaSondeRCS} object.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object to which the \code{MAXP} values will be assigned.
#' @param FOR_MAXP A list containing the maximum power values for each range cell.
#'
#' @details
#' The maximum power (\code{MAXP}) represents the highest spectral power detected in the first-order region.
#' This value is extracted from the self-spectra and used for setting first-order boundaries.
#'
#' **Validation Considerations:**
#' - The function does not currently perform explicit validation on \code{FOR_MAXP}.
#' - Future improvements should ensure that \code{FOR_MAXP} contains numeric values corresponding to each range cell.
#'
#' @return The updated \code{SeaSondeRCS} object with the \code{MAXP} values stored in the \code{FOR_data} attribute.
#'
#' @seealso
#' - \code{\link{seasonder_findFORNulls}} for computing \code{MAXP}.
#' - \code{\link{seasonder_setSeaSondeRCS_FOR_MAXP.bin}} for setting maximum power bin indices.
#'
#' @examples
#' \dontrun{
#' # Assign maximum power values to a SeaSondeRCS object
#' cs_obj <- seasonder_setSeaSondeRCS_FOR_MAXP(cs_obj, MAXP_values)
#' }
seasonder_setSeaSondeRCS_FOR_MAXP <- function(seasonder_cs_obj, FOR_MAXP) {

  # TODO: Implement validation to check that FOR_MAXP is a valid list of numeric values

  # Assign the maximum power values to the FOR_data attribute of the SeaSondeRCS object
  attr(seasonder_cs_obj, "FOR_data")$FOR_MAXP <- FOR_MAXP

  # Return the updated object
  return(seasonder_cs_obj)
}

#' Set Maximum Power Bin Indices for First Order Region (FOR)
#'
#' This function assigns the Doppler bin indices corresponding to the maximum power (\code{MAXP.bin})
#' for each range cell in the First Order Region (FOR) to the \code{SeaSondeRCS} object.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object to which the \code{MAXP.bin} values will be assigned.
#' @param FOR_MAXP.bin A list containing the Doppler bin indices of the maximum power for each range cell.
#'
#' @details
#' The maximum power bin (\code{MAXP.bin}) represents the Doppler bin index at which the highest spectral power
#' was detected in the first-order region. This information is used to refine first-order boundary detection.
#'
#' **Validation Considerations:**
#' - The function does not currently validate the format of \code{FOR_MAXP.bin}.
#' - Future improvements should ensure that \code{FOR_MAXP.bin} consists of integer values corresponding to Doppler bins.
#'
#' @return The updated \code{SeaSondeRCS} object with the \code{MAXP.bin} values stored in the \code{FOR_data} attribute.
#'
#' @seealso
#' - \code{\link{seasonder_findFORNulls}} for computing \code{MAXP.bin}.
#' - \code{\link{seasonder_setSeaSondeRCS_FOR_MAXP}} for setting maximum power values.
#'
#' @examples
#' \dontrun{
#' # Assign maximum power bin indices to a SeaSondeRCS object
#' cs_obj <- seasonder_setSeaSondeRCS_FOR_MAXP.bin(cs_obj, MAXP_bin_values)
#' }
seasonder_setSeaSondeRCS_FOR_MAXP.bin <- function(seasonder_cs_obj, FOR_MAXP.bin) {

  # TODO: Implement validation to check that FOR_MAXP.bin is a valid list of integer values

  # Assign the maximum power bin indices to the FOR_data attribute of the SeaSondeRCS object
  attr(seasonder_cs_obj, "FOR_data")$FOR_MAXP.bin <- FOR_MAXP.bin

  # Return the updated object
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

#' Retrieve First Order Region (FOR) Parameters
#'
#' This function retrieves the First Order Region (FOR) parameters associated with a
#' SeaSondeR cross-spectral object. If no FOR parameters are found in the object's
#' attributes, it initializes them using \code{\link{seasonder_validateFOR_parameters}}.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing FOR-related metadata.
#'
#' @details
#' The function extracts the FOR parameters stored within the object. If the parameters
#' are missing, the function initializes them using \code{\link{seasonder_validateFOR_parameters}}
#' and assigns default values where necessary.
#'
#' **FOR Parameters:**
#' - **nsm**: Doppler smoothing factor.
#' - **fdown**: Peak power dropoff threshold.
#' - **flim**: Null below peak power threshold.
#' - **noisefact**: Signal-to-noise threshold.
#' - **currmax**: Maximum velocity allowed.
#' - **reject_distant_bragg**: Flag to reject distant Bragg signals.
#' - **reject_noise_ionospheric**: Flag to reject ionospheric noise contamination.
#' - **reject_noise_ionospheric_threshold**: Threshold (in dB) for rejecting noise-affected Bragg signals.
#' - **reference_noise_normalized_limits**: Estimated limits for reference noise in normalized Doppler frequency.
#'
#' @return A named list containing the validated FOR parameters.
#'
#' @seealso
#' \code{\link{seasonder_validateFOR_parameters}} for initializing and validating FOR parameters.
#' \code{\link{seasonder_defaultFOR_parameters}} for retrieving default parameter values.
#'
#' @examples
#' \dontrun{
#' # Retrieve FOR parameters for a SeaSondeRCS object
#' for_params <- seasonder_getSeaSondeRCS_FOR_parameters(cs_obj)
#' print(for_params)
#' }
seasonder_getSeaSondeRCS_FOR_parameters <- function(seasonder_cs_obj) {

  # Extract FOR parameters from the object's attributes
  out <- attr(seasonder_cs_obj, "FOR_data", exact = TRUE)$FOR_parameters %||%
    seasonder_validateFOR_parameters(seasonder_cs_obj, list())

  # Return the FOR parameters
  return(out)
}


seasonder_getSeaSondeRCS_FOR_reference_noise_normalized_limits <- function(seasonder_cs_obj) {

  out <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$reference_noise_normalized_limits
  return(out)
}

#' @export
seasonder_getSeaSondeRCS_FOR <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "FOR_data", exact = TRUE)$FOR %||% seasonder_initSeaSondeRCS_FOR(seasonder_cs_obj)


  return(out)


}


#' Retrieve Smoothed Self-Spectra for First Order Region (FOR)
#'
#' This function retrieves the smoothed self-spectra (SS) matrix stored in the \code{FOR_data} attribute
#' of a \code{SeaSondeRCS} object. The smoothed self-spectra are used in First Order Region (FOR) processing
#' to refine the detection of the first-order boundaries.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing smoothed self-spectra data.
#'
#' @details
#' The function extracts the matrix assigned by \code{\link{seasonder_setSeaSondeRCS_FOR_SS_Smoothed}}.
#' If no smoothed self-spectra are found, the function returns \code{NULL}.
#'
#' The smoothed self-spectra are typically generated using \code{\link{seasonder_SmoothSS}} and applied
#' to the self-spectra of antenna 3. This smoothing aids in detecting the nulls that separate first- and
#' second-order regions.
#'
#' @return A matrix representing the smoothed self-spectra, or \code{NULL} if no smoothed data is stored.
#'
#' @seealso
#' - \code{\link{seasonder_SmoothFORSS}} for applying smoothing and storing the result.
#' - \code{\link{seasonder_setSeaSondeRCS_FOR_SS_Smoothed}} for setting smoothed self-spectra.
#'
#' @examples
#' \dontrun{
#' # Retrieve smoothed FOR self-spectra
#' smoothed_SS <- seasonder_getSeaSondeRCS_FOR_SS_Smoothed(cs_obj)
#' print(smoothed_SS)
#' }
seasonder_getSeaSondeRCS_FOR_SS_Smoothed  <- function(seasonder_cs_obj) {

  # Extract the smoothed self-spectra matrix from the FOR_data attribute
  out <- attr(seasonder_cs_obj, "FOR_data", exact = TRUE)$FOR_SS_Smoothed

  # Return the retrieved matrix (or NULL if it does not exist)
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

#' @export
seasonder_getSeaSondeRCS_FORConfig <- function(seasonder_cs_object){


  out <- list(FOR_parameters = seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_object),
              NoiseLevel = seasonder_getSeaSondeRCS_NoiseLevel(seasonder_cs_object))

  return(out)

}

#### Processing_steps ####


SeaSondeRCS_FOR_SeaSonde_start_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: FOR computation started using the SeaSonde method.")
}


SeaSondeRCS_FOR_SeaSonde_end_step_text <- function(seasonder_cs_object) {
  # Use glue to format the message with the current system time and the provided file path


  glue::glue("{Sys.time()}: FOR computation using the SeaSonde method ended.")
}

##### FOR #####



#' Estimate Reference Noise Limits in Normalized Doppler Frequency
#'
#' This function estimates the reference noise limits for normalized Doppler frequencies
#' in a SeaSondeR cross-spectral object. These limits are used to define the frequency range
#' where the noise level is assessed for first-order region (FOR) detection.
#'
#' @param seasonder_cs_obj A SeaSondeRCS object containing Doppler frequency metadata.
#'
#' @details
#' The function computes the noise limits based on the maximum normalized Doppler frequency
#' from \code{\link{seasonder_getDopplerBinsFrequency}}. The limits are scaled using fixed
#' factors to set a reference range for noise analysis:
#' - The lower bound is 56.5% of the maximum normalized Doppler frequency.
#' - The upper bound is 100% of the maximum normalized Doppler frequency.
#'
#' The lower bound value of 56.5% was determined through an empirical process involving
#' multiple sample spectra. The procedure began with an initial lower bound set at 100%,
#' and in each iteration, this value was decreased by 0.5%. The noise floor was
#' calculated repeatedly and compared with the temporal NoiseFloor.txt file generated
#' by the AnalyseSpectra Tool in Radial Suite R8. The final lower bound percentage
#' was established as the value that closely matched the corresponding noise floor
#' in the NoiseFloor.txt file. After this, several percentages were averaged and
#' rounded to the nearest 0.5%.
#'
#' The function is used in first-order detection processes where noise reference levels
#' are required for signal-to-noise ratio (SNR) calculations.
#'
#' @return A numeric vector of length two, representing the lower and upper reference
#' noise limits in normalized Doppler frequency.
#'
#' @seealso
#' \code{\link{seasonder_getDopplerBinsFrequency}} for retrieving Doppler bin frequencies.
#'
#' @examples
#' \dontrun{
#' # Estimate reference noise limits for a SeaSondeRCS object
#' noise_limits <- seasonder_estimateReferenceNoiseNormalizedLimits(cs_obj)
#' print(noise_limits)
#' }
seasonder_estimateReferenceNoiseNormalizedLimits <- function(seasonder_cs_obj) {

  # Retrieve Doppler bin frequencies in normalized units (relative to Bragg frequency)
  freq <- seasonder_getDopplerBinsFrequency(seasonder_cs_obj, normalized = TRUE)

  # Compute the noise limits using predefined scaling factors
  # - The lower (empirical) bound is 56.5% of the maximum normalized Doppler frequency
  # - The upper bound is 100% of the maximum normalized Doppler frequency
  out <- max(freq) * c(0.565, 1)

  # Return the estimated reference noise limits
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

#' Smooth Self-Spectra for First Order Region (FOR)
#'
#' This function applies a smoothing operation to the self-spectra (SS) matrix of antenna 3
#' in a SeaSondeR cross-spectral object, specifically for First Order Region (FOR) processing.
#' The smoothed self-spectra are stored as an attribute within the object.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing self-spectra data.
#'
#' @details
#' The function retrieves the default Doppler smoothing factor (\code{nsm}) from
#' \code{\link{seasonder_getSeaSondeRCS_FOR_parameters}} and applies the smoothing operation
#' using \code{\link{seasonder_SmoothSS}} on the self-spectra of antenna 3.
#'
#' **Steps:**
#' 1. Retrieve the Doppler smoothing factor (\code{nsm}).
#' 2. Apply the sliding mean smoothing to the self-spectra of antenna 3.
#' 3. Store the smoothed matrix as an attribute within the \code{SeaSondeRCS} object.
#'
#' The smoothing process helps stabilize the estimation of nulls between first- and second-order
#' regions, preventing over-smoothing that could distort boundaries or under-smoothing that could
#' introduce jagged edges.
#'
#' @return The input \code{SeaSondeRCS} object with the smoothed self-spectra stored as an attribute.
#'
#' @seealso
#' - \code{\link{seasonder_SmoothSS}} for performing the smoothing operation.
#' - \code{\link{seasonder_setSeaSondeRCS_FOR_SS_Smoothed}} for storing the smoothed self-spectra.
#' - \code{\link{seasonder_getSeaSondeRCS_FOR_parameters}} for retrieving default \code{nsm} values.
#'
#' @examples
#' \dontrun{
#' # Apply smoothing to the FOR self-spectra
#' cs_obj <- seasonder_SmoothFORSS(cs_obj)
#' }
seasonder_SmoothFORSS <- function(seasonder_cs_obj) {

  # Retrieve the Doppler smoothing factor (nsm) from the FOR parameters
  nsm <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$nsm

  # Apply smoothing to the self-spectra of antenna 3 using the retrieved nsm value
  SmoothSS <- seasonder_SmoothSS(seasonder_cs_obj, antenna = 3, smoothing = nsm)

  # Store the smoothed self-spectra within the SeaSondeRCS object
  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_SS_Smoothed(SmoothSS)

  # Return the updated object with the smoothed self-spectra attribute
  return(seasonder_cs_obj)
}


#' Smooth Self-Spectra Matrix Using a Sliding Window
#'
#' This function applies a smoothing operation to the self-spectra (SS) matrix of a specific antenna
#' in a SeaSondeR cross-spectral object. The smoothing is performed using a sliding mean over a specified
#' number of Doppler bins.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing self-spectra data.
#' @param antenna A character or numeric identifier of the antenna whose self-spectra will be smoothed.
#' @param smoothing Optional. An integer specifying the number of Doppler bins used for smoothing.
#'        If \code{NULL}, the function retrieves the default smoothing factor (\code{nsm}) from
#'        \code{\link{seasonder_getSeaSondeRCS_FOR_parameters}}.
#'
#' @details
#' The smoothing process is performed using a centered sliding mean filter with a window of \code{nsm} bins.
#' The window extends symmetrically before and after each bin, with adjustments based on whether \code{nsm}
#' is even or odd:
#' - If \code{nsm} is even, the window includes \code{nsm/2} bins before and after the target bin.
#' - If \code{nsm} is odd, the window includes \code{(nsm - 1)/2} bins before and \code{(nsm - 1)/2 + 1} bins after.
#'
#' The function utilizes \code{\link[slider]{slide_mean}} to apply the smoothing operation row-wise
#' across the self-spectra matrix.
#'
#' This smoothing implementation mimics the one performed by the tool AnalyzeSpectra of CODAR's Radial Suite R8.
#'
#' @return A matrix with the same dimensions as the input self-spectra matrix, but with smoothed values.
#'
#' @seealso
#' - \code{\link{seasonder_getSeaSondeRCS_FOR_parameters}} for retrieving default \code{nsm} values.
#' - \code{\link{seasonder_getSeaSondeRCS_antenna_SSdata}} for accessing self-spectra data.
#' - \code{\link[slider]{slide_mean}} for applying the sliding window mean operation.
#'
#' @examples
#' \dontrun{
#' # Smooth the self-spectra for a given antenna
#' smoothed_SS <- seasonder_SmoothSS(cs_obj, antenna = "A1")
#' print(smoothed_SS)
#' }
seasonder_SmoothSS <- function(seasonder_cs_obj, antenna, smoothing = NULL) {

  # Retrieve the default smoothing parameter (nsm) if not provided
  nsm <- smoothing %||% seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$nsm

  # Retrieve the self-spectra matrix for the specified antenna
  SS <- seasonder_getSeaSondeRCS_antenna_SSdata(seasonder_cs_obj, antenna = antenna)

  # Initialize smoothing window parameters
  after_bins <- 0
  before_bins <- 0

  # Determine the number of bins to include in the smoothing window
  if (nsm > 0) {
    # If nsm is even, split equally before and after the current bin
    after_bins  <- nsm / 2
    before_bins <- nsm / 2

    # If nsm is odd, assign one extra bin to the after window
    if (nsm %% 2 != 0) {
      after_bins <- (nsm - 1) / 2 + 1
      before_bins <- (nsm - 1) / 2
    }
  }

  # Apply the sliding mean filter row-wise to the self-spectra matrix
  out <- purrr::map(1:nrow(SS), \(i) {
    # Compute the smoothed values for the current row
    smoothed_row <- slider::slide_mean(abs(SS[i, , drop = TRUE]),
                                       after = after_bins,
                                       before = before_bins)

    # Convert the smoothed row into a matrix with the same dimensions
    smoothed_row %>% matrix(nrow = 1, byrow = TRUE) %>%
      magrittr::set_rownames(rownames(SS)[i])
  }) %>%
    purrr::reduce(\(x, y) rbind(x, y)) %>%  # Combine all rows into a single matrix
    magrittr::set_colnames(colnames(SS))  # Preserve original column names

  # Return the smoothed self-spectra matrix
  return(out)
}




#' Find Nulls in First Order Region (FOR)
#'
#' This function locates the null point in the First Order Region (FOR) spectrum, which separates the first-order Bragg peak from second-order energy or the noise floor.
#'
#' @param FOR A numeric vector representing the power spectrum in the FOR region.
#' @param start_point_P A numeric value representing the power threshold at which the search for the null point begins.
#' @param doppler_bins A numeric vector containing the Doppler bins corresponding to the spectrum in \code{FOR}.
#' @param left_region A logical value indicating whether the null is being searched for in the negative Bragg region. Default is \code{FALSE}.
#'
#' @details
#' The function follows these steps to determine the null point:
#' 1. If \code{left_region} is \code{TRUE}, the FOR spectrum and Doppler bins are reversed.
#' 2. The power spectrum is transformed to facilitate peak identification:
#'    - The absolute values of the power are taken and multiplied by -1.
#'    - The \code{start_point_P} threshold is also inverted.
#' 3. The function identifies the first local maximum in the transformed spectrum that exceeds \code{start_point_P}.
#' 4. The corresponding Doppler bin at the detected peak is returned as the null position.
#'
#' The function relies on \code{pracma::findpeaks} to identify the peak.
#'
#' @return A numeric value representing the Doppler bin at the detected null position.
#'
#' @seealso
#' - \code{\link{seasonder_findFORNullsInSpectrum}} for locating nulls in a full spectrum.
#' - \code{\link{pracma::findpeaks}} for peak detection.
#'
#' @examples
#' \dontrun{
#' FOR_spectrum <- c(0.1, 0.2, 0.3, 0.5, 0.8, 0.6, 0.4, 0.2, 0.1)
#' start_power <- 0.4
#' doppler_bins <- seq(-5, 5, length.out = length(FOR_spectrum))
#' null_bin <- seasonder_findFORNullsInFOR(FOR_spectrum, start_power, doppler_bins)
#' print(null_bin)
#' }
seasonder_findFORNullsInFOR <- function(FOR, start_point_P, doppler_bins, left_region = FALSE) {

  # If searching in the left region, reverse the spectrum and Doppler bins
  if (left_region) {
    FOR %<>% rev()
    doppler_bins %<>% rev()
  }

  # Transform the power spectrum by taking absolute values and multiplying by -1
  FOR %<>% abs() %>% magrittr::multiply_by(-1)

  # Invert the starting threshold for null detection
  start_point_P <- -1 * abs(start_point_P)

  # Identify the first local peak that exceeds the threshold start_point_P
  FOR_index <- pracma::findpeaks(FOR, nups = 1, ndowns = 1, zero = "0",
                                 minpeakheight = start_point_P, npeaks = 1, sortstr = FALSE)[,2,drop = TRUE]

  # Retrieve the corresponding Doppler bin for the detected null position
  FOR_bins <- doppler_bins[FOR_index]

  return(FOR_bins)
}


#' Identify Nulls in First Order Region (FOR) Spectrum
#'
#' This function locates the null points in the First Order Region (FOR) of a Doppler spectrum.
#' These nulls define the boundaries separating the first-order Bragg peak from the surrounding noise or second-order energy.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing the spectral data.
#' @param spectrum A numeric vector representing the power spectrum to analyze.
#' @param doppler_bins A numeric vector containing the Doppler bins corresponding to the spectrum.
#' @param negative_Bragg_region A logical value indicating whether the function should analyze the negative Bragg region. Default is \code{FALSE}.
#'
#' @details
#' The function executes the following steps:
#' 1. **Retrieve First Order Settings:** The function extracts the \code{fdown} parameter, which defines the drop-off level relative to the maximum power.
#' 2. **Prepare the Spectrum:**
#'    - Convert all values to negative absolute magnitudes to facilitate peak detection.
#'    - Reverse the spectrum and Doppler bins if analyzing the negative Bragg region.
#' 3. **Find the Main Spectral Peak:**
#'    - The function identifies the first major peak using \code{\link[pracma]{findpeaks}} with at least two consecutive increases and decreases.
#'    - The search is limited to the portion of the spectrum beyond this peak.
#' 4. **Determine the First Order Boundaries:**
#'    - The maximum power (\code{MAXP}) is found along with its bin index (\code{MAXP.bin}).
#'    - A threshold value \code{start_point_P} is computed as \code{MAXP / fdown} to establish the cutoff point for the null search.
#' 5. **Search for Nulls:** The spectrum is split into left and right sections:
#'    - The right-side spectrum is analyzed using \code{\link{seasonder_findFORNullsInFOR}} to find the right null.
#'    - The left-side spectrum undergoes the same process but reversed.
#' 6. **Output the Results:** The function returns a list containing:
#'    - The sequence of Doppler bins defining the FOR region.
#'    - The maximum power detected (\code{MAXP}).
#'    - The Doppler bin index where \code{MAXP} occurred (\code{MAXP.bin}).
#'
#' @return A list with three elements:
#' - \code{FOR}: A sequence of Doppler bins defining the first order region.
#' - \code{MAXP}: The maximum power found in the spectrum.
#' - \code{MAXP.bin}: The Doppler bin index of the maximum power.
#'
#' @seealso
#' - \code{\link{seasonder_findFORNullsInFOR}} for detecting nulls within a selected region.
#' - \code{\link[pracma]{findpeaks}} for peak identification.
#' - \code{\link{seasonder_getSeaSondeRCS_FOR_parameters}} for retrieving FOR settings.
#'
#' @examples
#' \dontrun{
#' # Sample spectrum
#' spectrum <- c(0.1, 0.3, 0.5, 1.0, 2.5, 3.8, 2.1, 1.0, 0.5, 0.2)
#' doppler_bins <- seq(-5, 5, length.out = length(spectrum))
#'
#' # Find nulls in the spectrum
#' result <- seasonder_findFORNullsInSpectrum(cs_obj, spectrum, doppler_bins)
#' print(result)
#' }
seasonder_findFORNullsInSpectrum <- function(seasonder_cs_obj, spectrum, doppler_bins, negative_Bragg_region = FALSE) {

  # Retrieve the 'fdown' parameter, which controls the threshold for null detection
  fdown <- seasonder_getSeaSondeRCS_FOR_parameters(seasonder_cs_obj)$fdown

  # Transform spectrum for peak detection by converting to negative absolute values
  sp <- -1 * abs(spectrum)
  dop_b <- doppler_bins

  # If analyzing the negative Bragg region, reverse the spectrum and Doppler bins
  if (negative_Bragg_region) {
    sp %<>% rev()
    dop_b %<>% rev()
  }

  # Identify the main peak in the spectrum to segment it into left and right regions
  center_region_index <- pracma::findpeaks(sp, nups = 2, ndowns = 2, zero = "0",
                                           npeaks = 1, sortstr = FALSE)[, 2, drop = TRUE]

  # Trim the spectrum to exclude regions before the main peak
  dop_b <- dop_b[center_region_index:length(dop_b)]
  sp <- sp[center_region_index:length(sp)]

  # Restore positive values to the spectrum for further analysis
  sp <- abs(sp)

  # If processing the negative Bragg region, restore the original order
  if (negative_Bragg_region) {
    sp %<>% rev()
    dop_b %<>% rev()
  }

  # Identify the maximum power in the spectrum
  MAXP <- max(sp, na.rm = TRUE)

  # Find the Doppler bin index corresponding to the maximum power
  MAXP.bin <- which.max(sp)

  # Calculate the starting threshold for null detection
  start_point_P <- MAXP / fdown

  # Define the right side of the spectrum for null detection
  right_FOR_index <- (MAXP.bin + 1):length(sp)
  right_FOR_sp <- sp[right_FOR_index]
  right_FOR_bins <- dop_b[right_FOR_index]

  # Define the left side of the spectrum for null detection
  left_FOR_index <- 1:(MAXP.bin - 1)
  left_FOR_sp <- sp[left_FOR_index]
  left_FOR_bins <- dop_b[left_FOR_index]

  # Detect the right-side null
  right_FOL <- seasonder_findFORNullsInFOR(right_FOR_sp, start_point_P, doppler_bins = right_FOR_bins, left_region = FALSE)

  # Detect the left-side null
  left_FOL <- seasonder_findFORNullsInFOR(left_FOR_sp, start_point_P, doppler_bins = left_FOR_bins, left_region = TRUE)

  # Validate that both nulls were successfully detected
  has_left_FOL <- length(left_FOL) == 1 && !is.na(left_FOL)
  has_right_FOL <- length(right_FOL) == 1 && !is.na(right_FOL)

  # Initialize output structure
  out <- list(FOR = integer(0), MAXP = MAXP, MAXP.bin = MAXP.bin)

  # If both nulls are found, define the first order region
  if (has_left_FOL && has_right_FOL) {
    out$FOR <- seq(left_FOL, right_FOL)
  }

  return(out)
}


#' Identify Nulls in First Order Region (FOR) for a Self-Spectra Matrix
#'
#' This function applies the null-finding algorithm to each row of a self-spectra (SS) matrix,
#' determining the boundaries of the First Order Region (FOR) for each range cell.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing spectral data and FOR parameters.
#' @param SS A numeric matrix representing the self-spectra data, where rows correspond to range cells
#'        and columns correspond to Doppler bins.
#' @param doppler_bins A numeric vector indicating the Doppler bins corresponding to the columns of \code{SS}.
#' @param negative_Bragg_region A logical value indicating whether to analyze the negative Bragg region. Default is \code{FALSE}.
#'
#' @details
#' This function processes each row of the self-spectra matrix, treating each row as an independent spectrum
#' for which the FOR nulls are identified. The nulls define the boundaries of the first-order Bragg region.
#'
#' **Processing Steps:**
#' 1. Iterate through each row of the \code{SS} matrix.
#' 2. Extract the power spectrum for the corresponding range cell.
#' 3. Apply \code{\link{seasonder_findFORNullsInSpectrum}} to determine the null positions.
#' 4. Store the results in a named list, where each entry corresponds to a range cell.
#'
#' @return A named list where each entry corresponds to a range cell, containing the detected FOR null positions.
#'
#' @seealso
#' - \code{\link{seasonder_findFORNullsInSpectrum}} for detecting nulls in a single spectrum.
#' - \code{\link{seasonder_findFORNulls}} for high-level null detection across all spectra.
#'
#' @examples
#' \dontrun{
#' # Assuming `cs_obj` is a valid SeaSondeRCS object with self-spectra data
#' doppler_bins <- seq(-5, 5, length.out = ncol(SS_matrix))
#' FOR_nulls <- seasonder_findFORNullsInSSMatrix(cs_obj, SS_matrix, doppler_bins)
#' print(FOR_nulls)
#' }
seasonder_findFORNullsInSSMatrix <- function(seasonder_cs_obj, SS, doppler_bins, negative_Bragg_region = FALSE) {

  # Iterate through each row of the self-spectra matrix
  out <- purrr::map(seq_len(nrow(SS)), \(i) {

    # Extract the power spectrum for the current range cell
    spectrum <- SS[i, , drop = TRUE]

    # Identify the nulls in the spectrum using seasonder_findFORNullsInSpectrum
    result <- seasonder_findFORNullsInSpectrum(seasonder_cs_obj, spectrum, doppler_bins, negative_Bragg_region = negative_Bragg_region)

    return(result)

  }) %>%
    # Assign row names (range cell identifiers) to the output list
    magrittr::set_names(rownames(SS))

  # Return the list of detected FOR nulls for each range cell
  return(out)
}


#' Identify Nulls in First Order Region (FOR) Across All Range Cells
#'
#' This function locates the null points in the First Order Region (FOR) of a SeaSondeR cross-spectral object.
#' It smooths the self-spectra (SS) data, extracts the relevant Doppler bins, and determines the boundaries
#' of the first-order Bragg region for each range cell.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing spectral data and FOR parameters.
#'
#' @details
#' The function follows these steps:
#' 1. **Smooth the Self-Spectra Data:** Calls \code{\link{seasonder_SmoothFORSS}} to apply a running mean filter.
#' 2. **Extract Smoothed Self-Spectra:** Retrieves the processed SS matrix using \code{\link{seasonder_getSeaSondeRCS_FOR_SS_Smoothed}}.
#' 3. **Identify the Doppler Center Bin:** Determines the central Doppler bin using \code{\link{seasonder_getCenterDopplerBin}}.
#' 4. **Segment the Spectrum:** Splits the smoothed SS data into:
#'    - The negative Bragg region (left side of the Doppler spectrum).
#'    - The positive Bragg region (right side of the Doppler spectrum).
#' 5. **Find Nulls in Each Region:** Uses \code{\link{seasonder_findFORNullsInSSMatrix}} to identify the null positions.
#' 6. **Store Results:** Extracts:
#'    - The First Order Region (\code{FOR}).
#'    - The maximum power (\code{MAXP}).
#'    - The Doppler bin index of the maximum power (\code{MAXP.bin}).
#' 7. **Update the SeaSondeRCS Object:** Saves the detected FOR boundaries and related metrics.
#'
#' @return The updated \code{SeaSondeRCS} object with the computed FOR nulls, maximum power, and bin indices.
#'
#' @seealso
#' - \code{\link{seasonder_findFORNullsInSpectrum}} for processing individual spectra.
#' - \code{\link{seasonder_findFORNullsInSSMatrix}} for batch processing spectra across multiple range cells.
#' - \code{\link{seasonder_getSeaSondeRCS_FOR_SS_Smoothed}} for retrieving smoothed SS data.
#'
#' @examples
#' \dontrun{
#' # Find First Order Nulls for a SeaSondeRCS object
#' cs_obj <- seasonder_findFORNulls(cs_obj)
#' }
seasonder_findFORNulls <- function(seasonder_cs_obj) {

  # Apply smoothing to the self-spectra matrix
  seasonder_cs_obj %<>% seasonder_SmoothFORSS()

  # Retrieve the smoothed self-spectra matrix
  SS3_smoothed <- seasonder_getSeaSondeRCS_FOR_SS_Smoothed(seasonder_cs_obj)

  # Identify the center Doppler bin
  Center_Bin <- seasonder_getCenterDopplerBin(seasonder_cs_obj)

  # Retrieve the total number of Doppler bins
  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  # Extract the positive Bragg region (bins to the right of the center bin)
  SS3_smoothed_positive <- SS3_smoothed[, (Center_Bin + 1):nDoppler]

  # Extract the negative Bragg region (bins to the left of the center bin)
  SS3_smoothed_negative <- SS3_smoothed[, 1:(Center_Bin - 1)]

  # Find the nulls in the negative Bragg region
  FOR_negative <- seasonder_findFORNullsInSSMatrix(
    seasonder_cs_obj,
    SS3_smoothed_negative,
    doppler_bins = 1:(Center_Bin - 1),
    negative_Bragg_region = TRUE
  )

  # Find the nulls in the positive Bragg region
  FOR_positive <- seasonder_findFORNullsInSSMatrix(
    seasonder_cs_obj,
    SS3_smoothed_positive,
    doppler_bins = (Center_Bin + 1):nDoppler,
    negative_Bragg_region = FALSE
  )

  # Organize results into a list
  FOR <- list(negative_FOR = FOR_negative, positive_FOR = FOR_positive)

  # Transpose the list structure for easier access
  FOR <- purrr::transpose(FOR)

  # Extract maximum power values for each range cell
  MAXP <- FOR %>% purrr::map(\(x) x %>% purrr::map(\(peak) purrr::pluck(peak, "MAXP")))

  # Store the maximum power in the SeaSondeRCS object
  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_MAXP(MAXP)

  # Extract the Doppler bin indices where maximum power occurs
  MAXP.bin <- FOR %>% purrr::map(\(x) x %>% purrr::map(\(peak) purrr::pluck(peak, "MAXP.bin")))

  # Store the Doppler bin indices in the SeaSondeRCS object
  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR_MAXP.bin(MAXP.bin)

  # Extract the actual First Order Region bin sequences
  FOR %<>% purrr::map(\(x) x %>% purrr::map(\(peak) purrr::pluck(peak, "FOR")))

  # Store the detected First Order Region bins in the SeaSondeRCS object
  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR(FOR)

  # Return the updated SeaSondeRCS object
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

#' Reject Distant Bragg Peaks
#'
#' This function evaluates Bragg peaks based on their proximity to expected Bragg line bins.
#' If the boundaries of a peak are farther from all Bragg lines than the width of the peak itself,
#' the peak is rejected by setting it to an empty integer vector.
#'
#' @param seasonder_cs_obj An object representing the current state of the radar cross section analysis,
#'        expected to contain methods or properties needed to determine Bragg lines.
#' @param peak A numeric vector indicating the positions of the peak under consideration.
#' @param range Optional; A numeric or integer NA indicating the range over which to consider the peak.
#'        Defaults to NA if not specified.
#' @param peak_name Optional; A character string representing the name or identifier of the peak.
#'        Defaults to an empty string if not specified.
#'
#' @details
#' The function computes the left and right limits of the given peak and checks if the distance
#' from these limits to the nearest Bragg lines exceeds the width of the peak. If both boundaries
#' exceed this threshold, the peak is rejected.
#'
#' @return Returns a possibly modified version of the `peak` argument, where a rejected peak
#'         is returned as `integer(0)`, indicating that no valid peak is present.
#'
#' @export
seasonder_rejectDistantBraggPeakTest <- function(seasonder_cs_obj, peak, range = NA, peak_name = ""){
  if(seasonder_is_debug_point_enabled("seasonder_rejectDistantBraggPeakTest")){
    browser() # Debug point, do not remove
  }
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

  if(seasonder_is_debug_point_enabled("seasonder_rejectNoiseIonosphericTest")){
    browser() # Debug point, do not remove
  }

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


#' Reject Ionospheric Noise in SeaSonde Data
#'
#' @description
#' This function rejects negative and positive Bragg peaks when the total external (non-Bragg)
#' power is greater than the Bragg power by a specified dB amount. This is applied across each
#' range for both negative and positive Bragg peaks.
#'
#' @param seasonder_cs_obj a SeaSonde cross-section object, which contains data for various ranges
#'        and peaks that need processing.
#'
#' @details
#' The function iterates over each range and applies a test to each negative and positive Bragg peak.
#' It uses the `seasonder_rejectNoiseIonosphericTest` function to evaluate whether the Bragg peak should
#' be rejected based on the comparison of Bragg and non-Bragg power levels.
#'
#' @return Returns the modified SeaSonde cross-section object with appropriate Bragg peaks rejected.
#' @export
seasonder_rejectNoiseIonospheric <- function(seasonder_cs_obj) {

  FORs <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_obj)

  # This test is applied for each range and each negative and positive Bragg.
  FORs %<>% purrr::map2(1:length(FORs), \(FOR, FOR_range) FOR %>% purrr::map2(names(.), \(peak, peak_name) seasonder_rejectNoiseIonosphericTest(seasonder_cs_obj, peak, FOR_range, peak_name)))

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_FOR(FORs)

  return(seasonder_cs_obj)
}


seasonder_computeFORsSeaSondeMethod <- function(seasonder_cs_obj) {

  seasonder_cs_obj  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_FOR_SeaSonde_start_step_text())

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

  seasonder_cs_obj  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_FOR_SeaSonde_end_step_text())

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

#### Utils ####

#' @export
seasonder_SeaSondeRCSExportFORBoundaries <- function(seasonder_cs_object){

  FOR <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_object)

  FOR <-  1:length(FOR) %>% purrr::map(\(range_cell) {

    o <- NULL

    doppler_bins <- integer(0)

    neg_bins <- FOR[[range_cell]]$negative_FOR
neg_range <- NA_integer_
    if(length(neg_bins) > 0){
      neg_range <- range(neg_bins)




    }

    pos_bins <- FOR[[range_cell]]$positive_FOR
    pos_range <- NA_integer_
    if(length(pos_bins) > 0){
      pos_range <- range(pos_bins)


    }







      o <- data.frame(range_cell = range_cell, first_neg_doppler_cell = neg_range[1], last_neg_doppler_cell = neg_range[2], first_pos_doppler_cell = pos_range[1], last_pos_doppler_cell = pos_range[2])


    return(o)

  }) %>% purrr::compact() %>% dplyr::bind_rows()

return(FOR)
}
