

#### Defaults ####

seasonder_defaultMUSIC_parameters <- function(){

  c(40,20,2)

}


seasonder_MUSICInitCov <- function(){

  out <- matrix(rep(NA_complex_,9),nrow = 3)

  return(out)

}


seasonder_MUSICInitDistances <- function(bearings = 0){




  out <- matrix(rep(NA_complex_,2*length(bearings)),nrow=2)

  rownames(out) <- c("single","dual")
  attr(out,"bearings") <- bearings


  return(out)

}

seasonder_MUSICInitDOASolutions <- function(){

  out <- list(single = list(bearing = NA_real_, a= NA_complex_, P = NA_complex_),
              dual = list(bearing = NA_real_, a= NA_complex_, P = matrix(rep(NA_complex_,4),nrow = 2)))



  return(out)

}

seasonder_MUSICInitEigenDecomp <- function(){

  out <- list(values = rep(NA_complex_,3), vectors =matrix(rep(NA_complex_,9),nrow = 3))

  return(out)

}

seasonder_MUSICInitInterpolatedData <- function(seasonder_cs_object){


  nDoppler <- seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_cs_object)
  nRanges <- seasonder_getnRangeCells(seasonder_cs_object)

  interpolated_data <- seasonder_initCSDataStructure(nRanges = nRanges, nDoppler = nDoppler)


  return(interpolated_data)

}


seasonder_NULLSeaSondeRCS_MUSIC <- function(){

  out <- data.frame(
    range_cell = numeric(0),
    doppler_bin = numeric(0),
    range= numeric(0),
    freq =  numeric(0),
    radial_v =  numeric(0),
    cov = list(),
    eigen = list(),
    distances = list(),
    DOA_solutions = list(),
    eigen_values_ratio= numeric(0),
    P1_check = logical(0),
    retained_solution = character(0),
    DOA = list(),
    lonlat = list(data.frame(lon = numeric(0) , lat = numeric(0)) ))

  out <- tibble::as_tibble(out)




  return(out)
}

seasonder_initSeaSondeRCS_MUSIC <- function(seasonder_cs_object, range_cells = NULL, doppler_bins = NULL){


  if(is.null(range_cells) || is.null(doppler_bins)){

    if (is.null(range_cells)) {

      range_cells <- 1:seasonder_getnRangeCells(seasonder_obj = seasonder_cs_object)
    }


    if (is.null(doppler_bins)) {

      doppler_bins <- 1:seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_cs_object = seasonder_cs_object)
    }

    out <- expand.grid(range_cell = range_cells, doppler_bin = doppler_bins)

  }else{
    out <- data.frame(range_cell = range_cells, doppler_bin = doppler_bins)
  }
  out <- tibble::as_tibble(out)


  out %<>% dplyr::mutate(range= seasonder_getCellsDistKm(seasonder_cs_object)[range_cell],
                         freq = seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency(seasonder_cs_object)[doppler_bin],
                         radial_v = seasonder_getSeaSondeRCS_MUSIC_BinsRadialVelocity(seasonder_cs_object)[doppler_bin],
                         cov = list(seasonder_MUSICInitCov()),
                         eigen = list(seasonder_MUSICInitEigenDecomp()),
                         distances = list(seasonder_MUSICInitDistances()),
                         DOA_solutions = list(seasonder_MUSICInitDOASolutions()),
                         eigen_values_ratio=NA_real_,
                         P1_check = TRUE,
                         retained_solution = "dual",
                         DOA = list(c(NA_real_, NA_real_)),
                         lonlat = list(data.frame(lon = NA_real_, lat = NA_real_))
  )



  return(out)
}

seasonder_initMUSICData <- function(seasonder_cs_object, range_cells = NULL, doppler_bins = NULL, NULL_MUSIC = FALSE){

  out <- seasonder_cs_object






  out %<>% seasonder_setSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(out))

  out %<>% seasonder_setSeaSondeRCS_MUSIC_parameters(seasonder_getSeaSondeRCS_MUSIC_parameters(out))

  MUSIC <- seasonder_NULLSeaSondeRCS_MUSIC()

  if(!NULL_MUSIC){

    MUSIC <- seasonder_initSeaSondeRCS_MUSIC(out, range_cells = range_cells, doppler_bins = doppler_bins)

  }
  out %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  out %<>% seasonder_MUSICComputePropDualSols()

  out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_data(seasonder_MUSICInitInterpolatedData(out))

  return(out)
}

#### Validation ####

#' Validate Doppler Interpolation Factor for SeaSondeRCS Objects
#'
#' This function validates the \code{doppler_interpolation} factor for a \code{SeaSondeRCS} object, ensuring it is within the allowed range and does not result in exceeding the maximum number of Doppler bins after interpolation.
#'
#' @param value An integer specifying the Doppler interpolation factor. Must be one of 1, 2, 3, or 4.
#' @param seasonder_cs_object A \code{SeaSondeRCS} object containing metadata for Doppler bin calculations.
#'
#' @details
#' Doppler interpolation is a process that increases the number of Doppler bins by the specified factor before radial processing.
#' The function performs the following validations:
#' - Ensures the \code{doppler_interpolation} factor is one of 1, 2, 3, or 4.
#' - Computes the total number of Doppler bins after applying the specified interpolation factor. If this number exceeds 2048, the function aborts with a descriptive error message.
#'
#' The maximum Doppler bins (2048) constraint is derived from CODAR's SeaSonde R8 Radial Config Setup, which specifies that the product of the interpolation factor and the original number of Doppler bins should not exceed this limit.
#'
#' @return
#' The validated \code{doppler_interpolation} factor as an integer.
#'
#' @section Warnings:
#' - Using Doppler interpolation factors of 3x or 4x is not recommended.
#' - Exceeding 2048 Doppler bins after interpolation will result in an error.
#'
#' @seealso
#' \code{\link{seasonder_getnDopplerCells}} for retrieving the number of Doppler bins,
#' \code{\link{seasonder_logAndAbort}} for error handling and logging.
#'
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Assume `cs_obj` is a valid SeaSondeRCS object
#' doppler_factor <- SeaSondeRCS_MUSIC_validate_doppler_interpolation(2, cs_obj)
#' }
#'
SeaSondeRCS_MUSIC_validate_doppler_interpolation <- function(value, seasonder_cs_object){
  value <- as.integer(value)

  value %in% c(1L,2L,3L,4L) || seasonder_logAndAbort(glue::glue("doppler_interpolation should be one of 1, 2, 3 or 4, but is {value}"), calling_function = "SeaSondeRCS_MUSIC_validate_doppler_interpolation")

  ndoppler <- value * seasonder_getnDopplerCells(seasonder_cs_object)

  ndoppler <= 2048 || seasonder_logAndAbort(glue::glue("The number of interpolated doppler cells should not exceed 2048, and current doppler_interpolation settion of {value} would result in {ndoppler} interpolated doppler cells. Please check."), calling_function = "SeaSondeRCS_MUSIC_validate_doppler_interpolation")

  return(value)

}

#### Processing_steps ####


SeaSondeRCS_MUSIC_start_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC algorithm started.")
}


SeaSondeRCS_MUSIC_end_step_text <- function(seasonder_cs_object) {
  # Use glue to format the message with the current system time and the provided file path

  proportion <- seasonder_cs_object %>% seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion()
  glue::glue("{Sys.time()}: MUSIC algorithm ended with {sprintf('%0.1f',proportion*100)}% of dual solutions.")
}

SeaSondeRCS_MUSIC_compute_cov_start_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC covariance matrix computation started")
}

SeaSondeRCS_MUSIC_covariance_decomposition_start_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC covariance matrix decomposition started")
}

SeaSondeRCS_compute_distances_start_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC distances computation started")
}


SeaSondeRCS_dual_solutions_testing_start_step_text  <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC dual solutions testing started")
}

SeaSondeRCS_peak_extraction_start_step_text  <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC peak extraction started")
}


SeaSondeRCS_doa_selection_start_step_text  <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC DOA selection started")
}


SeaSondeRCS_MUSIC_compute_cov_end_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC covariance matrix computation ended")
}

SeaSondeRCS_MUSIC_covariance_decomposition_end_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC covariance matrix decomposition ended")
}

SeaSondeRCS_compute_distances_end_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC distances computation ended")
}


SeaSondeRCS_dual_solutions_testing_end_step_text  <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC dual solutions testing ended")
}

SeaSondeRCS_peak_extraction_end_step_text  <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC peak extraction ended")
}


SeaSondeRCS_doa_selection_end_step_text  <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC DOA selection ended")
}

#### Setters ####

seasonder_setSeaSondeRCS_MUSIC_parameters <- function(seasonder_cs_obj, MUSIC_parameters = seasonder_defaultMUSIC_parameters()) {

  # TODO: validate MUSIC parameters

  attr(seasonder_cs_obj, "MUSIC_data")$MUSIC_parameters <- MUSIC_parameters


  return(seasonder_cs_obj)


}


seasonder_setSeaSondeRCS_MUSIC <- function(seasonder_cs_obj, MUSIC) {

  # TODO: validate MUSIC

  attr(seasonder_cs_obj, "MUSIC_data")$MUSIC <- MUSIC


  return(seasonder_cs_obj)


}

seasonder_setSeaSondeRCS_MUSIC_dual_solutions_proportion <- function(seasonder_cs_obj, dual_solutions_proportion) {

  # TODO: validate dual_solutions_proportion

  attr(seasonder_cs_obj, "MUSIC_data")$dual_solutions_proportion <- dual_solutions_proportion


  return(seasonder_cs_obj)


}

seasonder_setSeaSondeRCS_MUSIC_doppler_interpolation <- function(seasonder_cs_object, doppler_interpolation){

doppler_interpolation <- SeaSondeRCS_MUSIC_validate_doppler_interpolation(doppler_interpolation, seasonder_cs_object)

  attr(seasonder_cs_object, "MUSIC_data")$doppler_interpolation <- doppler_interpolation

  return(seasonder_cs_object)

}

seasonder_setSeaSondeRCS_MUSIC_interpolated_data <- function(seasonder_cs_object, interpolated_data){

  attr(seasonder_cs_object, "MUSIC_data")$interpolated_data <- interpolated_data %||% seasonder_MUSICInitInterpolatedData()



  return(seasonder_cs_object)

}

seasonder_setSeaSondeRCS_MUSIC_interpolated_doppler_cells_index <- function(seasonder_cs_object, interpolated_doppler_cells_index){

  # TODO: Valiate interpolated_doppler_cells_index (should be integer in the range of 1: (nDopplerCells))


  attr(seasonder_cs_object, "MUSIC_data")$interpolated_doppler_cells_index <- interpolated_doppler_cells_index

  return(seasonder_cs_object)

}
#### Getters ####


seasonder_getSeaSondeRCS_MUSIC_parameters <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "MUSIC_data", exact = TRUE)$MUSIC_parameters %||% seasonder_defaultMUSIC_parameters()


  return(out)


}

seasonder_getSeaSondeRCS_MUSIC <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "MUSIC_data", exact = TRUE)$MUSIC %||% seasonder_initSeaSondeRCS_MUSIC(seasonder_cs_obj)


  return(out)


}

#' @export
seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "MUSIC_data", exact = TRUE)$dual_solutions_proportion %||% NA_real_


  return(out)


}


seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation <- function(seasonder_cs_object){


  out <- attr(seasonder_cs_object, "MUSIC_data", exact = TRUE)$doppler_interpolation %||% 1L

  return(out)

}


seasonder_getSeaSondeRCS_MUSIC_interpolated_data <- function(seasonder_cs_object){



  out <- attr(seasonder_cs_object, "MUSIC_data", exact = TRUE)$interpolated_data %||% seasonder_MUSICInitInterpolatedData(seasonder_cs_object)

  return(out)
}

seasonder_getSeaSondeRCS_MUSIC_interpolated_doppler_cells_index <- function(seasonder_cs_object){

  out <-  attr(seasonder_cs_object, "MUSIC_data", exact = TRUE)$interpolated_doppler_cells_index

  return(out)

}

#' @export
seasonder_getSeaSondeRCS_MUSICConfig <- function(seasonder_cs_object){

  out <- list(doppler_interpolation = seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_cs_object),
              MUSIC_parameters = seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object))


  return(out)
}

#### Derived quantities ####

seasonder_MUSICComputePropDualSols <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  proportion <- sum(as.integer(MUSIC$retained_solution == "dual")) / nrow(MUSIC)

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC_dual_solutions_proportion(proportion)

  return(seasonder_cs_object)

}


seasonder_computePowerMatrix <- function(C,eig,a){

  P <- NULL

  if(ncol(a) > 0 ){

    a_star <- Conj(t(a))

    if(ncol(a) == 2){

      eigVector <- eig$vectors[,c(1,2)]
      eigValues <- eig$values[c(1,2)]

      G <- a_star %*% eigVector
      G_inv <- solve(G)

      G_inv_t <- Conj(t(G_inv))


      P <- G_inv_t %*% diag(eigValues) %*% G_inv
    }else if(ncol(a) == 1) {

      eigVector <- eig$vectors[,c(1), drop = F]
      eigValues <- eig$values[c(1)]
      G <- a_star %*% eigVector
      G_inv <- solve(G)

      G_inv_t <- Conj(t(G_inv))


      P <- G_inv_t %*% matrix(eigValues) %*% G_inv
    }
  }





  return(P)

}

seasonder_MUSICComputeSignalPowerMatrix <- function(seasonder_cs_object){


  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC %<>% dplyr::mutate(DOA_solutions = purrr::pmap(list(cov, eigen, DOA_solutions), \(C,eig,DOA_sol){

    out <- DOA_sol

    if(ncol(DOA_sol$dual$a) > 0){

      P_dual <- seasonder_computePowerMatrix(C,eig,DOA_sol$dual$a)

      if(!is.null(P_dual) ){

        out$dual$P <- P_dual

      }
    }
    P_single <- seasonder_computePowerMatrix(C,eig,DOA_sol$single$a)

    if(!is.null(P_single)){

      out$single$P <- P_single
    }

    return(out)

  }))


  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)



  return(seasonder_cs_object)
}

seasonder_getSeaSondeRCS_MUSIC_interpolated_dataMatrix <- function(seasonder_cs_obj, matrix_name) {

  matrix_name %in% c("SSA1","SSA2","SSA3","CS12","CS13","CS23","QC") || seasonder_logAndAbort(glue::glue("Unknown data matrix name '{matrix_name}'"),calling_function = "matrix_name", class = "seasonder_unknown_data_matrix_name", seasonder_matrix_name = matrix_name)

  matrix <- seasonder_getSeaSondeRCS_MUSIC_interpolated_data(seasonder_cs_object = seasonder_cs_obj)[[matrix_name]]

  return(matrix)

}

seasonder_getSeaSondeRCS_MUSIC_nDopplerCells <- function(seasonder_cs_object){

  out <- seasonder_getnDopplerCells(seasonder_cs_object)

  doppler_interpolation <- seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_cs_object)


  out <- out * doppler_interpolation

  return(out)

}


seasonder_getSeaSondeRCS_MUSIC_DopplerSpectrumResolution <- function(seasonder_cs_object){

  res <- seasonder_getDopplerSpectrumResolution(seasonder_cs_object)

  doppler_interpolation <- seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_cs_object)

  out <- res/doppler_interpolation

  return(out)


}

seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency <- function(seasonder_cs_obj, normalized = FALSE) {

  center_bin <- seasonder_getSeaSondeRCS_MUSIC_CenterDopplerBin(seasonder_cs_obj) # Freq 0

  nDoppler <- seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_cs_obj)

  spectra_res <- seasonder_getSeaSondeRCS_MUSIC_DopplerSpectrumResolution(seasonder_cs_obj)


  out <- seasonder_computeDopplerBinsFrequency(seasonder_cs_obj, nDoppler, center_bin, spectra_res)

  return(out)

}


seasonder_getSeaSondeRCS_MUSIC_CenterDopplerBin <- function(seasonder_cs_object){

  nDoppler <-seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_cs_object)

  out <- out <- seasonder_computeCenterDopplerBin(seasonder_cs_object, nDoppler)


  return(out)


}

seasonder_getSeaSondeRCS_MUSIC_BinsRadialVelocity <- function(seasonder_cs_obj) {

  freq <- seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency(seasonder_cs_obj)

  out <- seasonder_computeBinsRadialVelocity(seasonder_cs_obj, freq)

  return(out)

}


seasonder_MUSIC_DopplerFreq2Bins <- function(seasonder_cs_obj, doppler_values) {

  doppler_freqs <- seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency(seasonder_cs_obj, normalized = FALSE)

  delta_freq <- seasonder_getSeaSondeRCS_MUSIC_DopplerSpectrumResolution(seasonder_cs_obj)

  nDoppler <- seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_cs_obj)

  out <- seasonder_computeDopplerFreq2Bins(seasonder_cs_obj, doppler_values,  doppler_freqs, delta_freq, nDoppler)

  return(out)

}


seasonder_MUSIC_Bins2DopplerFreq <- function(seasonder_cs_obj, bins) {

  doppler_freqs <- seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency(seasonder_cs_obj, normalized = FALSE)

  return(doppler_freqs[bins])



}

#### Dual solution tests ####


seasonder_MUSICCheckEigenValueRatio <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC_eigen_values <- MUSIC %>% dplyr::pull("eigen") %>% purrr::map(\(eig) eig$values[1:2])


  eigen_values_ratio <- MUSIC_eigen_values %>% purrr::map(\(values) values[1]/values[2]) %>% purrr::list_c()

  MUSIC$eigen_values_ratio <- eigen_values_ratio

  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(1)

  P1_check <- eigen_values_ratio < MUSIC_parameter

  MUSIC$P1_check <- P1_check

  MUSIC$retained_solution[!P1_check] <- "single"

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)
}


seasonder_MUSICCheckSignalPowers <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(signal_power_ratio = purrr::map_dbl(DOA_solutions,\(DOA_sol){


    out <- NA_real_


    if(length(DOA_sol$dual$bearing) == 2){
      P_diag <- pracma::Real(diag(DOA_sol$dual$P))
      out <- max(P_diag)/min(P_diag)
    }


    return(out)
  }), .after = "P1_check")


  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(2)

  MUSIC %<>% dplyr::mutate(P2_check = purrr::map_lgl(DOA_solutions, \(DOA_sol) length(DOA_sol$dual$bearing) == 2 ) & !is.na(signal_power_ratio) & signal_power_ratio < MUSIC_parameter, .after = "signal_power_ratio")


  MUSIC$retained_solution[!MUSIC$P2_check] <- "single"

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)
}


seasonder_MUSICCheckSignalMatrix <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC %<>% dplyr::mutate(diag_off_diag_power_ratio = purrr::map_dbl(DOA_solutions,\(DOA_sol){
    out <- NA_real_
    if(length(DOA_sol$dual$bearing) == 2){
      P_diag <- pracma::Real(diag(DOA_sol$dual$P)) %>% prod()
      P_off_diag <- DOA_sol$dual$P
      diag(P_off_diag) <- 1

      P_off_diag <- pracma::Real(P_off_diag) %>% prod()

      out <- P_diag/P_off_diag
    }
    return(out)
  }), .after = "P2_check")


  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(3)

  MUSIC %<>% dplyr::mutate(P3_check = purrr::map_lgl(DOA_solutions, \(DOA_sol) length(DOA_sol$dual$bearing) == 2 ) & !is.na(diag_off_diag_power_ratio) & diag_off_diag_power_ratio > MUSIC_parameter, .after = "diag_off_diag_power_ratio")


  MUSIC$retained_solution[!MUSIC$P3_check] <- "single"

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)



  return(seasonder_cs_object)
}


seasonder_MUSICTestDualSolutions <- function(seasonder_cs_object){

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_dual_solutions_testing_start_step_text())


  # P1
  seasonder_cs_object %<>% seasonder_MUSICCheckEigenValueRatio()

  # P2
  seasonder_cs_object %<>% seasonder_MUSICCheckSignalPowers()

  # P3
  seasonder_cs_object %<>% seasonder_MUSICCheckSignalMatrix()

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_dual_solutions_testing_end_step_text())

  return(seasonder_cs_object)

}

##### Doppler interpolation #####


#' Perform Doppler Interpolation for SeaSonde Cross-Spectra Data
#'
#' This function performs Doppler interpolation on the cross-spectra data of a SeaSondeRCS object, preparing the data for MUSIC processing.
#' Interpolation is achieved by inserting additional Doppler bins using linear interpolation, potentially increasing the number of detected vectors while possibly smoothing the radials. The function tries to mimic CODAR's AnalyzeSpectra tool interpolation, including the addition of a wraparound Doppler cell before interpolation.
#'
#' @param seasonder_cs_obj A \code{SeaSondeRCS} object containing cross-spectra data and metadata for processing.
#'
#' @details
#' Doppler interpolation increases the number of Doppler bins by a factor of 2, 3, or 4 before radial processing.
#' This is accomplished by linearly interpolating between existing bins, increasing the number of radial vectors by approximately 15% for a 2x interpolation, and yielding smoother radials. The interpolation factor is configurable via the SeaSondeRCS object's \code{doppler_interpolation} attribute and it's setter \code{seasonder_setSeaSondeRCS_MUSIC_doppler_interpolation}. The number of Doppler bins after interpolation should not exceed 2048; exceeding this limit will result in an error.
#'
#' The interpolation process is as follows:
#' 1. A wraparound Doppler cell is added to the right of the data.
#' 2. For non-quality-control (QC) matrices, linear interpolation is applied to fill in the newly added Doppler bins.
#' 3. QC matrices are updated with a default value (-1) for interpolated bins.
#'
#' @note
#' - CODAR's SeaSonde R8 Radial Config Setup documentation advises against using 3x or 4x interpolation.
#' - The function ensures the number of Doppler bins after interpolation does not exceed 2048.
#' - Doppler interpolation is a preprocessing step typically performed by CODAR's AnalyzeSpectra tool before MUSIC processing.
#'
#' @return
#' A \code{SeaSondeRCS} object with updated interpolated cross-spectra data and metadata.
#'
#'
#' @seealso
#' \code{\link{seasonder_setSeaSondeRCS_MUSIC_interpolated_data}} for setting interpolated data,
#' \code{\link{seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation}} for retrieving the interpolation factor,
#' \code{\link{seasonder_setSeaSondeRCS_MUSIC_doppler_interpolation}} for setting the interpolation factor,
#' \code{\link{seasonder_initCSDataStructure}} for initializing the interpolated data structure.
#'
#' @importFrom dplyr setdiff
#' @importFrom purrr map2 reduce
#' @importFrom zoo na.approx
#' @importFrom pracma Real Imag
#'
#' @examples
#' \dontrun{
#' # Assume `cs_obj` is a valid SeaSondeRCS object
#' cs_obj <- seasonder_SeaSondeRCSMUSICInterpolateDoppler(cs_obj)
#' }
#'
seasonder_SeaSondeRCSMUSICInterpolateDoppler <- function(seasonder_cs_obj){

  # Initialize the output object as a copy of the input object
  out <- seasonder_cs_obj

  # Extract the existing cross-spectra data from the input object
  data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)

  # Set the initial interpolated data in the output object as a copy of the original data
  out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_data(data)

  # Retrieve the Doppler interpolation factor, indicating how much to interpolate
  doppler_interpolation <- seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_cs_obj)

  # Proceed only if the Doppler interpolation factor is greater than 1 (indicating interpolation is needed)
  if(doppler_interpolation > 1L){

    # Get the number of Doppler and range cells after interpolation
    nDoppler <- seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_cs_obj)
    nRanges <- seasonder_getnRangeCells(seasonder_cs_obj)

    # Initialize an empty data structure to store the interpolated results
    interpolated_data <- seasonder_initCSDataStructure(nRanges = nRanges, nDoppler = nDoppler)

    # Map the original indices to the interpolated indices based on the interpolation factor
    index_mapping <- data.frame(
      original = 1:ncol(data[[1]]),
      mapped = (0:(ncol(data[[1]]) - 1)) * doppler_interpolation + 1
    )

    # Determine the indices of the Doppler cells that will be interpolated
    interpolated_cells <- dplyr::setdiff(1:nDoppler, index_mapping$mapped)

    # Perform the interpolation for each matrix in the cross-spectra data structure
    interpolated_data %<>% purrr::map2(names(.), \(matrix, name){

      # Validate that the matrix name exists in the original data
      if(!name %in% names(data)){
        seasonder_logAndAbort(
          glue::glue("{name} is not a data matrix name."),
          calling_function = "seasonder_SeaSondeRCSInterpolateDoppler"
        )
      }

      # Copy the original data into the interpolated matrix based on the mapped indices
      original_matrix <- data[[name]]
      matrix[, index_mapping$mapped] <- original_matrix[, index_mapping$original]

      # Special handling for the quality control (QC) matrix
      if(name == "QC"){
        # Set interpolated cells in the QC matrix to a default value (-1)
        matrix[, interpolated_cells] <- -1L
      } else {
        # For other matrices, perform linear interpolation for the interpolated cells
        matrix <- 1:nrow(matrix) %>% purrr::reduce(\(matrix_so_far, i){

          # Prepare the row data for interpolation, adding a wraparound element
          data <- c(matrix_so_far[i,, drop = TRUE], matrix_so_far[i, 1, drop = TRUE])

          if(!rlang::is_complex(data)){
            # Perform linear interpolation for real-valued data
            data[interpolated_cells] <- zoo::na.approx(abs(data))[interpolated_cells]
            data <- data[-length(data)]  # Remove the wraparound element
          } else {
            # Perform linear interpolation for complex-valued data
            data <- complex(
              real = zoo::na.approx(pracma::Real(data))[-length(data)],
              imaginary = zoo::na.approx(pracma::Imag(data))[-length(data)]
            )
          }

          # Update the row in the matrix
          matrix_so_far[i, ] <- data

          return(matrix_so_far)
        }, .init = matrix)
      }

      return(matrix)
    })

    # Set the indices of the interpolated Doppler cells in the output object
    out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_doppler_cells_index(interpolated_cells)

    # Set the interpolated cross-spectra data in the output object
    out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_data(interpolated_data)
  }

  # Return the updated object with interpolated data
  return(out)
}



#### MUSIC algorithm ####

# TODO: update docs

#' Calculate the MUSIC Covariance Matrix for each Given Cell Range and Doppler Bin
#'
#' This function computes the Multiple Signal Classification (MUSIC) covariance matrix
#' for each cell range and Doppler bin from SeaSonde Cross Spectra (CS) data. The MUSIC
#' algorithm is used in direction finding and spectral estimation.
#'
#' @param seasonder_cs_object A SeaSondeRCS object containing the cross-spectra data.
#'
#' @return A SeaSondeRCS object updated with a computed 3x3 complex covariance matrix for each cell range and Doppler bin.
#'         The covariance matrix is stored in the MUSIC data field. Each matrix element \eqn{C_{ij}} is calculated
#'         based on auto-spectra (for diagonal elements) or cross-spectra (for off-diagonal elements).
#'         - Diagonal elements (\eqn{i = j}) are derived from auto-spectra `SSA{i}`.
#'         - Off-diagonal elements (\eqn{i \neq j}) are derived from cross-spectra `CSij`.
#'         - Auto-spectra values for the third antenna (\code{SSA3}) are taken as absolute values to comply
#'           with CODAR's recommendation to handle negative values indicating noise or interference.
#'
#' @details
#' The MUSIC algorithm estimates the direction of arrival (DOA) of signals, requiring the computation of a
#' covariance matrix from sensor data. This function constructs the covariance matrix by iterating through
#' the auto-spectra (`SSA{i}`) and cross-spectra (`CSij`) fields of the cross-spectra data.
#'
#' For diagonal elements (\eqn{i = j}), the matrix uses data from the auto-spectra field corresponding to
#' the antenna index (\code{SSA1}, \code{SSA2}, or \code{SSA3}). Negative values in \code{SSA3}, which
#' indicate noise or interference, are converted to their absolute values before use, as per the
#' Cross Spectra File Format Version 6 guidelines.
#'
#' Off-diagonal elements (\eqn{i \neq j}) are derived from cross-spectra fields, such as \code{CS12} or \code{CS23}.
#' If the row index is greater than the column index, the conjugate of the value is used.
#'
#' @seealso
#' \code{\link{seasonder_getSeaSondeRCS_MUSIC}}, \code{\link{seasonder_setSeaSondeRCS_MUSIC}}
#'
#' @references
#' Cross Spectra File Format Version 6, CODAR. (2016).
#' Paolo, T. de, Cook, T. & Terrill, E. Properties of HF RADAR Compact Antenna Arrays and Their Effect on the MUSIC Algorithm. OCEANS 2007 1–10 (2007) doi:10.1109/oceans.2007.4449265.
#'
#' @examples
#' \dontrun{
#' # Assume seasonder_cs_obj is a valid SeaSondeRCS object
#' updated_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)
#' }
seasonder_MUSICComputeCov <- function(seasonder_cs_object) {

  # Log the start of the MUSIC covariance matrix computation
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_ProcessingSteps(
    SeaSondeRCS_MUSIC_compute_cov_start_step_text()
  )

  # Retrieve the MUSIC data object from the input
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Update the MUSIC data by computing the covariance matrix for each range cell and Doppler bin
  MUSIC %<>% dplyr::mutate(
    cov = purrr::map2(range_cell, doppler_bin, \(r, d) {
      # Initialize a 3x3 complex matrix for the covariance computation
      out <- seasonder_MUSICInitCov()

      # Iterate over the rows and columns of the covariance matrix
      for (i in 1:3) {
        for (j in 1:3) {
          if (i == j) {
            # Diagonal elements: Retrieve the auto-spectra from the corresponding SSA matrix
            value <- seasonder_getSeaSondeRCS_MUSIC_interpolated_dataMatrix(
              seasonder_cs_object,
              paste0("SSA", i)
            )[r, d]

            # For the third antenna, take the absolute value of the auto-spectra
            if (i == 3) {
              value <- abs(value)
            }
          } else {
            # Off-diagonal elements: Retrieve the cross-spectra from the corresponding CS matrix
            value <- seasonder_getSeaSondeRCS_MUSIC_interpolated_dataMatrix(
              seasonder_cs_object,
              paste0("CS", paste0(as.character(sort(c(i, j))), collapse = ""))
            )[r, d]

            # Conjugate the value if the row index is greater than the column index
            if (i > j) {
              value <- Conj(value)
            }
          }

          # Assign the computed value to the covariance matrix
          out[i, j] <- value
        }
      }

      # Return the computed covariance matrix
      return(out)
    })
  )

  # Update the MUSIC data object with the computed covariance matrices
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Log the end of the MUSIC covariance matrix computation
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_ProcessingSteps(
    SeaSondeRCS_MUSIC_compute_cov_end_step_text()
  )

  # Return the updated SeaSondeRCS object
  return(seasonder_cs_object)
}


seasonder_eigen_decomp_C <- function(C){
  out <- seasonder_MUSICInitEigenDecomp()

  # Get eigen-decomposition
  eigen_decomp <- eigen(C, symmetric = TRUE)

  out$values <- eigen_decomp$values
  out$vectors <- eigen_decomp$vectors

  return(out)
}

# TODO: update docs

#' Eigen Decomposition of the MUSIC Covariance Matrix
#'
#' Performs the eigen decomposition of a MUSIC covariance matrix to obtain the eigenvalues
#' and eigenvectors. This decomposition is a critical step in the MUSIC algorithm for spectral
#' estimation and direction finding.
#'
#' @param C A 3x3 complex covariance matrix obtained from the `seasonder_MUSICComputeCov` function.
#'
#' @return A list containing the eigenvalues and eigenvectors of the covariance matrix.
#'         The eigenvalues are sorted in descending order to facilitate their use in
#'         MUSIC algorithm applications. The list has the following components:
#'         \itemize{
#'           \item \code{values}: A numeric vector containing the sorted eigenvalues.
#'           \item \code{vectors}: A 3x3 matrix of the corresponding eigenvectors in columns, reordered to match the sorted eigenvalues.
#'         }
#'
#' @details
#' The function uses the \code{eigen} function to perform the eigen decomposition of the covariance
#' matrix. It then sorts the eigenvalues in descending order, as this ordering is often required
#' for further analysis in MUSIC algorithm applications, such as identifying the signal and noise subspaces.
#' The eigenvectors are reordered accordingly to maintain the correspondence with their eigenvalues.
#'
seasonder_MUSICCovDecomposition <- function(seasonder_cs_object){

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_MUSIC_covariance_decomposition_start_step_text())

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(eigen = purrr::map(cov,seasonder_eigen_decomp_C))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_MUSIC_covariance_decomposition_end_step_text())

  return(seasonder_cs_object)

}

# TODO: update docs

seasonder_compute_antenna_pattern_proyections <- function(En, a){

  Conj(t(a)) %*% (En %*% Conj(t(En))) %*% a

}

#' Calculate Euclidean Distances for MUSIC Algorithm
#'
#' Computes the Euclidean distances based on the eigenvalue decomposition from the MUSIC covariance matrix and
#' the antenna pattern measurements (APM) for single and dual solutions.
#'
#' @param eigen_analysis A list containing the results of the eigenvalue decomposition of the MUSIC covariance
#'        matrix. This list must have at least two elements: `$values` for eigenvalues and `$vectors` for
#'        eigenvectors.
#' @param seasonder_apm_obj A matrix representing the antenna pattern measurements (APM), where each column
#'        corresponds to a specific bearing and contains the complex-valued response of the antenna at that bearing.
#'
#' @return A 2xN complex matrix, where N is the number of bearings. The first row (`"single"`) contains the
#'         Euclidean distances for the single solution scenario, and the second row (`"dual"`) for the dual
#'         solution scenario, based on the eigenvalue decomposition and the antenna pattern measurements.
#'
#' @details
#' The Euclidean distance in the context of the MUSIC algorithm is a measure used to estimate the direction
#' of arrival (DOA) of signals. This function computes these distances by projecting the antenna pattern measurements
#' onto the subspace orthogonal to the signal's eigenvectors (from the eigenvalue decomposition of the MUSIC covariance
#' matrix). The computation for `single` and `dual` solutions involves the use of eigenvectors corresponding to the
#' noise subspace (`En`). For each bearing, the function calculates the projected Euclidean distance using the formula:
#'
#' \deqn{D_i = a^H (En E_n^H) a}
#'
#' where \eqn{a} represents the complex-valued response of the antenna at a given bearing, \eqn{En} the matrix of
#' eigenvectors corresponding to the noise subspace, and \eqn{H} denotes the conjugate transpose. The distances are
#' calculated for both single and dual solutions, indicating scenarios with different numbers of signal sources.
#'
seasonder_MUSICEuclideanDistance <- function(seasonder_cs_object){

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_compute_distances_start_step_text())


  seasonder_apm_obj <- seasonder_getSeaSondeRCS_APM(seasonder_cs_object)


  bearings <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_obj)

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(distances = purrr::map(eigen,\(eigen_analysis){

    out <- seasonder_MUSICInitDistances(bearings = bearings)


    for(i in 1:2){ # Number of solutions
      En <- eigen_analysis$vectors[,(i+1):3, drop = F]

      for(j in 1:length(bearings)){
        a <- seasonder_apm_obj[,j, drop= F]
        names(a) <- NULL
        out[i,j] <- seasonder_compute_antenna_pattern_proyections(En,a)


      }


    }

    return(out)
  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_compute_distances_end_step_text())

  return(seasonder_cs_object)

}


seasonder_MUSICExtractPeaks <- function(seasonder_cs_object){

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_peak_extraction_start_step_text())


  seasonder_apm_obj <- seasonder_getSeaSondeRCS_APM(seasonder_cs_object)

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(DOA_solutions = purrr::map(distances,\(distances){

    out <- seasonder_MUSICInitDOASolutions()


    bearings <- attr(distances,"bearings",exact = TRUE)

    rev_single_solution_dist = pracma::Real(1/distances['single',,drop = TRUE])

    rev_dual_solution_dist = pracma::Real(1/distances['dual',,drop = TRUE])

    single_peaks_results <- pracma::findpeaks(rev_single_solution_dist,npeaks = 1, sortstr = TRUE)

    single_peak <-  single_peaks_results[,2,drop = T]# which.max(rev_single_solution_dist)
    single_peak_resp <- NA
    if(!is.null(single_peaks_results)){
      single_peak_resp <- 10*log10(single_peaks_results[,1,drop = T])
    }
    dual_peaks_results <- pracma::findpeaks(rev_dual_solution_dist,npeaks = 2, sortstr = TRUE)



    out$single$bearing <-  bearings[single_peak]
    out$single$a <- seasonder_apm_obj[,single_peak, drop = FALSE]
    out$single$peak_resp <- single_peak_resp

    dual_peaks <- dual_peaks_results[,2,drop = T]
    dual_peaks_resp <- NA
    if(!is.null(dual_peaks_results)){
      dual_peaks_resp <- 10*log10(dual_peaks_results[,1,drop = T])
    }

    out$dual$bearing <- bearings[dual_peaks]
    out$dual$a <- seasonder_apm_obj[,dual_peaks, drop = FALSE]
    out$dual$peak_resp <- dual_peaks_resp


    return(out)
  }))


  MUSIC %<>% dplyr::mutate(retained_solution = purrr::map2_chr(retained_solution, DOA_solutions, \(ret_sol, DOA_sol){

    out <- ret_sol

    if(ret_sol == "dual"){

      if(length(DOA_sol$dual$bearing) == 0){
        if(length(DOA_sol$single$bearing )==1){
          out <- "single"
        }else{
          out <- "none"
        }

      }

    }else if(ret_sol == "single"){
      if(length(DOA_sol$single$bearing ) != 1){
        out <- "none"
      }
    }

    return(out)

  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)


  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_peak_extraction_end_step_text())

  return(seasonder_cs_object)

}






seasonder_MUSICSelectDOA <- function(seasonder_cs_object){

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_doa_selection_start_step_text())


  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC %<>% dplyr::mutate(DOA = purrr::map2(DOA_solutions, retained_solution, \(DOA_sol, retained_sol) {
    if(retained_sol != "none"){
      DOA_sol[[retained_sol]]
    }

  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_doa_selection_end_step_text())

  return(seasonder_cs_object)
}


seasonder_runMUSIC <- function(seasonder_cs_object){

  seasonder_logAndMessage("seasonder_runMUSIC: MUSIC algorithm started.", "info")


  out <- seasonder_cs_object

  out  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_MUSIC_start_step_text())

  out %<>% seasonder_MUSICComputeCov()

  out %<>% seasonder_MUSICCovDecomposition()

  out %<>% seasonder_MUSICEuclideanDistance()

  out %<>% seasonder_MUSICExtractPeaks()

  out %<>% seasonder_MUSICComputeSignalPowerMatrix()

  out %<>% seasonder_MUSICTestDualSolutions()

  out %<>% seasonder_MUSICComputePropDualSols()

  out %<>% seasonder_MUSICSelectDOA()

  out %<>% seasonder_MUSIC_LonLat()

  out  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_MUSIC_end_step_text(out))

  seasonder_logAndMessage("seasonder_runMUSIC: MUSIC algortihm finished.", "info")


  return(out)

}

#' @export
seasonder_runMUSIC_in_FOR <- function(seasonder_cs_object, doppler_interpolation = 2L){

  out <- seasonder_cs_object

  out %<>% seasonder_setSeaSondeRCS_MUSIC_doppler_interpolation(doppler_interpolation)


  FOR <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_object)

  FOR <-  1:length(FOR) %>% purrr::map(\(range_cell) {

    o <- NULL

    doppler_bins <- integer(0)

    neg_bins <- FOR[[range_cell]]$negative_FOR

    if(length(neg_bins) > 0){
      neg_range <- range(neg_bins)

      neg_range_freq  <- seasonder_Bins2DopplerFreq(seasonder_cs_object, neg_range)

      new_neg_range_bins <- seasonder_MUSIC_DopplerFreq2Bins(out, neg_range_freq)
      doppler_bins <- c(doppler_bins, new_neg_range_bins[1]:new_neg_range_bins[2])

    }

    pos_bins <- FOR[[range_cell]]$positive_FOR

    if(length(pos_bins) > 0){
      pos_range <- range(pos_bins)

      pos_range_freq  <- seasonder_Bins2DopplerFreq(seasonder_cs_object, pos_range)

      new_pos_range_bins <- seasonder_MUSIC_DopplerFreq2Bins(out, pos_range_freq)

      doppler_bins <- c(doppler_bins, new_pos_range_bins[1]:new_pos_range_bins[2])
    }






    if(length(doppler_bins) > 0){
      o <- data.frame(range_cell = range_cell, doppler_bin = doppler_bins)
    }

    return(o)

  }) %>% purrr::compact() %>% dplyr::bind_rows()



  out %<>% seasonder_initMUSICData(range_cells = FOR$range_cell, doppler_bins = FOR$doppler_bin, NULL_MUSIC = nrow(FOR) == 0)



  out %<>% seasonder_SeaSondeRCSMUSICInterpolateDoppler()


  out %<>% seasonder_runMUSIC()



  return(out)


}


#### Utils ####

seasonder_MUSICBearing2GeographicalBearing <- function(bearings, seasonder_apm_object){

  antennaBearing <-
    seasonder_apm_object %>%
    seasonder_getSeaSondeRAPM_AntennaBearing()
  bearings %<>% purrr::map(\(angles) ((-1 * angles %% 360) + antennaBearing) %% 360)

  return(bearings)

}

#' @export
seasonder_computeLonLatFromOriginDistBearing <- function(origin_lon, origin_lat, dist, bearing){

  geosphere::destPoint(c(origin_lon, origin_lat), bearing, dist * 1000) %>%
    as.data.frame()


}


#' Convert MUSIC Algorithm Output to Geolocated Coordinates
#'
#' This function takes the output from the MUSIC algorithm, typically used in direction finding
#' within the SeaSonde remote sensing system, and converts the directional data (range and bearings)
#' into geographic coordinates based on the originating radar site.
#'
#' @param seasonder_cs_object A complex SeaSondeR data object that includes both the MUSIC data
#'        and the Antenna Pattern Matching (APM) data necessary for locating the source of the signals.
#'
#' @return Returns the modified `seasonder_cs_object` with MUSIC data updated to include longitude
#'         and latitude coordinates for each detected signal.
#'
#' @export
#' @importFrom geosphere destPoint
seasonder_MUSIC_LonLat <- function(seasonder_cs_object) {

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Retrieve original longitude and latitude, or fallback to APM data if unavailable
  longitude <- seasonder_getfLongitude(seasonder_cs_object)
  latitude <- seasonder_getfLatitude(seasonder_cs_object)

  if (is.null(longitude) || is.null(latitude)) {
    origin <- seasonder_cs_object %>%
      seasonder_getSeaSondeRCS_APM() %>%
      seasonder_getSeaSondeRAPM_SiteOrigin()
    latitude <- origin[1]
    longitude <- origin[2]
  }

  # Calculate geographic coordinates for each MUSIC detection
  range <- MUSIC$range
  bearings <- MUSIC$DOA %>% purrr::map("bearing")

  seasonder_apm_object <- seasonder_cs_object %>%
    seasonder_getSeaSondeRCS_APM()

  bearings %<>% seasonder_MUSICBearing2GeographicalBearing(seasonder_apm_object)

  MUSIC$lonlat <- purrr::map2(range, bearings, \(dist, bear) {

    if(length(bear ) > 0){
      seasonder_computeLonLatFromOriginDistBearing(longitude, latitude, dist = dist, bearing = bear)
    }else{
      data.frame(lon = NA_real_, lat = NA_real_)
    }


  })

  # Update the seasonder_cs_object with new lon-lat data
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)
}

#' Export a MUSIC Table from a SeaSondeRCS Object
#'
#' This function extracts and exports a MUSIC (Multiple Signal Classification) table from a
#' SeaSondeRCS (Compact System) object. The MUSIC algorithm is used to estimate the direction of
#' arrival (DOA) of signals, and this function processes the results to produce a data frame
#' containing relevant information such as longitude, latitude, range, doppler frequency, radial
#' velocity, signal power, and bearing.
#'
#' @param seasonder_cs_object A SeaSondeRCS object from which the MUSIC table will be extracted.
#'
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item \code{longitude} - Longitude of the detected signal.
#'   \item \code{latitude} - Latitude of the detected signal.
#'   \item \code{range_cell} - Range cell number.
#'   \item \code{range} - Range of the detected signal.
#'   \item \code{doppler_bin} - Doppler cell number.
#'   \item \code{doppler_freq} - Doppler frequency of the detected signal.
#'   \item \code{radial_velocity} - Radial velocity of the detected signal.
#'   \item \code{signal_power} - Signal power.
#'   \item \code{bearing} - Geographical bearing of the detected signal.
#' }
#'
#' @export
seasonder_exportMUSICTable <- function(seasonder_cs_object){


  datetime <-  seasonder_getSeaSondeRCS_headerField(seasonder_cs_object, "nDateTime") %||% as.POSIXct(0)
  # Initialize an empty data frame with the required columns
  longitude <- numeric(0)
  latitude <- numeric(0)
  cell_number <- integer(0)
  range <- numeric(0)
  doppler_cell <- integer(0)
  freq <- numeric(0)
  velocity <- numeric(0)
  power <- numeric(0)
  bearing <- numeric(0)
  noise_level <- numeric(0)
  signal_power_db  <- numeric(0)
  SNR <- numeric(0)
  DOA_peak_resp_db <- numeric(0)

  out <- data.frame(
    longitude = longitude,
    latitude = latitude,
    range_cell = cell_number,
    range = range,
    doppler_bin = doppler_cell,
    doppler_freq = freq,
    radial_velocity = velocity,
    signal_power = power,
    bearing = bearing,
    noise_level = noise_level,
    signal_power_db = signal_power_db,
    SNR = SNR,
    DOA_peak_resp_db = DOA_peak_resp_db
  )

  # Retrieve MUSIC data from the SeaSondeRCS object
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  out <- data.frame(
    longitude = numeric(0),
    latitude = numeric(0),
    range_cell = numeric(0),
    range = numeric(0),
    doppler_bin = numeric(0),
    doppler_freq = numeric(0),
    radial_velocity = numeric(0),
    signal_power = numeric(0),
    bearing = numeric(0),
    noise_level = numeric(0),
    signal_power_db = numeric(0),
    SNR = numeric(0),
    DOA_peak_resp_db = numeric(0))
  # Select relevant columns from MUSIC data

  if(nrow(MUSIC) > 0 ){


    out <- MUSIC %>% dplyr::select(range_cell, doppler_bin, range, doppler_freq = freq, radial_velocity = radial_v, DOA, lonlat)

    # Process the DOA and lonlat columns and unnest them
    out %<>% dplyr::mutate(DOA = purrr::map(DOA, \(DOA_sol) {
      if(length(DOA_sol$bearing)>0){
        data.frame(bearing = DOA_sol$bearing, signal_power = pracma::Real(diag(DOA_sol$P)), DOA_peak_resp_db = DOA_sol$peak_resp)
      }else{
        data.frame(bearing = NA_real_, signal_power = NA_real_, DOA_peak_resp_db = NA_real_)
      }
    })) %>%
      tidyr::unnest(c(DOA, lonlat))

    # Get APM object from the SeaSondeRCS object
    seasonder_apm_object <- seasonder_cs_object %>% seasonder_getSeaSondeRCS_APM()

    out$bearing_raw <- out$bearing

    # Convert MUSIC bearing to geographical bearing
    out$bearing %<>% seasonder_MUSICBearing2GeographicalBearing(seasonder_apm_object) %>% unlist()

    FOR_config <- SeaSondeR::seasonder_getSeaSondeRCS_FORConfig(seasonder_cs_object)

    out$noise_level <- FOR_config$NoiseLevel[out$range_cell] %>% magrittr::set_names(NULL)

    out %<>% dplyr::mutate(signal_power_db = self_spectra_to_dB(signal_power, seasonder_getReceiverGain_dB(seasonder_cs_object)),
                           SNR = signal_power_db - noise_level)

    # Reorder columns to match the final output structure
    out %<>% dplyr::select(longitude = lon,
                           latitude = lat,
                           range_cell,
                           range,
                           doppler_bin,
                           doppler_freq,
                           radial_velocity,
                           signal_power,
                           bearing,
                           bearing_raw,
                           noise_level,
                           signal_power_db,
                           SNR,
                           DOA_peak_resp_db)
  }

  out %<>% dplyr::mutate(datetime = datetime, .before = 1)

  return(out)
}

#' @export
seasonder_exportCSVMUSICTable <- function(seasonder_cs_object, filepath){

  table <- seasonder_exportMUSICTable(seasonder_cs_object) %>% as.data.frame()



  data.table::fwrite(table,file = filepath)


  invisible(NULL)


}

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
