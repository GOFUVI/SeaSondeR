

#### Defaults ####

seasonder_defaultMUSIC_parameters <- function(){

  c(40,20,2)

}


seasonder_MUSICInitCov <- function(){

  out <- matrix(rep(NA_complex_,9),nrow = 3)

  return(out)

}


seasonder_MUSICInitProjections <- function(bearings = 0){




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
    projections = list(),
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
                         projections = list(seasonder_MUSICInitProjections()),
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

SeaSondeRCS_compute_DOA_functions_start_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC DOA functions computation started")
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

SeaSondeRCS_compute_DOA_functions_end_step_text <- function() {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: MUSIC DOA functions computation ended")
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


#' Compute Power Matrix
#'
#' This function calculates the power matrix based on the provided steering vector, eigenvalues, and eigenvectors. The computation differs depending on the number of columns in the steering vector matrix.
#'
#' @param eig A list containing the eigenvalues and eigenvectors of a covariance matrix. The list should include:
#'   - \code{values}: A numeric vector of eigenvalues.
#'   - \code{vectors}: A matrix where each column is an eigenvector.
#' @param a A complex matrix representing the steering vector(s). Each column corresponds to a direction of arrival.
#'
#' @return A complex matrix representing the power matrix, calculated based on the provided eigenvalues, eigenvectors, and steering vectors. If the number of columns in \code{a} is zero, the function returns \code{NULL}.
#'
#' @details
#' The function computes the power matrix using the following steps:
#' - If \code{a} has two columns:
#'   1. Select the first two eigenvalues and their corresponding eigenvectors.
#'   2. Compute the matrix \eqn{G = a^* \cdot \text{eigVector}}, where \eqn{a^*} is the conjugate transpose of \code{a}.
#'   3. Calculate the inverse of \code{G} and its conjugate transpose.
#'   4. Compute the power matrix \eqn{P = G_{\text{inv}}^* \cdot \text{diag(eigValues)} \cdot G_{\text{inv}}}.
#' - If \code{a} has one column:
#'   1. Select the first eigenvalue and its corresponding eigenvector.
#'   2. Follow similar steps as above with single-column operations.
#'
#' If \code{a} has no columns, the function returns \code{NULL}.
#'
#' @section Mathematical Formula:
#' For a steering vector matrix \eqn{a}, eigenvectors \eqn{\text{eigVector}}, and eigenvalues \eqn{\text{eigValues}}, the power matrix is calculated as:
#' \deqn{P = G_{\text{inv}}^* \cdot \text{diag(eigValues)} \cdot G_{\text{inv}}}
#' where:
#' \eqn{G = a^* \cdot \text{eigVector}}
#' and \eqn{G_{\text{inv}}} is the inverse of \eqn{G}.
#'
#' @section References:
#' - Paolo, T. de, Cook, T., & Terrill, E. (2007). Properties of HF RADAR Compact Antenna Arrays and Their Effect on the MUSIC Algorithm. \emph{OCEANS 2007}, 1–10. doi:10.1109/oceans.2007.4449265.
#'
#' @examples
#' \dontrun{
#' # Example with two steering vectors
#' eig <- list(
#'   values = c(10, 5),
#'   vectors = matrix(c(1+1i, 1-1i, 2+2i, 2-2i), ncol = 2)
#' )
#' a <- matrix(c(1+1i, 2+2i, 3+3i, 4+4i), ncol = 2)
#' power_matrix <- seasonder_computePowerMatrix(eig, a)
#'
#' # Example with one steering vector
#' a <- matrix(c(1+1i, 2+2i), ncol = 1)
#' power_matrix <- seasonder_computePowerMatrix(eig, a)
#' }
seasonder_computePowerMatrix <- function(eig, a) {

  # Initialize the output matrix P as NULL, to store the computed power matrix
  P <- NULL

  # Check if the matrix `a` has at least one column
  if (ncol(a) > 0) {

    # Compute the conjugate transpose of matrix `a`
    a_star <- Conj(t(a))

    # If `a` has exactly two columns, compute the power matrix for two eigenvalues
    if (ncol(a) == 2) {

      # Select the first two eigenvectors and eigenvalues
      eigVector <- eig$vectors[, c(1, 2)]
      eigValues <- eig$values[c(1, 2)]

      # Compute the matrix G as the product of the conjugate transpose of `a` and the selected eigenvectors
      G <- a_star %*% eigVector

      # Compute the inverse of G
      G_inv <- solve(G)

      # Compute the conjugate transpose of G_inv
      G_inv_t <- Conj(t(G_inv))

      # Compute the power matrix P using G_inv_t, the diagonal matrix of eigenvalues, and G_inv
      P <- G_inv_t %*% diag(eigValues) %*% G_inv

      # If `a` has exactly one column, compute the power matrix for one eigenvalue
    } else if (ncol(a) == 1) {

      # Select the first eigenvector and eigenvalue
      eigVector <- eig$vectors[, c(1), drop = FALSE]
      eigValues <- eig$values[c(1)]

      # Compute the matrix G as the product of the conjugate transpose of `a` and the selected eigenvector
      G <- a_star %*% eigVector

      # Compute the inverse of G
      G_inv <- solve(G)

      # Compute the conjugate transpose of G_inv
      G_inv_t <- Conj(t(G_inv))

      # Compute the power matrix P using G_inv_t, the eigenvalue (in a matrix form), and G_inv
      P <- G_inv_t %*% matrix(eigValues) %*% G_inv
    }
  }

  # Return the computed power matrix P
  return(P)
}


#' Compute Signal Power Matrix for MUSIC Algorithm
#'
#' This function computes the signal power matrix for each direction of arrival (DOA) solution obtained
#' from the MUSIC algorithm. It updates the MUSIC data in the provided SeaSondeRCS object with the computed
#' power matrices.
#'
#' @param seasonder_cs_object A SeaSondeRCS object containing MUSIC data, including eigenvalues, eigenvectors,
#' and DOA solutions.
#'
#' @return The updated SeaSondeRCS object with the MUSIC data containing the computed power matrices for
#' both dual and single solutions.
#'
#' @details
#' The function performs the following steps:
#' 1. Retrieves the MUSIC data from the SeaSondeRCS object.
#' 2. Defines an internal function to update the DOA solutions with computed power matrices:
#'    - For dual steering vectors (\code{DOA_sol$dual$a}), computes the power matrix using
#'      \code{seasonder_computePowerMatrix} and updates \code{DOA_sol$dual$P}.
#'    - For single steering vectors (\code{DOA_sol$single$a}), computes the power matrix using
#'      \code{seasonder_computePowerMatrix} and updates \code{DOA_sol$single$P}.
#' 3. Iterates through the MUSIC data, applying the update function to each set of eigenvalues and DOA solutions.
#' 4. Updates the SeaSondeRCS object with the modified MUSIC data.
#'
#' @seealso
#' \code{\link{seasonder_computePowerMatrix}}
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map2
#' @importFrom rlang zap
#'
#' @examples
#' \dontrun{
#' # Assuming seasonder_cs_object is a valid SeaSondeRCS object
#' updated_object <- seasonder_MUSICComputeSignalPowerMatrix(seasonder_cs_object)
#' }
seasonder_MUSICComputeSignalPowerMatrix <- function(seasonder_cs_object) {

  # Initialize the DOA_solutions with a placeholder indicating no prior solutions
  DOA_solutions <- rlang::zap()

  # Retrieve the MUSIC data from the SeaSondeRCS object
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Define an internal function to update the DOA solutions with computed power matrices
  update_DOA_solutions <- function(eig, DOA_sol) {

    # Copy the current DOA solution to modify and return
    out <- DOA_sol

    # If dual steering vectors are available, compute the dual power matrix
    if (ncol(DOA_sol$dual$a) > 0) {

      # Compute the dual power matrix
      P_dual <- seasonder_computePowerMatrix(eig, DOA_sol$dual$a)

      # Update the dual power matrix in the DOA solution if computation was successful
      if (!is.null(P_dual)) {
        out$dual$P <- P_dual
      }
    }

    # Compute the single power matrix for single steering vectors
    P_single <- seasonder_computePowerMatrix(eig, DOA_sol$single$a)

    # Update the single power matrix in the DOA solution if computation was successful
    if (!is.null(P_single)) {
      out$single$P <- P_single
    }

    return(out) # Return the updated DOA solution
  }

  # Update DOA solutions for each eigenvalue-eigenvector pair in the MUSIC data
  MUSIC %<>% dplyr::mutate(
    DOA_solutions = purrr::map2(eigen, DOA_solutions, update_DOA_solutions)
  )

  # Update the SeaSondeRCS object with the modified MUSIC data
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Return the updated SeaSondeRCS object
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


#' Validate Eigenvalue Ratio Using MUSIC Algorithm
#'
#' This function implements the P1 test for solutions derived using the MUSIC algorithm.
#' The test checks the ratio between the largest and the second-largest eigenvalues, which
#' serves as an indicator of signal quality.
#'
#' @param seasonder_cs_object A SeaSondeRCS object containing the MUSIC solutions and related data.
#'
#' @details
#' The P1 test is based on the ratio of the largest eigenvalue (\(\lambda_1\)) to the second-largest eigenvalue (\(\lambda_2\)):
#' \[
#' \text{Ratio} = \frac{\lambda_1}{\lambda_2}
#' \]
#' This ratio is compared to a threshold defined in the MUSIC parameters to determine whether the solution
#' is considered valid. Solutions failing this test are marked as "single."
#'
#' @return The updated SeaSondeRCS object with the following modifications:
#' - A new column \code{eigen_values_ratio} in the MUSIC data.
#' - A logical column \code{P1_check} indicating whether each solution passes the P1 test.
#' - Updated \code{retained_solution} values for solutions that fail the test.
#'
#' @seealso
#' \code{\link{seasonder_getSeaSondeRCS_MUSIC}}, \code{\link{seasonder_setSeaSondeRCS_MUSIC}}
#'
#' @importFrom dplyr pull
#' @importFrom purrr map list_c
#'
#' @examples
#' \dontrun{
#' updated_obj <- seasonder_MUSICCheckEigenValueRatio(seasonder_cs_object)
#' }
seasonder_MUSICCheckEigenValueRatio <- function(seasonder_cs_object){
  # Extract MUSIC data from the object
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Extract eigenvalues and keep only the first two for each solution
  MUSIC_eigen_values <- MUSIC %>% dplyr::pull("eigen") %>% purrr::map(\(eig) eig$values[1:2])

  # Calculate the ratio of the largest to the second largest eigenvalue
  eigen_values_ratio <- MUSIC_eigen_values %>% purrr::map(\(values) values[1]/values[2]) %>% purrr::list_c()

  # Add the eigenvalues ratio to the MUSIC data
  MUSIC$eigen_values_ratio <- eigen_values_ratio

  # Retrieve the threshold parameter for the P1 test
  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(1)

  # Determine whether each solution passes the P1 test
  P1_check <- eigen_values_ratio < MUSIC_parameter

  # Add the P1 check results to the MUSIC data
  MUSIC$P1_check <- P1_check

  # Mark solutions that fail the P1 check as "single"
  MUSIC$retained_solution[!P1_check] <- "single"

  # Update the MUSIC data in the object
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Return the modified object
  return(seasonder_cs_object)
}


#' Validate Signal Power Ratios Using MUSIC Algorithm
#'
#' This function implements the P2 test for solutions derived using the MUSIC algorithm.
#' The test evaluates the ratio between the largest and smallest signal powers for dual-bearing solutions.
#'
#' @param seasonder_cs_object A SeaSondeRCS object containing the MUSIC solutions and related data.
#'
#' @details
#' The P2 test is based on the ratio of the largest signal power (\(P_{\text{max}}\)) to the smallest signal power (\(P_{\text{min}}\)):
#' \[
#' \text{Ratio} = \frac{P_{\text{max}}}{P_{\text{min}}}
#' \]
#' This ratio is compared to a threshold defined in the MUSIC parameters. Only solutions that meet the following criteria are retained:
#' - The solution has two bearings.
#' - The signal power ratio is below the threshold.
#'
#' Solutions failing this test are marked as "single."
#'
#' @return The updated SeaSondeRCS object with the following modifications:
#' - A new column \code{signal_power_ratio} in the MUSIC data.
#' - A logical column \code{P2_check} indicating whether each solution passes the P2 test.
#' - Updated \code{retained_solution} values for solutions that fail the test.
#'
#' @seealso
#' \code{\link{seasonder_getSeaSondeRCS_MUSIC}}, \code{\link{seasonder_setSeaSondeRCS_MUSIC}}
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_dbl
#'
#' @examples
#' \dontrun{
#' updated_obj <- seasonder_MUSICCheckSignalPowers(seasonder_cs_object)
#' }
seasonder_MUSICCheckSignalPowers <- function(seasonder_cs_object){
  # Extract MUSIC data from the object
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Compute the ratio of signal powers for dual-bearing solutions
  MUSIC %<>% dplyr::mutate(signal_power_ratio = purrr::map_dbl(DOA_solutions,\(DOA_sol){
    out <- NA_real_
    if(length(DOA_sol$dual$bearing) == 2){
      P_diag <- pracma::Real(diag(DOA_sol$dual$P))
      out <- max(P_diag)/min(P_diag)
    }
    return(out)
  }), .after = "P1_check")

  # Retrieve the threshold parameter for the P2 test
  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(2)

  # Determine whether each solution passes the P2 test
  MUSIC %<>% dplyr::mutate(P2_check = purrr::map_lgl(DOA_solutions, \(DOA_sol) length(DOA_sol$dual$bearing) == 2 ) & !is.na(signal_power_ratio) & signal_power_ratio < MUSIC_parameter, .after = "signal_power_ratio")

  # Mark solutions that fail the P2 test as "single"
  MUSIC$retained_solution[!MUSIC$P2_check] <- "single"

  # Update the MUSIC data in the object
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Return the modified object
  return(seasonder_cs_object)
}


#' Validate Signal Matrix Power Ratios Using MUSIC Algorithm
#'
#' This function implements the P3 test for solutions derived using the MUSIC algorithm.
#' The test evaluates the ratio between diagonal and off-diagonal powers in the signal covariance matrix.
#'
#' @param seasonder_cs_object A SeaSondeRCS object containing the MUSIC solutions and related data.
#'
#' @details
#' The P3 test evaluates the power ratio between diagonal (\(P_{\text{diag}}\)) and off-diagonal (\(P_{\text{off-diag}}\)) elements of the covariance matrix:
#' \[
#' \text{Ratio} = \frac{P_{\text{diag}}}{P_{\text{off-diag}}}
#' \]
#' This ratio is compared to a threshold defined in the MUSIC parameters. Only solutions that meet the following criteria are retained:
#' - The solution has two bearings.
#' - The power ratio is above the threshold.
#'
#' Solutions failing this test are marked as "single."
#'
#' @return The updated SeaSondeRCS object with the following modifications:
#' - A new column \code{diag_off_diag_power_ratio} in the MUSIC data.
#' - A logical column \code{P3_check} indicating whether each solution passes the P3 test.
#' - Updated \code{retained_solution} values for solutions that fail the test.
#'
#' @seealso
#' \code{\link{seasonder_getSeaSondeRCS_MUSIC}}, \code{\link{seasonder_setSeaSondeRCS_MUSIC}}
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_dbl
#'
#' @examples
#' \dontrun{
#' updated_obj <- seasonder_MUSICCheckSignalMatrix(seasonder_cs_object)
#' }
seasonder_MUSICCheckSignalMatrix <- function(seasonder_cs_object){
  # Extract MUSIC data from the object
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Compute the ratio of diagonal to off-diagonal power for dual-bearing solutions
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

  # Retrieve the threshold parameter for the P3 test
  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(3)

  # Determine whether each solution passes the P3 test
  MUSIC %<>% dplyr::mutate(P3_check = purrr::map_lgl(DOA_solutions, \(DOA_sol) length(DOA_sol$dual$bearing) == 2 ) & !is.na(diag_off_diag_power_ratio) & diag_off_diag_power_ratio > MUSIC_parameter, .after = "diag_off_diag_power_ratio")

  # Mark solutions that fail the P3 test as "single"
  MUSIC$retained_solution[!MUSIC$P3_check] <- "single"

  # Update the MUSIC data in the object
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Return the modified object
  return(seasonder_cs_object)
}


#' Test Dual-Bearing Solutions Using MUSIC Algorithm
#'
#' This function applies a sequence of tests (P1, P2, and P3) to validate dual-bearing solutions
#' derived using the MUSIC algorithm. The tests evaluate the quality of solutions based on
#' eigenvalue ratios, signal power ratios, and covariance matrix power ratios.
#'
#' @param seasonder_cs_object A SeaSondeRCS object containing MUSIC solutions and related data.
#'
#' @details
#' The function applies the following sequence of tests:
#' 1. **P1: Eigenvalue Ratio Test**:
#'    - Evaluates the ratio between the largest and second-largest eigenvalues.
#' 2. **P2: Signal Power Ratio Test**:
#'    - Validates the ratio of signal powers for dual-bearing solutions.
#' 3. **P3: Signal Matrix Power Ratio Test**:
#'    - Checks the ratio of diagonal to off-diagonal powers in the covariance matrix.
#'
#' Each test updates the MUSIC solutions in the input object, marking solutions that fail the tests as "single."
#' The function also logs the start and end of the testing process as part of the object's processing steps.
#'
#' @return The updated SeaSondeRCS object with validated dual-bearing solutions and recorded processing steps.
#'
#' @seealso
#' \code{\link{seasonder_MUSICCheckEigenValueRatio}}, \code{\link{seasonder_MUSICCheckSignalPowers}},
#' \code{\link{seasonder_MUSICCheckSignalMatrix}}, \code{\link{seasonder_setSeaSondeRCS_ProcessingSteps}}
#'
#' @examples
#' \dontrun{
#' updated_obj <- seasonder_MUSICTestDualSolutions(seasonder_cs_object)
#' }
seasonder_MUSICTestDualSolutions <- function(seasonder_cs_object) {

  # Log the start of the dual solutions testing process
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_dual_solutions_testing_start_step_text())

  # Apply the P1 test to validate eigenvalue ratios
  seasonder_cs_object %<>% seasonder_MUSICCheckEigenValueRatio()

  # Apply the P2 test to validate signal power ratios for dual-bearing solutions
  seasonder_cs_object %<>% seasonder_MUSICCheckSignalPowers()

  # Apply the P3 test to validate diagonal to off-diagonal power ratios
  seasonder_cs_object %<>% seasonder_MUSICCheckSignalMatrix()

  # Log the end of the dual solutions testing process
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_dual_solutions_testing_end_step_text())

  # Return the updated object with validated dual solutions
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
  # Initialize an empty list to store the eigen decomposition results
  out <- seasonder_MUSICInitEigenDecomp()

  # Perform the eigen decomposition of the symmetric complex covariance matrix
  eigen_decomp <- eigen(C, symmetric = TRUE)

  # Extract eigenvalues and store them in the output list
  out$values <- eigen_decomp$values
  # Extract eigenvectors and store them in the output list
  out$vectors <- eigen_decomp$vectors

  # Return the list containing eigenvalues and eigenvectors
  return(out)
}




#' Eigen Decomposition of the MUSIC Covariance Matrix
#'
#' Performs the eigen decomposition of a MUSIC covariance matrix to obtain the eigenvalues
#' and eigenvectors. This decomposition is a critical step in the MUSIC algorithm for spectral
#' estimation and direction finding, as it enables the identification of the signal and noise subspaces.
#'
#' @param seasonder_cs_object A SeaSondeRCS object containing the covariance matrices derived from cross-spectra data.
#'
#' @return An updated SeaSondeRCS object where each Doppler cell includes the eigenvalues and eigenvectors of its covariance matrix.
#'         The eigenvalues are sorted in descending order, and the eigenvectors are aligned accordingly. The updates include:
#'         \itemize{
#'           \item \code{eigen$values}: A numeric vector containing the sorted eigenvalues for each Doppler cell.
#'           \item \code{eigen$vectors}: A 3x3 matrix of the corresponding eigenvectors for each Doppler cell, aligned with the eigenvalues.
#'         }
#'
#' @details
#' The covariance matrix represents one Doppler cell of the averaged cross-spectra of three received signals. This matrix captures
#' the summation of signals from all bearings (plus noise) received by the antennas. To estimate the direction of arrival (DOA),
#' the covariance matrix is subjected to eigenvalue decomposition (diagonalization) to estimate the signal and noise subspaces.
#'
#' In practical HF radar systems, there are two primary sources of noise:
#' 1. \emph{System (thermal) noise}: Generated by the receiving equipment and assumed to be uncorrelated between antennas.
#' 2. \emph{Spatial noise field}: Includes wind-wave noise and current noise, modeled as Gaussian, which introduces correlation.
#'
#' The eigenvalue decomposition produces:
#' - Three eigenvalues, ordered from largest to smallest.
#' - Three corresponding eigenvectors forming a 3-dimensional orthonormal basis.
#'
#' Based on the largest eigenvalues:
#' - If there is one signal present, the first eigenvector defines a 1-dimensional signal subspace, and the remaining eigenvectors
#'   represent a 2-dimensional noise subspace.
#' - If two signals are present, the first two eigenvectors form a 2-dimensional signal subspace, while the remaining eigenvector
#'   represents a 1-dimensional noise subspace.
#'
#' The signal and noise subspaces are orthogonal. This decomposition facilitates identifying the signal's direction by finding the
#' antenna manifold that best fits the signal subspace.
#'
#' @references Paolo, T. de, Cook, T. & Terrill, E. Properties of HF RADAR Compact Antenna Arrays and Their Effect on the MUSIC Algorithm. OCEANS 2007 1–10 (2007) doi:10.1109/oceans.2007.4449265.
#'
#' @seealso
#' \code{\link{seasonder_MUSICComputeCov}} for computing the covariance matrix.
#'
#' @examples
#' \dontrun{
#' # Assuming `cs_object` is a valid SeaSondeRCS object
#' cs_object <- seasonder_MUSICCovDecomposition(cs_object)
#' }
seasonder_MUSICCovDecomposition <- function(seasonder_cs_object){
  # Add a processing step entry indicating the start of the MUSIC covariance decomposition
  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_MUSIC_covariance_decomposition_start_step_text())

  # Retrieve the MUSIC data structure from the SeaSondeRCS object
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Apply the eigen decomposition function to each covariance matrix in the MUSIC data
  MUSIC %<>% dplyr::mutate(eigen = purrr::map(cov, seasonder_eigen_decomp_C))

  # Update the MUSIC data in the SeaSondeRCS object with the new eigen decomposition results
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Add a processing step entry indicating the end of the MUSIC covariance decomposition
  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_MUSIC_covariance_decomposition_end_step_text())

  # Return the updated SeaSondeRCS object
  return(seasonder_cs_object)
}




#' Compute Antenna Pattern Projections for the MUSIC Algorithm
#'
#' This function computes the projection of the antenna pattern vector onto the noise subspace,
#' a critical step in the Multiple Signal Classification (MUSIC) algorithm. It is used to estimate
#' the direction of arrival (DOA) by identifying the bearing that minimizes this projection.
#'
#' @param En A matrix containing the eigenvectors of the noise subspace, derived from the
#'           covariance matrix of the signals.
#' @param a A complex-valued vector representing the antenna manifold response for a specific
#'          bearing. Each element corresponds to the response of an antenna element.
#'
#' @return A complex scalar representing the magnitude of the projection of the antenna
#'         manifold vector onto the noise subspace. This value indicates how close the
#'         antenna manifold vector is to being orthogonal to the noise subspace.
#'
#' @details
#' The MUSIC algorithm leverages the property that the antenna manifold vector is orthogonal
#' to the noise subspace eigenvectors in an ideal scenario. However, in practice, noise in the
#' covariance matrix perturbs the noise subspace, resulting in a small but non-zero projection.
#' This function calculates the magnitude of this projection using the formula:
#'
#' \deqn{P = a^H (En E_n^H) a}
#'
#' where:
#' - \eqn{a} is the antenna manifold vector.
#' - \eqn{En} is the noise subspace eigenvector matrix.
#' - \eqn{H} denotes the Hermitian (conjugate transpose) operator.
#'
#' The bearing that produces the smallest projection is considered the best estimate of the signal bearing,
#' as it corresponds to the direction where the signal is strongest relative to the noise.
#'
#' @section References:
#' - Paolo, T. de, Cook, T., & Terrill, E. (2007). Properties of HF RADAR Compact Antenna Arrays and Their Effect on the MUSIC Algorithm. \emph{OCEANS 2007}, 1–10. doi:10.1109/oceans.2007.4449265.
#'
#' @examples
#' \dontrun{
#' # Assume En is a 3x3 noise subspace eigenvector matrix and a is the antenna manifold vector.
#' En <- matrix(c(0.5+0.5i, -0.3i, 0, 0.4, 0.6-0.2i, -0.4i, 0, 0.3+0.3i, -0.1i), nrow = 3)
#' a <- c(1+1i, -0.5+0.5i, 0.2-0.3i)
#' projection <- seasonder_compute_antenna_pattern_proyections(En, a)
#' }
#'
seasonder_compute_antenna_pattern_proyections <- function(En, a){
  # Computes the projection of the antenna pattern onto the noise subspace.
  # This function is used to calculate the Euclidean distance in the MUSIC algorithm.
  # En: A matrix containing eigenvectors corresponding to the noise subspace.
  # a: A vector representing the antenna pattern response for a specific bearing.
  # The formula used is a conjugate transpose of 'a' times En, times En's conjugate transpose, times 'a'.
  Conj(t(a)) %*% (En %*% Conj(t(En))) %*% a
}


#' Compute DOA Functions Using the MUSIC Algorithm
#'
#' This function calculates the Direction of Arrival (DOA) functions based on the MUSIC algorithm
#' for a given SeaSonde cross-spectra (CS) object. It projects the antenna patterns onto the noise
#' subspace for each Doppler bin and computes single and dual signal solutions, following the MUSIC method.
#'
#' @param seasonder_cs_object An object representing the cross-spectra (CS) data from SeaSonde.
#'
#' @return The updated `seasonder_cs_object` with the MUSIC DOA functions computed and appended.
#'
#' @details
#' The function operates as follows:
#' 1. It sets a processing step indicating the start of DOA function computation.
#' 2. Retrieves the Antenna Pattern Measurement (APM) and bearings associated with the CS object.
#' 3. Iteratively computes projections of antenna pattern responses into the noise subspace for each
#'    Doppler bin using the MUSIC algorithm. This includes:
#'    - Initializing storage for projection results.
#'    - Calculating projections for single (m = 1) and dual (m = 2) signal solutions using
#'      the eigenvectors defining the noise subspace.
#'    - For each bearing, projecting the antenna manifold vector onto the noise subspace,
#'      as described by the formula:
#'      \deqn{DOA(\theta) = \frac{1}{A^*(\theta) E_n E_n^* A(\theta)}}
#'      where:
#'      - \eqn{E_n} is the eigenvector matrix of the noise subspace.
#'      - \eqn{A(\theta)} is the antenna pattern response vector at bearing \eqn{\theta}.
#'      - \eqn{A^*(\theta)} is its conjugate transpose.
#' 4. Appends the computed DOA functions to the MUSIC data of the CS object.
#' 5. Updates the processing step to indicate completion.
#'
#' @section References:
#' - Paolo, T. de, Cook, T., & Terrill, E. (2007). Properties of HF RADAR Compact Antenna Arrays and Their Effect on the MUSIC Algorithm. \emph{OCEANS 2007}, 1–10. doi:10.1109/oceans.2007.4449265.
#'
#' @seealso
#' \code{\link{seasonder_compute_antenna_pattern_proyections}} for computing projections.
#'
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom magrittr %<>%
#'
#' @examples
#' \dontrun{
#' cs_object <- seasonder_MUSICComputeDOAFunctions(cs_object)
#' }
seasonder_MUSICComputeDOAFunctions <- function(seasonder_cs_object){
  # Sets a processing step message indicating the start of distance computation.
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_compute_DOA_functions_start_step_text())

  # Retrieves the Antenna Pattern Measurement (APM) object associated with the cross spectra (CS) object.
  seasonder_apm_obj <- seasonder_getSeaSondeRCS_APM(seasonder_cs_object)

  # Retrieves the bearings associated with the APM object.
  bearings <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_obj)

  # Retrieves the MUSIC-related data associated with the CS object.
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Updates the MUSIC object by calculating proyections of the antenna pattern into the noise sub-space for each Doppler bin.
  MUSIC %<>% dplyr::mutate(projections = purrr::map(eigen, \(eigen_analysis){
    # Initializes an empty matrix to store the projections for single and dual solutions.
    out <- seasonder_MUSICInitProjections(bearings = bearings)

    # Iterates over the two possible signal solutions (single and dual).
    for(i in 1:2){
      # Extracts the noise subspace eigenvectors corresponding to the current solution.
      En <- eigen_analysis$vectors[,(i+1):3, drop = F]

      # Iterates over all bearings to calculate the projection.
      for(j in 1:length(bearings)){
        # Extracts the antenna pattern response for the current bearing.
        a <- seasonder_apm_obj[,j, drop = F]
        names(a) <- NULL

        # Calculates the projection for the current solution and bearing.
        out[i,j] <- seasonder_compute_antenna_pattern_proyections(En,a)
      }
    }

    # Returns the calculated projections for the current Doppler bin.
    return(out)
  }))

  # Updates the CS object with the modified MUSIC data.
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Sets a processing step message indicating the end of projections computation.
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_compute_DOA_functions_end_step_text())

  # Returns the updated CS object.
  return(seasonder_cs_object)
}

#' Extract Direction of Arrival (DOA) Solutions Using MUSIC Algorithm
#'
#' This function analyzes projection data using the Multiple Signal Classification (MUSIC) algorithm
#' to identify Direction of Arrival (DOA) solutions for radar signals. It implements the methodology
#' described by Paolo and Terril (2007) for HF radar signal analysis.
#'
#' @param projections A matrix of projections where each column corresponds to a set of MUSIC spectra for single and dual solutions.
#'                    The matrix should have the attribute \code{"bearings"} indicating the corresponding bearing angles in degrees.
#'
#' @return A list containing the extracted single and dual DOA solutions, each with:
#' \itemize{
#'   \item \code{bearing}: The bearing(s) corresponding to the detected peak(s).
#'   \item \code{a}: The associated antenna pattern matrix values for the detected peak(s).
#'   \item \code{peak_resp}: The response levels at the detected peak(s) in dB.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reverses the distances for single and dual solution projections to enhance peak detectability.
#'   \item Detects peaks in the reversed single solution projection, retaining the highest peak.
#'   \item Detects peaks in the reversed dual solution projection, retaining the two highest peaks.
#'   \item Maps the identified peak positions back to their corresponding bearings.
#' }
#'
#' The identification of DOA solutions using MUSIC relies on the inversion of spectral distances, as detailed in Paolo and Terril (2007),
#' to emphasize potential peaks corresponding to source directions.
#'
#' @references
#' Paolo, S., & Terril, E. (2007). Detection and characterization of signals in HF radar cross-spectra using the MUSIC algorithm.
#' \emph{Journal of Atmospheric and Oceanic Technology}.
#'
#' @seealso \code{\link{seasonder_MUSICExtractPeaks}}, \code{\link{pracma::findpeaks}}
#'
#' @examples
#' \dontrun{
#' projections <- matrix(runif(100), nrow = 2, ncol = 50)
#' attr(projections, "bearings") <- seq(0, 359, length.out = 50)
#' result <- seasonder_MUSICExtractDOASolutions(projections)
#' print(result)
#' }
#'
seasonder_MUSICExtractDOASolutions <- function(projections){

  # Initialize an empty DOA solutions structure
  out <- seasonder_MUSICInitDOASolutions()

  # Extract the bearings attribute for projections
  bearings <- attr(projections,"bearings",exact = TRUE)

  # Reverse the single and dual solution projection matrices
  rev_single_solution_dist = pracma::Real(1/projections['single',,drop = TRUE])
  rev_dual_solution_dist = pracma::Real(1/projections['dual',,drop = TRUE])

  # Detect peaks in the single solution projections
  single_peaks_results <- pracma::findpeaks(rev_single_solution_dist, npeaks = 1, sortstr = TRUE)
  single_peak <- single_peaks_results[,2,drop = TRUE] # Peak location
  single_peak_resp <- NA

  # If valid peaks are found, calculate the corresponding responses
  if (!is.null(single_peaks_results)) {
    single_peak_resp <- 10*log10(single_peaks_results[,1,drop = TRUE])
  }

  # Detect peaks in the dual solution projections
  dual_peaks_results <- pracma::findpeaks(rev_dual_solution_dist, npeaks = 2, sortstr = TRUE)

  # Populate the single DOA solution fields
  out$single$bearing <-  bearings[single_peak]
  out$single$a <- seasonder_apm_obj[,single_peak, drop = FALSE]
  out$single$peak_resp <- single_peak_resp

  # Populate the dual DOA solution fields
  dual_peaks <- dual_peaks_results[,2,drop = TRUE]
  dual_peaks_resp <- NA
  if (!is.null(dual_peaks_results)) {
    dual_peaks_resp <- 10*log10(dual_peaks_results[,1,drop = TRUE])
  }

  out$dual$bearing <- bearings[dual_peaks]
  out$dual$a <- seasonder_apm_obj[,dual_peaks, drop = FALSE]
  out$dual$peak_resp <- dual_peaks_resp

  return(out)
}

#' Validate Retained Solution in MUSIC Algorithm Peak Extraction
#'
#' This function verifies and adjusts the retained solution type ("single" or "dual") based on the
#' Direction of Arrival (DOA) solutions extracted using the MUSIC algorithm.
#'
#' @param ret_sol A character string specifying the initial solution type to retain. Valid values are \code{"single"} or \code{"dual"}.
#' @param DOA_sol A list containing extracted DOA solutions, as returned by \code{\link{seasonder_MUSICExtractDOASolutions}}.
#'
#' @return A character string indicating the validated solution type:
#' \itemize{
#'   \item \code{"single"}: If only one single solution bearing is valid.
#'   \item \code{"dual"}: If valid dual solution bearings are detected.
#'   \item \code{"none"}: If no valid bearings are found.
#' }
#'
#' @details
#' The function performs the following checks:
#' \enumerate{
#'   \item If the retained solution is "dual" but no valid dual solution bearings exist, it defaults to "single" if valid.
#'   \item If the retained solution is "single" but no valid single solution bearings exist, it defaults to "none".
#' }
#'
#' This validation ensures the output solutions are consistent with the detected peaks, addressing potential discrepancies
#' in the initial assumptions about the solution type.
#'
#' @seealso \code{\link{seasonder_MUSICExtractPeaks}}, \code{\link{seasonder_MUSICExtractDOASolutions}}
#'
#' @examples
#' \dontrun{
#' ret_sol <- "dual"
#' DOA_sol <- list(single = list(bearing = 45), dual = list(bearing = c(30, 60)))
#' result <- seasonder_MUSICExtractPeaksCheckRetainedSolution(ret_sol, DOA_sol)
#' print(result)
#' }
#'
seasonder_MUSICExtractPeaksCheckRetainedSolution <- function(ret_sol, DOA_sol){

  out <- ret_sol

  # Validate dual solutions
  if (ret_sol == "dual") {

    if (length(DOA_sol$dual$bearing) == 0) {
      if (length(DOA_sol$single$bearing )==1) {
        out <- "single"
      }else{
        out <- "none"
      }
    }

    # Validate single solutions
  }else if (ret_sol == "single") {
    if (length(DOA_sol$single$bearing ) != 1) {
      out <- "none"
    }
  }

  return(out)
}

#' Extract and Validate DOA Peaks Using MUSIC Algorithm
#'
#' This function processes a \code{SeaSondeRCS} object to extract Direction of Arrival (DOA) solutions using the MUSIC algorithm
#' and validates the retained solutions based on the extracted peaks.
#'
#' @param seasonder_cs_object An object of class \code{SeaSondeRCS} containing cross-spectra data processed with the MUSIC algorithm.
#'
#' @return An updated \code{SeaSondeRCS} object with the following fields modified:
#' \itemize{
#'   \item \code{MUSIC}: Contains the extracted DOA solutions.
#'   \item \code{ProcessingSteps}: Includes a log of the peak extraction process.
#' }
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Initializes the peak extraction process and logs the start.
#'   \item Extracts DOA solutions for each set of projections using \code{\link{seasonder_MUSICExtractDOASolutions}}.
#'   \item Validates and adjusts the retained solution types using \code{\link{seasonder_MUSICExtractPeaksCheckRetainedSolution}}.
#'   \item Updates the \code{SeaSondeRCS} object with the extracted and validated solutions.
#'   \item Logs the completion of the peak extraction process.
#' }
#'
#' The MUSIC algorithm's implementation follows the theoretical framework outlined by Paolo and Terril (2007), emphasizing the identification of signal directions
#' in HF radar cross-spectra.
#'
#' @references
#' Paolo, S., & Terril, E. (2007). Detection and characterization of signals in HF radar cross-spectra using the MUSIC algorithm.
#' \emph{Journal of Atmospheric and Oceanic Technology}.
#'
#' @seealso \code{\link{seasonder_MUSICExtractDOASolutions}}, \code{\link{seasonder_MUSICExtractPeaksCheckRetainedSolution}}
#'
#' @examples
#' \dontrun{
#' cs_object <- seasonder_createSeaSondeRCS(x = "path/to/cs_file")
#' cs_object <- seasonder_MUSICExtractPeaks(cs_object)
#' print(cs_object)
#' }
#'
seasonder_MUSICExtractPeaks <- function(seasonder_cs_object){

  projections <- retained_solution <- DOA_solutions  <- rlang::zap()

  # Add a log entry to indicate the start of the peak extraction process
  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_peak_extraction_start_step_text())

  # Retrieve the Antenna Pattern Matrix (APM) from the object
  seasonder_apm_obj <- seasonder_getSeaSondeRCS_APM(seasonder_cs_object)

  # Retrieve the MUSIC data structure
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Iterate over the projections matrix in the MUSIC object to extract DOA solutions
  MUSIC %<>% dplyr::mutate(DOA_solutions = purrr::map(projections, seasonder_MUSICExtractDOASolutions))

  # Update the retained solution field based on DOA solutions
  MUSIC %<>% dplyr::mutate(retained_solution = purrr::map2_chr(retained_solution, DOA_solutions, seasonder_MUSICExtractPeaksCheckRetainedSolution))

  # Update the MUSIC field in the `SeaSondeRCS` object
  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Log the end of the peak extraction process
  seasonder_cs_object  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_peak_extraction_end_step_text())

  # Return the updated object
  return(seasonder_cs_object)
}






#' Select Direction of Arrival (DOA) from MUSIC Algorithm Results
#'
#' This function processes the results of the MUSIC algorithm, selects the relevant Direction of Arrival (DOA)
#' based on the specified retained solution, and updates the corresponding `SeaSondeRCS` object with the selected
#' DOA and updated processing steps.
#'
#' @param seasonder_cs_object A `SeaSondeRCS` object containing the results of the MUSIC algorithm and associated metadata.
#'
#' @return An updated `SeaSondeRCS` object with the selected DOA stored in the MUSIC results and updated processing steps.
#'
#' @details
#' The function performs the following steps:
#' 1. Updates the processing steps to indicate the start of the DOA selection process.
#' 2. Retrieves the MUSIC algorithm results from the `SeaSondeRCS` object.
#' 3. Maps the retained solution index to the corresponding DOA solution for each entry in the MUSIC results.
#' 4. Stores the updated MUSIC results, including the selected DOA, back into the `SeaSondeRCS` object.
#' 5. Updates the processing steps to indicate the end of the DOA selection process.
#'
#' @section Processing Steps:
#' The function appends the following processing steps to the `ProcessingSteps` attribute of the `SeaSondeRCS` object:
#' - Start of DOA selection.
#' - End of DOA selection.
#'
#' @seealso
#' \code{\link{seasonder_setSeaSondeRCS_ProcessingSteps}} to manage processing steps.
#' \code{\link{seasonder_getSeaSondeRCS_MUSIC}} to retrieve MUSIC results.
#' \code{\link{seasonder_setSeaSondeRCS_MUSIC}} to update MUSIC results.
#'
#' @importFrom purrr map2
#' @importFrom dplyr mutate
#' @importFrom magrittr %<>%
#'
#' @examples
#' \dontrun{
#' # Assuming `seasonder_cs_obj` is a valid SeaSondeRCS object with MUSIC results
#' updated_obj <- seasonder_MUSICSelectDOA(seasonder_cs_obj)
#' }
seasonder_MUSICSelectDOA <- function(seasonder_cs_object) {

  # Placeholder variables to initialize empty values for DOA solutions and retained solutions
  DOA_solutions <- retained_solution <- rlang::zap()

  # Append a processing step to indicate the start of the DOA selection process
  # This logs and tracks the beginning of this operation in the processing history
  seasonder_cs_object %<>%
    seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_doa_selection_start_step_text())

  # Retrieve the MUSIC algorithm results from the SeaSondeRCS object
  # These results contain potential DOA solutions and metadata
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Define a helper function to extract the relevant DOA solution
  extract_DOA_sol <- function(DOA_sol, retained_sol) {
    out <- NULL
    # If a retained solution is specified, select the corresponding DOA solution
    if (retained_sol != "none") {
      out <- DOA_sol[[retained_sol]]
    }
    return(out)
  }

  # For each row in the MUSIC results, map the retained solution to the corresponding DOA solution
  # This updates the MUSIC results to include only the selected DOA
  MUSIC %<>%
    dplyr::mutate(DOA = purrr::map2(DOA_solutions, retained_solution, extract_DOA_sol))

  # Update the MUSIC results in the SeaSondeRCS object with the selected DOA
  seasonder_cs_object %<>%
    seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  # Append a processing step to indicate the end of the DOA selection process
  # This logs and tracks the completion of this operation in the processing history
  seasonder_cs_object %<>%
    seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_doa_selection_end_step_text())

  # Return the updated SeaSondeRCS object
  # This object now includes the selected DOA in the MUSIC results
  return(seasonder_cs_object)
}



seasonder_runMUSIC <- function(seasonder_cs_object){

  seasonder_logAndMessage("seasonder_runMUSIC: MUSIC algorithm started.", "info")


  out <- seasonder_cs_object

  out  %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_MUSIC_start_step_text())

  out %<>% seasonder_MUSICComputeCov()

  out %<>% seasonder_MUSICCovDecomposition()

  out %<>% seasonder_MUSICComputeDOAFunctions()

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
