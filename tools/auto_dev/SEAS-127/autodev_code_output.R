#' Extrapolate SeaSondeR APM Matrix
#'
#' This function performs linear extrapolation on the SeaSondeR APM measurement matrix.
#' It adds \code{n} extrapolated columns to both the left and right sides of the matrix.
#'
#' The function retrieves the original bearing vector from the APM object using
#' \code{seasonder_getSeaSondeRAPM_BEAR} and obtains the bearing resolution (attribute
#' "BearingResolution"). If \code{n == 0}, the original matrix is returned unchanged.
#' For \code{n > 0}, new bearings are generated for both sides using the resolution. The
#' left side is extrapolated using the slope computed from the first two columns of the matrix,
#' and the right side is extrapolated using the slope from the last two columns. The new columns
#' are then combined with the original matrix, and the column names and the "BEAR" attribute
#' are updated to reflect the complete set of bearings.
#'
#' @param seasonder_apm_object A matrix containing SeaSondeR APM measurements. Its attributes
#'        include "BEAR" (numeric vector of bearings) and "BearingResolution" (numeric resolution).
#' @param n An integer specifying how many extrapolated columns to add on each side (default is 1).
#' @return A modified matrix with \code{n} extrapolated columns added to both sides. The column names
#'         and the "BEAR" attribute are updated with the new bearings, while the "BearingResolution"
#'         attribute remains unchanged.
#' @examples
#' \dontrun{
#'   # Create a dummy APM object
#'   dummy_mat <- matrix(1:15, nrow = 3)
#'   attr(dummy_mat, "BEAR") <- c(10, 20, 30)
#'   attr(dummy_mat, "BearingResolution") <- 10
#'   
#'   # Extrapolate 1 column on each side
#'   result <- seasonder_extrapolateAPM(dummy_mat, n = 1)
#' }
#' @export
seasonder_extrapolateAPM <- function(seasonder_apm_object, n = 1) {

  # Retrieve the original BEAR vector and bearing resolution from the input object
  BEAR <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_object)
  res <- attr(seasonder_apm_object, "BearingResolution")

  # If no extrapolation is requested (n == 0), return the original object unchanged
  if (n == 0) return(seasonder_apm_object)

  # Generate new bearings for the left and right sides using the bearing resolution
  # left_new: sequence of new bearings to the left of the original (decreasing values)
  # right_new: sequence of new bearings to the right of the original (increasing values)
  left_new <- seq(from = BEAR[1] - n * res, to = BEAR[1] - res, by = res)
  right_new <- seq(from = tail(BEAR, 1) + res, to = tail(BEAR, 1) + n * res, by = res)
  new_BEAR <- c(left_new, BEAR, right_new)

  # Store the original measurement matrix in M
  M <- seasonder_apm_object

  # Extrapolate the left side using linear extrapolation based on the first two columns of M
  # Calculate the slope from the first two columns
  slope_left <- (M[, 2] - M[, 1]) / (BEAR[2] - BEAR[1])
  # For each new left bearing, extrapolate the value from the first column
  left_mat <- sapply(left_new, function(b) M[, 1] + slope_left * (b - BEAR[1]))
  # Ensure that left_mat is a matrix with the correct number of columns
  if (is.null(dim(left_mat))) {
    left_mat <- matrix(left_mat, ncol = length(left_new))
  }

  # Extrapolate the right side using linear extrapolation based on the last two columns of M
  n_orig <- ncol(M)
  slope_right <- (M[, n_orig] - M[, n_orig - 1]) / (BEAR[length(BEAR)] - BEAR[length(BEAR) - 1])
  # For each new right bearing, extrapolate the value from the last column
  right_mat <- sapply(right_new, function(b) M[, n_orig] + slope_right * (b - BEAR[length(BEAR)]))
  if (is.null(dim(right_mat))) {
    right_mat <- matrix(right_mat, ncol = length(right_new))
  }

  # Combine the left extrapolated columns, the original matrix, and the right extrapolated columns
  new_M <- cbind(left_mat, M, right_mat)

  # Update the column names to match the new bearings and update the BEAR attribute
  colnames(new_M) <- as.character(new_BEAR)
  attr(new_M, "BEAR") <- new_BEAR
  attr(new_M, "BearingResolution") <- res

  # Return the updated measurement matrix with extrapolated columns
  return(new_M)
}

