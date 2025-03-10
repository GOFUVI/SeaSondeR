#' Transform CSSY Body to SeaSonde CS Data Structure
#'
#' This function converts the body structure of a CSSY file into a list of matrices that conform
#' to the data structure required for creating a SeaSondeRCS object. The conversion is performed
#' by mapping specific fields:
#' \describe{
#'   \item{SSA1, SSA2, SSA3}{Matrices are built using the numeric vectors found in the \code{cs1a}, \code{cs2a}
#'          and \code{cs3a} fields respectively.}
#'   \item{CS12, CS13, CS23}{Each complex cross-spectra matrix is formed by combining the real parts
#'          from \code{c12r}, \code{c13r} and \code{c23r} with the corresponding imaginary parts
#'          from \code{c12i}, \code{c13i} and \code{c23i}.}
#'   \item{QC}{The quality control matrix is obtained directly from the \code{csqf} field.}
#' }
#'
#' Each row in the output matrices corresponds to the index provided by \code{cell$indx$index} in the input list.
#'
#' @param body A list representing the body of a CSSY file. Each element of the list is expected to be a
#'   cell containing the following fields: \code{indx} (which includes an \code{index}), \code{cs1a}, \code{cs2a}, \code{cs3a},
#'   \code{c12r}, \code{c12i}, \code{c13r}, \code{c13i}, \code{c23r}, \code{c23i} and \code{csqf}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{SSA1}{A numeric matrix containing self-spectra from \code{cs1a}.}
#'   \item{SSA2}{A numeric matrix containing self-spectra from \code{cs2a}.}
#'   \item{SSA3}{A numeric matrix containing self-spectra from \code{cs3a}.}
#'   \item{CS12}{A complex matrix formed by pairing \code{c12r} (real) and \code{c12i} (imaginary).}
#'   \item{CS13}{A complex matrix formed by pairing \code{c13r} (real) and \code{c13i} (imaginary).}
#'   \item{CS23}{A complex matrix formed by pairing \code{c23r} (real) and \code{c23i} (imaginary).}
#'   \item{QC}{A numeric matrix containing the quality control data from \code{csqf}.}
#' }
#'
#' @details
#' The function first determines the maximum index among the cells in the body, which defines the number of rows
#' for the matrices. Then, it calculates the number of columns for each matrix based on the length of the corresponding
#' vectors from the first cell where they appear. Finally, each cell's data is inserted into the appropriate row
#' of the matrices as indicated by the cell's \code{indx$index} value.
#'
#' @examples
#' \dontrun{
#'   # Example with a single cell
#'   cell <- list(
#'     indx  = list(index = 1),
#'     cs1a  = c(1, 2, 3),
#'     cs2a  = c(4, 5, 6),
#'     cs3a  = c(7, 8, 9),
#'     c12r  = c(10, 11, 12),
#'     c12i  = c(13, 14, 15),
#'     c13r  = c(16, 17, 18),
#'     c13i  = c(19, 20, 21),
#'     c23r  = c(22, 23, 24),
#'     c23i  = c(25, 26, 27),
#'     csqf  = c(28, 29, 30)
#'   )
#'   body <- list(cell)
#'   transformed <- seasonder_CSSY2CSData(body)
#'   print(transformed)
#' }
#'
#' @export

seasonder_CSSY2CSData <- function(body) {
  # Validate the input to ensure it's a non-empty list
  if (!is.list(body) || length(body) == 0) stop("Invalid body: must be a non-empty list")

  # Extract the row indices from each cell using cell$indx$index
  indices <- sapply(body, function(cell) cell$indx$index)
  max_index <- max(indices)

  # Helper function: for a given field, determine the number of columns based on the first cell that contains that field
  get_ncols <- function(field) {
    for (cell in body) {
      if (!is.null(cell[[field]])) {
        return(length(cell[[field]]))
      }
    }
    return(0)
  }

  # Determine the number of columns for self spectra (SSA), quality control (QC) and cross-spectra (for CS matrices)
  ncols_SSA1 <- get_ncols("cs1a")
  ncols_SSA2 <- get_ncols("cs2a")
  ncols_SSA3 <- get_ncols("cs3a")
  ncols_QC   <- get_ncols("csqf")
  ncols_CS12 <- get_ncols("c12r")  # assume c12r and c12i have same length
  ncols_CS13 <- get_ncols("c13r")
  ncols_CS23 <- get_ncols("c23r")

  # Create empty matrices for each required field. Each matrix will have 'max_index' rows (provided by cell$indx$index)
  # and a number of columns as determined by the helper function above.
  SSA1 <- matrix(NA_real_, nrow = max_index, ncol = ncols_SSA1)
  SSA2 <- matrix(NA_real_, nrow = max_index, ncol = ncols_SSA2)
  SSA3 <- matrix(NA_real_, nrow = max_index, ncol = ncols_SSA3)
  QC   <- matrix(NA_real_, nrow = max_index, ncol = ncols_QC)

  # Initialize complex matrices for cross spectra
  CS12 <- matrix(NA_complex_, nrow = max_index, ncol = ncols_CS12)
  CS13 <- matrix(NA_complex_, nrow = max_index, ncol = ncols_CS13)
  CS23 <- matrix(NA_complex_, nrow = max_index, ncol = ncols_CS23)

  # Iterate over each cell and assign the corresponding data to the appropriate row in the matrices
  for (cell in body) {
    row <- cell$indx$index
    if (!is.null(cell$cs1a)) SSA1[row, ] <- cell$cs1a
    if (!is.null(cell$cs2a)) SSA2[row, ] <- cell$cs2a
    if (!is.null(cell$cs3a)) SSA3[row, ] <- cell$cs3a
    if (!is.null(cell$csqf)) QC[row, ]   <- cell$csqf

    # For cross spectra, combine the real and imaginary parts to create complex numbers
    if (!is.null(cell$c12r) && !is.null(cell$c12i)) {
      CS12[row, ] <- complex(real = cell$c12r, imaginary = cell$c12i)
    }
    if (!is.null(cell$c13r) && !is.null(cell$c13i)) {
      CS13[row, ] <- complex(real = cell$c13r, imaginary = cell$c13i)
    }
    if (!is.null(cell$c23r) && !is.null(cell$c23i)) {
      CS23[row, ] <- complex(real = cell$c23r, imaginary = cell$c23i)
    }
  }

  # Return a list containing all the matrices required for a SeaSondeRCS object
  list(
    SSA1 = SSA1,
    SSA2 = SSA2,
    SSA3 = SSA3,
    CS12 = CS12,
    CS13 = CS13,
    CS23 = CS23,
    QC   = QC
  )
}
