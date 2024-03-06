#' Calculate the MUSIC Covariance Matrix for a Given Cell Range and Doppler Bin
#'
#' This function computes the MUltiple SIgnal Classification (MUSIC) covariance matrix
#' for a specific cell range and Doppler bin from SeaSonde Cross Spectra (CS) data. The MUSIC
#' algorithm is used in direction finding and spectral estimation.
#'
#' @param seasonder_cs_obj A SeaSondeRCS object containing the cross spectra data.
#' @param cell_range A numeric vector specifying the range cells for which to calculate the covariance matrix.
#' @param doppler_bin An integer specifying the Doppler bin for which the covariance matrix is calculated.
#'
#' @return A 3x3 complex matrix representing the MUSIC covariance matrix for the specified cell range and Doppler bin.
#'         Each element \eqn{C_{ij}} of the matrix is calculated based on the auto-spectra (for diagonal elements) or
#'         cross-spectra (for off-diagonal elements) data. For diagonal elements \eqn{i = j}, \eqn{C_{ii}} is obtained
#'         from the auto-spectra `SSA{i}`, and for off-diagonal elements \eqn{i \neq j}, \eqn{C_{ij}} is obtained from the
#'         cross-spectra `CSij`, where `i` and `j` are indices of the matrix.
#'
#' @details
#' The MUSIC algorithm is widely used for estimating the direction of arrival of signals and requires the computation
#' of a covariance matrix from sensor data. In the context of SeaSonde radar data, this function utilizes the cross
#' spectra (CS) and auto-spectra (SSA) data to construct the covariance matrix necessary for MUSIC analysis.
#' The function iterates over a 3x3 matrix to fill in the values based on whether the indices of the matrix are equal
#' (diagonal elements) or not (off-diagonal elements). Diagonal elements are derived from auto-spectra data `SSA{i}`,
#' where `i` corresponds to the antenna number. Off-diagonal elements are calculated from cross-spectra data `CSij`,
#' representing the cross-spectrum between antennas `i` and `j`.
#'
#'
seasonder_getMUSICCov <- function(seasonder_cs_obj,cell_range, doppler_bin){

out <- matrix(rep(NA_complex_,9),nrow = 3)

for(i in 1:3){

  for(j in 1:3){

    if(i==j){
      value <- seasonder_getSeaSondeRCS_dataMatrix(seasonder_cs_obj,paste0("SSA",i))[cell_range, doppler_bin]
    }else{
      value <- seasonder_getSeaSondeRCS_dataMatrix(seasonder_cs_obj,paste0("CS",paste0(as.character(sort(c(i,j))),collapse = "")))[cell_range, doppler_bin]
    }

    out[i,j] <- value

  }
}

return(out)


}

#' Eigen Decomposition of the MUSIC Covariance Matrix
#'
#' Performs the eigen decomposition of a MUSIC covariance matrix to obtain the eigenvalues
#' and eigenvectors. This decomposition is a critical step in the MUSIC algorithm for spectral
#' estimation and direction finding.
#'
#' @param C A 3x3 complex covariance matrix obtained from the `seasonder_getMUSICCov` function.
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
seasonder_MUSICCovDecomposition <- function(C){

out <- list(values = rep(NA_complex_,3), vectors =matrix(rep(NA_complex_,9),nrow = 3))

  # Get eigen-decomposition
  eigen_decomp <- eigen(C, symmetric = TRUE)

  # eigenvalues and eigenvectors
  values <- eigen_decomp$values
  vectors <- eigen_decomp$vectors

  # sort eigenvalues from smallest to largest and reorder eigenvectors accordingly

  sorted_values <- rev(values)
  sorted_vectors <- vectors[, 3:1]

out$values <- sorted_values
out$vectors <- sorted_vectors

return(out)

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
seasonder_MUSICEuclideanDistance <- function(eigen_analysis, seasonder_apm_obj){



  bearings <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_obj)

  out <- matrix(rep(NA_complex_,2*length(bearings)),nrow=2)

  rownames(out) <- c("single","dual")
  for(i in 1:2){ # Number of solutions
    En <- eigen_analysis$vectors[,1:(3-i)]

    for(j in 1:length(bearings)){
      a <- seasonder_apm_obj[,j]
names(a) <- NULL
      out[i,j] <- t(Conj(a)) %*% (En %*% t(Conj(En))) %*% a


    }


  }

  return(out)

}
