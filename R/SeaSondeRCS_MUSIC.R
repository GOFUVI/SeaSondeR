

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



seasonder_initSeaSondeRCS_MUSIC <- function(seasonder_cs_object, range_cells = NULL, doppler_bins = NULL){


  if(is.null(range_cells) || is.null(doppler_bins)){

    if (is.null(range_cells)) {

      range_cells <- 1:seasonder_getnRangeCells(seasonder_obj = seasonder_cs_object)
    }


    if (is.null(doppler_bins)) {

      doppler_bins <- 1:seasonder_getnDopplerCells(seasonder_obj = seasonder_cs_object)
    }

    out <- expand.grid(range_cell = range_cells, doppler_bin = doppler_bins)

  }else{
    out <- data.frame(range_cell = range_cells, doppler_bin = doppler_bins)
  }
  out <- tibble::as_tibble(out)


  out %<>% dplyr::mutate(range= seasonder_getCellsDistKm(seasonder_cs_object)[range_cell],
                         radial_v = seasonder_getBinsRadialVelocity(seasonder_cs_object)[doppler_bin],
                         cov = list(seasonder_MUSICInitCov()),
                         eigen = list(seasonder_MUSICInitEigenDecomp()),
                         distances = list(seasonder_MUSICInitDistances()),
                         DOA_solutions = list(seasonder_MUSICInitDOASolutions()),
                         eigen_values_ratio=NA_real_,
                         P1_check = TRUE,
                         retained_solution = "dual",
                         DOA = list(c(NA_real_, NA_real_))
  )



  return(out)
}

seasonder_initMUSICData <- function(seasonder_cs_object){

  out <- seasonder_cs_object

  out %<>% seasonder_setSeaSondeRCS_MUSIC_parameters(seasonder_defaultMUSIC_parameters())
  out %<>% seasonder_setSeaSondeRCS_MUSIC(seasonder_initSeaSondeRCS_MUSIC(out))
  out %<>% seasonder_MUSICComputePropDualSols()
  return(out)
}

#### Validation ####

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

#### Getters ####


seasonder_getSeaSondeRCS_MUSIC_parameters <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "MUSIC_data", exact = TRUE)$MUSIC_parameters %||% seasonder_defaultMUSIC_parameters()


  return(out)


}

seasonder_getSeaSondeRCS_MUSIC <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "MUSIC_data", exact = TRUE)$MUSIC %||% seasonder_initSeaSondeRCS_MUSIC(seasonder_cs_obj)


  return(out)


}


seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion <- function(seasonder_cs_obj) {


  out <- attr(seasonder_cs_obj, "MUSIC_data", exact = TRUE)$dual_solutions_proportion %||% NA_real_


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
      G_t <- Conj(t(eigVector)) %*% a

      G_inv <- solve(G)
      G_t_inv <- solve(G_t)
      P <- G_t_inv %*% diag(eigValues) %*% G_inv
    }else if(ncol(a) == 1) {

      eigVector <- eig$vectors[,c(1)]
      eigValues <- eig$values[c(1)]

      G <- a_star %*% eigVector
      G_t <- Conj(t(eigVector)) %*% a

      G_inv <- solve(G)
      G_t_inv <- solve(G_t)
      P <- G_t_inv %*% eigValues %*% G_inv
    }
  }





  return(P)

}

seasonder_MUSICComputeSignalPowerMatrix <- function(seasonder_cs_object){


  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC %<>% dplyr::mutate(DOA_solutions = purrr::pmap(list(cov, eigen, DOA_solutions), \(C,eig,DOA_sol){

    out <- DOA_sol

    if(ncol(DOA_sol$dual$a) == 2){

      P_dual <- seasonder_computePowerMatrix(C,eig,DOA_sol$dual$a)

      if(!is.null(P_dual) && all(dim(P_dual) == c(2,2))){

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

    P_diag <- pracma::Real(diag(DOA_sol$dual$P))
    out <- max(P_diag)/min(P_diag)

    return(out)
  }), .after = "P1_check")


  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(2)

  MUSIC %<>% dplyr::mutate(P2_check = !is.na(signal_power_ratio) & signal_power_ratio < MUSIC_parameter, .after = "signal_power_ratio")


  MUSIC$retained_solution[!MUSIC$P2_check] <- "single"

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)
}


seasonder_MUSICCheckSignalMatrix <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC %<>% dplyr::mutate(diag_off_diag_power_ratio = purrr::map_dbl(DOA_solutions,\(DOA_sol){

    P_diag <- pracma::Real(diag(DOA_sol$dual$P)) %>% prod()
    P_off_diag <- DOA_sol$dual$P
    diag(P_off_diag) <- 1

    P_off_diag <- pracma::Real(P_off_diag) %>% prod()

    out <- P_diag/P_off_diag

    return(out)
  }), .after = "P2_check")


  MUSIC_parameter <- seasonder_getSeaSondeRCS_MUSIC_parameters(seasonder_cs_object) %>% magrittr::extract(3)

  MUSIC %<>% dplyr::mutate(P3_check = !is.na(diag_off_diag_power_ratio) & diag_off_diag_power_ratio > MUSIC_parameter, .after = "diag_off_diag_power_ratio")


  MUSIC$retained_solution[!MUSIC$P3_check] <- "single"

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)



  return(seasonder_cs_object)
}


seasonder_MUSICTestDualSolutions <- function(seasonder_cs_object){

  # P1
  seasonder_cs_object %<>% seasonder_MUSICCheckEigenValueRatio()

  # P2
  seasonder_cs_object %<>% seasonder_MUSICCheckSignalPowers()

  # P3
  seasonder_cs_object %<>% seasonder_MUSICCheckSignalMatrix()


  return(seasonder_cs_object)

}

#### MUSIC algorithm ####

# TODO: update docs


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
seasonder_MUSICComputeCov <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(cov = purrr::map2(range_cell,doppler_bin,\(r,d) {

    out <- seasonder_MUSICInitCov()


    for(i in 1:3){

      for(j in 1:3){

        if(i==j){
          value <- seasonder_getSeaSondeRCS_dataMatrix(seasonder_cs_object,paste0("SSA",i))[r, d]
          if(i == 3){
            value <- abs(value)
          }
        }else{
          value <- seasonder_getSeaSondeRCS_dataMatrix(seasonder_cs_object,paste0("CS",paste0(as.character(sort(c(i,j))),collapse = "")))[r, d]
          if(j > i){
            value <- Conj(value)
          }
        }

        out[i,j] <- value

      }
    }

    return(out)

  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)
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

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(eigen = purrr::map(cov,\(C){
    out <- seasonder_MUSICInitEigenDecomp()

    # Get eigen-decomposition
    eigen_decomp <- eigen(C, symmetric = TRUE)

    out$values <- eigen_decomp$values
    out$vectors <- eigen_decomp$vectors

    return(out)
  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)

}

# TODO: update docs

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


  seasonder_apm_obj <- seasonder_getSeaSondeRCS_APM(seasonder_cs_object)

  # TODO: Validate APM object

  bearings <- seasonder_getSeaSondeRAPM_BEAR(seasonder_apm_obj)

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(distances = purrr::map(eigen,\(eigen_analysis){

    out <- seasonder_MUSICInitDistances(bearings = bearings)


    for(i in 1:2){ # Number of solutions
      En <- eigen_analysis$vectors[,(i+1):3, drop = F]

      for(j in 1:length(bearings)){
        a <- seasonder_apm_obj[,j, drop= F]
        names(a) <- NULL
        out[i,j] <- Conj(t(a)) %*% (En %*% Conj(t(En))) %*% a


      }


    }

    return(out)
  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)

}


seasonder_MUSICExtractPeaks <- function(seasonder_cs_object){


  seasonder_apm_obj <- seasonder_getSeaSondeRCS_APM(seasonder_cs_object)

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  MUSIC %<>% dplyr::mutate(DOA_solutions = purrr::map(distances,\(distances){

    out <- seasonder_MUSICInitDOASolutions()


    bearings <- attr(distances,"bearings",exact = TRUE)

    rev_single_solution_dist = pracma::Real(1/distances['single',,drop = TRUE])

    rev_dual_solution_dist = pracma::Real(1/distances['dual',,drop = TRUE])


    single_peak <- which.max(rev_single_solution_dist)

    dual_peaks_results <- pracma::findpeaks(rev_dual_solution_dist,npeaks = 2, sortstr = TRUE)



    out$single$bearing <-  bearings[single_peak]
    out$single$a <- seasonder_apm_obj[,single_peak, drop = FALSE]


    dual_peaks <- dual_peaks_results[,2,drop = T]

    out$dual$bearing <- bearings[dual_peaks]
    out$dual$a <- seasonder_apm_obj[,dual_peaks, drop = FALSE]



    return(out)
  }))


  MUSIC %<>% dplyr::mutate(retained_solution = purrr::map2_chr(retained_solution, DOA_solutions, \(ret_sol, DOA_sol){

    out <- ret_sol

    if(ret_sol == "dual"){

      if(length(DOA_sol$dual$bearing) != 2){
        ret_sol <- "single"
      }

    }

    return(out)

  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  return(seasonder_cs_object)

}






seasonder_MUSICSelectDOA <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC %<>% dplyr::mutate(DOA = purrr::map2(DOA_solutions, retained_solution, \(DOA_sol, retained_sol) {
    DOA_sol[[retained_sol]]
  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)



  return(seasonder_cs_object)
}


seasonder_runMUSIC <- function(seasonder_cs_object){

  seasonder_cs_object %<>% seasonder_MUSICComputeCov()

  seasonder_cs_object %<>% seasonder_MUSICCovDecomposition()

  seasonder_cs_object %<>% seasonder_MUSICEuclideanDistance()

  seasonder_cs_object %<>% seasonder_MUSICExtractPeaks()

  seasonder_cs_object %<>% seasonder_MUSICComputeSignalPowerMatrix()

  seasonder_cs_object %<>% seasonder_MUSICTestDualSolutions()

  seasonder_cs_object %<>% seasonder_MUSICComputePropDualSols()

  seasonder_cs_object %<>% seasonder_MUSICSelectDOA()

  return(seasonder_cs_object)

}

seasonder_runMUSIC_in_FOR <- function(seasonder_cs_object){

  FOR <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_object)

  FOR <-  1:length(FOR) %>% purrr::map(\(range_cell) {

    o <- NULL
    doppler_bins <- c(FOR[[range_cell]]$negative_FOR,FOR[[range_cell]]$positive_FOR)
    if(length(doppler_bins) > 0){
      o <- data.frame(range_cell = range_cell, doppler_bin = doppler_bins)
    }

    return(o)

  }) %>% purrr::compact() %>% dplyr::bind_rows()

  MUSIC <- seasonder_initSeaSondeRCS_MUSIC(seasonder_cs_object,range_cells = FOR$range_cell, doppler_bins = FOR$doppler_bin)

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)

  seasonder_cs_object %<>% seasonder_runMUSIC()

  return(seasonder_cs_object)


}
