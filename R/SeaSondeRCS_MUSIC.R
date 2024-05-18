

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

seasonder_initSeaSondeRCS_MUSIC <- function(seasonder_cs_object, range_cells = NULL, doppler_bins = NULL){


  if(is.null(range_cells) || is.null(doppler_bins)){

    if (is.null(range_cells)) {

      range_cells <- 1:seasonder_getnRangeCells(seasonder_obj = seasonder_cs_object)
    }


    if (is.null(doppler_bins)) {

      doppler_bins <- 1:seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_obj = seasonder_cs_object)
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
                         DOA = list(c(NA_real_, NA_real_))
  )



  return(out)
}

seasonder_initMUSICData <- function(seasonder_cs_object, range_cells = NULL, doppler_bins = NULL){

  out <- seasonder_cs_object






  out %<>% seasonder_setSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(out))
  out %<>% seasonder_setSeaSondeRCS_MUSIC_parameters(seasonder_getSeaSondeRCS_MUSIC_parameters(out))
  out %<>% seasonder_setSeaSondeRCS_MUSIC(seasonder_initSeaSondeRCS_MUSIC(out, range_cells = range_cells, doppler_bins = doppler_bins))
  out %<>% seasonder_MUSICComputePropDualSols()
  out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_data(seasonder_MUSICInitInterpolatedData(out))
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

seasonder_setSeaSondeRCS_MUSIC_doppler_interpolation <- function(seasonder_cs_object, doppler_interpolation){

  # TODO: Valiate doppler_interpolation (must be 1L, 2L, 3L or 4L, also check the number of final doppler bins SEAS-72). The default is 1 (no interpolation)


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

  matrix <- seasonder_getSeaSondeRCS_MUSIC_interpolated_data(seasonder_cs_obj = seasonder_cs_obj)[[matrix_name]]

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

  MUSIC %<>% dplyr::mutate(P2_check = purrr::map_lgl(DOA_solutions, \(DOA_sol) length(DOA_sol$dual$bearing) == 1 ) | !is.na(signal_power_ratio) & signal_power_ratio < MUSIC_parameter, .after = "signal_power_ratio")


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

  MUSIC %<>% dplyr::mutate(P3_check = purrr::map_lgl(DOA_solutions, \(DOA_sol) length(DOA_sol$dual$bearing) == 1 ) | !is.na(diag_off_diag_power_ratio) & diag_off_diag_power_ratio > MUSIC_parameter, .after = "diag_off_diag_power_ratio")


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

##### Doppler interpolation #####

seasonder_SeaSondeRCSMUSICInterpolateDoppler <- function(seasonder_cs_obj){


  out <- seasonder_cs_obj

  data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)

  out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_data(data)

  doppler_interpolation <- seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_cs_obj)

  if(doppler_interpolation > 1L){


    nDoppler <- seasonder_getSeaSondeRCS_MUSIC_nDopplerCells(seasonder_cs_obj)
    nRanges <- seasonder_getnRangeCells(seasonder_cs_obj)

    interpolated_data <- seasonder_initCSDataStructure(nRanges = nRanges, nDoppler = nDoppler)


    # 1, int,2,int,3 n*2-1
    # 1-> 1 (i-1)*2 +1 = i*2 -2 +1 = i * 2 -1
    # 2 -> 3 i * 2 -1
    # 3 -> 5 i * 2 -1
    # 1, int, int ,2,int, int ,3 n*3-2
    # 1 -> 1 (i-1)*3 +1 = i *3 -3 +1 = i*3 -2
    # 2 -> 4 i*3 -2
    # 3 -> 7 i* 3-2
    # 1, int, int, int ,2,int, int, int ,3 n*4-3

    index_mapping <- data.frame(original=1:ncol(data[[1]]), mapped=(0:(ncol(data[[1]])-1))*doppler_interpolation  +1)

    interpolated_cells <- dplyr::setdiff(1:nDoppler, index_mapping$mapped
    )

    interpolated_data %<>% purrr::map2(names(.), \(matrix, name){

      if(!name %in% names(data)){
        seasonder_logAndAbort(glue::glue("{name} is not a data matrix name."), calling_function = "seasonder_SeaSondeRCSInterpolateDoppler")
      }



      original_matrix <- data[[name]]

      matrix[,index_mapping$mapped] <- original_matrix[,index_mapping$original]

      if(name == "QC"){
        matrix[,interpolated_cells] <- -1L
      }else{
        matrix <-  1:nrow(matrix) %>% purrr::reduce(\(matrix_so_far,i){

          data <- c(matrix_so_far[i,,drop = TRUE],matrix_so_far[i,1,drop = TRUE])

          if(!rlang::is_complex(data)){

            data[interpolated_cells] <- zoo::na.approx(abs(data))[interpolated_cells]
            data <- data[-length(data)]
          }else{


            data <- complex(real= zoo::na.approx(pracma::Real(data))[-length(data)],
                            imaginary= zoo::na.approx(pracma::Imag(data))[-length(data)])

          }

          matrix_so_far[i,] <- data

          matrix_so_far

        }, .init=matrix)

      }


      matrix

    })


    out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_doppler_cells_index(interpolated_cells)


    out %<>% seasonder_setSeaSondeRCS_MUSIC_interpolated_data(interpolated_data)

  }

  return(out)
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
          value <- seasonder_getSeaSondeRCS_MUSIC_interpolated_dataMatrix(seasonder_cs_object,paste0("SSA",i))[r, d]
          if(i == 3){
            value <- abs(value)
          }
        }else{
          value <- seasonder_getSeaSondeRCS_MUSIC_interpolated_dataMatrix(seasonder_cs_object,paste0("CS",paste0(as.character(sort(c(i,j))),collapse = "")))[r, d]
          if(i > j){
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



    single_peaks_results <- pracma::findpeaks(rev_single_solution_dist,npeaks = 1, sortstr = TRUE)

    single_peak <-  single_peaks_results[,2,drop = T]# which.max(rev_single_solution_dist)

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

  return(seasonder_cs_object)

}






seasonder_MUSICSelectDOA <- function(seasonder_cs_object){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)


  MUSIC %<>% dplyr::mutate(DOA = purrr::map2(DOA_solutions, retained_solution, \(DOA_sol, retained_sol) {
    if(retained_sol != "none"){
      DOA_sol[[retained_sol]]
    }

  }))

  seasonder_cs_object %<>% seasonder_setSeaSondeRCS_MUSIC(MUSIC)



  return(seasonder_cs_object)
}


seasonder_runMUSIC <- function(seasonder_cs_object){

  out <- seasonder_cs_object

  out %<>% seasonder_MUSICComputeCov()

  out %<>% seasonder_MUSICCovDecomposition()

  out %<>% seasonder_MUSICEuclideanDistance()

  out %<>% seasonder_MUSICExtractPeaks()

  out %<>% seasonder_MUSICComputeSignalPowerMatrix()

  out %<>% seasonder_MUSICTestDualSolutions()

  out %<>% seasonder_MUSICComputePropDualSols()

  out %<>% seasonder_MUSICSelectDOA()

  out %<>% seasonder_MUSIC_LonLat()

  return(out)

}

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



  out %<>% seasonder_initMUSICData(range_cells = FOR$range_cell, doppler_bins = FOR$doppler_bin)


  out %<>% seasonder_SeaSondeRCSMUSICInterpolateDoppler()


  out %<>% seasonder_runMUSIC()

  return(out)


}


#### Utils ####

seasonder_MUSICBearing2GeographicalBearing <- function(bearings, seasonder_apm_object){

  antennaBearing <-
    seasonder_apm_object %>%
    seasonder_getSeaSondeRAPM_AntennaBearing()
  bearings %<>% purrr::map_dbl(\(angles) ((-1 * angles %% 360) + antennaBearing) %% 360)

  return(bearings)

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
    geosphere::destPoint(c(longitude, latitude), bear, dist * 1000) %>%
      as.data.frame()
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

  out <- data.frame(longitude = longitude,
                    latitude = latitude,
                    range_cell = cell_number,
                    range = range,
                    doppler_bin = doppler_cell,
                    doppler_freq = freq,
                    radial_velocity = velocity,
                    signal_power = power,
                    bearing = bearing)

  # Retrieve MUSIC data from the SeaSondeRCS object
  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_object)

  # Select relevant columns from MUSIC data
  out <- MUSIC %>% dplyr::select(range_cell, doppler_bin, range, doppler_freq = freq, radial_velocity = radial_v, DOA, lonlat)

  # Process the DOA and lonlat columns and unnest them
  out %<>% dplyr::mutate(DOA = purrr::map(DOA, \(DOA_sol) data.frame(bearing = DOA_sol$bearing, signal_power = pracma::Real(diag(DOA_sol$P))))) %>%
    tidyr::unnest(c(DOA, lonlat))

  # Get APM object from the SeaSondeRCS object
  seasonder_apm_object <- seasonder_cs_object %>% seasonder_getSeaSondeRCS_APM()

  # Convert MUSIC bearing to geographical bearing
  out$bearing %<>% seasonder_MUSICBearing2GeographicalBearing(seasonder_apm_object)

  # Reorder columns to match the final output structure
  out %<>% dplyr::select(longitude = lon,
                         latitude = lat,
                         range_cell,
                         range,
                         doppler_bin,
                         doppler_freq,
                         radial_velocity,
                         signal_power,
                         bearing)

  return(out)
}

#' @export
seasonder_exportCSVMUSICTable <- function(seasonder_cs_object, filepath){

  table <- seasonder_exportMUSICTable(seasonder_cs_object) %>% as.data.frame()



data.table::fwrite(table,file = filepath)


  invisible(NULL)


}
