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
