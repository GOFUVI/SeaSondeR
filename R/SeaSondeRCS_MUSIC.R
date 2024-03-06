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
