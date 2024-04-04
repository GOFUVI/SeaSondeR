seasonder_defaultShortTimeRadials_parameters <- function() {

  out <- list(
    method = "median",
    AngularResolution = 1,
    SpatialResolution = 5
  )


  return(out)
}


seasonder_validateShortTimeRadials_parameters <- function(seasonder_cs_obj, short_time_radials_parameters) {





    # short_time_radials_parameters$x <- short_time_radials_parameters$x %||% seasonder_defaultShortTimeRadials_parameters()$x




  invisible(short_time_radials_parameters)




}

compute_radial_median <- function(v){

  v <- sort(v,decreasing = T)

  if(length(v) %% 2 == 0){
    out <- v[length(v)/2 +1]
  }else{
    out <- v[(length(v)-1)/2+1]
  }

  return(out)

}

seasonder_MUSIC2ShortTimeRadials <- function(seasonder_cs_obj, short_time_radials_parameters = seasonder_defaultShortTimeRadials_parameters()){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)



  antenna_bearing <- seasonder_getSeaSondeRAPM_AntennaBearing(seasonder_apm_obj)

  out <- MUSIC %>% dplyr::select(range, range_cell, doppler_bin,radial_v,DOA_solutions,retained_solution) %>%
    dplyr::mutate(bearing = purrr::map2(DOA_solutions, retained_solution, \(x,y) data.frame(bearing=x[[y]]$bearing))) %>% tidyr::unnest(bearing) %>% dplyr::select(bearing,range, range_cell, doppler_bin, radial_v) %>% dplyr::mutate(bearing = (((bearing * -1 + 360) %% 360) + antenna_bearing ) %% 360) %>% dplyr::arrange(range_cell,bearing)



  if(short_time_radials_parameters$method == "median"){



    SpatialResolution <- short_time_radials_parameters$SpatialResolution

    AngularResolution <- short_time_radials_parameters$AngularResolution

    half_angular_window <- AngularResolution/2

    bearings <-seq(0,359,AngularResolution)

    half_spatial_window <- SpatialResolution/2






out %<>% tidyr::nest(range_data = -c(range_cell)) %>%
      dplyr::mutate(range_data = purrr::map(range_data,\(range_df){

        radial_v_df <- bearings %>% purrr::map(\(comp_bearing){

  df <- range_df %>% dplyr::filter(bearing > comp_bearing - half_angular_window & bearing <= comp_bearing + half_angular_window) %>% dplyr::summarise(radial_v = compute_radial_median(radial_v), bearing = comp_bearing)

}) %>% dplyr::bind_rows() %>% dplyr::filter(complete.cases(.))

       quality_df <-  bearings %>% purrr::map(\(comp_bearing){

          df <- range_df %>% dplyr::filter(bearing > comp_bearing - half_spatial_window & bearing <= comp_bearing + half_spatial_window)
if(nrow(df) >0){
  df %<>% dplyr::summarise(ESPC = sd(radial_v), EDVC = length(unique(doppler_bin)), ERSC = length(radial_v),MAXV = max(radial_v), MINV = min(radial_v), bearing = comp_bearing )
}else{
  NULL
}


          }) %>% purrr::compact() %>% dplyr::bind_rows()

df <- dplyr::full_join(radial_v_df, quality_df, by = "bearing")

        return(df)

      })) %>% tidyr::unnest(range_data)
browser()
  }else{

    stop("only median method is implemented for short-time radials")
  }

  return(out)

}

new_SeaSondeRCSShortTimeRadials <- function(seasonder_cs_obj, short_time_radials_parameters = seasonder_defaultShortTimeRadials_parameters()){


radials <- seasonder_MUSIC2ShortTimeRadials(seasonder_cs_obj, short_time_radials_parameters)

attr(radials,"class") <- c("SeaSondeRShortTimeRadials", class(radials))

out <- radials

return(out)

}
