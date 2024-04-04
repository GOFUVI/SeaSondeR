seasonder_defaultShortTimeRadials_parameters <- function() {

  out <- list(
  )


  return(out)
}


seasonder_validateShortTimeRadials_parameters <- function(seasonder_cs_obj, short_time_radials_parameters) {





    # short_time_radials_parameters$x <- short_time_radials_parameters$x %||% seasonder_defaultShortTimeRadials_parameters()$x




  invisible(short_time_radials_parameters)




}

seasonder_MUSIC2ShortTimeRadials <- function(seasonder_cs_obj, short_time_radials_parameters = seasonder_defaultShortTimeRadials_parameters()){

  MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)



  antenna_bearing <- seasonder_getSeaSondeRAPM_AntennaBearing(seasonder_apm_obj)

  out <- MUSIC %>% dplyr::select(range, range_cell, doppler_bin,radial_v,DOA_solutions,retained_solution) %>%
    dplyr::mutate(bearing = purrr::map2(DOA_solutions, retained_solution, \(x,y) data.frame(bearing=x[[y]]$bearing))) %>% tidyr::unnest(bearing) %>% dplyr::select(bearing,range, range_cell, doppler_bin, radial_v) %>% dplyr::mutate(bearing = (((bearing * -1 + 360) %% 360) + antenna_bearing ) %% 360)

  browser()

  return(out)

}

new_SeaSondeRCSShortTimeRadials <- function(seasonder_cs_obj, short_time_radials_parameters = seasonder_defaultShortTimeRadials_parameters()){


radials <- seasonder_MUSIC2ShortTimeRadials(seasonder_cs_obj, short_time_radials_parameters)

out <- structure(radials,

                        class = "SeaSondeRShortTimeRadials")


return(out)

}
