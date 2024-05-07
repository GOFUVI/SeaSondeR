test_that("Related functions are defined",{

  expect_true(is.function(new_SeaSondeRCSShortTimeRadials))

})

describe("short-time radials",{

  describe("compute and return a SeaSondeRShortTimeRadials object",{

test_that("test 1 works with ideals",{
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"), override_antenna_bearing = 13.0)





phasec1 <-    readLines("tests/testthat/data/TORA/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 1) %>% as.numeric()
phasec2 <-    readLines("tests/testthat/data/TORA/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 2) %>% as.numeric()

amp1 <-  1.22398329

amp2 <-  1.32768297

# amp1 <-    seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)
#
# amp2 <-   seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)


# amp1 <- seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)
#
# amp2 <- seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)

   seasonder_apm_obj[1,] <- seasonder_apm_obj[1,]*amp1*exp(1i*phasec1*pi/180)

   seasonder_apm_obj[2,] <- seasonder_apm_obj[2,]*amp2*exp(1i*phasec2*pi/180)

   #     smoothing <- 20
   #
   #     seasonder_apm_obj[1,] <- complex(real = slider::slide_mean(pracma::Real(seasonder_apm_obj[1,]),before = smoothing, na_rm = T), imaginary =
   # slider::slide_mean(pracma::Imag(seasonder_apm_obj[1,]),before = smoothing, na_rm = T))
   #
   #     seasonder_apm_obj[2,] <- complex(real = slider::slide_mean(pracma::Real(seasonder_apm_obj[2,]),before = smoothing, na_rm = T), imaginary =
   #                                        slider::slide_mean(pracma::Imag(seasonder_apm_obj[2,]),before = smoothing, na_rm = T))


   plot(attr(seasonder_apm_obj, "BEAR",exact = T),Arg(seasonder_apm_obj[1,])*180/pi,xlim = c(-180,180),ylim = c(-180, 180))

    plot(attr(seasonder_apm_obj, "BEAR",exact = T),Mod(seasonder_apm_obj[1,]),xlim = c(-180,180))


  FOS <-   list(nsm = 2,
                fdown = 10^(10/10),
                flim = 10^(20/10),
                noisefact = 10^(6/10),
                currmax = 1,
                reject_distant_bragg = TRUE, #  Default is to apply this test
                reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
  )


  seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)



  # seasonder_csd_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSD_XXXX_0000_00_00_0000.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)


  # seasonder_cs_obj %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

  # seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC_parameters(c(40, 20, 4))



  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

#   orig_data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)
#   interp_data <- seasonder_getSeaSondeRCS_MUSIC_interpolated_data(seasonder_cs_obj)
#   SSA_orig <- orig_data$SSA3
#   SSA_interp <- interp_data$SSA3
# csd_data <- seasonder_getSeaSondeRCS_data(seasonder_csd_obj)
# SSA_csd <- csd_data$SSA3
#
# x <- (SSA_csd-SSA_interp)
# which(abs(x) > 1e-12,arr.ind = T)

  MUSIC <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()
  #
  # test1txt <- readLines(here::here("tests/testthat/data/TORA/test1/test1.txt"))

  # test1txt[grepl("Reading CrossSpectra Range|Vectors|dual/sing",test1txt)]

  # MUSIC %>% dplyr::filter(range_cell == 20) %$% table(retained_solution)






  # ruv <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 52, header = F)
  #
  # header <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 50, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist
  #
  # header[14] <- paste(header[14],header[15])
  #
  # header <- header[-15]
  #
  # header[15] <- paste(header[15],header[16])
  #
  # header <- header[-16]
  #
  # header[3] <- paste(header[3],header[4])
  #
  # header <- header[-4]
  #
  # header[4] <- paste(header[4],header[5])
  #
  # header <- header[-5]
  #
  #
  # header2 <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 51, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist
  #
  #
  # names(ruv) <- paste(header,header2)
  #
  # test_vectors <- ruv %>% dplyr::rename(range_cell = `Spectra RngCell`, bearing = `Bearing (True)`,maxv = `Velocity Maximum`, minv = `Velocity Minimum`)


  ruv <- data.table::fread("tests/testthat/data/TORA/test1/RdliXXXX_00_00_00_0000sr.rv",skip = 3, header = F)




  names(ruv) <- c("dx", "dy", "u", "v", "bearing", "radial_v", "espc", "maxv", "minv", "range_cell", "etmp", "nvel", "ntmp", "nspc")

  test_vectors <- ruv

  test_vectors %<>% dplyr::select(range_cell,bearing, maxv, minv) %>% tidyr::pivot_longer(cols = c(maxv, minv), names_to = "drop", values_to = "radial_v") %>% dplyr::select(-drop) %>% dplyr::distinct()




  test_vectors %<>% dplyr::arrange(range_cell, radial_v)

  # test_vectors$radial_v[order(abs(test_vectors$radial_v))]
  #
  # MUSIC$radial_v[order(abs(MUSIC$radial_v))]
  #
  # bragg_freq <- seasonder_getBraggDopplerAngularFrequency(seasonder_cs_obj)
  #
  # k0 <- seasonder_getRadarWaveNumber(seasonder_cs_obj)/(2*pi)
  #
  # freq <- seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency(seasonder_cs_obj)
  #
  # v <- c((freq[freq < 0]  - bragg_freq[1])/(2*k0),(freq[freq >= 0]  - bragg_freq[2])/(2*k0))
  #
  # MUSIC$radial_v %>% abs() %>% sort() %>% unique() %>%  magrittr::extract(1:2) %>% diff()
  #
  music_v <- MUSIC %>% dplyr::pull("radial_v") %>% unique()
#
#   test_vectors$radial_v %>% abs() %>% sort() %>% unique() %>%  magrittr::extract(1:2) %>% diff()

  closest_vel <- test_vectors %>% dplyr::select(range_cell, radial_v) %>% dplyr::distinct() %>% dplyr::group_by(range_cell) %>% dplyr::mutate(music_v = purrr::map_dbl(radial_v,\(x) music_v[which.min(abs((x-music_v*100)))])) %>% dplyr::mutate(music_v = music_v, radial_v = radial_v)%>% dplyr::ungroup() %>% dplyr::arrange(range_cell, radial_v) %>% dplyr::mutate(d = abs(music_v*100 - radial_v)) %>% dplyr::arrange(range_cell, radial_v)


  test_vectors %<>% dplyr::left_join(closest_vel, by = c("range_cell","radial_v"))

  test_vectors %<>% dplyr::left_join(MUSIC, by = c("music_v" = "radial_v", "range_cell" = "range_cell"))

  test_vectors %<>% dplyr::mutate(music_bearing = purrr::map(DOA, "bearing" )) %>% tidyr::unnest(music_bearing) %>% dplyr::arrange(range_cell,doppler_bin) %>% dplyr::mutate(music_bearing = music_bearing %% 360)


  inspect <- test_vectors %>% dplyr::select(range_cell, doppler_bin, radial_v,music_v, bearing, music_bearing) %>% dplyr::group_by(range_cell, doppler_bin, bearing) %>% dplyr::filter(music_bearing == music_bearing[which.min(abs(bearing-music_bearing))]) %>% dplyr::ungroup() %>% dplyr::arrange(range_cell, doppler_bin) %>% dplyr::mutate(music_bearing = (music_bearing ) %% 360) %>% dplyr::group_by(range_cell, doppler_bin, music_bearing)  %>% dplyr::mutate( bearing  = dplyr::case_when(bearing >200 ~ bearing - 360, TRUE ~ bearing), music_bearing  = dplyr::case_when(music_bearing >200 ~ music_bearing - 360, TRUE ~ music_bearing)) %>% dplyr::filter(bearing == bearing[which.min(abs(bearing - music_bearing))])


  ggplot2::ggplot(inspect, ggplot2::aes(x= music_bearing , y = bearing )) + ggplot2::geom_point(alpha = 0.2) + ggplot2::geom_smooth(method = "lm") + ggplot2::geom_abline(slope = 1, intercept = 0, color = "red")  + ggplot2::scale_x_continuous(limits = c(-180, 180),n.breaks = 12,expand = c(0,0),minor_breaks = NULL)  + ggplot2::scale_y_continuous(limits = c(-180, 180),n.breaks = 12, expand = c(0,0),minor_breaks = NULL)



  summary(lm(bearing ~ music_bearing, inspect))


  rc <- 3
  db <- 322

  MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% View()

  MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("cov") %>% magrittr::extract2(1)

  MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("eigen") %>% magrittr::extract2(1)

  MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("DOA_solutions") %>% magrittr::extract2(1)


  distances <- MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("distances") %>% magrittr::extract2(1)
  rev_dual_solution_dist = pracma::Real(1/distances['single',,drop = TRUE])

  pracma::findpeaks(rev_dual_solution_dist,npeaks = 2, sortstr = TRUE)

  plot((attr(distances,"bearing")) %% 360,rev_dual_solution_dist)

  ruv %<>% dplyr::filter(range_cell < 20)

  test <- seasonder_getSeaSondeRCSShortTimeRadials(seasonder_cs_obj)

  test %<>% dplyr::filter((range_cell >= 3 & range_cell <= 20))


plot_radials(ruv$range_cell, ruv$bearing,ruv$radial_v)

plot_radials(test$range_cell,test$bearing,test$radial_v*100)






  radial_comparison <-  ruv %>% dplyr::rename(radial_v_ruv = radial_v) %>% dplyr::filter(range_cell < 20)





  radial_comparison %<>% dplyr::left_join(test, by = c("bearing","range_cell")) %>% dplyr::mutate(radial_v = radial_v * 100, MINV = MINV * 100, MAXV = MAXV* 100, ESPC = ESPC * 100)


  radial_comparison %>% dplyr::select(range_cell, bearing, MINV, `Velocity Minimum`, MAXV, `Velocity Maximum`, radial_v, `Velocity (cm/s)`, `Velocity Count`,EDVC, `Spatial Count`,  ERSC) %>% dplyr::arrange(range_cell, bearing) %>% View()




  lm(radial_v ~ radial_v_ruv, radial_comparison)




  ggplot2::ggplot(radial_comparison, ggplot2::aes(x= radial_v_ruv, y = radial_v)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm")
})

    test_that("test 1 works with measured",{


      ideal_seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"), override_antenna_bearing = 13.0)





      phasec1 <-    readLines("tests/testthat/data/TORA/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 1) %>% as.numeric()
      phasec2 <-     readLines("tests/testthat/data/TORA/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 2) %>% as.numeric()


      amp1 <-  1 #1.4163 #  seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)

      amp2 <-  1 # 1.1232 # seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)

      # amp1 <- seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)
      #
      # amp2 <- seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)

      ideal_seasonder_apm_obj[1,] <- ideal_seasonder_apm_obj[1,]*amp1*exp(1i*phasec1)

      ideal_seasonder_apm_obj[2,] <- ideal_seasonder_apm_obj[2,]*amp2*exp(1i*phasec2)


      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/MeasPattern.txt"))


      phasec1 <-   0# -13.50 #readLines("tests/testthat/data/TORA/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 1) %>% as.numeric()
      phasec2 <-  0#  -55.80 # readLines("tests/testthat/data/TORA/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 2) %>% as.numeric()

      amp1 <-  1.4163 #  seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)

      amp2 <-  1.1232 # seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)


      seasonder_apm_obj[1,] <- seasonder_apm_obj[1,]*amp1*exp(1i*phasec1)

      seasonder_apm_obj[2,] <- seasonder_apm_obj[2,]*amp2*exp(1i*phasec2)


      trimming <- 1

      attrib <- attributes(seasonder_apm_obj)

      trimmed_seasonder_apm_obj <-   seasonder_apm_obj[,-c(1:(trimming),(dim(seasonder_apm_obj)[2]-trimming)+1:dim(seasonder_apm_obj)[2])]

      attrib[["dim"]] <- dim(trimmed_seasonder_apm_obj)

      attrib[["dimnames"]][[2]] <- attrib[["dimnames"]][[2]][-c(1:(trimming),(length(attrib[["dimnames"]][[2]])-trimming+1):length(attrib[["dimnames"]][[2]]))]

      new_bear <-  attrib[["BEAR"]]

      new_bear <- new_bear[-c(1:(trimming),(length(new_bear)-trimming+1):length(new_bear))]

      attrib[["BEAR"]] <- new_bear

      attributes(trimmed_seasonder_apm_obj) <- attrib

      smoothed_seasonder_apm_obj <- trimmed_seasonder_apm_obj

      smoothing <- 20

      smoothed_seasonder_apm_obj %<>% seasonder_smoothAPM(smoothing)


      # combined_apm_obj <- seasonder_apm_obj
      # combined_apm_obj <- trimmed_seasonder_apm_obj
combined_apm_obj <- smoothed_seasonder_apm_obj
      # combined_apm_obj <- ideal_seasonder_apm_obj



# combined_apm_obj[,as.character(as.integer(dimnames(seasonder_apm_obj)[[2]]))] <- seasonder_apm_obj

plot(attr(combined_apm_obj, "BEAR",exact = T),Arg(combined_apm_obj[1,])*180/pi,xlim = c(-180,180),ylim = c(-180, 180))

plot(attr(combined_apm_obj, "BEAR",exact = T),Mod(combined_apm_obj[1,]),xlim = c(-180,180))


plot(attr(combined_apm_obj, "BEAR",exact = T),Arg(combined_apm_obj[2,])*180/pi,xlim = c(-180,180),ylim = c(-180, 180))

plot(attr(combined_apm_obj, "BEAR",exact = T),Mod(combined_apm_obj[2,]),xlim = c(-180,180))

ggplot2::ggplot() + ggplot2::coord_polar(theta = "x", start = 0) + ggplot2::xlim(0,360) +
  ggplot2::geom_point(data=data.frame(theta = attr(ideal_seasonder_apm_obj, "BEAR",exact = T) %% 360, mag = Mod(ideal_seasonder_apm_obj[1,])),  ggplot2::aes(x = theta, y = mag), color = "black") +
  ggplot2::geom_point(data=data.frame(theta = attr(ideal_seasonder_apm_obj, "BEAR",exact = T) %% 360, mag = Mod(ideal_seasonder_apm_obj[2,])),  ggplot2::aes(x = theta, y = mag), color = "black") +

  ggplot2::geom_point(data=data.frame(theta = attr(combined_apm_obj, "BEAR",exact = T) %% 360, mag = Mod(combined_apm_obj[1,])),  ggplot2::aes(x = theta, y = mag), color = "red") +
  ggplot2::geom_point(data=data.frame(theta = attr(combined_apm_obj, "BEAR",exact = T) %% 360, mag = Mod(combined_apm_obj[2,])),  ggplot2::aes(x = theta, y = mag), color = "blue") +
  ggplot2::scale_x_continuous(n.breaks = 18)









      FOS <-   list(nsm = 2,
                    fdown = 10^(10/10),
                    flim = 10^(20/10),
                    noisefact = 10^(6/10),
                    currmax = 1,
                    reject_distant_bragg = TRUE, #  Default is to apply this test
                    reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                    reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
      )


      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = combined_apm_obj)



      # seasonder_csd_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSD_XXXX_0000_00_00_0000.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)


      # seasonder_cs_obj %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)




      seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)


      #   orig_data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)
      #   interp_data <- seasonder_getSeaSondeRCS_MUSIC_interpolated_data(seasonder_cs_obj)
      #   SSA_orig <- orig_data$SSA3
      #   SSA_interp <- interp_data$SSA3
      # csd_data <- seasonder_getSeaSondeRCS_data(seasonder_csd_obj)
      # SSA_csd <- csd_data$SSA3
      #
      # x <- (SSA_csd-SSA_interp)
      # which(abs(x) > 1e-12,arr.ind = T)

      MUSIC <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()
      #
      # test1txt <- readLines(here::here("tests/testthat/data/TORA/test1/test1.txt"))

      # test1txt[grepl("Reading CrossSpectra Range|Vectors|dual/sing",test1txt)]

      # MUSIC %>% dplyr::filter(range_cell == 20) %$% table(retained_solution)






      # ruv <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 52, header = F)
      #
      # header <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 50, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist
      #
      # header[14] <- paste(header[14],header[15])
      #
      # header <- header[-15]
      #
      # header[15] <- paste(header[15],header[16])
      #
      # header <- header[-16]
      #
      # header[3] <- paste(header[3],header[4])
      #
      # header <- header[-4]
      #
      # header[4] <- paste(header[4],header[5])
      #
      # header <- header[-5]
      #
      #
      # header2 <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 51, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist
      #
      #
      # names(ruv) <- paste(header,header2)
      #
      # test_vectors <- ruv %>% dplyr::rename(range_cell = `Spectra RngCell`, bearing = `Bearing (True)`,maxv = `Velocity Maximum`, minv = `Velocity Minimum`)


      ruv <- data.table::fread("tests/testthat/data/TORA/test1/RdlmXXXX_00_00_00_0000sr.rv",skip = 3, header = F)

      iruv <- data.table::fread("tests/testthat/data/TORA/test1/RdliXXXX_00_00_00_0000sr.rv",skip = 3, header = F)





      names(ruv) <- c("dx", "dy", "u", "v", "bearing", "radial_v", "espc", "maxv", "minv", "range_cell", "etmp", "nvel", "ntmp", "nspc")

      names(iruv) <- c("dx", "dy", "u", "v", "ibearing", "radial_v", "espc", "maxv", "minv", "range_cell", "etmp", "nvel", "ntmp", "nspc")

      x <- dplyr::full_join(ruv,iruv, by = c("radial_v","range_cell")) %>% dplyr::select(range_cell, radial_v, bearing, ibearing)

      test_vectors <- ruv

      test_vectors %<>% dplyr::select(range_cell,bearing, maxv, minv) %>% tidyr::pivot_longer(cols = c(maxv, minv), names_to = "drop", values_to = "radial_v") %>% dplyr::select(-drop) %>% dplyr::distinct()


      # itest_vectors <- iruv
      #
      # itest_vectors %<>% dplyr::select(range_cell,bearing, maxv, minv) %>% tidyr::pivot_longer(cols = c(maxv, minv), names_to = "drop", values_to = "radial_v") %>% dplyr::select(-drop) %>% dplyr::distinct()
      #
      # x <- dplyr::full_join(test_vectors,itest_vectors, by = c("radial_v","range_cell")) %>% dplyr::select(range_cell, radial_v, bearing, ibearing)

      test_vectors %<>% dplyr::arrange(range_cell, radial_v)

      # test_vectors$radial_v[order(abs(test_vectors$radial_v))]
      #
      # MUSIC$radial_v[order(abs(MUSIC$radial_v))]
      #
      # bragg_freq <- seasonder_getBraggDopplerAngularFrequency(seasonder_cs_obj)
      #
      # k0 <- seasonder_getRadarWaveNumber(seasonder_cs_obj)/(2*pi)
      #
      # freq <- seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency(seasonder_cs_obj)
      #
      # v <- c((freq[freq < 0]  - bragg_freq[1])/(2*k0),(freq[freq >= 0]  - bragg_freq[2])/(2*k0))
      #
      # MUSIC$radial_v %>% abs() %>% sort() %>% unique() %>%  magrittr::extract(1:2) %>% diff()
      #
      music_v <- MUSIC %>% dplyr::pull("radial_v") %>% unique()
      #
      #   test_vectors$radial_v %>% abs() %>% sort() %>% unique() %>%  magrittr::extract(1:2) %>% diff()

      closest_vel <- test_vectors %>% dplyr::select(range_cell, radial_v) %>% dplyr::distinct() %>% dplyr::group_by(range_cell) %>% dplyr::mutate(music_v = purrr::map_dbl(radial_v,\(x) music_v[which.min(abs((x-music_v*100)))])) %>% dplyr::mutate(music_v = music_v, radial_v = radial_v)%>% dplyr::ungroup() %>% dplyr::arrange(range_cell, radial_v) %>% dplyr::mutate(d = abs(music_v*100 - radial_v)) %>% dplyr::arrange(range_cell, radial_v)


      test_vectors %<>% dplyr::left_join(closest_vel, by = c("range_cell","radial_v"))

      test_vectors %<>% dplyr::left_join(MUSIC, by = c("music_v" = "radial_v", "range_cell" = "range_cell"))

      test_vectors %<>% dplyr::mutate(music_bearing = purrr::map(DOA, "bearing" )) %>% tidyr::unnest(music_bearing) %>% dplyr::arrange(range_cell,doppler_bin) %>% dplyr::mutate(music_bearing = music_bearing %% 360)


      inspect <- test_vectors %>% dplyr::select(range_cell, doppler_bin, radial_v,music_v, bearing, music_bearing) %>% dplyr::group_by(range_cell, doppler_bin, bearing) %>% dplyr::filter(music_bearing == music_bearing[which.min(abs(bearing-music_bearing))]) %>% dplyr::ungroup() %>% dplyr::arrange(range_cell, doppler_bin) %>% dplyr::mutate(music_bearing = (music_bearing ) %% 360) %>% dplyr::group_by(range_cell, doppler_bin, music_bearing)  %>% dplyr::mutate( bearing  = dplyr::case_when(bearing >200 ~ bearing - 360, TRUE ~ bearing), music_bearing  = dplyr::case_when(music_bearing >200 ~ music_bearing - 360, TRUE ~ music_bearing)) %>% dplyr::filter(bearing == bearing[which.min(abs(bearing - music_bearing))])


      ggplot2::ggplot(inspect, ggplot2::aes(x= music_bearing , y = bearing )) + ggplot2::geom_point(alpha = 0.2) + ggplot2::geom_smooth(method = "lm") + ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") + ggplot2::scale_x_continuous(limits = c(-30, 120),n.breaks = 15,expand = c(0,0),minor_breaks = NULL)  + ggplot2::scale_y_continuous(limits = c(-60, 120),n.breaks = 18, expand = c(0,0),minor_breaks = NULL)



      summary(lm(bearing ~ music_bearing, inspect))


      rc <- 3
      db <- 322

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% View()

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("cov") %>% magrittr::extract2(1)

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("eigen") %>% magrittr::extract2(1)

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("distances") %>% magrittr::extract2(1)

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("DOA_solutions") %>% magrittr::extract2(1)


      distances <- MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("distances") %>% magrittr::extract2(1)
      rev_dual_solution_dist = pracma::Real(1/distances['dual',,drop = TRUE])

      pracma::findpeaks(rev_dual_solution_dist,npeaks = 2, sortstr = TRUE)

      plot((attr(distances,"bearing")) %% 360,rev_dual_solution_dist)

      plot(rev_dual_solution_dist)

      ruv %<>% dplyr::filter(range_cell < 20)

      test <- seasonder_getSeaSondeRCSShortTimeRadials(seasonder_cs_obj)

      test %<>% dplyr::filter((range_cell >= 3 & range_cell <= 20))


      plot_radials(ruv$range_cell, ruv$bearing,ruv$radial_v)

      plot_radials(test$range_cell,test$bearing,test$radial_v*100)


### Amplitudes ####


      to.plot <- MUSIC %>% dplyr::filter(purrr::map_lgl(DOA, \(x) all(!is.na(pracma::Real(x$P))))) %>% dplyr::mutate(bearing = purrr::map(DOA, \(x) x$bearing), P = purrr::map(DOA, \(x) diag(x$P) )) %>% dplyr::select(range, bearing,P) %>% tidyr::unnest(c(bearing, P)) %>% dplyr::mutate(bearing = (bearing * -1) %% 360, P = 10*log10(pracma::Real(P)))

to.plot %>% ggplot2::ggplot(ggplot2::aes(x=range, y = bearing ,color = P))+ ggplot2::geom_point() + ggplot2::coord_polar(theta = "y") + ggplot2::ylim(c(0,360))

to.plot %>% dplyr::group_by(range) %>% dplyr::summarise(P = mean(P)) %>% ggplot2::ggplot(ggplot2::aes(x=range, y= P)) + ggplot2::geom_point()


    })

    test_that("CIES test 1 works with ideals",{
      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/CIES/IdealPattern.txt"), override_antenna_bearing = 128)





      phasec1 <-    readLines("tests/testthat/data/CIES/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 1) %>% as.numeric()
      phasec2 <-    readLines("tests/testthat/data/CIES/Phases.txt") %>% stringr::str_extract("([-\\d\\.]+)\\s*([-\\d\\.]+)",group = 2) %>% as.numeric()

      # amp1 <-  0.358626842 #  seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)
      #
      # amp2 <-  0.369909227 # seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)


      amp1 <-    seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)

      amp2 <-   seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)

      # amp1 <- seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(1)
      #
      # amp2 <- seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AmplitudeFactors() %>% magrittr::extract(2)

      seasonder_apm_obj[1,] <- seasonder_apm_obj[1,]*amp1*exp(1i*phasec1*pi/180)

      seasonder_apm_obj[2,] <- seasonder_apm_obj[2,]*amp2*exp(1i*phasec2*pi/180)




      plot(attr(seasonder_apm_obj, "BEAR",exact = T),Arg(seasonder_apm_obj[1,])*180/pi,xlim = c(-180,180),ylim = c(-180, 180))

      plot(attr(seasonder_apm_obj, "BEAR",exact = T),Mod(seasonder_apm_obj[1,]),xlim = c(-180,180))


      FOS <-   list(nsm = 2,
                    fdown = 10^(10/10),
                    flim = 10^(20/10),
                    noisefact = 10^(6/10),
                    currmax = 1,
                    reject_distant_bragg = TRUE, #  Default is to apply this test
                    reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                    reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
      )


      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CIES/test1/CSS_CIES_24_04_18_0530.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)



      # seasonder_csd_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSD_XXXX_0000_00_00_0000.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)


      # seasonder_cs_obj %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

      # seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC_parameters(c(40, 20, 4))



      seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

      #   orig_data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)
      #   interp_data <- seasonder_getSeaSondeRCS_MUSIC_interpolated_data(seasonder_cs_obj)
      #   SSA_orig <- orig_data$SSA3
      #   SSA_interp <- interp_data$SSA3
      # csd_data <- seasonder_getSeaSondeRCS_data(seasonder_csd_obj)
      # SSA_csd <- csd_data$SSA3
      #
      # x <- (SSA_csd-SSA_interp)
      # which(abs(x) > 1e-12,arr.ind = T)

      MUSIC <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()
      #
      # test1txt <- readLines(here::here("tests/testthat/data/TORA/test1/test1.txt"))

      # test1txt[grepl("Reading CrossSpectra Range|Vectors|dual/sing",test1txt)]

      # MUSIC %>% dplyr::filter(range_cell == 20) %$% table(retained_solution)






      # ruv <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 52, header = F)
      #
      # header <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 50, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist
      #
      # header[14] <- paste(header[14],header[15])
      #
      # header <- header[-15]
      #
      # header[15] <- paste(header[15],header[16])
      #
      # header <- header[-16]
      #
      # header[3] <- paste(header[3],header[4])
      #
      # header <- header[-4]
      #
      # header[4] <- paste(header[4],header[5])
      #
      # header <- header[-5]
      #
      #
      # header2 <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 51, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist
      #
      #
      # names(ruv) <- paste(header,header2)
      #
      # test_vectors <- ruv %>% dplyr::rename(range_cell = `Spectra RngCell`, bearing = `Bearing (True)`,maxv = `Velocity Maximum`, minv = `Velocity Minimum`)


      ruv <- data.table::fread("tests/testthat/data/CIES/test1/RdliXXXX_00_00_00_0000sr.rv",skip = 3, header = F)




      names(ruv) <- c("dx", "dy", "u", "v", "bearing", "radial_v", "espc", "maxv", "minv", "range_cell", "etmp", "nvel", "ntmp", "nspc")

      test_vectors <- ruv

      test_vectors %<>% dplyr::select(range_cell,bearing, maxv, minv) %>% tidyr::pivot_longer(cols = c(maxv, minv), names_to = "drop", values_to = "radial_v") %>% dplyr::select(-drop) %>% dplyr::distinct()




      test_vectors %<>% dplyr::arrange(range_cell, radial_v)

      # test_vectors$radial_v[order(abs(test_vectors$radial_v))]
      #
      # MUSIC$radial_v[order(abs(MUSIC$radial_v))]
      #
      # bragg_freq <- seasonder_getBraggDopplerAngularFrequency(seasonder_cs_obj)
      #
      # k0 <- seasonder_getRadarWaveNumber(seasonder_cs_obj)/(2*pi)
      #
      # freq <- seasonder_getSeaSondeRCS_MUSIC_DopplerBinsFrequency(seasonder_cs_obj)
      #
      # v <- c((freq[freq < 0]  - bragg_freq[1])/(2*k0),(freq[freq >= 0]  - bragg_freq[2])/(2*k0))
      #
      # MUSIC$radial_v %>% abs() %>% sort() %>% unique() %>%  magrittr::extract(1:2) %>% diff()
      #
      music_v <- MUSIC %>% dplyr::pull("radial_v") %>% unique()
      #
      #   test_vectors$radial_v %>% abs() %>% sort() %>% unique() %>%  magrittr::extract(1:2) %>% diff()

      closest_vel <- test_vectors %>% dplyr::select(range_cell, radial_v) %>% dplyr::distinct() %>% dplyr::group_by(range_cell) %>% dplyr::mutate(music_v = purrr::map_dbl(radial_v,\(x) music_v[which.min(abs((x-music_v*100)))])) %>% dplyr::mutate(music_v = music_v, radial_v = radial_v)%>% dplyr::ungroup() %>% dplyr::arrange(range_cell, radial_v) %>% dplyr::mutate(d = abs(music_v*100 - radial_v)) %>% dplyr::arrange(range_cell, radial_v)


      test_vectors %<>% dplyr::left_join(closest_vel, by = c("range_cell","radial_v"))

      test_vectors %<>% dplyr::left_join(MUSIC, by = c("music_v" = "radial_v", "range_cell" = "range_cell"))

      test_vectors %<>% dplyr::mutate(music_bearing = purrr::map(DOA, "bearing" )) %>% tidyr::unnest(music_bearing) %>% dplyr::arrange(range_cell,doppler_bin) %>% dplyr::mutate(music_bearing = music_bearing %% 360)


      inspect <- test_vectors %>% dplyr::select(range_cell, doppler_bin, radial_v,music_v, bearing, music_bearing) %>% dplyr::group_by(range_cell, doppler_bin, bearing) %>% dplyr::filter(music_bearing == music_bearing[which.min(abs(bearing-music_bearing))]) %>% dplyr::ungroup() %>% dplyr::arrange(range_cell, doppler_bin) %>% dplyr::mutate(music_bearing = (music_bearing  ) %% 360, bearing = (bearing ) %% 360) %>% dplyr::group_by(range_cell, doppler_bin, music_bearing) %>% dplyr::filter(bearing == bearing[which.min(abs(bearing - music_bearing))]) # %>% dplyr::mutate( bearing  = dplyr::case_when(bearing >200 ~ bearing - 360, TRUE ~ bearing), music_bearing  = dplyr::case_when(music_bearing >200 ~ music_bearing - 360, TRUE ~ music_bearing))


      ggplot2::ggplot(inspect, ggplot2::aes(x= music_bearing , y = bearing )) + ggplot2::geom_point(alpha = 0.2) + ggplot2::geom_smooth(method = "lm") + ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") #+ ggplot2::scale_x_continuous(limits = c(-60, 180),n.breaks = 12,expand = c(0,0),minor_breaks = NULL)  + ggplot2::scale_y_continuous(limits = c(-60, 180),n.breaks = 12, expand = c(0,0),minor_breaks = NULL)



      summary(lm(bearing ~ music_bearing, inspect))


      rc <- 3
      db <- 653

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% View()

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("cov") %>% magrittr::extract2(1)

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("eigen") %>% magrittr::extract2(1)

      MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("DOA_solutions") %>% magrittr::extract2(1)


      distances <- MUSIC %>% dplyr::filter(range_cell == rc & doppler_bin == db) %>% dplyr::pull("distances") %>% magrittr::extract2(1)
      rev_dual_solution_dist = pracma::Real(1/distances['single',,drop = TRUE])

      pracma::findpeaks(rev_dual_solution_dist,npeaks = 2, sortstr = TRUE)

      plot((attr(distances,"bearing")) %% 360,rev_dual_solution_dist)

      ruv %<>% dplyr::filter(range_cell < 20)

      test <- seasonder_getSeaSondeRCSShortTimeRadials(seasonder_cs_obj)

      test %<>% dplyr::filter((range_cell >= 3 & range_cell <= 20))


      plot_radials(ruv$range_cell, ruv$bearing,ruv$radial_v)

      plot_radials(test$range_cell,test$bearing,test$radial_v*100)






      radial_comparison <-  ruv %>% dplyr::rename(radial_v_ruv = radial_v) %>% dplyr::filter(range_cell < 20)





      radial_comparison %<>% dplyr::left_join(test, by = c("bearing","range_cell")) %>% dplyr::mutate(radial_v = radial_v * 100, MINV = MINV * 100, MAXV = MAXV* 100, ESPC = ESPC * 100)


      radial_comparison %>% dplyr::select(range_cell, bearing, MINV, `Velocity Minimum`, MAXV, `Velocity Maximum`, radial_v, `Velocity (cm/s)`, `Velocity Count`,EDVC, `Spatial Count`,  ERSC) %>% dplyr::arrange(range_cell, bearing) %>% View()




      lm(radial_v ~ radial_v_ruv, radial_comparison)




      ggplot2::ggplot(radial_comparison, ggplot2::aes(x= radial_v_ruv, y = radial_v)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm")
    })
  })

})

describe("radials computation",{

  it("should run the MUSIC algorithm on the cells and doppler bins specified to get the ideals",{

    seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))



    plot(attr(seasonder_apm_obj, "BEAR",exact = T),Mod(seasonder_apm_obj[2,]),xlim = c(-180,180))
    plot(attr(seasonder_apm_obj, "BEAR",exact = T),Arg(seasonder_apm_obj[2,]),xlim = c(-180,180))


    FOS <-   list(nsm = 2,
                  fdown = 10^(10/10),
                  flim = 10^(20/10),
                  noisefact = 10^(6/10),
                  currmax = 1,
                  reject_distant_bragg = TRUE, #  Default is to apply this test
                  reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)
                  # TODO: implement default reject_noise_ionospheric = FALSE for 42 MHz
                  reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
    )


    seasonder_cs_obj_1 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1610.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)

    seasonder_cs_obj_1 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_1 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_1)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_1)

    test_1 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_1)


    seasonder_cs_obj_2 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1620.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)

    seasonder_cs_obj_2 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_2 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_2)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_2)

    test_2 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_2)


    seasonder_cs_obj_3 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1630.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)

    seasonder_cs_obj_3 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_3 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_3)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_3)

    test_3 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_3)


    seasonder_cs_obj_4 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1640.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)

    seasonder_cs_obj_4 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_4 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_4)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_4)

    test_4 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_4)


    seasonder_cs_obj_5 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1650.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)

    seasonder_cs_obj_5 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_5 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_5)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_5)

    test_5 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_5)


    test <- dplyr::bind_rows( test_1 %>% dplyr::mutate(time = 1),test_2%>% dplyr::mutate(time = 2), test_3%>% dplyr::mutate(time = 3), test_4%>% dplyr::mutate(time = 4), test_5%>% dplyr::mutate(time = 5))

    test %<>% dplyr::filter(range < 5)
    test_ideal <- test
    antenna_bearing <- seasonder_getSeaSondeRAPM_AntennaBearing(seasonder_apm_obj)

    to.plot <- test %>% dplyr::select(range, range_cell,radial_v,DOA_solutions,retained_solution, time) %>%
      dplyr::mutate(bearing = purrr::map2(DOA_solutions, retained_solution, \(x,y) data.frame(bearing=x[[y]]$bearing))) %>% tidyr::unnest(bearing) %>% dplyr::select(bearing,range, range_cell,radial_v, retained_solution, time) %>% dplyr::mutate(bearing = (((bearing * -1 + 360) %% 360) + antenna_bearing ) %% 360)





    ggplot2::ggplot(to.plot,ggplot2::aes(y=range_cell, x = bearing , color = retained_solution)) +

      ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar() + ggplot2::xlim(c(0,360))





    ggplot2::ggplot(to.plot, ggplot2::aes(x=bearing)) + ggplot2::geom_histogram()


    ggplot2::ggplot(to.plot, ggplot2::aes(x=radial_v)) + ggplot2::geom_histogram()

    ggplot2::ggplot(to.plot, ggplot2::aes(x=bearing, y = radial_v, color = retained_solution)) + ggplot2::geom_point(alpha = 0.5)


    bearing_bins <- seq(0,360,2)
    bearing_bin_center <- seq(1,359,2)
    radials <- to.plot %>%
      dplyr::mutate(bearing_bin = findInterval(bearing,bearing_bins)) %>%
      dplyr::group_by(range, range_cell, bearing_bin) %>%
      dplyr::group_by(range, range_cell, bearing_bin) %>% dplyr::summarise(max_v = max(radial_v), min_v = min(radial_v),radial_v = median(radial_v), n = dplyr::n(), time_count = length(unique(time)), .groups = "drop") %>%
      dplyr::mutate(bearing = bearing_bin_center[bearing_bin])








    test$distances[sample(1:nrow(to.plot),10)] %>% purrr::compact() %>% purrr::map2(factor(seq_along(.)),\(dist,i) data.frame(i = i, dist = 1/pracma::Real(dist["single",, drop=T]) , bearing = attr(dist,"bearings", exact = T))) %>% dplyr::bind_rows() %>%   ggplot2::ggplot(ggplot2::aes(y=dist, x=bearing, color = i)) + ggplot2::geom_point(alpha = 0.5) + ggplot2::xlim(-180,180)


    ruv <- data.table::fread("tests/testthat/data/TORA/RDLi_TORA_2024_03_19_1630.ruv",skip = 56, header = F)

    header <- data.table::fread("tests/testthat/data/TORA/RDLi_TORA_2024_03_19_1630.ruv",skip = 53, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist

    header[14] <- paste(header[14],header[15])

    header <- header[-15]

    header[15] <- paste(header[15],header[16])

    header <- header[-16]

    header[3] <- paste(header[3],header[4])

    header <- header[-4]

    header[4] <- paste(header[4],header[5])

    header <- header[-5]


    header2 <- data.table::fread("tests/testthat/data/TORA/RDLi_TORA_2024_03_19_1630.ruv",skip = 54, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist


    names(ruv) <- paste(header,header2)


    ruv %<>% dplyr::filter(`Range (km)`<5)

    to.plot_ruv <- ruv %>% dplyr::select(range_cell = `Spectra RngCell`,bearing = `Bearing (True)`)




    ggplot2::ggplot(to.plot_ruv,ggplot2::aes(y=range_cell, x = bearing )) +

      ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar()






    radial_comparison <-  ruv %>% dplyr::rename(bearing = `Bearing (True)`, range_cell = `Spectra RngCell`) %>% dplyr::mutate(bearing_bin = findInterval(bearing,bearing_bins))





    radial_comparison %<>% dplyr::left_join(radials, by = c("bearing","range_cell")) %>% dplyr::mutate(radial_v = radial_v * 100, min_v = min_v * 100, max_v = max_v* 100)


    radial_comparison %>% dplyr::select(range_cell, bearing, min_v, `Velocity Minimum`, max_v, `Velocity Maximum`, radial_v, `Velocity (cm/s)`, n, `Spatial Count`, `Temporal Count`) %>% dplyr::arrange(range_cell, bearing) %>% View()


    lm(radial_v ~ `Velocity (cm/s)`, radial_comparison)




    ggplot2::ggplot(radial_comparison, ggplot2::aes(x= `Velocity (cm/s)`, y = radial_v)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm")


    ggplot2::ggplot(radial_comparison, ggplot2::aes(x=bearing, y = radial_v)) + ggplot2::geom_point(alpha = 0.5)



  })



})

