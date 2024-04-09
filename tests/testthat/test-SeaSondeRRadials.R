test_that("Related functions are defined",{

  expect_true(is.function(new_SeaSondeRCSShortTimeRadials))

})

describe("short-time radials",{

  describe("compute and return a SeaSondeRShortTimeRadials object",{

test_that("test 1 works with ideals",{
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))

  attr(seasonder_apm_obj,"AntennaBearing") <- 13.0

  FOS <-   list(nsm = 2,
                fdown = 10^(10/10),
                flim = 10^(20/10),
                noisefact = 10^(6/10),
                currmax = 1,
                reject_distant_bragg = TRUE, #  Default is to apply this test
                reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
  )


  seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)



  seasonder_csd_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSD_XXXX_0000_00_00_0000.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)


  # seasonder_cs_obj %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR()
  orig_data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)
  interp_data <- seasonder_getSeaSondeRCS_MUSIC_interpolated_data(seasonder_cs_obj)
  SSA1_orig <- orig_data$SSA1
  SSA1_interp <- interp_data$SSA1
csd_data <- seasonder_getSeaSondeRCS_data(seasonder_csd_obj)
SSA1_csd <- csd_data$SSA1
  # MUSIC <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()
  #
  # test1txt <- readLines(here::here("tests/testthat/data/TORA/test1/test1.txt"))

  # test1txt[grepl("Reading CrossSpectra Range|Vectors|dual/sing",test1txt)]

  # MUSIC %>% dplyr::filter(range_cell == 20) %$% table(retained_solution)

  test <- seasonder_getSeaSondeRCSShortTimeRadials(seasonder_cs_obj)

  test %<>% dplyr::filter(range_cell >= 3 & range_cell <= 48)

  to.plot <- test %>% dplyr::select(range_cell,bearing)




  ggplot2::ggplot(to.plot,ggplot2::aes(y=range_cell, x = bearing )) +

    ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar()



  ruv <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 52, header = F)

  header <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 50, header = F,nrows = 1)  %>% unlist

  header[14] <- paste(header[14],header[15])

  header <- header[-15]

  header[15] <- paste(header[15],header[16])

  header <- header[-16]

  header[4] <- paste(header[4],header[5])

  header <- header[-5]

  header[5] <- paste(header[5],header[6])

  header <- header[-6]

  header <- header[-1]

  header2 <- data.table::fread("tests/testthat/data/TORA/test1/RDLx_TORA_2024_04_05_0730.ruv",skip = 51, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist


  names(ruv) <- paste(header,header2)


  ruv_vels <- c(ruv$`Velocity Minimum`,ruv$`Velocity Maximum`) %>% unique() %>% sort()

  test_vels <- (MUSIC$radial_v*100) %>% unique() %>% sort() %>% round(3)


  dplyr::intersect(ruv_vels, test_vels)

  to.plot_ruv <- ruv %>% dplyr::select(range_cell = `Spectra RngCell`,bearing = `Bearing (True)`, radial_v =`Velocity (cm/s)`)

plot_radials(to.plot_ruv$range_cell, to.plot_ruv$bearing,to.plot_ruv$radial_v)

plot_radials(test$range_cell,test$bearing,test$radial_v)



  ggplot2::ggplot(to.plot_ruv,ggplot2::aes(y=range_cell, x = bearing )) +

    ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar()




  radial_comparison <-  ruv %>% dplyr::rename(bearing = `Bearing (True)`, range_cell = `Spectra RngCell`)





  radial_comparison %<>% dplyr::left_join(test, by = c("bearing","range_cell")) %>% dplyr::mutate(radial_v = radial_v * 100, MINV = MINV * 100, MAXV = MAXV* 100, ESPC = ESPC * 100)


  radial_comparison %>% dplyr::select(range_cell, bearing, MINV, `Velocity Minimum`, MAXV, `Velocity Maximum`, radial_v, `Velocity (cm/s)`, `Velocity Count`,EDVC, `Spatial Count`,  ERSC) %>% dplyr::arrange(range_cell, bearing) %>% View()


  dplyr::intersect(radial_comparison$MINV, radial_comparison$`Velocity Minimum`)

  lm(radial_v ~ `Velocity (cm/s)`, radial_comparison)




  ggplot2::ggplot(radial_comparison, ggplot2::aes(x= `Velocity (cm/s)`, y = radial_v)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm")
})

    test_that("test 1 works with measured",{
      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/MeasPattern.txt"))



      FOS <-   list(nsm = 2,
                    fdown = 10^(10/10),
                    flim = 10^(20/10),
                    noisefact = 10^(6/10),
                    currmax = 1,
                    reject_distant_bragg = TRUE, #  Default is to apply this test
                    reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                    reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
      )


      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj, doppler_interpolation = 2L)


      # seasonder_cs_obj %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

      seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR()

      # MUSIC <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()
      #
      # test1txt <- readLines(here::here("tests/testthat/data/TORA/test1/test1.txt"))

      # test1txt[grepl("Reading CrossSpectra Range|Vectors|dual/sing",test1txt)]

      # MUSIC %>% dplyr::filter(range_cell == 20) %$% table(retained_solution)

      test <- seasonder_getSeaSondeRCSShortTimeRadials(seasonder_cs_obj)

      test %<>% dplyr::filter(range_cell >= 3 & range_cell <= 48)

      to.plot <- test %>% dplyr::select(range_cell,bearing)




      ggplot2::ggplot(to.plot,ggplot2::aes(y=range_cell, x = bearing )) +

        ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar()



      ruv <- data.table::fread("tests/testthat/data/TORA/test1/RDLy_TORA_2024_04_05_0730.ruv",skip = 53, header = F)

      header <- data.table::fread("tests/testthat/data/TORA/test1/RDLy_TORA_2024_04_05_0730.ruv",skip = 51, header = F,nrows = 1)  %>% unlist

      header[14] <- paste(header[14],header[15])

      header <- header[-15]

      header[15] <- paste(header[15],header[16])

      header <- header[-16]

      header[4] <- paste(header[4],header[5])

      header <- header[-5]

      header[5] <- paste(header[5],header[6])

      header <- header[-6]

      header <- header[-1]

      header2 <- data.table::fread("tests/testthat/data/TORA/test1/RDLy_TORA_2024_04_05_0730.ruv",skip = 52, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist


      names(ruv) <- paste(header,header2)


      ruv_vels <- c(ruv$`Velocity Minimum`,ruv$`Velocity Maximum`) %>% unique() %>% sort()

      test_vels <- (MUSIC$radial_v*100) %>% unique() %>% sort() %>% round(3)


      dplyr::intersect(ruv_vels, test_vels)

      to.plot_ruv <- ruv %>% dplyr::select(range_cell = `Spectra RngCell`,bearing = `Bearing (True)`, radial_v =`Velocity (cm/s)`)

      plot_radials(to.plot_ruv$range_cell, to.plot_ruv$bearing,to.plot_ruv$radial_v)

      plot_radials(test$range_cell,test$bearing,test$radial_v*100)



      ggplot2::ggplot(to.plot_ruv,ggplot2::aes(y=range_cell, x = bearing )) +

        ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar()




      radial_comparison <-  ruv %>% dplyr::rename(bearing = `Bearing (True)`, range_cell = `Spectra RngCell`)





      radial_comparison %<>% dplyr::left_join(test, by = c("bearing","range_cell")) %>% dplyr::mutate(radial_v = radial_v * 100, MINV = MINV * 100, MAXV = MAXV* 100, ESPC = ESPC * 100)


      radial_comparison %>% dplyr::select(range_cell, bearing, MINV, `Velocity Minimum`, MAXV, `Velocity Maximum`, radial_v, `Velocity (cm/s)`, `Velocity Count`,EDVC, `Spatial Count`,  ERSC) %>% dplyr::arrange(range_cell, bearing) %>% View()


      dplyr::intersect(radial_comparison$MINV, radial_comparison$`Velocity Minimum`)

      lm(radial_v ~ `Velocity (cm/s)`, radial_comparison)




      ggplot2::ggplot(radial_comparison, ggplot2::aes(x= `Velocity (cm/s)`, y = radial_v)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm")
    })


  })

})

describe("radials computation",{

  it("should run the MUSIC algorithm on the cells and doppler bins specified to get the ideals",{

    seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))

    #     smoothing <- 10
    #
    #     seasonder_apm_obj[1,] <- complex(real = slider::slide_mean(pracma::Real(seasonder_apm_obj[1,]),before = smoothing, na_rm = T), imaginary =
    # slider::slide_mean(pracma::Imag(seasonder_apm_obj[1,]),before = smoothing, na_rm = T))
    #
    #     seasonder_apm_obj[2,] <- complex(real = slider::slide_mean(pracma::Real(seasonder_apm_obj[2,]),before = smoothing, na_rm = T), imaginary =
    #                                        slider::slide_mean(pracma::Imag(seasonder_apm_obj[2,]),before = smoothing, na_rm = T))

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

