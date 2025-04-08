#### MUSIC ####

test_that("SeaSondeRCS_MUSIC Related functions are defined",{

  expect_true(is.function(seasonder_MUSICComputeCov))
  expect_true(is.function(seasonder_MUSICCovDecomposition))
  expect_true(is.function(seasonder_MUSICComputeDOAProjections))
  expect_true(exists('seasonder_exportRadialMetrics', mode = 'function'), info = 'seasonder_exportRadialMetrics should exist.')




})

describe("MUSIC", {



  describe("seasonder_MUSICComputeCov",{

    it("should return the cov matrix",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(12) & doppler_bin %in%  c(711))

      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test <-seasonder_getSeaSondeRCS_MUSIC(test_obj)$cov[[1]]

target <- matrix(rep(NA_complex_,9),ncol=3)

      target[1,1] <- seasonder_cs_obj$data$SSA1[reduced_MUSIC$range_cell[1],reduced_MUSIC$doppler_bin[1]]
      target[2,2] <- seasonder_cs_obj$data$SSA2[reduced_MUSIC$range_cell[1],reduced_MUSIC$doppler_bin[1]]
      target[3,3] <- abs(seasonder_cs_obj$data$SSA3[reduced_MUSIC$range_cell[1],reduced_MUSIC$doppler_bin[1]])

      target[1,2] <- seasonder_cs_obj$data$CS12[reduced_MUSIC$range_cell[1],reduced_MUSIC$doppler_bin[1]]
      target[1,3] <- seasonder_cs_obj$data$CS13[reduced_MUSIC$range_cell[1],reduced_MUSIC$doppler_bin[1]]
      target[2,3] <- seasonder_cs_obj$data$CS23[reduced_MUSIC$range_cell[1],reduced_MUSIC$doppler_bin[1]]

      target[2,1] <- Conj(target[1,2])
      target[3,1] <- Conj(target[1,3])
      target[3,2] <- Conj(target[2,3])

      expect_equal(test,target)

    })

  })

  describe("seasonder_MUSICCovDecomposition",{

    it("should return the eigen decomposition of the cov matrix",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(20) & doppler_bin %in%  c(702))

      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test_obj <- seasonder_MUSICCovDecomposition(test_obj)

      test <-seasonder_getSeaSondeRCS_MUSIC(test_obj)$eigen


      cov <- test_obj %>% seasonder_getSeaSondeRCS_MUSIC() %>% dplyr::pull("cov") %>% magrittr::extract2(1)

      eig_decomp <- eigen(cov, symmetric = TRUE)

      values_order <- order(eig_decomp$values,decreasing = F)

      target_values <- eig_decomp$values[values_order]

      test_values <- test[[1]]$values

      expect_equal(test_values,target_values)


      target_vectors <- eig_decomp$vectors[,values_order]

      test_vectors <- test[[1]]$vectors


      expect_equal(test_vectors, target_vectors)
    })

  })


  describe("seasonder_MUSICComputeDOAProjections",{

    it("should return the euclidean distances",{


      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)





      reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(7) & doppler_bin %in%  c(346))


      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test_obj <- seasonder_MUSICCovDecomposition(test_obj)


      test_obj <- seasonder_MUSICCheckEigenValueRatio(test_obj)

      test_obj <- seasonder_MUSICComputeDOAProjections(test_obj)

      MUSIC <- seasonder_getSeaSondeRCS_MUSIC(test_obj)

      test <-MUSIC$distances[[1]]



      eigen_analysis <- MUSIC$eigen[[1]]

      # dual solution

      G <- eigen_analysis$vectors[,3, drop = F]

      target_distances <- rep(NA_complex_,ncol(seasonder_apm_obj))

      for(i in seq_along(attr(seasonder_apm_obj, "BEAR",exact = T))){
        target_distances[i] <- Conj(t(seasonder_apm_obj[,i, drop=F])) %*% G %*% Conj(t(G)) %*% seasonder_apm_obj[,i, drop = F]
      }

      plot(attr(seasonder_apm_obj, "BEAR",exact = T),1/pracma::Real(target_distances))

      expect_equal(target_distances,test["dual",])

      # single solution

      G <- eigen_analysis$vectors[,2:3, drop = F]

      target_distances <- rep(NA_complex_,ncol(seasonder_apm_obj))

      for(i in seq_along(attr(seasonder_apm_obj, "BEAR",exact = T))){
        target_distances[i] <- Conj(t(seasonder_apm_obj[,i, drop=F])) %*% G %*% Conj(t(G)) %*% seasonder_apm_obj[,i, drop = F]
      }

      plot(attr(seasonder_apm_obj, "BEAR",exact = T),1/pracma::Real(target_distances))

      expect_equal(target_distances,test["single",])
# plot(attr(seasonder_apm_obj, "BEAR",exact = T),Mod(seasonder_apm_obj[1,]))
      # plot(attr(seasonder_apm_obj, "BEAR",exact = T),Arg(seasonder_apm_obj[1,]))
# plot(Mod(test["dual",]))

      # plot(attr(seasonder_apm_obj, "BEAR",exact = T),Mod(target_distances))
      # plot(attr(seasonder_apm_obj, "BEAR",exact = T),Mod(test["dual",]))


      expect_snapshot_value(test,style = "serialize")



    })

  })



  describe("seasonder_MUSICExtractPeaks",{

    it("should return the bearings of the minimum distances",{

      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)





      reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(7) & doppler_bin %in%  c(346))


      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test_obj <- seasonder_MUSICCovDecomposition(test_obj)

      test_obj <- seasonder_MUSICComputeDOAProjections(test_obj)

      test_obj <- seasonder_MUSICExtractPeaks(test_obj)

      MUSIC <-seasonder_getSeaSondeRCS_MUSIC(test_obj)

      test <- MUSIC$DOA_solutions[[1]]

# single solution

distances <- MUSIC$distances[[1]]

target_bearing <- attr(distances, "bearings",exact = T)[which.max(1/Mod(distances["single",]))]

expect_equal(target_bearing,test$single$bearing)

target_a <- seasonder_apm_obj[,which.max(1/Mod(distances["single",])), drop = F]

expect_equal(target_a,test$single$a)

# dual solution

peaks <- pracma::findpeaks(1/Mod(distances["dual",]),npeaks = 2, sortstr = TRUE)

target_bearing <- attr(distances, "bearings",exact = T)[peaks[,2]]

expect_equal(target_bearing,test$dual$bearing)

target_a <- seasonder_apm_obj[,peaks[,2], drop = F]

expect_equal(target_a,test$dual$a)



    })

  })

})



describe("seasonder_MUSICComputeSignalPowerMatrix",{

  it("should compute the power matrix correctly",{

    seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))

    seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)



    reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(7) & doppler_bin %in%  c(346))


    seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

    test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

    test_obj <- seasonder_MUSICCovDecomposition(test_obj)

    test_obj <- seasonder_MUSICComputeDOAProjections(test_obj)

    test_obj <- seasonder_MUSICExtractPeaks(test_obj)

    test_obj <- seasonder_MUSICComputeSignalPowerMatrix(test_obj)

    MUSIC <-seasonder_getSeaSondeRCS_MUSIC(test_obj)

    for(i in seq_len(nrow(MUSIC))){
      test <- MUSIC$DOA_solutions[[i]]
      a <- test$dual$a
      a_star <- Conj(t(a))
      eigVector <- MUSIC$eigen[[i]]$vectors[,c(1,2)]
      eigValues <- MUSIC$eigen[[i]]$values[c(1,2)]

      G <- a_star %*% eigVector
      G_t <- Conj(t(eigVector)) %*% a

      G_inv <- solve(G)
      G_t_inv <- solve(G_t)
      target_P <- G_t_inv %*% diag(eigValues) %*% G_inv

      expect_equal(target_P, test$dual$P)
    }



  })

})

describe("seasonder_MUSICCheckSignalMatrix",{


  it("should run the third check correctly",{


    seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))

    seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)





    reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(7) & doppler_bin %in%  c(346))

    seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

    test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

    test_obj <- seasonder_MUSICCovDecomposition(test_obj)

    test_obj <- seasonder_MUSICComputeDOAProjections(test_obj)

    test_obj <- seasonder_MUSICExtractPeaks(test_obj)

    test_obj <- seasonder_MUSICComputeSignalPowerMatrix(test_obj)

      test_obj <- seasonder_MUSICCheckSignalPowers(test_obj)

    test_obj <- seasonder_MUSICCheckSignalMatrix(test_obj)

    MUSIC <-seasonder_getSeaSondeRCS_MUSIC(test_obj)
for(i in seq_len(nrow(MUSIC))){

  P <- MUSIC$DOA_solutions[[i]]$dual$P

  diag_prod <- prod(pracma::Real(diag(P)))


  off_diag_prod <- prod(diag(pracma::Real(P[,2:1])))

  target <- diag_prod/off_diag_prod

  expect_equal(target,MUSIC$diag_off_diag_power_ratio[i])

}




  })

})


describe("seasonder_runMUSIC_in_FOR",{

#   it("should run the MUSIC algorithm on the cells and doppler bins specified",{
#
#     seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))
#
#     seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj,doppler_interpolation=2L)
#
#
#
#     seasonder_cs_obj %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = list(nsm = 2, flim = 100, noisefact = 10, reject_distant_bragg = T, reject_noise_ionospheric = T, currmax = 1))
#
#     test_obj <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj)
#
#      seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj)
#
# test <- seasonder_getSeaSondeRCS_MUSIC(test_obj)
#
#
#
#
#
# to.plot <- test %>% dplyr::select(range_cell,radial_v,DOA_solutions,retained_solution) %>%
#   dplyr::mutate(bearing = purrr::map2(DOA_solutions, retained_solution, \(x,y) data.frame(bearing=x[[y]]$bearing))) %>% tidyr::unnest(bearing) %>% dplyr::select(bearing,range_cell,radial_v, retained_solution)
#
#
#
# ggplot2::ggplot(to.plot,ggplot2::aes(y=range_cell, x = bearing*-1 , color = retained_solution)) +
#
#   ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar(start = pi+pi/8) + ggplot2::xlim(c(-180,180))
#
#
#
#
#
#   ggplot2::ggplot(to.plot, ggplot2::aes(x=bearing)) + ggplot2::geom_histogram()
#
#
#   ggplot2::ggplot(to.plot, ggplot2::aes(x=radial_v)) + ggplot2::geom_histogram()
#
#   ggplot2::ggplot(to.plot, ggplot2::aes(x=bearing, y = radial_v, color = retained_solution)) + ggplot2::geom_point(alpha = 0.5)
#
#   test$distances[900:1000] %>% purrr::map(\(dist) Real(dist["single",, drop=F]) %>% data.frame() %>% set_colnames(sprintf("X%03d",seq_len(ncol(dist))))) %>% dplyr::bind_rows() %>% tidyr::pivot_longer(cols = dplyr::everything()) %>% ggplot2::ggplot(ggplot2::aes(y=value, x=name)) + ggplot2::geom_point(alpha = 0.5)
#
# })


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


  it("should run the MUSIC algorithm on the cells and doppler bins specified to get the measured",{

    seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/MeasPattern.txt"))

    #     smoothing <- 10
    #
    #     seasonder_apm_obj[1,] <- complex(real = slider::slide_mean(pracma::Real(seasonder_apm_obj[1,]),before = smoothing, na_rm = T), imaginary =
    # slider::slide_mean(pracma::Imag(seasonder_apm_obj[1,]),before = smoothing, na_rm = T))
    #
    #     seasonder_apm_obj[2,] <- complex(real = slider::slide_mean(pracma::Real(seasonder_apm_obj[2,]),before = smoothing, na_rm = T), imaginary =
    #                                        slider::slide_mean(pracma::Imag(seasonder_apm_obj[2,]),before = smoothing, na_rm = T))

    plot(attr(seasonder_apm_obj, "BEAR",exact = T),Mod(seasonder_apm_obj[2,]),xlim = c(-180,180))
    plot(attr(seasonder_apm_obj, "BEAR",exact = T),Arg(seasonder_apm_obj[2,]),xlim = c(-180,180))



    seasonder_cs_obj_1 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1610.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)

    seasonder_cs_obj_1 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_1 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_1)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_1)

    test_1 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_1)


    seasonder_cs_obj_2 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1620.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)

    seasonder_cs_obj_2 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_2 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_2)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_2)

    test_2 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_2)


    seasonder_cs_obj_3 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1630.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)

    seasonder_cs_obj_3 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_3 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_3)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_3)

    test_3 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_3)


    seasonder_cs_obj_4 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1640.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)

    seasonder_cs_obj_4 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_4 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_4)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_4)

    test_4 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_4)


    seasonder_cs_obj_5 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1650.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)

    seasonder_cs_obj_5 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_5 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_5)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_5)

    test_5 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_5)

    seasonder_cs_obj_6 <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/CSS_TORA_24_03_19_1700.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)

    seasonder_cs_obj_6 %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test_obj_6 <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj_6)

    seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(test_obj_6)

    test_6 <- seasonder_getSeaSondeRCS_MUSIC(test_obj_6)

    test <- dplyr::bind_rows(test_0, test_1,test_2, test_3, test_4, test_5, test_6)

    test %<>% dplyr::filter(range < 5)
test_measured <- test
    antenna_bearing <- seasonder_getSeaSondeRAPM_AntennaBearing(seasonder_apm_obj)

    to.plot <- test %>% dplyr::select(range, range_cell,radial_v,DOA_solutions,retained_solution) %>%
      dplyr::mutate(bearing = purrr::map2(DOA_solutions, retained_solution, \(x,y) data.frame(bearing=x[[y]]$bearing))) %>% tidyr::unnest(bearing) %>% dplyr::select(bearing,range, range_cell,radial_v, retained_solution) %>% dplyr::mutate(bearing = (((bearing * -1 + 360) %% 360) + antenna_bearing ) %% 360)





    ggplot2::ggplot(to.plot,ggplot2::aes(y=range_cell, x = bearing , color = retained_solution)) +

      ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar() + ggplot2::xlim(c(0,360))





    ggplot2::ggplot(to.plot, ggplot2::aes(x=bearing)) + ggplot2::geom_histogram()


    ggplot2::ggplot(to.plot, ggplot2::aes(x=radial_v)) + ggplot2::geom_histogram()

    ggplot2::ggplot(to.plot, ggplot2::aes(x=bearing, y = radial_v, color = retained_solution)) + ggplot2::geom_point(alpha = 0.5)



    radials <- to.plot %>% dplyr::group_by(range, range_cell, bearing) %>% dplyr::summarise(max_v = max(radial_v), min_v = min(radial_v),radial_v = mean(radial_v), n = dplyr::n(), .groups = "drop")



    test$distances[sample(1:nrow(to.plot),10)] %>% purrr::compact() %>% purrr::map2(factor(seq_along(.)),\(dist,i) data.frame(i = i, dist = pracma::Real(dist["dual",, drop=T]) , bearing = attr(dist,"bearings", exact = T))) %>% dplyr::bind_rows() %>%   ggplot2::ggplot(ggplot2::aes(y=dist, x=bearing, color = i)) + ggplot2::geom_point(alpha = 0.5) + ggplot2::xlim(-180,180)


    ruv <- data.table::fread("tests/testthat/data/TORA/RDLm_TORA_2024_03_19_1630.ruv",skip = 56, header = F)

    header <- data.table::fread("tests/testthat/data/TORA/RDLm_TORA_2024_03_19_1630.ruv",skip = 54, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist

    header[14] <- paste(header[14],header[15])

    header <- header[-15]

    header[15] <- paste(header[15],header[16])

    header <- header[-16]

    header[3] <- paste(header[3],header[4])

    header <- header[-4]

    header[4] <- paste(header[4],header[5])

    header <- header[-5]


    header2 <- data.table::fread("tests/testthat/data/TORA/RDLm_TORA_2024_03_19_1630.ruv",skip = 55, header = F,nrows = 1) %>% dplyr::select(-1) %>% unlist


    names(ruv) <- paste(header,header2)


    ruv %<>% dplyr::filter(`Range (km)`<5)

    to.plot_ruv <- ruv %>% dplyr::select(range_cell = `Spectra RngCell`,bearing = `Bearing (True)`)




    ggplot2::ggplot(to.plot_ruv,ggplot2::aes(y=range_cell, x = bearing )) +

      ggplot2::geom_point(alpha = 0.5) +  ggplot2::coord_polar()






    radial_comparison <-  ruv %>% dplyr::rename(bearing = `Bearing (True)`, range_cell = `Spectra RngCell`) %>% dplyr::left_join(radials, by = c("bearing","range_cell")) %>% dplyr::mutate(radial_v = radial_v * 100, min_v = min_v * 100, max_v = max_v* 100)


    radial_comparison %>% dplyr::select(range_cell, bearing, min_v, `Velocity Minimum`, max_v, `Velocity Maximum`, radial_v, `Velocity (cm/s)`, n, `Spatial Count`) %>% dplyr::arrange(range_cell, bearing) %>% View()


    lm(radial_v ~ `Velocity (cm/s)`, radial_comparison)

    ggplot2::ggplot(radial_comparison, ggplot2::aes(x= `Velocity (cm/s)`, y = radial_v)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm")


    ggplot2::ggplot(radial_comparison, ggplot2::aes(x=bearing, y = radial_v)) + ggplot2::geom_point(alpha = 0.5)



  })

})

#### seasonder_MUSIC_LonLat ####

describe("seasonder_MUSIC_lonlat",{

  phase_path <- here::here("tests/testthat/data/TORA/Phases.txt")

  amplitude_corrections <- c(1.22398329,1.32768297)

  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"), override_antenna_bearing = 13.0, override_phase_corrections = phase_path, override_amplitude_factors = amplitude_corrections, apply_phase_and_amplitude_corrections = TRUE)


  seasonder_apm_obj %<>% seasonder_applyAPMAmplitudeAndPhaseCorrections()

  seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)


  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::select(-dplyr::one_of("lonlat")))

test_that("Longitude and latitude are added to the MUSIC matrix",{


  seasonder_cs_obj %<>% seasonder_MUSIC_LonLat()

  MUSIC <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()

expect_true("lonlat" %in% names(MUSIC))

# to.plot_bearing <- MUSIC %>% dplyr::mutate(bearing = DOA %>% purrr::map("bearing")) %>% dplyr::select(range_cell, bearing, radial_v) %>% tidyr::unnest(bearing) %>% dplyr::mutate(bearing = ((-1* bearing %% 360) + seasonder_apm_obj %>% seasonder_getSeaSondeRAPM_AntennaBearing()) %% 360)
#
#
#
# plot_radials(to.plot_bearing$range_cell,to.plot_bearing$bearing,to.plot_bearing$radial_v*100)
#
# MUSIC %>%  dplyr::select(lonlat, radial_v) %>% tidyr::unnest(lonlat) %>% ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = radial_v)) + ggplot2::geom_point() +ggplot2::scale_color_gradient2()



})

})

#### seasonder_exportMUSICTable ####


describe("SUNS test", {


  # Create a SeaSondeRAPM object with corrections
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/TORA/IdealPattern.txt"),
    override_antenna_bearing = 303,
    override_phase_corrections = c(41.66,  40.69),
    override_amplitude_factors = c(8.9078,  7.5399),
    apply_phase_and_amplitude_corrections = TRUE
  )

  seasonder_apm_obj %<>% seasonder_applyAPMAmplitudeAndPhaseCorrections()

  # Create a SeaSondeRCS object
  seasonder_cs_obj <- seasonder_createSeaSondeRCS(
    here::here("tests/testthat/data/CSS_SUNS_2025_02_17_060000.csr"),
    system.file("specs", "CS_V1.yaml", package = "SeaSondeR"),
    seasonder_apm_object = seasonder_apm_obj
  )

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

  test <- seasonder_exportMUSICTable(seasonder_cs_obj)

  test_that("The table created includes longitude, latitude, cell_number, range, doppler_cell, doppler_freq, radial_velocity, signal_power, bearing", {
    expect_named(test, c("longitude", "latitude", "range_cell", "range", "doppler_bin", "doppler_freq", "radial_velocity", "signal_power", "bearing"))
  })


})
describe("seasonder_exportMUSICTable", {
  phase_path <- here::here("tests/testthat/data/TORA/Phases.txt")
  amplitude_corrections <- c(1.22398329, 1.32768297)

  # Create a SeaSondeRAPM object with corrections
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/TORA/IdealPattern.txt"),
    override_antenna_bearing = 13.0,
    override_phase_corrections = phase_path,
    override_amplitude_factors = amplitude_corrections,
    apply_phase_and_amplitude_corrections = TRUE
  )

  seasonder_apm_obj %<>% seasonder_applyAPMAmplitudeAndPhaseCorrections()

  # Create a SeaSondeRCS object
  seasonder_cs_obj <- seasonder_createSeaSondeRCS(
    here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"),
    system.file("specs", "CS_V1.yaml", package = "SeaSondeR"),
    seasonder_apm_object = seasonder_apm_obj
  )

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

  test <- seasonder_exportMUSICTable(seasonder_cs_obj)

  test_that("The table created includes longitude, latitude, cell_number, range, doppler_cell, doppler_freq, radial_velocity, signal_power, bearing", {
    expect_named(test, c("longitude", "latitude", "range_cell", "range", "doppler_bin", "doppler_freq", "radial_velocity", "signal_power", "bearing"))
  })

  test_that("The table created has the same data as the CS object MUSIC table", {
    MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)
    col_test <- test %>% dplyr::select(range_cell, doppler_bin) %>% dplyr::distinct()

    expect_equal(col_test$range_cell, MUSIC$range_cell)
    expect_equal(col_test$doppler_bin, MUSIC$doppler_bin)
  })
})

describe("seasonder_exportCSVMUSICTable",{

  phase_path <- here::here("tests/testthat/data/TORA/Phases.txt")
  amplitude_corrections <- c(1.22398329, 1.32768297)

  # Create a SeaSondeRAPM object with corrections
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/TORA/IdealPattern.txt"),
    override_antenna_bearing = 13.0,
    override_phase_corrections = phase_path,
    override_amplitude_factors = amplitude_corrections,
    apply_phase_and_amplitude_corrections = TRUE
  )

  seasonder_apm_obj %<>% seasonder_applyAPMAmplitudeAndPhaseCorrections()

  # Create a SeaSondeRCS object
  seasonder_cs_obj <- seasonder_createSeaSondeRCS(
    here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"),
    system.file("specs", "CS_V1.yaml", package = "SeaSondeR"),
    seasonder_apm_object = seasonder_apm_obj
  )

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

  target <- seasonder_exportMUSICTable(seasonder_cs_obj) %>% as.data.frame()

test_that("writes the table to the file path",{

file_path <- tempfile(fileext = ".csv")

seasonder_exportCSVMUSICTable(seasonder_cs_obj, file_path)

test <- data.table::fread(file_path) %>% as.data.frame()

expect_equal(test, target)

})

})


#### Processing steps ####

test_that("The CS object records the steps of the MUSIC algorithm",{

  phase_path <- here::here("tests/testthat/data/TORA/Phases.txt")
  amplitude_corrections <- c(1.22398329, 1.32768297)

  # Create a SeaSondeRAPM object with corrections
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/TORA/IdealPattern.txt"),
    override_antenna_bearing = 13.0,
    override_phase_corrections = phase_path,
    override_amplitude_factors = amplitude_corrections,
    apply_phase_and_amplitude_corrections = TRUE
  )

  seasonder_apm_obj %<>% seasonder_applyAPMAmplitudeAndPhaseCorrections()

  # Create a SeaSondeRCS object
  seasonder_cs_obj <- seasonder_createSeaSondeRCS(
    here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"),
    system.file("specs", "CS_V1.yaml", package = "SeaSondeR"),
    seasonder_apm_object = seasonder_apm_obj
  )

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

  test <- seasonder_getSeaSondeRCS_ProcessingSteps(seasonder_cs_obj)

  expect_snapshot_value(test, style = "json2")

})

#### seasonder_computeSignalSNR ####

describe("seasonder_exportMUSICTable", {
  phase_path <- here::here("tests/testthat/data/TORA/Phases.txt")
  amplitude_corrections <- c(1.22398329, 1.32768297)

  # Create a SeaSondeRAPM object with corrections
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/TORA/IdealPattern.txt"),
    override_antenna_bearing = 13.0,
    override_phase_corrections = phase_path,
    override_amplitude_factors = amplitude_corrections,
    apply_phase_and_amplitude_corrections = TRUE
  )

  seasonder_apm_obj %<>% seasonder_applyAPMAmplitudeAndPhaseCorrections()

  # Create a SeaSondeRCS object
  seasonder_cs_obj <- seasonder_createSeaSondeRCS(
    here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"),
    system.file("specs", "CS_V1.yaml", package = "SeaSondeR"),
    seasonder_apm_object = seasonder_apm_obj
  )

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 1L)

  MUSIC_table <- seasonder_exportMUSICTable(seasonder_cs_obj)

  receiver_gain <- seasonder_getReceiverGain_dB(seasonder_cs_obj)

  seasonder_cs_obj <- seasonder_computeNoiseLevel(seasonder_cs_obj)

  SNR <- data.frame(range_cell = 1:seasonder_getnRangeCells(seasonder_cs_obj), noise_level =  seasonder_getSeaSondeRCS_NoiseLevel(seasonder_cs_obj, dB = T))

  test <- seasonder_computeSignalSNR(MUSIC_table, SNR  = SNR,receiver_gain = receiver_gain)

  test_that("SNR is computed correctly", {

   expect_true("SNR" %in% names(test))

    noise_level <- SNR %>% dplyr::select(range_cell, noise_level) %>% dplyr::mutate(noise_level = dB_to_self_spectra(dB_values = noise_level, receiver_gain = receiver_gain)) %>% dplyr::right_join(MUSIC_table,by = "range_cell") %>% dplyr::pull("noise_level")

    expect_equal(test$SNR, MUSIC_table$signal_power/noise_level)

  })


})


#### Paolo, T. de, Cook, T. & Terrill, E. Properties of HF RADAR Compact Antenna Arrays and Their Effect on the MUSIC Algorithm. OCEANS 2007 1–10 (2007) doi:10.1109/oceans.2007.4449265. ####

describe("Paolo and Cook 2007",{

  C <- matrix(c(
    complex(real = 0.2162, imaginary = 0),
    complex(real = 0.0303, imaginary = -0.0090),
    complex(real = 0.3170, imaginary = -0.0063),
    complex(real = 0.0303, imaginary = 0.0090),
    complex(real = 0.0436, imaginary = 0),
    complex(real = -0.0091, imaginary = 0.0213),
    complex(real = 0.3170, imaginary = 0.0063),
    complex(real = -0.0091, imaginary = -0.0213),
    complex(real = 0.5416, imaginary = 0)
  ), nrow = 3, byrow = TRUE)

  E <- matrix(c(
    complex(real = 0.6916, imaginary = -0.0499),
    complex(real = 0.4957, imaginary = 0.0432),
    complex(real = 0.5212, imaginary = -0.0087),
    complex(real = -0.5785, imaginary = 0.0956),
    complex(real = 0.8084, imaginary = -0.0392),
    complex(real = 0.0117, imaginary = 0.0326),
    complex(real = -0.4189, imaginary = 0),
    complex(real = -0.3121, imaginary = 0),
    complex(real = 0.8527, imaginary = 0)
  ), nrow = 3, byrow = TRUE)

  # Definir la matriz Lambda (diagonal)
  Lambda <- c(0.0001, 0.0653, 0.7362)


  A_255 <- matrix(as.complex(c(1,0,1)))
  A_225 <- matrix(as.complex(c(0.8397,0.5420,1)))
  A_205 <- matrix(as.complex(c(0.9397,0.3420,1)))
  A_190 <- matrix(as.complex(c(0.7397,0.6420,1)))
  A_330 <- matrix(as.complex(c(-0.2588,-0.9659, 1)))
  A_340 <- matrix(as.complex(c(-0.4588,-0.8659, 1)))
APM <- cbind(A_190, A_205, A_225, A_255,A_330, A_340)

  apm_obj <- seasonder_createSeaSondeRAPM(calibration_matrix = APM, BEAR = c(190, 205, 225,255,330, 340)-360)


  seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = apm_obj)

  reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(12) & doppler_bin %in%  c(711))

  reduced_MUSIC$cov[[1]] <- C

  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

  seasonder_cs_obj %<>% seasonder_MUSICCovDecomposition()


  seasonder_cs_obj %<>% seasonder_MUSICComputeDOAProjections()

  seasonder_cs_obj %<>% seasonder_MUSICExtractPeaks()

  seasonder_cs_obj %<>% seasonder_MUSICComputeSignalPowerMatrix()

  seasonder_cs_obj %<>% seasonder_MUSICTestDualSolutions()



  describe("seasonder_MUSICCovDecomposition",{

    it("should work",{



      test <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$eigen[[1]]

      expect_equal(rev(test$values), Lambda, tolerance = 0.001)

      expect_equal(abs(pracma::Real(test$vectors[,3:1])),abs(pracma::Real(E)), tolerance = 0.01)
      expect_equal(abs(pracma::Imag(test$vectors[,3:1])),abs(pracma::Imag(E)), tolerance = 0.01)

    })
  })



  describe("seasonder_MUSICComputeDOAProjections",{

    it("should work",{



      test <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$projections[[1]]

      expect_equal(10*log10(1/as.vector(abs(test["single",4]))), 9.5, tolerance = 0.05)

      expect_equal(10*log10(1/as.vector(abs(test["dual",2]))), 28.6, tolerance = 0.05)

      expect_equal(10*log10(1/as.vector(abs(test["dual",5]))), 21.1, tolerance = 0.05)

    })
  })

  describe("seasonder_MUSICExtractPeaks",{

    it("should work",{



      test <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$DOA_solutions[[1]]

expect_equal(test$single$bearing , -105)

expect_equal(test$dual$bearing , c(-155, -30))



    })
  })


  describe("seasonder_MUSICSelectDOA",{


    describe("seasonder_MUSICCheckEigenValueRatio",{
      it("should work",{



        test <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$eigen_values_ratio

        expect_equal(test , 11.28, tolerance = 0.005)
        expect_true(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$P1_check)


      })
    })

    describe("seasonder_MUSICCheckSignalPowers",{
      it("should work",{



        test <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$signal_power_ratio

        expect_equal(test , 4.43, tolerance = 0.005)
        expect_true(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$P2_check)


      })
    })

    describe("seasonder_MUSICCheckSignalMatrix",{
      it("should work",{



        test <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$diag_off_diag_power_ratio

        expect_equal(test , 2.72, tolerance = 0.05)
        expect_true(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)$P3_check)


      })
    })

  })

})

#### seasonder_exportRadialMetrics ####


# Tests for seasonder_exportRadialMetrics

describe('seasonder_exportRadialMetrics', {
load(here::here("tests/testthat/data/MUSIC_table.RData"))

  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/SUNS/MeasPattern.txt")
  )

  it('fills correctly columns common to single and dual solutions',{

    # Create a fake MUSIC table with a single solution
    music <- MUSIC %>% dplyr::filter(retained_solution == "single") %>% dplyr::slice(1)

    # Mock seasonder_getSeaSondeRCS_MUSIC to return our fake MUSIC table
    with_mocked_bindings(
      seasonder_getSeaSondeRCS_APM = function(...) {seasonder_apm_obj},
      seasonder_getSeaSondeRCS_MUSIC = function(...) { music },
      res <- seasonder_exportRadialMetrics("fake_object")
    )



    # Check that one row is returned
    expect_equal(nrow(res), 1, info = 'Single solution should produce one row')

    expect_equal(res$VELO, music$radial_v, info = 'VELO not set properly')

    # Check solution type and bearing values
    expect_equal(res$MSA1, seasonder_MUSICBearing2GeographicalBearing(music$DOA_solutions[[1]]$single$bearing,seasonder_apm_obj)[[1]], info = 'MSA1 should be set to the single solution bearing')
    expect_equal(res$MDA1, seasonder_MUSICBearing2GeographicalBearing(music$DOA_solutions[[1]]$dual$bearing[1],seasonder_apm_obj)[[1]], info = 'MDA1 should be set to the first dual solution bearing')
    expect_equal(res$MDA2, seasonder_MUSICBearing2GeographicalBearing(music$DOA_solutions[[1]]$dual$bearing[2],seasonder_apm_obj)[[1]], info = 'MDA2 should be set to the second dual solution bearing')


    # Check that other columns are correctly filled from the MUSIC row
    expect_equal(res$SPDC, music$doppler_bin -1, info = 'SPDC should be assigned from doppler_bin')
    expect_equal(res$SPRC, music$range_cell, info = 'SPDC should be assigned from doppler_bin')
    expect_equal(res$MEGR, music$eigen_values_ratio, info = 'MEGR should be assigned correctly')
    expect_equal(res$MPKR, music$signal_power_ratio, info = 'MPKR should be assigned correctly')
    expect_equal(res$MOFR, music$diag_off_diag_power_ratio, info = 'MOFR should be assigned correctly')
    expect_equal(res$MSR1, 10^(music$DOA_solutions[[1]]$single$peak_resp/10), info = 'MSR1 should be assigned correctly')
    expect_equal(res$MDR1, 10^(music$DOA_solutions[[1]]$dual$peak_resp/10)[1], info = 'MDR1 should be assigned correctly')
    expect_equal(res$MDR2, 10^(music$DOA_solutions[[1]]$dual$peak_resp/10)[2], info = 'MDR2 should be assigned correctly')

    expect_equal(res$MSP1, as.numeric(10*log10(abs(music$DOA_solutions[[1]]$single$P))), info = 'MSP1 should be assigned correctly')
    expect_equal(res$MDP1, as.numeric(10*log10(abs(music$DOA_solutions[[1]]$dual$P[1,1]))), info = 'MDP1 should be assigned correctly')
    expect_equal(res$MDP2, as.numeric(10*log10(abs(music$DOA_solutions[[1]]$dual$P[2,2]))), info = 'MDP2 should be assigned correctly')

    expect_equal(res$MEI1, music$eigen[[1]]$values[1], info = 'MEI1 should be assigned correctly')
    expect_equal(res$MEI2, music$eigen[[1]]$values[2], info = 'MEI2 should be assigned correctly')
    expect_equal(res$MEI3, music$eigen[[1]]$values[3], info = 'MEI3 should be assigned correctly')

  })
  it('handles single solution correctly', {

    # Create a fake MUSIC table with a single solution
    music <- MUSIC %>% dplyr::filter(retained_solution == "single") %>% dplyr::slice(1)

with_mocked_bindings(
  seasonder_getSeaSondeRCS_APM = function(...) {seasonder_apm_obj},
  seasonder_getSeaSondeRCS_MUSIC = function(...) { music },
  res <- seasonder_exportRadialMetrics("fake_object")
)



    # Check that one row is returned
    expect_equal(nrow(res), 1, info = 'Single solution should produce one row')

    # Check solution type and bearing values
    expect_equal(res$MSEL, 1, info = 'MSEL should be 1 for single solution')

    expect_equal(res$BEAR, seasonder_MUSICBearing2GeographicalBearing(music$DOA_solutions[[1]]$single$bearing,seasonder_apm_obj)[[1]], info = 'BEAR should be set to the single solution bearing')
    expect_equal(res$HEAD, (res$BEAR -180) %% 360, info = 'HEAD not set properly')

    # Check location and velocity assignments
    expect_equal(res$LOND, music$lonlat[[1]]$lon, info = 'LOND should be assigned from lonlat')
    expect_equal(res$LATD, music$lonlat[[1]]$lat, info = 'LATD should be assigned from lonlat')

  })

  it('handles dual solution correctly', {

    # Create a fake MUSIC table with a single solution
    music <- MUSIC %>% dplyr::filter(retained_solution == "dual") %>% dplyr::slice(1)

    with_mocked_bindings(
      seasonder_getSeaSondeRCS_APM = function(...) {seasonder_apm_obj},
      seasonder_getSeaSondeRCS_MUSIC = function(...) { music },
      res <- seasonder_exportRadialMetrics("fake_object")
    )


  })

  it('handles both single and dual solutions together', {
    # Create a fake MUSIC table where both single and dual solutions are provided
    music <- data.frame(
      radial_v = 12,
      range = 6,
      range_cell = 4,
      doppler_bin = 320,
      eigen_values_ratio = 0.75,
      signal_power_ratio = 1.05,
      diag_off_diag_power_ratio = 1.45,
      retained_solution = 'dual',  # The field can be arbitrary since both keys are checked
      stringsAsFactors = FALSE
    )
    music$lonlat <- list(data.frame(lon = 120, lat = 70))
    # Provide both single and dual solutions
    music$DOA_solutions <- list(list(single = list(bearing = 333), dual = list(bearing = c(444, 555))))

    local_mocked_bindings(
      seasonder_getSeaSondeRCS_MUSIC = function(...) { music }
    )

    res <- seasonder_exportRadialMetrics()

    # Expect three rows: one for single, two for dual
    expect_equal(nrow(res), 3, info = 'Both single and dual solutions should produce three rows')

    single_row <- res[res$MSEL == 1, ]
    dual_row1 <- res[res$MSEL == 2, ]
    dual_row2 <- res[res$MSEL == 3, ]

    expect_equal(nrow(single_row), 1, info = 'There should be one single solution row')
    expect_equal(nrow(dual_row1), 1, info = 'There should be one dual solution row with MSEL 2')
    expect_equal(nrow(dual_row2), 1, info = 'There should be one dual solution row with MSEL 3')

    expect_equal(single_row$MSA1, 333, info = 'Single solution row: MSA1 should match single bearing')
    expect_equal(single_row$BEAR, 333, info = 'Single solution row: BEAR should match single bearing')

    expect_equal(dual_row1$MDA1, 444, info = 'Dual solution row (MSEL 2): MDA1 should match first dual bearing')
    expect_equal(dual_row1$BEAR, 444, info = 'Dual solution row (MSEL 2): BEAR should match first dual bearing')

    expect_equal(dual_row2$MDA2, 555, info = 'Dual solution row (MSEL 3): MDA2 should match second dual bearing')
    expect_equal(dual_row2$BEAR, 555, info = 'Dual solution row (MSEL 3): BEAR should match second dual bearing')

    # Check that shared columns are consistently set
    for (col in c('SPDC', 'MEGR', 'MPKR', 'MOFR', 'LOND', 'LATD', 'VELU', 'VELV')) {
      expect_true(all(res[[col]] == music[[col]][1]), info = paste('Column', col, 'should be consistently assigned'))
    }

    # Verify that the resulting data frame has exactly 34 columns with correct names
    expected_cols <- c('LOND', 'LATD', 'VELU', 'VELV', 'VFLG', 'RNGE', 'BEAR', 'VELO', 'HEAD',
                       'SPRC', 'SPDC', 'MSEL', 'MSA1', 'MDA1', 'MDA2', 'MEGR', 'MPKR', 'MOFR',
                       'MSP1', 'MDP1', 'MDP2', 'MSW1', 'MDW1', 'MDW2', 'MSR1', 'MDR1', 'MDR2',
                       'MA1S', 'MA2S', 'MA3S', 'MEI1', 'MEI2', 'MEI3', 'MDRJ')
    expect_equal(colnames(res), expected_cols, info = 'Result should have 34 columns with correct names')
  })

  it('returns an empty data frame with 34 columns when MUSIC table is empty', {
    # Create an empty MUSIC table
    music <- data.frame(
      radial_v = numeric(0),
      range = numeric(0),
      range_cell = integer(0),
      doppler_bin = integer(0),
      eigen_values_ratio = numeric(0),
      signal_power_ratio = numeric(0),
      diag_off_diag_power_ratio = numeric(0),
      retained_solution = character(0),
      stringsAsFactors = FALSE
    )
    music$lonlat <- list()
    music$DOA_solutions <- list()

    local_mocked_bindings(
      seasonder_getSeaSondeRCS_MUSIC = function(...) { music }
    )

    res <- seasonder_exportRadialMetrics()

    expect_equal(nrow(res), 0, info = 'Empty MUSIC table should produce a data frame with 0 rows')
    expected_cols <- c('LOND', 'LATD', 'VELU', 'VELV', 'VFLG', 'RNGE', 'BEAR', 'VELO', 'HEAD',
                       'SPRC', 'SPDC', 'MSEL', 'MSA1', 'MDA1', 'MDA2', 'MEGR', 'MPKR', 'MOFR',
                       'MSP1', 'MDP1', 'MDP2', 'MSW1', 'MDW1', 'MDW2', 'MSR1', 'MDR1', 'MDR2',
                       'MA1S', 'MA2S', 'MA3S', 'MEI1', 'MEI2', 'MEI3', 'MDRJ')
    expect_equal(colnames(res), expected_cols, info = 'Empty data frame should have 34 columns with correct names')
  })

})

