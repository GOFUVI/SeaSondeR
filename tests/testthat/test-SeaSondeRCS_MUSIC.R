#### FOR ####

test_that("SeaSondeRCS Related functions are defined",{

  expect_true(is.function(seasonder_MUSICComputeCov))
  expect_true(is.function(seasonder_MUSICCovDecomposition))
  expect_true(is.function(seasonder_MUSICEuclideanDistance))



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


  describe("seasonder_MUSICEuclideanDistance",{

    it("should return the euclidean distances",{


      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/TORA/IdealPattern.txt"))

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)





      reduced_MUSIC <- seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj) %>% dplyr::filter(range_cell %in% c(7) & doppler_bin %in%  c(346))


      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(reduced_MUSIC)

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test_obj <- seasonder_MUSICCovDecomposition(test_obj)


      test_obj <- seasonder_MUSICCheckEigenValueRatio(test_obj)

      test_obj <- seasonder_MUSICEuclideanDistance(test_obj)

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

      test_obj <- seasonder_MUSICEuclideanDistance(test_obj)

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

    test_obj <- seasonder_MUSICEuclideanDistance(test_obj)

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

    test_obj <- seasonder_MUSICEuclideanDistance(test_obj)

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



