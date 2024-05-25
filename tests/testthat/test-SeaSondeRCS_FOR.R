#### FOR ####

test_that("SeaSondeRCS Related functions are defined",{

  expect_true(is.function(seasonder_computeFORs))
  expect_true(is.function(seasonder_computeFORsSeaSondeMethod))


})

describe("FOR", {



  describe("seasonder_getSeaSondeRCS_NoiseLevel",{

    it("should return the noise level",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      test <- seasonder_getSeaSondeRCS_NoiseLevel(seasonder_cs_obj, c(2,2.5))

      expect_snapshot_value(test,style = "deparse")

    })

  })

  describe("seasonder_SmoothSS",{

    seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

    test <- seasonder_SmoothSS(seasonder_cs_obj,antenna = 3, nsm = 5)

    expect_snapshot_value(test, style = "deparse")

  })


  describe("seasonder_findFORNulls",{

    it("should return the NULLs for each Bragg region",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))


      test <- seasonder_findFORNulls(seasonder_cs_obj = seasonder_cs_obj,nsm = 11, fdown = 7.5)

      expect_snapshot_value(test, style = "json2")



    })

  })

  describe("seasonder_filterFORAmplitudes",{

    it("should filter the First order region",{


      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))


      FORs <- seasonder_findFORNulls(seasonder_cs_obj = seasonder_cs_obj,nsm = 11, fdown = 7.5)

      test <- seasonder_filterFORAmplitudes(seasonder_cs_obj, FORs, reference_noise_normalized_limits = c(2.5,2.8))

      expect_snapshot_value(test, style = "json2")


    })


  })

  describe("seasonder_computeFORs",{




    it("should compute the FORs",{
      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      target <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_obj)

      test <- seasonder_cs_obj %>% seasonder_computeFORs(method = "SeaSonde", FOR_control = list(nsm = 2, flim = 100, noisefact = 10)) %>% seasonder_getSeaSondeRCS_FOR()


      expect_snapshot_value(test, style = "json2")

    })


  })

})

describe("NoiseFloor tests",{


  describe("Ideal",{
test_that("test 1 is correct",{
  FOS <-   list(nsm = 2,
                fdown = 10^(10/10),
                flim = 10^(20/10),
                noisefact = 10^(6/10),
                currmax = 1,
                reject_distant_bragg = TRUE, #  Default is to apply this test
                reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
  )



  seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))



  test_cs_obj <- seasonder_cs_obj %>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

  test <- seasonder_getSeaSondeRCS_NoiseLevel(test_cs_obj)
  seasonder_getSeaSondeRCS_FOR_reference_noise_normalized_limits(test_cs_obj)
  target <- read.table("tests/testthat/data/TORA/test1/NoiseFloor.ideal.txt", skip=2, header = F,col.names = c("range_cell", "Amp_1","Amp_2","Amp_3","dB_1","dB_2","dB_3"))

  comparison <- data.frame(test=test[2:48],target = target$dB_3)
  sqrt(sum((comparison$test-comparison$target)^2))
})

    test_that("test 2 is correct",{
      FOS <-   list(nsm = 2,
                    fdown = 10^(10/10),
                    flim = 10^(20/10),
                    noisefact = 10^(6/10),
                    currmax = 1,
                    reject_distant_bragg = TRUE, #  Default is to apply this test
                    reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                    reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
      )



      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test2/CSS_TORA_24_04_05_0740.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"),doppler_interpolation = 1L)



      test_cs_obj <- seasonder_cs_obj %>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

      test <- seasonder_getSeaSondeRCS_NoiseLevel(test_cs_obj)
      seasonder_getSeaSondeRCS_FOR_reference_noise_normalized_limits(test_cs_obj)
      target <- read.table("tests/testthat/data/TORA/test2/NoiseFloor.ideal.txt", skip=2, header = F,col.names = c("range_cell", "Amp_1","Amp_2","Amp_3","dB_1","dB_2","dB_3"))

      comparison <- data.frame(test=test[2:48],target = target$dB_3)
      sqrt(sum((comparison$test-comparison$target)^2))
    })

  })
})


describe("FOL tests",{


  describe("Ideal",{
    test_that("test 1 is correct",{
      FOS <-   list(nsm = 2,
                    fdown = 10^(10/10),
                    flim = 10^(20/10),
                    noisefact = 10^(6/10),
                    currmax = 1,
                    reject_distant_bragg = TRUE, #  Default is to apply this test
                    reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                    reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
      )



      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))



      test_cs_obj <- seasonder_cs_obj %>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

    test <- seasonder_getSeaSondeRCS_FOR(test_cs_obj) %>% magrittr::extract(2:48) %>% purrr::map(\(x) data.frame(min_p = min(x$positive_FOR), max_p = max(x$positive_FOR), min_n = min(x$negative_FOR), max_n = max(x$negative_FOR))) %>% dplyr::bind_rows()
      seasonder_getSeaSondeRCS_FOR_reference_noise_normalized_limits(test_cs_obj)
      target <- read.table("tests/testthat/data/TORA/test1/FirstOrderLimits.ideal.txt", skip=5, header = F,col.names = c("range_cell", "tgt_min_p","tgt_max_p","tgt_min_n","tgt_max_n"))

comparison <- dplyr::bind_cols(test,target) %>% dplyr::select(range_cell, min_p, tgt_min_p, max_p,tgt_max_p, min_n, tgt_min_n, max_n, tgt_max_n)

    })

    test_that("test 2 is correct",{
      FOS <-   list(nsm = 2,
                    fdown = 10^(10/10),
                    flim = 10^(20/10),
                    noisefact = 10^(6/10),
                    currmax = 1,
                    reject_distant_bragg = TRUE, #  Default is to apply this test
                    reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                    reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
      )



      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test2/CSS_TORA_24_04_05_0740.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"),doppler_interpolation = 2L)



      test_cs_obj <- seasonder_cs_obj %>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

      test <- seasonder_getSeaSondeRCS_NoiseLevel(test_cs_obj)
      seasonder_getSeaSondeRCS_FOR_reference_noise_normalized_limits(test_cs_obj)
      target <- read.table("tests/testthat/data/TORA/test2/NoiseFloor.ideal.txt", skip=2, header = F,col.names = c("range_cell", "Amp_1","Amp_2","Amp_3","dB_1","dB_2","dB_3"))

      comparison <- data.frame(test=test[2:48],target = target$dB_3)
      sqrt(sum((comparison$test-comparison$target)^2))
    })

  })
})


describe("plots",{
  describe("test 1",{

    FOS <-   list(nsm = 2,
                  fdown = 10^(10/10),
                  flim = 10^(20/10),
                  noisefact = 10^(6/10),
                  currmax = 1,
                  reject_distant_bragg = TRUE, #  Default is to apply this test
                  reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                  reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
    )



    seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/TORA/test1/CSS_TORA_24_04_05_0730.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

    seasonder_SeaSondeRCS_plotSelfSpectrum(seasonder_cs_obj, 3 , 20,plot_FORs = TRUE)

    test_cs_obj <- seasonder_cs_obj %>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)

          seasonder_SeaSondeRCS_plotSelfSpectrum(test_cs_obj, 3 , 20,plot_FORs = TRUE)




  })
})


#### Processing steps ####

test_that("The CS object records the steps of the FOR algorithm",{

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

  FOS <-   list(nsm = 2,
                fdown = 10^(10/10),
                flim = 10^(20/10),
                noisefact = 10^(6/10),
                currmax = 1,
                reject_distant_bragg = TRUE, #  Default is to apply this test
                reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

                reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
  )

  seasonder_cs_obj %<>% seasonder_computeFORs(method = "SeaSonde", FOR_control = FOS)
  test <- seasonder_getSeaSondeRCS_ProcessingSteps(seasonder_cs_obj)

  expect_snapshot_value(test, style = "json2")

})
