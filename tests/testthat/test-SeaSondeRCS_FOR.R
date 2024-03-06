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
