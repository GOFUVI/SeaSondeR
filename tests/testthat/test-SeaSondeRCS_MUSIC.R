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

      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)[1:1000,])

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test <-seasonder_getSeaSondeRCS_MUSIC(test_obj)$cov

      expect_snapshot_value(test,style = "serialize")

    })

  })

  describe("seasonder_MUSICCovDecomposition",{

    it("should return the eigen decomposition of the cov matrix",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)[1:1000,])

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test_obj <- seasonder_MUSICCovDecomposition(test_obj)

      test <-seasonder_getSeaSondeRCS_MUSIC(test_obj)$eigen


      expect_snapshot_value(test,style = "serialize")

    })

  })


  describe("seasonder_MUSICEuclideanDistance",{

    it("should return the euclidean distances",{


      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/MeasPattern.txt"))

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)





      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)[1:1000,])

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test_obj <- seasonder_MUSICCovDecomposition(test_obj)

      test_obj <- seasonder_MUSICEuclideanDistance(test_obj)

      test <-seasonder_getSeaSondeRCS_MUSIC(test_obj)$distances


      expect_snapshot_value(test,style = "serialize")



    })

  })



  describe("seasonder_MUSICExtractPeaks",{

    it("should return the bearings of the minimum distances",{

      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/MeasPattern.txt"))

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)





      seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_MUSIC(seasonder_getSeaSondeRCS_MUSIC(seasonder_cs_obj)[1:1000,])

      test_obj <- seasonder_MUSICComputeCov(seasonder_cs_obj)

      test_obj <- seasonder_MUSICCovDecomposition(test_obj)

      test_obj <- seasonder_MUSICEuclideanDistance(test_obj)

      test_obj <- seasonder_MUSICExtractPeaks(test_obj)

      test <-seasonder_getSeaSondeRCS_MUSIC(test_obj)$DOA_solutions


      expect_snapshot_value(test,style = "serialize")


    })

  })

})



describe("seasonder_runMUSIC_in_FOR",{

  it("should run the MUSIC algorithm on the cells and doppler bins specified",{

    seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/MeasPattern.txt"))

    seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"), seasonder_apm_object = seasonder_apm_obj)


    test_obj <- seasonder_runMUSIC_in_FOR(seasonder_cs_obj)

test <- seasonder_getSeaSondeRCS_MUSIC(test_obj)

    expect_snapshot_value(test,style = "serialize")

  })

})
