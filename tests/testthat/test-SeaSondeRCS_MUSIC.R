#### FOR ####

test_that("SeaSondeRCS Related functions are defined",{

  expect_true(is.function(seasonder_getMUSICCov))
  expect_true(is.function(seasonder_MUSICCovDecomposition))
  expect_true(is.function(seasonder_MUSICEuclideanDistance))



})

describe("MUSIC", {



  describe("seasonder_getMUSICCov",{

    it("should return the cov matrix",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      test <- seasonder_getMUSICCov(seasonder_cs_obj, 2,3)

      expect_snapshot_value(test,style = "serialize")

    })

  })

  describe("seasonder_MUSICCovDecomposition",{

    it("should return the eigen decomposition of the cov matrix",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      C <- seasonder_getMUSICCov(seasonder_cs_obj, 2,3)

      test <- seasonder_MUSICCovDecomposition(C)

      expect_snapshot_value(test,style = "serialize")

    })

  })


  describe("seasonder_MUSICEuclideanDistance",{

    it("should return the euclidean distances",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/MeasPattern.txt"))

      C <- seasonder_getMUSICCov(seasonder_cs_obj, 2,3)

      eigen_analysis <- seasonder_MUSICCovDecomposition(C)

      test <- seasonder_MUSICEuclideanDistance(eigen_analysis, seasonder_apm_obj)

      expect_snapshot_value(test,style = "serialize")

    })

  })



  describe("seasonder_MUSICExtractPeaks",{

    it("should return the euclidean distances",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(here::here("tests/testthat/data/MeasPattern.txt"))

      C <- seasonder_getMUSICCov(seasonder_cs_obj, 2,3)

      eigen_analysis <- seasonder_MUSICCovDecomposition(C)

      distances <- seasonder_MUSICEuclideanDistance(eigen_analysis, seasonder_apm_obj)

      test <- seasonder_MUSICExtractPeaks(distances)

      expect_snapshot_value(test,style = "serialize")

    })

  })

})
