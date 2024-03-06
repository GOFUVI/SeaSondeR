#### FOR ####

test_that("SeaSondeRCS Related functions are defined",{

  expect_true(is.function(seasonder_getMUSICCov))



})

describe("MUSIC", {



  describe("seasonder_getMUSICCov",{

    it("should return the cov matrix",{

      seasonder_cs_obj <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_V6.cs"), system.file("specs","CS_V1.yaml",package = "SeaSondeR"))

      test <- seasonder_getMUSICCov(seasonder_cs_obj, 2,3)

      expect_snapshot_value(test,style = "deparse")

    })

  })


})
