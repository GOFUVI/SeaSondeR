source(here::here("tools/seasonder_ProcessCS.R"))
APMs_folder <- "tools/APMs/"

describe("ProcessCS on VILA",{

  testthat("CSS_VILA_18_07_30_0900.cs",{

filepath <- "tools/VILA/CSS_VILA_18_07_30_0900.cs"

test <- seasonder_ProcessCSRuntime(filepath, APMs_folder)

  })


  testthat("CSS_VILA_18_07_25_2230.cs",{

    filepath <- "tools/VILA/CSS_VILA_18_07_25_2230.cs"

    test <- seasonder_ProcessCSRuntime(filepath, APMs_folder)

  })


  testthat("CSS_VILA_22_03_01_0000.cs",{

    filepath <- "tools/VILA/CSS_VILA_22_03_01_0000.cs"

    test <- seasonder_ProcessCSRuntime(filepath, APMs_folder)

  })

})
