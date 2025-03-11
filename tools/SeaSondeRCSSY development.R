
rm(list=ls())
devtools::document()
devtools::load_all()
library(magrittr)
  filepath <- here::here("tests/testthat/data/SUNS/CSS/CSS_SUNS_2025_02_17_060000.csr")
  specs_path <- here::here("inst/specs/CSSY_V1.yaml")
  endian <-  "big"

#   key_specs <-  seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"),c("key_size_block"))
#
#
#
#
# specs <-   seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"),c("CSSY","HEAD"))
#
#
# con <-   file(filepath, "rb")
# seek(con,16,origin = "start")
# header <- seasonder_readCSSYHeader(con, specs, specs_key_size = key_specs)
# dbRef <- header$dbrf$dBmReference
#
#
# seek(con,0,origin = "start")
#
#
# con <-   file(filepath, "rb")
# seek(con,8,origin = "start")
#
# key <- seasonder_readSeaSondeCSFileBlock(key_specs,con, endian)
#
# seek(con,key$size, origin = "current")
# key <- seasonder_readSeaSondeCSFileBlock(key_specs,con, endian)
# specs <-   seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"),c("CSSY","BODY"))
#
#
# body <- seasonder_readCSSYBody(connection = con, size= key$size, dbRef = dbRef, specs = specs,specs_key_size = key_specs)
#
#
# close(con)


  # Create a SeaSondeRAPM object with corrections
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/SUNS/MeasPattern.txt")
  )

  # smoothing <- 20
  #
  # seasonder_apm_obj %<>% seasonder_smoothAPM(smoothing)

  seasonder_cs_obj <- seasonder_createSeaSondeRCS(filepath, seasonder_apm_object = seasonder_apm_obj)

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 2L)



  MUSIC <-   seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()

  test <- MUSIC %>% dplyr::filter(range_cell == 2 & doppler_bin == 696) %>% as.list()
table <- seasonder_cs_obj %>% seasonder_exportMUSICTable()
(1/abs(test$projections[[1]]["single",]) ) %>% max()



table_test <-  table %>% dplyr::filter(range_cell == 2 & doppler_bin == 696) %>% as.list()
# seasonder_SeaSondeRCS_plotSelfSpectrum(seasonder_cs_obj, 3 , 20,plot_FORs = TRUE)


10*log10(abs(test$DOA[[1]]$P))

sink(here::here("tools/MUSIC_str.txt"))
str(MUSIC, list.len = 18)
sink()
