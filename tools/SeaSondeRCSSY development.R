
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



#   MUSIC <-   seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()
#
#   test <- MUSIC %>% dplyr::filter(range_cell == 2 & doppler_bin == 696) %>% as.list()
# table <- seasonder_cs_obj %>% seasonder_exportMUSICTable()
# (1/abs(test$projections[[1]]["single",]) ) %>% max()
#
#
#
# table_test <-  table %>% dplyr::filter(range_cell == 2 & doppler_bin == 696) %>% as.list()
# seasonder_SeaSondeRCS_plotSelfSpectrum(seasonder_cs_obj, 3 , 20,plot_FORs = TRUE)


#
# sink(here::here("tools/MUSIC_str.txt"))
# str(MUSIC, list.len = 18)
# sink()


rm <- seasonder_exportRadialMetrics(seasonder_cs_obj)

c_names <- c("LOND","LATD","VELU","VELV","VFLG","XDST","YDST","RNGE","BEAR","VELO","HEAD","SPRC","SPDC","MSEL","MSA1","MDA1","MDA2","MEGR","MPKR","MOFR","MSAD","MA13","MP13","MA23","MP23","MSP1","MDP1","MDP2","MSW1","MDW1","MDW2","MSR1","MDR1","MDR2","MA1S","MA2S","MA3S","MEI1","MEI2","MEI3","MSPK","MDPK","MDRJ")

test <- read.table("tests/testthat/data/SUNS/RadialMetric/RDLw_SUNS_2025_02_17_0600.ruv", comment.char = "%") %>% magrittr::set_colnames(c_names)

check <- dplyr::full_join(test,rm, by = c("SPRC","SPDC","MSEL"))

summary(single_check$MEI1.x)

summary(single_check$MEI2.x)

summary(single_check$MEI3.x)

table(abs(single_check$MEI1.x - single_check$MEI1.y) <1e-11)
table(abs(single_check$MEI2.x - single_check$MEI2.y) <1e-11)
table(abs(single_check$MEI3.x - single_check$MEI3.y) <1e-11)


summary(single_check$MSR1.x)

summary(single_check$MDR1.x)

summary(single_check$MDR2.x)

table(abs(single_check$MSR1.x - single_check$MSR1.y) < 1e-1)
table(abs(single_check$MDR1.x - single_check$MDR1.y) <1e-1)
table(abs(single_check$MDR2.x - single_check$MDR2.y) <1e-1)

single_check <- check %>% dplyr::filter(MSEL == 1 & !is.na(BEAR.x) & !is.na(BEAR.y))

table(single_check$BEAR.x == single_check$BEAR.y)


single_check %>% dplyr::filter(BEAR.x != BEAR.y)


dual_check_1 <- check %>% dplyr::filter(MSEL == 2 & !is.na(BEAR.x) & !is.na(BEAR.y))

table(dual_check_1$BEAR.x == dual_check_1$BEAR.y)


dual_check_2 <- check %>% dplyr::filter(MSEL == 3 & !is.na(BEAR.x) & !is.na(BEAR.y))

table(dual_check_2$BEAR.x == dual_check_2$BEAR.y)

dual_check_2 %>% dplyr::filter(BEAR.x != BEAR.y)


not_matched <- check %>% dplyr::filter(is.na(BEAR.x) | is.na(BEAR.y))

not_matched %>% dplyr::filter(!is.na(BEAR.x)) %>% nrow()
