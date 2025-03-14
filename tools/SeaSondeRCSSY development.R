
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

  # smoothing <- 10
  #
  # seasonder_apm_obj %<>% seasonder_smoothAPM(smoothing)

  seasonder_cs_obj <- seasonder_createSeaSondeRCS(filepath, seasonder_apm_object = seasonder_apm_obj)

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 2L)



  MUSIC <-   seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()

  check_doppler_cell <- MUSIC %>% dplyr::filter(range_cell == 2 & doppler_bin == 699) %>% as.list()
  (1/(check_doppler_cell$projections[[1]]["dual",] %>% abs())) %>% plot()
  (10*log10(1/(check_doppler_cell$projections[[1]]["dual",] %>% abs()))) %>% plot()

P <-   check_doppler_cell$DOA_solutions[[1]]$dual$P
abs(P)
abs(diag(P)) %>% prod()
off_P <- P
diag(off_P) <- 1
abs(off_P) %>% prod()
abs(off_P) %>% prod()/abs(diag(P)) %>% prod()
10*log10(abs(P[1,1]))
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

  # sink(here::here("tools/APM_str.txt"))
  # str(seasonder_apm_obj, list.len = 18)
  # sink()

range_info <- seasonder_exportRangeInfo(seasonder_cs_obj)



test <- seasonder_exportRadialMetrics(seasonder_cs_obj)

test %<>% dplyr::filter(MA1S >5 | (MA2S > 5 & MA3S > 5))

c_names <- c("LOND","LATD","VELU","VELV","VFLG","XDST","YDST","RNGE","BEAR","VELO","HEAD","SPRC","SPDC","MSEL","MSA1","MDA1","MDA2","MEGR","MPKR","MOFR","MSAD","MA13","MP13","MA23","MP23","MSP1","MDP1","MDP2","MSW1","MDW1","MDW2","MSR1","MDR1","MDR2","MA1S","MA2S","MA3S","MEI1","MEI2","MEI3","MSPK","MDPK","MDRJ")

target <- read.table("tests/testthat/data/SUNS/RadialMetric/RDLw_SUNS_2025_02_17_0600.ruv", comment.char = "%") %>% magrittr::set_colnames(c_names)


# test %<>% dplyr::filter(MSEL !=1 | (MSEL == 1 & MSW1 < 120)) %>%
#   dplyr::filter(MSEL !=2 | (MSEL == 2 & MDW1 < 120)) %>%
#   dplyr::filter(MSEL !=3 | (MSEL == 3 & MDW2 < 120))

check <- dplyr::full_join(target %>% dplyr::mutate(id = "target") ,test %>% dplyr::mutate(id = "test",VELO = round(VELO*100,digits = 3)), by = c("SPRC","SPDC","MSEL","BEAR", "VELO"))



single_check <- check %>% dplyr::filter(MSEL == 1)

dual_check_1 <- check %>% dplyr::filter(MSEL == 2)

dual_check_2 <- check %>% dplyr::filter(MSEL == 3)


not_matched <- check %>% dplyr::filter(is.na(id.x) | is.na(id.y)) %>% dplyr::arrange(SPRC, SPDC) %>% dplyr::mutate(r_id = paste(SPRC,SPDC,sep = "_"))


target %>% dplyr::filter(SPRC == 2 & SPDC == 696)
test %>% dplyr::filter(SPRC == 2 & SPDC == 696)

not_matched_MDRJ_4 <- not_matched %>% dplyr::filter(MDRJ.x == 4 | MDRJ.y == 4) %>% dplyr::arrange(SPRC, SPDC)

not_matched_MDRJ_non_4 <- not_matched %>% dplyr::filter(! r_id %in% not_matched_MDRJ_4$r_id ) %>% dplyr::arrange(SPRC, SPDC)


dplyr::full_join(not_matched_MDRJ_non_4 %>% dplyr::select(SPRC, SPDC,MDRJ.x) %>% dplyr::filter(!is.na(MDRJ.x)) %>% dplyr::distinct(),
                 not_matched_MDRJ_non_4 %>% dplyr::select(SPRC, SPDC, MDRJ.y)  %>% dplyr::filter(!is.na(MDRJ.y)) %>% dplyr::distinct()) %>% dplyr::filter(complete.cases(.)) %>% dplyr::select(dplyr::starts_with("MDRJ")) %>% table()


#### Error rate ####


not_matched_target <- not_matched %>% dplyr::filter(id.x == "target") %>% dplyr::select(SPRC, SPDC) %>% dplyr::filter(complete.cases(.)) %>% dplyr::distinct() %>% nrow()
not_matched_test <- not_matched %>% dplyr::filter(id.y == "test") %>% dplyr::select(SPRC, SPDC) %>% dplyr::filter(complete.cases(.)) %>% dplyr::distinct() %>% nrow()
cat("Target ")
cat(not_matched_target/nrow(target)*100)
cat("\nTest ")
cat(not_matched_test/nrow(test)*100)

 dplyr::full_join(not_matched %>% dplyr::select(SPRC, SPDC,MDRJ.x) %>% dplyr::filter(!is.na(MDRJ.x)) %>% dplyr::distinct(),
 not_matched %>% dplyr::select(SPRC, SPDC, MDRJ.y)  %>% dplyr::filter(!is.na(MDRJ.y)) %>% dplyr::distinct()) %>% dplyr::filter(complete.cases(.)) %>% dplyr::select(dplyr::starts_with("MDRJ")) %>% table()

 #### MJDR ####

MDRJ <- not_matched_MDRJ_non_4 %>% dplyr::select(id.x,id.y,dplyr::one_of(c("SPRC","SPDC","MSEL","BEAR", "VELO")), dplyr::starts_with("MDRJ"))

#### MOFR ####

MOFR <- check %>% dplyr::select(id.x,id.y,dplyr::one_of(c("SPRC","SPDC","MSEL","BEAR", "VELO")), dplyr::starts_with("MA1S"), dplyr::starts_with("MA2S"), dplyr::starts_with("MA3S")) %>% dplyr::arrange(SPRC,SPDC)

MOFR_not_matched <-  MOFR %>% dplyr::filter(is.na(id.x) | is.na(id.y)) %>% dplyr::arrange(SPRC,SPDC)# %>% dplyr::mutate(MDR1.y = round(MDR1.y,1),MDR2.x = round(MDR2.x,1),MDR2.y = round(MDR2.y,1))

test %>% dplyr::filter(SPRC == 2 & SPDC == 696)

#### single_check ####



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




#### dual check ####



