rm(list=ls())
devtools::document()

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

  devtools::load_all()
  # Create a SeaSondeRAPM object with corrections
  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(
    here::here("tests/testthat/data/SUNS/MeasPattern.txt")
  )

  # smoothing <- 10
  #
  # seasonder_apm_obj %<>% seasonder_smoothAPM(smoothing)

  seasonder_cs_obj_or <- seasonder_createSeaSondeRCS(filepath, seasonder_apm_object = seasonder_apm_obj)

  seasonder_cs_obj <- seasonder_cs_obj_or

  seasonder_cs_obj %<>% seasonder_setFOR_noisefact(5.01)

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 2L, options = list(PPMIN = 5, PWMAX = 50))


  apm_object <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_APM()

  radial_metrics <- seasonder_exportRadialMetrics(seasonder_cs_obj)

  MUSIC_params <- seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC_parameters() %>% magrittr::extract(1:3)
header <- seasonder_cs_obj$header
  APM_attributes <- attributes(apm_object)

  sprintf_vector <- function(x,format,sep = " "){
    vec_format <- rep(format, length(x)) %>% paste0(collapse = sep)
do.call(sprintf,c(list(vec_format), as.list(x)))
  }



  get_col_format <- function(col_name){

    col_formats <- list(
      # Coordenadas (en grados)
      list(cols = c("LOND"), format = "%12.7f"),
      list(cols = c( "LATD"), format = "%12.7f"),
      # Componentes de velocidad (cm/s)
      list(cols = c("VELU", "VELV"), format = "%9.3f"),
      list(cols = c("VELO"), format = "%11.3f"),
      # Código de vector (entero)
      list(cols = c("VFLG"), format = "%11d"),
      # Distancias (km)
      list(cols = c("RNGE"), format = "%10.4f"),
      # Ángulo de rumbo (por ejemplo, Bearing en grados)
      list(cols = c("BEAR"), format = "%8.1f"),
      # Dirección (por ejemplo, Head)
      list(cols = c("HEAD"), format = "%10.1f"),
      # Celdas asociadas (por ejemplo, RngCell, DopCell y flag de selección)
      list(cols = c("SPRC"), format = "%10d"),
      list(cols = c( "SPDC"), format = "%9d"),
      list(cols = c("MSEL"), format = "%6d  "),
      # Medidas asociadas a MusicSngl/MusicDual (valores numéricos con un decimal)
      list(cols = c( "MSA1", "MDA1", "MDA2"), format = "%9.1f "),
      # Razón Eigen (Eigen Ratio)
      list(cols = c("MEGR"), format = "%14.4f"),
      # Razón de potencia (Power Ratio)
      list(cols = c("MPKR"), format = "%13.5f"),
      # Razón de offset (Off Ratio)
      list(cols = c("MOFR"), format = "%13.6f"),
      # Fases A13 y A23 (ángulos)
      list(cols = c("MP13", "MP23"), format = "%8.1f "),
      # Columnas asociadas a Pwr, Pk Width, Peak Resp, S/N, etc. (se muestran con un decimal)
      list(cols = c("MSP1","MDP1", "MDP2"), format = "%10.1f"),
      list(cols = c(
                    "MSW1", "MDW1", "MDW2"), format = "%9.1f "),
      list(cols = c(

        "MSR1", "MDR1", "MDR2"), format = "%10.1f"),
      list(cols = c(
        "MA1S", "MA2S", "MA3S"), format = "%8.1f  "),
      # Valores muy pequeños en notación científica
      list(cols = c("MEI1", "MEI2", "MEI3"), format = "%14.5e"),
      # Columnas de conteo (picos, rechazos, etc.)
      list(cols = c("MDRJ", "PPFG", "PWFG"), format = "%6d  ")


    )

    fmt <- purrr::keep(col_formats, \(fmt) col_name %in% fmt$cols)
    out <- NULL
    if(length(fmt) >0){
      out <- fmt %>% magrittr::extract2(1) %>% purrr::pluck("format")
    }

    return(out)
  }

  radial_metrics_fmt <- radial_metrics%>% dplyr::mutate(dplyr::across(dplyr::everything(), \(x) sprintf(get_col_format(dplyr::cur_column()), x)))

  radial_metrics_fmt <- as.list(radial_metrics_fmt) %>% purrr::transpose()

  # Preparar templates

  template_data <- system.file("templates", "LLUV_RDM1_data.mustache", package = "SeaSondeR") %>%
    readLines() %>% paste0(collapse = "\n")

  # Inserta la función helper para generar un UUID a partir de una cadena
  StringToUUID <- function(Name) {
    # Convierte la cadena Name a su representación en bytes
    name_bytes <- charToRaw(Name)
    
    # Genera el hash SHA-1 utilizando openssl
    sha1_hash <- openssl::sha1(name_bytes)
    
    # Extrae los primeros 16 bytes del hash
    result <- sha1_hash[1:16]
    
    # Ajusta el byte 7 (índice 6 en R) para la versión 5 del UUID
    result[7] <- as.raw(as.integer(result[7]) & 0x0F | 0x50)
    
    # Ajusta el byte 9 (índice 8 en R) para la variante del UUID
    result[9] <- as.raw(as.integer(result[9]) & 0x3F | 0x80)
    
    # Convierte los bytes resultantes en un UUID en formato estándar
    uuid_str <- paste(sprintf("%02x", as.integer(result[1:4])), collapse = "")
    uuid_str <- paste0(uuid_str, "-", paste(sprintf("%02x", as.integer(result[5:6])), collapse = ""))
    uuid_str <- paste0(uuid_str, "-", paste(sprintf("%02x", as.integer(result[7:8])), collapse = ""))
    uuid_str <- paste0(uuid_str, "-", paste(sprintf("%02x", as.integer(result[9:10])), collapse = ""))
    uuid_str <- paste0(uuid_str, "-", paste(sprintf("%02x", as.integer(result[11:16])), collapse = ""))
    
    return(uuid_str)
  }

  # Renderizar el template de data a partir de radial_metrics_fmt
  data_string <- whisker::whisker.render(template_data, radial_metrics_fmt)

  # Calcular UUID_data de forma determinista usando data_string como semilla
  UUID_data <- toupper(StringToUUID(data_string))

  # Crear lista de datos para el template principal
  data <- list(
    RadialMusicParameters = sprintf_vector(MUSIC_params,"%0.3f"," "),
    ncols = ncol(radial_metrics),
    nrows = nrow(radial_metrics),
    PatternPhaseCorrections = sprintf_vector(APM_attributes$PhaseCorrections,"%0.2f"," "),
    PatternAmplitudeCorrections = sprintf_vector(APM_attributes$AmplitudeFactors,"%0.4f"," "),
    RadialBraggNoiseThreshold = sprintf("%0.3f", seasonder_getFOR_noisefact(seasonder_cs_obj)),
    RadialBraggPeakNull = sprintf("%0.3f", seasonder_getFOR_fdown(seasonder_cs_obj)),
    RadialBraggPeakDropOff = sprintf("%0.3f", seasonder_getFOR_flim(seasonder_cs_obj)),
    data = data_string,
    TimeStamp = format(as.POSIXct(seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "nDateTime"), origin = "1970-01-01"), "%Y %m %d  %H %M %S"),
    TransmitCenterFreqMHz = sprintf("%0.6f", seasonder_getCenterFreqMHz(seasonder_cs_obj)),
    TransmitBandwidthKHz = sprintf("%0.6f",
                                   -1^(seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "bSweepUp") == 0) *
                                   seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "fBandwidthKHz")),
    TransmitSweepRateHz = sprintf("%0.6f", seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "fRepFreqHz")),
    RangeResolutionKMeters = sprintf("%0.6f", seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "fRangeCellDistKm")),
    Site = seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "nSiteCodeName"),
    TimeZone = seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "szTimeZone"),
    fHoursFromUTC = sprintf("%+0.3f", seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "fHoursFromUTC")),
    TimeCoverage = sprintf("%0.3f", seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "nCoverMinutes")),
    Origin = paste(APM_attributes$SiteOrigin, collapse = " "),
    UUID = UUID_data,
    PatternUUID = APM_attributes$FileID,
    RangeStart = sprintf("%d",min(radial_metrics$SPRC)),
    RangeEnd = sprintf("%d",max(radial_metrics$SPRC)),
    RangeCells = sprintf("%d",length(unique(radial_metrics$SPRC))),
    DopplerInterpolation = sprintf("%d", seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_cs_obj)),
    DopplerCells = sprintf("%d",seasonder_getnDopplerCells(seasonder_cs_obj)*seasonder_getSeaSondeRCS_MUSIC_doppler_interpolation(seasonder_cs_obj))
  )

  template <- system.file("templates", "LLUV_RDM1.mustache", package = "SeaSondeR") %>%
    readLines() %>% paste0(collapse = "\n")

  LLUV <- whisker::whisker.render(template, data=data)

  LLUV %>% writeLines("tools/test.ruv")

  cat(LLUV)
stop()
  MUSIC <-   seasonder_cs_obj %>% seasonder_getSeaSondeRCS_MUSIC()

#   check_doppler_cell <- MUSIC %>% dplyr::filter(range_cell == 14 & doppler_bin == 778) %>% as.list()
#   (1/(check_doppler_cell$projections[[1]]["dual",] %>% abs())) %>% plot()
#   (10*log10(1/(check_doppler_cell$projections[[1]]["dual",] %>% abs()))) %>% plot()
#
# P <-   check_doppler_cell$DOA_solutions[[1]]$dual$P
# abs(P)
# abs(diag(P)) %>% prod()
# off_P <- P
# diag(off_P) <- 1
# abs(off_P) %>% prod()
# abs(off_P) %>% prod()/abs(diag(P)) %>% prod()
# 10*log10(abs(P[1,1]))
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


test_or <- seasonder_exportRadialMetrics(seasonder_cs_obj)

test <- test_or


c_names <- c("LOND","LATD","VELU","VELV","VFLG","XDST","YDST","RNGE","BEAR","VELO","HEAD","SPRC","SPDC","MSEL","MSA1","MDA1","MDA2","MEGR","MPKR","MOFR","MSAD","MA13","MP13","MA23","MP23","MSP1","MDP1","MDP2","MSW1","MDW1","MDW2","MSR1","MDR1","MDR2","MA1S","MA2S","MA3S","MEI1","MEI2","MEI3","MSPK","MDPK","MDRJ")

target <- read.table("tests/testthat/data/SUNS/RadialMetric/RDLw_SUNS_2025_02_17_0600.ruv", comment.char = "%") %>% magrittr::set_colnames(c_names)


# test %<>% dplyr::filter(MSEL !=1 | (MSEL == 1 & MSW1 < 120)) %>%
#   dplyr::filter(MSEL !=2 | (MSEL == 2 & MDW1 < 120)) %>%
#   dplyr::filter(MSEL !=3 | (MSEL == 3 & MDW2 < 120))

check <- dplyr::full_join(target %>% dplyr::mutate(id = "target") ,test %>% dplyr::mutate(id = "test",VELO = round(VELO,digits = 3)), by = c("SPRC","SPDC","MSEL","BEAR", "VELO"))

check_view <- check %>% dplyr::select(id.x,id.y,dplyr::one_of(c("SPRC","SPDC","MSEL","BEAR", "VELO")), dplyr::starts_with("MDRJ"), dplyr::starts_with("MA1S"), dplyr::starts_with("MA2S"), dplyr::starts_with("MA3S")) %>% dplyr::arrange(SPRC,SPDC)

check_view_not_matched <-  check_view %>% dplyr::filter(is.na(id.x) | is.na(id.y)) %>% dplyr::arrange(SPRC,SPDC)# %>% dplyr::mutate(MDR1.y = round(MDR1.y,1),MDR2.x = round(MDR2.x,1),MDR2.y = round(MDR2.y,1))



not_matched <- check %>% dplyr::filter(is.na(id.x) | is.na(id.y)) %>% dplyr::arrange(SPRC, SPDC) %>% dplyr::mutate(r_id = paste(SPRC,SPDC,sep = "_"))


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



