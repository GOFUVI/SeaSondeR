rm(list=ls())
devtools::document()

library(magrittr)
  filepath <- here::here("tests/testthat/data/SUNS/CSS/CSS_SUNS_2025_02_17_080000.csr")
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

# seasonder_cs_obj %<>% seasonder_computeFORs()

  seasonder_cs_obj %<>% seasonder_runMUSIC_in_FOR(doppler_interpolation = 2L, options = list(PPMIN = 5, PWMAX = 50))



range_info <- seasonder_exportCTFRangeInfo(seasonder_cs_obj, "tools/test.rng", tableStart = "")





  test_or <-seasonder_exportLLUVRadialMetrics(seasonder_cs_obj,"tools/test.ruv")

  

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

  # sink(here::here("tools/range_info_str.txt"))
  # str(range_info, list.len = 18)
  # sink()





test <- test_or


c_names <- c("LOND","LATD","VELU","VELV","VFLG","XDST","YDST","RNGE","BEAR","VELO","HEAD","SPRC","SPDC","MSEL","MSA1","MDA1","MDA2","MEGR","MPKR","MOFR","MSAD","MA13","MP13","MA23","MP23","MSP1","MDP1","MDP2","MSW1","MDW1","MDW2","MSR1","MDR1","MDR2","MA1S","MA2S","MA3S","MEI1","MEI2","MEI3","MSPK","MDPK","MDRJ")

target <- read.table("tests/testthat/data/SUNS/RadialMetric/RDLw_SUNS_2025_02_17_0800.ruv", comment.char = "%") %>% magrittr::set_colnames(c_names)


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

# Se asume que seasonder_cs_obj ya está creado y configurado
# Si no, se debe crear con seasonder_createSeaSondeRCS(...)



read_rng_table <- function(file){
  lines <- readLines(file)
  
  # Extraer nombres de columna desde TableColumnTypes o cabecera "%%"
  types_line <- grep("^%TableColumnTypes:", lines, value = TRUE)
  if(length(types_line) > 0){
    cnames <- strsplit(sub("^%TableColumnTypes:\\s*", "", types_line), "\\s+")[[1]]
  } else {
    header_lines <- lines[grep("^%%", lines)]
    if(length(header_lines) >= 2){
      header_line <- header_lines[2]
    } else {
      header_line <- header_lines[1]
    }
    header_line <- sub("^%%\\s*", "", header_line)
    cnames <- strsplit(header_line, "\\s+")[[1]]
  }
  
  # Extraer únicamente las líneas de datos entre %TableStart: y %TableEnd:
  start_idx <- which(grepl("^%TableStart:", lines))
  end_idx <- which(grepl("^%TableEnd:", lines))
  if(length(start_idx) == 0 || length(end_idx) == 0 || start_idx >= end_idx){
    stop("No se encontró un bloque de datos válido.")
  }
  data_lines <- lines[(start_idx + 1):(end_idx - 1)]
  
  # Eliminar líneas de encabezado que comienzan con "%%"
  data_lines <- data_lines[!grepl("^\\s*%%", data_lines)]
  # Eliminar espacios en blanco y el caracter "%"
  data_lines <- trimws(gsub("^\\s*%", "", data_lines))
  
  if(length(data_lines) == 0){
    stop("No hay datos para leer después de eliminar comentarios.")
  }
  
  # Para cada línea, si tiene (length(cnames)+1) tokens, eliminar el segundo token ("RadDist")
  data_lines <- sapply(data_lines, function(line) {
    tokens <- strsplit(line, "\\s+")[[1]]
    if(length(tokens) == length(cnames) + 1){
      tokens <- tokens[-2]
    }
    paste(tokens, collapse = " ")
  })
  
  text <- paste(paste(cnames, collapse = " "), paste(data_lines, collapse = "\n"), sep = "\n")
  
  tbl <- tryCatch({
    read.table(text = text, header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat("Error al leer la tabla:\n")
    cat(e$message, "\n")
    cat("Texto que se intentó leer:\n")
    cat(text, "\n")
    return(NULL)
  })
  
  return(tbl)
}

ref_rng  <- tryCatch(read_rng_table("tools/RNGI.txt"), error = function(e) NULL)

results <- data.frame(low_limit = numeric(0), error = numeric(0))



for(low_limit in seq(0.99, 0.8, by = -0.01)){
  
  # 1. Configurar limite de estimación de noise reference
  seasonder_cs_obj %<>% seasonder_setSeaSondeRCS_reference_noise_normalized_limits_estimation_interval(list(low_limit = low_limit, high_limit = 1.00))
  
  # 2. Recalcular el nivel de ruido (para antena 3)
  seasonder_cs_obj <- seasonder_computeNoiseLevel(seasonder_cs_obj, antenna = 3, smoothed = T)
  
  # 3. Exportar la tabla range_info a "tools/test.rng"
  seasonder_exportCTFRangeInfo(seasonder_cs_obj, file = "tools/test.rng", tableStart = "")
  
  # 4. Leer la tabla exportada y la de referencia
  test_rng <- tryCatch(read_rng_table("tools/test.rng"), error = function(e) NULL)
  
  
  # Validar existencia de ambas tablas y columna "NF03" para la antena 3
  if(!is.null(test_rng) && !is.null(ref_rng) &&
     "NF03" %in% names(test_rng) && "NF03" %in% names(ref_rng)){
    
    # Calcular error; por ejemplo, la diferencia media absoluta en la columna NF03
    err <- sqrt(mean((test_rng$NF03[1:10]-ref_rng$NF03[1:10])^2, na.rm = TRUE))
    
    results <- rbind(results, data.frame(low_limit = low_limit, error = err))
  }
}

# Seleccionar el/los low_limit con el mínimo error
best <- results[abs(1-results$error) == min(abs(1-results$error)), ]
print(best)

# También se puede guardar la tabla de resultados
write.table(results, file = "tools/estimation_results.txt", row.names = FALSE)


