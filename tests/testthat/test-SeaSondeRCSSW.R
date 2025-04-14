library(testthat)
library(SeaSondeR)
library(magrittr)

test_that("El fichero exportado coincide con el fichero de referencia", {
  # Definir rutas y cargar objetos
  CS_file_path <- system.file("css_data/CSS_TORA_2024_04_04_070000.csr", package = "SeaSondeR")
  APM_path     <- system.file("css_data/MeasPattern.txt", package = "SeaSondeR")
  
seasonder_disableMessages()

  seasonder_apm_obj <- seasonder_readSeaSondeRAPMFile(APM_path)
  
  seasonder_cs_obj <- seasonder_createSeaSondeRCS(CS_file_path, seasonder_apm_object = seasonder_apm_obj)
  
  # Configuración del primer orden
  first_order_settings <- list(
    nsm = 2,
    fdown = 10^(10 / 10),
    flim = 10^(20 / 10),
    noisefact = 10^(6 / 10),
    currmax = 2,
    reject_distant_bragg = TRUE,
    reject_noise_ionospheric = FALSE,
    reject_noise_ionospheric_threshold = 0
  )
  seasonder_cs_obj <- seasonder_computeFORs(seasonder_cs_obj, FOR_control = first_order_settings)
  
  # Configuración y ejecución del algoritmo MUSIC
  MUSIC_options <- list(
    doppler_interpolation = 2,
    smoothNoiseLevel = TRUE,
    PPMIN = 5, 
    PWMAX = 50
  )
  seasonder_cs_obj <- seasonder_setMUSICOptions(seasonder_cs_obj, MUSIC_options)
  seasonder_cs_obj <- seasonder_runMUSICInFOR(seasonder_cs_obj)
  
  # Generar un archivo temporal para el LLUV
  lluv_temp_path <- tempfile(pattern = "CSS_TORA_24_04_04_0700", fileext = ".ruv")
  # Definir segmentos angulares y exportar métricas radiales
  AngSeg <- purrr::list_c(lapply(45:61, function(i) list(c(i, 313, 360), c(i, 0, 31))))
  seasonder_exportLLUVRadialMetrics(
    seasonder_cs_obj, 
    LLUV_path = lluv_temp_path, 
    AngSeg = AngSeg
  )
  
  # Leer ficheros de texto
  expected_metrics_text <- readLines(system.file("css_data/CSR_TORA_24_04_04_0700.ruv", package = "SeaSondeR"))
  radial_metrics_text   <- readLines(lluv_temp_path)
  
  # Función para eliminar las líneas que empiezan por %ProcessedTimeStamp
  filter_text <- function(txt) {
    txt[!grepl("^%ProcessedTimeStamp", txt)]
  }
  
  # Aplicar el filtro a ambos textos
  expected_metrics_text <- filter_text(expected_metrics_text)
  radial_metrics_text   <- filter_text(radial_metrics_text)
  
  # Nuevo bloque para comparar considerando la línea %UUID:
  uuid_expected <- grep("^%UUID:", expected_metrics_text, value = TRUE)
  uuid_radial   <- grep("^%UUID:", radial_metrics_text, value = TRUE)
  
  if (length(uuid_expected) > 0 && length(uuid_radial) > 0) {
    if (all(uuid_expected == uuid_radial)) {
      # Si la línea %UUID: es idéntica, comparar todo el texto
      expect_equal(radial_metrics_text, expected_metrics_text)
    } else {
      # Si la línea %UUID: difiere, comparar el resto de líneas permitiendo hasta 1 carácter de diferencia
      expected_no_uuid <- expected_metrics_text[!grepl("^%UUID:", expected_metrics_text)]
      radial_no_uuid   <- radial_metrics_text[!grepl("^%UUID:", radial_metrics_text)]
      expect_equal(length(expected_no_uuid), length(radial_no_uuid))
      for(i in seq_along(expected_no_uuid)) {
        diff <- adist(expected_no_uuid[i], radial_no_uuid[i])
        if(diff > 1) {
          fail(sprintf("La línea %d difiere más de lo permitido: esperado '%s', obtenido '%s'", 
                       i, expected_no_uuid[i], radial_no_uuid[i]))
        }
      }
    }
  } else {
    # Si no hay línea %UUID: en ambos textos, comparar el texto completo
    expect_equal(radial_metrics_text, expected_metrics_text)
  }
})

