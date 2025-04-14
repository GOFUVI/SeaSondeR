library(testthat)
library(SeaSondeR)
library(magrittr)

test_that("El fichero exportado coincide con el fichero de referencia", {
  # Definir rutas y cargar objetos
  CS_file_path <- system.file("css_data/CSS_TORA_24_04_04_0700.cs", package = "SeaSondeR")
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
  expected_metrics_text <- readLines(system.file("css_data/CSS_TORA_24_04_04_0700.ruv", package = "SeaSondeR"))
  radial_metrics_text   <- readLines(lluv_temp_path)
  
  # Función para eliminar las líneas que empiezan por %ProcessedTimeStamp
  filter_text <- function(txt) {
    txt[!grepl("^%ProcessedTimeStamp", txt)]
  }
  
  # Aplicar el filtro a ambos textos
  expected_metrics_text <- filter_text(expected_metrics_text)
  radial_metrics_text   <- filter_text(radial_metrics_text)
  
  # Comparar los textos exportados con el fichero de referencia
  expect_equal(radial_metrics_text, expected_metrics_text)
})

