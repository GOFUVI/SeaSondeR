library(covr)
library(testthat)
archivo_codigo <- "R/SeaSondeRAPM.R"
archivo_pruebas <- "tests/testthat/test-SeaSondeRAPM.R"


cobertura <- file_coverage(archivo_codigo, archivo_pruebas)


report(cobertura)
