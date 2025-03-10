library(testthat)

# Se crean funciones simuladas (mocks) para simular el comportamiento esperado
seasonder_CSSY2CSData <- function(filePath) {
  # Devuelve un objeto con ambos campos, spectra y autospectra
  list(spectra = c(2, 4, 6), autospectra = c(3, 6, 9))
}

seasonder_read_csign <- function(filePath) {
  # Signo para spectra
  -1
}

seasonder_read_asign <- function(filePath) {
  # Signo para autospectra
  2
}

# Test que verifica la correcta aplicación de los signos
test_that("seasonder_readSeaSondeRCSSYFile applies sign corrections correctly when both fields are present", {
  result <- seasonder_readSeaSondeRCSSYFile("dummy_file")
  expect_equal(result$spectra, c(2, 4, 6) * (-1))
  expect_equal(result$autospectra, c(3, 6, 9) * 2)
})

# Test para el caso en el que falta el campo 'spectra'
test_that("applySignsToData warns when spectra is missing", {
  dummyData <- list(autospectra = c(3, 6, 9))
  expect_warning(
    corrected <- applySignsToData(dummyData, cellSign = 1, autoSign = 1),
    "Spectra data not found"
  )
  expect_null(corrected$spectra)
  expect_equal(corrected$autospectra, c(3, 6, 9))
})

# Test para el caso en el que falta el campo 'autospectra'
test_that("applySignsToData warns when autospectra is missing", {
  dummyData <- list(spectra = c(2, 4, 6))
  expect_warning(
    corrected <- applySignsToData(dummyData, cellSign = 1, autoSign = -1),
    "Autospectra data not found"
  )
  expect_equal(corrected$spectra, c(2, 4, 6))
  expect_null(corrected$autospectra)
})

