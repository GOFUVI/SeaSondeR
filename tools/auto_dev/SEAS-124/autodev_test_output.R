library(testthat)

context("Tests for seasonder_apply_signs and seasonder_readSeaSondeRCSSYFile")

# Test case 1: Vectores
# Se simulan los datos de entrada y se mockean las funciones dependientes para trabajar con vectores.
test_that("seasonder_readSeaSondeRCSSYFile works correctly with vector data", {
  # Definición de las funciones mock
  seasonder_CSSY2CSData <<- function(file_path) {
    list(spectra = c(1, 2, 3), autospectra = c(4, 5, 6))
  }
  
  seasonder_read_csign <<- function(file_path) {
    -1  # Factor de corrección para spectra
  }
  
  seasonder_read_asign <<- function(file_path) {
    2   # Factor de corrección para autospectra
  }
  
  # Datos esperados tras la aplicación de los signos
  expected_spectra <- c(1, 2, 3) * (-1)  # c(-1, -2, -3)
  expected_autospectra <- c(4, 5, 6) * 2   # c(8, 10, 12)
  
  result <- seasonder_readSeaSondeRCSSYFile('dummy_path')
  
  expect_equal(result$spectra, expected_spectra)
  expect_equal(result$autospectra, expected_autospectra)
})

# Test case 2: Matrices
# Se simulan los datos de entrada y se mockean las funciones dependientes para trabajar con matrices.
test_that("seasonder_readSeaSondeRCSSYFile works correctly with matrix data", {
  # Definición de las funciones mock
  seasonder_CSSY2CSData <<- function(file_path) {
    list(
      spectra = matrix(c(1, 2, 3, 4), nrow = 2),
      autospectra = matrix(c(10, 20, 30, 40), nrow = 2)
    )
  }
  
  seasonder_read_csign <<- function(file_path) {
    matrix(rep(2, 4), nrow = 2)  # Factor de corrección para spectra
  }
  
  seasonder_read_asign <<- function(file_path) {
    matrix(rep(3, 4), nrow = 2)  # Factor de corrección para autospectra
  }
  
  # Datos esperados tras la aplicación de los signos
  expected_spectra <- matrix(c(1, 2, 3, 4), nrow = 2) * matrix(rep(2, 4), nrow = 2)
  expected_autospectra <- matrix(c(10, 20, 30, 40), nrow = 2) * matrix(rep(3, 4), nrow = 2)
  
  result <- seasonder_readSeaSondeRCSSYFile('dummy_path')
  
  expect_equal(result$spectra, expected_spectra)
  expect_equal(result$autospectra, expected_autospectra)
})

