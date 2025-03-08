#### Unit testing agent runs ####


##### Run #####


#Acción a realizar: write_new_tests.
#Detalles: Se requiere agregar tests que validen la nueva funcionalidad implementada en seasonder_readBodyRangeCell. En concreto, los tests deben verificar que, cuando se encuentre la llave 'scal' (la cual contiene los parámetros de escalado: fmax, fmin, fscale y dbRef), los datos reducidos leídos (por ejemplo, de la llave 'cs1a') sean transformados utilizando la función seasonder_SeaSondeRCSSYApplyScaling. Asimismo, se debe probar el caso en que no se proporciona la llave 'scal', verificando que los datos se retornen sin escalado. Se sugiere simular la secuencia de lectura de llaves usando funciones ficticias (stub) para seasonder_readSeaSondeCSFileBlock y seasonder_read_reduced_encoded_data, de modo que se pueda evaluar el comportamiento esperado.
#
library(testthat)
library(magrittr)

# First test: check that the functions to be tested exist
test_that("Functions existence", {
  expect_true(exists("seasonder_readBodyRangeCell", mode = "function"), info = "The function seasonder_readBodyRangeCell should exist.")
  expect_true(exists("seasonder_SeaSondeRCSSYApplyScaling", mode = "function"), info = "The function seasonder_SeaSondeRCSSYApplyScaling should exist.")
})

# Tests for seasonder_readBodyRangeCell functionality

describe("seasonder_readBodyRangeCell scaling functionality", {

  it("applies scaling when 'scal' key is provided", {
    # Define a minimal specs with keys 'scal' and 'cs1a'
    specs <- list(
      scal = list(),
      cs1a = list()
    )
    
    # Simulated key sequence: first a 'scal' key, then 'cs1a', then 'END '
    local({
      counter <- 0
      keys_sequence <- list(
        list(key = "scal", size = 4),
        list(key = "cs1a", size = 4),
        list(key = "END ", size = 0)
      )
      
      # Stub seasonder_readSeaSondeCSFileBlock to return keys from our sequence
      local_mocked_bindings(
        seasonder_readSeaSondeCSFileBlock = function(specs_key_size, connection, endian) {
          counter <<- counter + 1
          keys_sequence[[counter]]
        }
      )
      
      # Stub seasonder_readCSSYFields to simulate reading scaling parameters
      local_mocked_bindings(
        seasonder_readCSSYFields = function(connection, spec, endian, parent_key = NULL) {
          # Return scaling parameters for the 'scal' key
          return(list(fmax = 5, fmin = 0, fscale = 1000, dbRef = -20))
        }
      )
      
      # Stub seasonder_read_reduced_encoded_data for key 'cs1a'
      local_mocked_bindings(
        seasonder_read_reduced_encoded_data = function(connection, key, endian) {
          # Simulate a reduced data vector
          return(c(1000, 0xFFFFFFFF, 2000))
        }
      )
      
      # Create a dummy connection (the connection is not truly used in the stubs)
      con <- rawConnection(raw(0), "rb")
      on.exit(close(con))
      
      # Execute the function under test
      result <- seasonder_readBodyRangeCell(con, specs, "big", specs_key_size = list())
      
      # Expected scaling calculation:
      # For value = 1000: intermediate = 1000*(5/1000) = 5; voltage = 10^((5 + (-20))/10) = 10^(-1.5) ~ 0.03162278
      # For value = 0xFFFFFFFF, expect NaN
      # For value = 2000: intermediate = 2000*(5/1000) = 10; voltage = 10^((10 + (-20))/10) = 10^(-1) = 0.1
      expected <- c(10^((5 - 20)/10), NaN, 10^((10 - 20)/10))
      
      expect_true("cs1a" %in% names(result), info = "Result should contain key 'cs1a'.")
      expect_equal(result$cs1a, expected, tolerance = 1e-6, info = "Data should be scaled when 'scal' key is provided.")
    })
  })
  
  it("returns raw data when no 'scal' key is provided", {
    # Define specs with only the 'cs1a' key
    specs <- list(
      cs1a = list()
    )
    
    # Simulated key sequence: directly the 'cs1a' key then 'END '
    local({
      counter <- 0
      keys_sequence <- list(
        list(key = "cs1a", size = 4),
        list(key = "END ", size = 0)
      )
      
      local_mocked_bindings(
        seasonder_readSeaSondeCSFileBlock = function(specs_key_size, connection, endian) {
          counter <<- counter + 1
          keys_sequence[[counter]]
        }
      )
      
      # Stub for seasonder_read_reduced_encoded_data for key 'cs1a'
      local_mocked_bindings(
        seasonder_read_reduced_encoded_data = function(connection, key, endian) {
          return(c(500, 600))
        }
      )
      
      con <- rawConnection(raw(0), "rb")
      on.exit(close(con))
      
      result <- seasonder_readBodyRangeCell(con, specs, "big", specs_key_size = list())
      
      expect_true("cs1a" %in% names(result), info = "Result should contain key 'cs1a'.")
      expect_equal(result$cs1a, c(500, 600), info = "Raw data should be returned without scaling when 'scal' key is absent.")
    })
  })
})

