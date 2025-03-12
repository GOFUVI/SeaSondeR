#### Unit testing agent runs ####


##### Run #####


#Acción a realizar: write_new_tests.
#Detalles: Se deben escribir tests desde cero para la función seasonder_extrapolateAPM. Los tests deben comprobar que:
#- La matriz 'SeaSondeRAPM' se extrapola linealmente por ambos extremos agregando n columnas adicionales conforme a la diferencia entre bearings.
#- El vector BEAR se extiende correctamente con los nuevos bearings, manteniendo la misma separación que en el original.
#- Los nombres de las columnas de la matriz quedan actualizados para reflejar los nuevos bearings.
#- Incluir casos de prueba para n = 0 (sin extrapolación) y n = 1 o mayores para validar la extrapolación en ambos extremos.
#Por favor, cubran estos escenarios en el test suite.
#
# Test for the existence of seasonder_extrapolateAPM function
test_that("Function existence", {
  expect_true(exists("seasonder_extrapolateAPM", mode = "function"),
              info = "seasonder_extrapolateAPM should be defined as a function.")
})


describe("seasonder_extrapolateAPM", {
  
  # Test with n = 0 (no extrapolation)
  it("should not change the object when n = 0", {
    # Create a dummy seasonder_apm_object with a simple 2x3 matrix
    original_matrix <- matrix(c(1, 10,
                                  2, 20,
                                  3, 30), nrow = 2, byrow = FALSE)
    original_bear <- c(10, 20, 30)
    colnames(original_matrix) <- as.character(original_bear)
    seasonder_obj <- list(SeaSondeRAPM = original_matrix)
    attr(seasonder_obj, "BEAR") <- original_bear
    
    # Mock the getter and setter functions so that the behavior of
    # seasonder_getSeaSondeRAPM_BEAR simply retrieves the BEAR attribute
    # and seasonder_setSeaSondeRAPM_BEAR updates the BEAR and column names.
    local({
      seasonder_getSeaSondeRAPM_BEAR <- function(seasonde_apm_obj) {
        attr(seasonde_apm_obj, "BEAR")
      }
      seasonder_setSeaSondeRAPM_BEAR <- function(seasonder_apm_object, new_BEAR) {
        attr(seasonder_apm_object, "BEAR") <- new_BEAR
        if (!is.null(seasonder_apm_object$SeaSondeRAPM))
          colnames(seasonder_apm_object$SeaSondeRAPM) <- as.character(new_BEAR)
        seasonder_apm_object
      }
      testthat::local_mocked_bindings(list(
        seasonder_getSeaSondeRAPM_BEAR = seasonder_getSeaSondeRAPM_BEAR,
        seasonder_setSeaSondeRAPM_BEAR = seasonder_setSeaSondeRAPM_BEAR
      ))
      
      # Call the function with n = 0, so no extrapolation should occur
      new_obj <- seasonder_extrapolateAPM(seasonder_obj, n = 0)
      
      # Check that the original matrix remains unchanged
      expect_equal(new_obj$SeaSondeRAPM, original_matrix,
                   info = "With n=0, the SeaSondeRAPM matrix should remain unchanged.")
      
      # Check that the BEAR attribute is unchanged
      expect_equal(attr(new_obj, "BEAR"), original_bear,
                   info = "With n=0, the BEAR attribute should remain unchanged.")
      
      # Check that the column names are still equal to the original BEAR values
      expect_equal(colnames(new_obj$SeaSondeRAPM), as.character(original_bear),
                   info = "Column names should match the original BEAR attribute.")
    })
  })
  
  # Test with n = 1 (extrapolation of one column at each end)
  it("should extrapolate the SeaSondeRAPM matrix and BEAR attribute correctly for n = 1", {
    # Create dummy object: a 2x3 matrix with simple, linearly increasing values
    original_matrix <- matrix(c(1, 10,
                                  2, 20,
                                  3, 30), nrow = 2, byrow = FALSE)
    original_bear <- c(10, 20, 30)
    colnames(original_matrix) <- as.character(original_bear)
    seasonder_obj <- list(SeaSondeRAPM = original_matrix)
    attr(seasonder_obj, "BEAR") <- original_bear
    
    # Expected BEAR extrapolation:
    # Left new bearing: first bearing - difference (20-10 = 10) => 10-10 = 0
    # Right new bearing: last bearing + difference (30-20 = 10) => 30+10 = 40
    expected_bear <- c( original_bear[1] - (original_bear[2]-original_bear[1]), original_bear,
                        original_bear[length(original_bear)] + (original_bear[length(original_bear)] - original_bear[length(original_bear)-1]) )
    # expected_bear should be: c(0, 10, 20, 30, 40)
    
    # Expected matrix extrapolation computed row-wise:
    # For left side: new column = first column - (second column - first column)
    left_extrapolated <- original_matrix[,1] - (original_matrix[,2] - original_matrix[,1])
    # For right side: new column = last column + (last column - second last column)
    right_extrapolated <- original_matrix[,ncol(original_matrix)] + (original_matrix[,ncol(original_matrix)] - original_matrix[,ncol(original_matrix)-1])
    expected_matrix <- cbind(left_extrapolated, original_matrix, right_extrapolated)
    colnames(expected_matrix) <- as.character(expected_bear)
    
    local({
      seasonder_getSeaSondeRAPM_BEAR <- function(seasonde_apm_obj) {
        attr(seasonde_apm_obj, "BEAR")
      }
      seasonder_setSeaSondeRAPM_BEAR <- function(seasonder_apm_object, new_BEAR) {
        attr(seasonder_apm_object, "BEAR") <- new_BEAR
        if (!is.null(seasonder_apm_object$SeaSondeRAPM))
          colnames(seasonder_apm_object$SeaSondeRAPM) <- as.character(new_BEAR)
        seasonder_apm_object
      }
      testthat::local_mocked_bindings(list(
        seasonder_getSeaSondeRAPM_BEAR = seasonder_getSeaSondeRAPM_BEAR,
        seasonder_setSeaSondeRAPM_BEAR = seasonder_setSeaSondeRAPM_BEAR
      ))
      
      new_obj <- seasonder_extrapolateAPM(seasonder_obj, n = 1)
      
      # Check that the number of columns increased by 2 (one each side)
      expect_equal(ncol(new_obj$SeaSondeRAPM), ncol(original_matrix) + 2,
                   info = "For n=1, the matrix should have two additional columns.")
      
      # Check that the BEAR attribute matches the expected extrapolated values
      expect_equal(attr(new_obj, "BEAR"), expected_bear,
                   info = "The BEAR attribute should be extrapolated correctly for n=1.")
      
      # Check that the column names of the matrix match the new BEAR values
      expect_equal(colnames(new_obj$SeaSondeRAPM), as.character(expected_bear),
                   info = "Column names should match the extrapolated BEAR values for n=1.")
      
      # Check that the matrix has been extrapolated correctly by comparing its values
      expect_equal(new_obj$SeaSondeRAPM, expected_matrix,
                   info = "The SeaSondeRAPM matrix should be extrapolated linearly for n=1.")
    })
  })
  
  # Test with n = 2 (extrapolation of two columns at each end)
  it("should extrapolate the SeaSondeRAPM matrix and BEAR attribute correctly for n = 2", {
    # Create dummy object: same 2x3 matrix
    original_matrix <- matrix(c(1, 10,
                                  2, 20,
                                  3, 30), nrow = 2, byrow = FALSE)
    original_bear <- c(10, 20, 30)
    colnames(original_matrix) <- as.character(original_bear)
    seasonder_obj <- list(SeaSondeRAPM = original_matrix)
    attr(seasonder_obj, "BEAR") <- original_bear
    
    # For n = 2, extrapolate two columns on each side.
    # Using the differences computed from the original BEAR vector (assumed constant):
    diff_left <- original_bear[2] - original_bear[1]  # 10
    diff_right <- original_bear[length(original_bear)] - original_bear[length(original_bear)-1]  # 10
    new_left_bear <- original_bear[1] - diff_left * (2:1)  # yields c(10-20, 10-10) = c(-10,0)
    new_right_bear <- original_bear[length(original_bear)] + diff_right * (1:2)  # yields c(30+10, 30+20) = c(40,50)
    expected_bear <- c(new_left_bear, original_bear, new_right_bear)
    # expected_bear should be: c(-10, 0, 10, 20, 30, 40, 50)
    
    # Expected matrix extrapolation computed row-wise:
    # For the left side, extrapolate recursively:
    left_extrapolated <- cbind(
      original_matrix[,1] - 2 * (original_matrix[,2] - original_matrix[,1]),
      original_matrix[,1] - (original_matrix[,2] - original_matrix[,1])
    )
    # For the right side:
    right_extrapolated <- cbind(
      original_matrix[,ncol(original_matrix)] + (original_matrix[,ncol(original_matrix)] - original_matrix[,ncol(original_matrix)-1]),
      original_matrix[,ncol(original_matrix)] + 2 * (original_matrix[,ncol(original_matrix)] - original_matrix[,ncol(original_matrix)-1])
    )
    expected_matrix <- cbind(left_extrapolated, original_matrix, right_extrapolated)
    colnames(expected_matrix) <- as.character(expected_bear)
    
    local({
      seasonder_getSeaSondeRAPM_BEAR <- function(seasonde_apm_obj) {
        attr(seasonde_apm_obj, "BEAR")
      }
      seasonder_setSeaSondeRAPM_BEAR <- function(seasonder_apm_object, new_BEAR) {
        attr(seasonder_apm_object, "BEAR") <- new_BEAR
        if (!is.null(seasonder_apm_object$SeaSondeRAPM))
          colnames(seasonder_apm_object$SeaSondeRAPM) <- as.character(new_BEAR)
        seasonder_apm_object
      }
      testthat::local_mocked_bindings(list(
        seasonder_getSeaSondeRAPM_BEAR = seasonder_getSeaSondeRAPM_BEAR,
        seasonder_setSeaSondeRAPM_BEAR = seasonder_setSeaSondeRAPM_BEAR
      ))
      
      new_obj <- seasonder_extrapolateAPM(seasonder_obj, n = 2)
      
      # Check that the number of columns increased by 4 (2 on each side)
      expect_equal(ncol(new_obj$SeaSondeRAPM), ncol(original_matrix) + 4,
                   info = "For n=2, the matrix should have four additional columns.")
      
      # Check that the BEAR attribute matches the expected extrapolated values
      expect_equal(attr(new_obj, "BEAR"), expected_bear,
                   info = "The BEAR attribute should be extrapolated correctly for n=2.")
      
      # Check that the column names of the matrix match the extrapolated BEAR values
      expect_equal(colnames(new_obj$SeaSondeRAPM), as.character(expected_bear),
                   info = "Column names should match the extrapolated BEAR values for n=2.")
      
      # Check that the matrix values are extrapolated linearly
      expect_equal(new_obj$SeaSondeRAPM, expected_matrix,
                   info = "The SeaSondeRAPM matrix should be extrapolated linearly for n=2.")
    })
  })

})

