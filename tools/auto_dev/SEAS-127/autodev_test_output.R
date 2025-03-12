# Test for the existence of seasonder_extrapolateAPM function
library(testthat)

# Verifica que la función esté definida

test_that("Function existence", {
  expect_true(exists("seasonder_extrapolateAPM", mode = "function"),
              info = "seasonder_extrapolateAPM debe estar definida como función.")
})


describe("seasonder_extrapolateAPM", {

  # Caso n = 0: Sin extrapolación
  it("debe mantener el objeto sin cambios cuando n = 0", {
    # Crear un objeto dummy con una matriz 2x3
    original_matrix <- matrix(c(1, 10,
                                  2, 20,
                                  3, 30), nrow = 2, byrow = FALSE)
    original_bear <- c(10, 20, 30)
    colnames(original_matrix) <- as.character(original_bear)
    seasonder_obj <- list(SeaSondeRAPM = original_matrix)
    attr(seasonder_obj, 'BEAR') <- original_bear

    # Simular los getters y setters
    local({
      seasonder_getSeaSondeRAPM_BEAR <- function(seasonde_apm_obj) {
        attr(seasonde_apm_obj, 'BEAR')
      }
      seasonder_setSeaSondeRAPM_BEAR <- function(seasonder_apm_object, new_BEAR) {
        attr(seasonder_apm_object, 'BEAR') <- new_BEAR
        if (!is.null(seasonder_apm_object$SeaSondeRAPM))
          colnames(seasonder_apm_object$SeaSondeRAPM) <- as.character(new_BEAR)
        seasonder_apm_object
      }
      testthat::local_mocked_bindings(list(
        seasonder_getSeaSondeRAPM_BEAR = seasonder_getSeaSondeRAPM_BEAR,
        seasonder_setSeaSondeRAPM_BEAR = seasonder_setSeaSondeRAPM_BEAR
      ))

      new_obj <- seasonder_extrapolateAPM(seasonder_obj, n = 0)
      
      # La matriz, BEAR y los nombres de las columnas deben permanecer sin cambios
      expect_equal(new_obj$SeaSondeRAPM, original_matrix,
                   info = "Con n=0, la matriz SeaSondeRAPM no debe modificarse.")
      expect_equal(attr(new_obj, 'BEAR'), original_bear,
                   info = "Con n=0, el atributo BEAR debe permanecer sin cambios.")
      expect_equal(colnames(new_obj$SeaSondeRAPM), as.character(original_bear),
                   info = "Los nombres de columnas deben coincidir con BEAR cuando n=0.")
    })
  })
  
  # Caso n = 1: Extrapolación de 1 columna en cada extremo
  it("debe extrapolar correctamente la matriz y BEAR para n = 1", {
    original_matrix <- matrix(c(1, 10,
                                  2, 20,
                                  3, 30), nrow = 2, byrow = FALSE)
    original_bear <- c(10, 20, 30)
    colnames(original_matrix) <- as.character(original_bear)
    seasonder_obj <- list(SeaSondeRAPM = original_matrix)
    attr(seasonder_obj, 'BEAR') <- original_bear

    # Calcular las diferencias
    diff_left <- original_bear[2] - original_bear[1]
    diff_right <- original_bear[length(original_bear)] - original_bear[length(original_bear)-1]

    # Nuevos bearings: uno a cada lado
    expected_bear <- c(original_bear[1] - diff_left, original_bear, original_bear[length(original_bear)] + diff_right)
    
    # Calcular la extrapolación en la matriz
    # Columna izquierda: primer valor de cada fila extrapolado linealmente
    left_col <- original_matrix[,1] - (original_matrix[,2] - original_matrix[,1])
    # Columna derecha: último valor de cada fila extrapolado linealmente
    right_col <- original_matrix[,ncol(original_matrix)] + (original_matrix[,ncol(original_matrix)] - original_matrix[,ncol(original_matrix)-1])
    expected_matrix <- cbind(left_col, original_matrix, right_col)
    colnames(expected_matrix) <- as.character(expected_bear)

    local({
      seasonder_getSeaSondeRAPM_BEAR <- function(seasonde_apm_obj) {
        attr(seasonde_apm_obj, 'BEAR')
      }
      seasonder_setSeaSondeRAPM_BEAR <- function(seasonder_apm_object, new_BEAR) {
        attr(seasonder_apm_object, 'BEAR') <- new_BEAR
        if (!is.null(seasonder_apm_object$SeaSondeRAPM))
          colnames(seasonder_apm_object$SeaSondeRAPM) <- as.character(new_BEAR)
        seasonder_apm_object
      }
      testthat::local_mocked_bindings(list(
        seasonder_getSeaSondeRAPM_BEAR = seasonder_getSeaSondeRAPM_BEAR,
        seasonder_setSeaSondeRAPM_BEAR = seasonder_setSeaSondeRAPM_BEAR
      ))

      new_obj <- seasonder_extrapolateAPM(seasonder_obj, n = 1)
      
      expect_equal(ncol(new_obj$SeaSondeRAPM), ncol(original_matrix) + 2,
                   info = "Para n=1, la matriz debe tener 2 columnas adicionales (una por cada lado).")
      expect_equal(attr(new_obj, 'BEAR'), expected_bear,
                   info = "El vector BEAR debe ser extrapolado correctamente para n=1.")
      expect_equal(colnames(new_obj$SeaSondeRAPM), as.character(expected_bear),
                   info = "Los nombres de las columnas deben actualizarse acorde a los nuevos BEAR para n=1.")
      expect_equal(new_obj$SeaSondeRAPM, expected_matrix,
                   info = "La extrapolación lineal de la matriz SeaSondeRAPM debe ser correcta para n=1.")
    })
  })
  
  # Caso n = 2: Extrapolación de 2 columnas en cada extremo
  it("debe extrapolar correctamente la matriz y BEAR para n = 2", {
    original_matrix <- matrix(c(1, 10,
                                  2, 20,
                                  3, 30), nrow = 2, byrow = FALSE)
    original_bear <- c(10, 20, 30)
    colnames(original_matrix) <- as.character(original_bear)
    seasonder_obj <- list(SeaSondeRAPM = original_matrix)
    attr(seasonder_obj, 'BEAR') <- original_bear

    # Para n = 2, extrapolar dos columnas en cada lado usando la diferencia lineal
    diff_left <- original_bear[2] - original_bear[1]
    diff_right <- original_bear[length(original_bear)] - original_bear[length(original_bear)-1]

    # Generar los nuevos valores BEAR
    new_left <- sapply(2:1, function(i) original_bear[1] - diff_left * i)
    new_right <- sapply(1:2, function(i) original_bear[length(original_bear)] + diff_right * i)
    expected_bear <- c(new_left, original_bear, new_right)

    # Extrapolar la matriz, columna por columna
    left_col1 <- original_matrix[,1] - 2 * (original_matrix[,2] - original_matrix[,1])
    left_col2 <- original_matrix[,1] - (original_matrix[,2] - original_matrix[,1])
    right_col1 <- original_matrix[,ncol(original_matrix)] + (original_matrix[,ncol(original_matrix)] - original_matrix[,ncol(original_matrix)-1])
    right_col2 <- original_matrix[,ncol(original_matrix)] + 2 * (original_matrix[,ncol(original_matrix)] - original_matrix[,ncol(original_matrix)-1])
    expected_matrix <- cbind(left_col1, left_col2, original_matrix, right_col1, right_col2)
    colnames(expected_matrix) <- as.character(expected_bear)

    local({
      seasonder_getSeaSondeRAPM_BEAR <- function(seasonde_apm_obj) {
        attr(seasonde_apm_obj, 'BEAR')
      }
      seasonder_setSeaSondeRAPM_BEAR <- function(seasonder_apm_object, new_BEAR) {
        attr(seasonder_apm_object, 'BEAR') <- new_BEAR
        if (!is.null(seasonder_apm_object$SeaSondeRAPM))
          colnames(seasonder_apm_object$SeaSondeRAPM) <- as.character(new_BEAR)
        seasonder_apm_object
      }
      testthat::local_mocked_bindings(list(
        seasonder_getSeaSondeRAPM_BEAR = seasonder_getSeaSondeRAPM_BEAR,
        seasonder_setSeaSondeRAPM_BEAR = seasonder_setSeaSondeRAPM_BEAR
      ))

      new_obj <- seasonder_extrapolateAPM(seasonder_obj, n = 2)
      
      expect_equal(ncol(new_obj$SeaSondeRAPM), ncol(original_matrix) + 4,
                   info = "Para n=2, la matriz debe tener 4 columnas adicionales (dos por cada lado).")
      expect_equal(attr(new_obj, 'BEAR'), expected_bear,
                   info = "El vector BEAR debe ser extrapolado correctamente para n=2.")
      expect_equal(colnames(new_obj$SeaSondeRAPM), as.character(expected_bear),
                   info = "Los nombres de las columnas deben actualizarse acorde a los nuevos BEAR para n=2.")
      expect_equal(new_obj$SeaSondeRAPM, expected_matrix,
                   info = "La extrapolación lineal de la matriz SeaSondeRAPM debe ser correcta para n=2.")
    })
  })

})

