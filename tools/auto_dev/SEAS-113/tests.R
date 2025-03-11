#### Unit testing agent runs ####


##### Run #####


#Acción a realizar: write_new_tests.
#Detalles: Se debe crear desde cero los tests para la función seasonder_exportRadialMetrics. Los tests deben simular la llamada a seasonder_getSeaSondeRCS_MUSIC generando datos de entrada representativos (por ejemplo, con casos de soluciones 'single' y 'dual', valores de rangos, doppler, etc.) y verificar que la función retorne una tabla con la estructura y columnas especificadas en la definición de la tabla RDM1 (34 columnas, incluyendo LOND, LATD, VELU, VELV, VFLG, RNGE, BEAR, VELO, HEAD, SPRC, SPDC, MSEL, MSA1, MDA1, MDA2, MEGR, MPKR, MOFR, MSP1, MDP1, MDP2, MSW1, MDW1, MDW2, MSR1, MDR1, MDR2, MA1S, MA2S, MA3S, MEI1, MEI2, MEI3, MDRJ). Además, los tests deben evaluar el correcto procesamiento de los datos provenientes de la tabla MUSIC, la transformación de los resultados (incluyendo el manejo de indicadores y flags) y la duplicación de filas en caso de solución dual. Se recomienda utilizar un framework de testing en R, como 'testthat', para estructurar los casos de prueba.
#
# First, check that the function exists
testthat::test_that("Function seasonder_exportRadialMetrics exists", {
  testthat::expect_true(exists("seasonder_exportRadialMetrics", mode = "function"),
                          info = "The function seasonder_exportRadialMetrics should be defined")
})

# Now, use describe() and it() to group tests regarding the transformation from MUSIC to RDM1

testthat::describe("seasonder_exportRadialMetrics", {
  
  # Test processing a mocked MUSIC table containing both 'single' and 'dual' solutions
  testthat::it("returns a data.frame/tibble with correct structure, row duplication, and processed values", {
    # Create a mocked MUSIC table with 2 rows:
    # - First row: 'single' solution
    # - Second row: 'dual' solution (should result in 2 output rows)
    
    music_mock <- data.frame(
      range_cell = c(1, 2),
      doppler_bin = c(300, 305),
      range = c(2.5, 3.1),
      freq = c(-0.5, -0.6),
      radial_v = c(-0.7, -0.8),
      cov = I(list(matrix(1+0i, 3, 3), matrix(2+0i, 3, 3))),
      eigen = I(list(
        list(values = c(1, 0, 0), vectors = matrix(0+0i, 3, 3)),
        list(values = c(2, 0, 0), vectors = matrix(0+0i, 3, 3))
      )),
      projections = I(list(
        structure(matrix(0+0i, 2, 5), dimnames = list(c("single", "dual"), NULL)),
        structure(matrix(0+0i, 2, 5), dimnames = list(c("single", "dual"), NULL))
      )),
      DOA_solutions = I(list(
        # For the first MUSIC row: single solution
        list(single = list(bearing = 314, a = matrix(0+0i, 3, 1), P = 1+0i, peak_resp = 134),
             dual = NULL),
        # For the second MUSIC row: dual solution with two bearings
        list(single = NULL,
             dual = list(
               bearing = c(260, 198), 
               a = matrix(0+0i, 3, 2), 
               P = matrix(c(1+0i, 2+0i), nrow = 2),
               peak_resp = c(80, 18)
             ))
      )),
      eigen_values_ratio = c(5.5, 2.3),
      P1_check = c(TRUE, TRUE),
      signal_power_ratio = c(1.1, 1.6),
      P2_check = c(TRUE, TRUE),
      diag_off_diag_power_ratio = c(1.0, 2.0),
      P3_check = c(FALSE, TRUE),
      retained_solution = c("single", "dual"),
      DOA = I(list(
        list(bearing = 314, a = matrix(0+0i, 3, 1), P = 1+0i, peak_resp = 134),
        list(bearing = c(102, 8), a = matrix(0+0i, 3, 2), P = matrix(c(0.5+0i, 0.6+0i), nrow = 2), peak_resp = c(26, 11.2))
      )),
      lonlat = I(list(
        data.frame(lon = -122.75, lat = 37.93),
        data.frame(lon = c(-122.76, -122.70), lat = c(37.91, 37.88))
      ))
    )
    
    # Use with_mocked_bindings to simulate seasonder_getSeaSondeRCS_MUSIC
    result <- testthat::with_mocked_bindings(
      seasonder_getSeaSondeRCS_MUSIC = function(...) { music_mock },
      seasonder_exportRadialMetrics()
    )
    
    # Check that the output is a data.frame or tibble
    testthat::expect_true(is.data.frame(result), info = "Output should be a data.frame or tibble")
    
    # Define expected 34 columns (as specified in RDM1 table)
    expected_columns <- c("LOND", "LATD", "VELU", "VELV", "VFLG", "RNGE", "BEAR", "VELO", "HEAD",
                          "SPRC", "SPDC", "MSEL", "MSA1", "MDA1", "MDA2", "MEGR", "MPKR", "MOFR",
                          "MSP1", "MDP1", "MDP2", "MSW1", "MDW1", "MDW2", "MSR1", "MDR1", "MDR2",
                          "MA1S", "MA2S", "MA3S", "MEI1", "MEI2", "MEI3", "MDRJ")
    
    testthat::expect_equal(ncol(result), length(expected_columns),
                           info = "Output must have 34 columns")
    testthat::expect_equal(colnames(result), expected_columns,
                           info = "Column names in output must match the specification")
    
    # Verify the duplication of rows:
    # - The MUSIC row with retained_solution 'single' (range_cell==1) should yield 1 row (assumed SPRC == 1)
    # - The MUSIC row with retained_solution 'dual' (range_cell==2) should yield 2 rows (assumed SPRC == 2)
    testthat::expect_equal(nrow(result), 3,
                           info = "Output should contain 3 rows (1 from single solution and 2 from dual solution)")
    
    # Validate specific transformation for the single solution row
    single_row <- result[result$SPRC == 1, ]
    testthat::expect_equal(single_row$MSEL, 1,
                           info = "For a single solution, MSEL should be 1")
    testthat::expect_equal(single_row$MSA1, 314,
                           info = "For single solution, MSA1 should match the bearing value 314")
    
    # Validate transformation for dual solution rows
    dual_rows <- result[result$SPRC == 2, ]
    testthat::expect_true(all(sort(dual_rows$MSEL) == c(2, 3)),
                           info = "For dual solutions, MSEL should be 2 and 3 for the two rows")
    
    # For dual solution, assume:
    #  - Row with MSEL == 2 should have MDA1 equal to the first dual bearing (260)
    #  - Row with MSEL == 3 should have MDA2 equal to the second dual bearing (198)
    row_MSEL2 <- dual_rows[dual_rows$MSEL == 2, ]
    row_MSEL3 <- dual_rows[dual_rows$MSEL == 3, ]
    testthat::expect_equal(row_MSEL2$MDA1, 260,
                           info = "Dual solution row with MSEL 2 should have MDA1 equal to 260")
    testthat::expect_equal(row_MSEL3$MDA2, 198,
                           info = "Dual solution row with MSEL 3 should have MDA2 equal to 198")
  })
  
  # Test that an empty MUSIC table yields an empty RDM1 table with the correct columns
  testthat::it("returns an empty table with correct structure when MUSIC input is empty", {
    empty_music <- data.frame(
      range_cell = integer(0),
      doppler_bin = integer(0),
      range = numeric(0),
      freq = numeric(0),
      radial_v = numeric(0),
      cov = I(list()),
      eigen = I(list()),
      projections = I(list()),
      DOA_solutions = I(list()),
      eigen_values_ratio = numeric(0),
      P1_check = logical(0),
      signal_power_ratio = numeric(0),
      P2_check = logical(0),
      diag_off_diag_power_ratio = numeric(0),
      P3_check = logical(0),
      retained_solution = character(0),
      DOA = I(list()),
      lonlat = I(list())
    )
    
    result <- testthat::with_mocked_bindings(
      seasonder_getSeaSondeRCS_MUSIC = function(...) { empty_music },
      seasonder_exportRadialMetrics()
    )
    
    expected_columns <- c("LOND", "LATD", "VELU", "VELV", "VFLG", "RNGE", "BEAR", "VELO", "HEAD",
                          "SPRC", "SPDC", "MSEL", "MSA1", "MDA1", "MDA2", "MEGR", "MPKR", "MOFR",
                          "MSP1", "MDP1", "MDP2", "MSW1", "MDW1", "MDW2", "MSR1", "MDR1", "MDR2",
                          "MA1S", "MA2S", "MA3S", "MEI1", "MEI2", "MEI3", "MDRJ")
    
    testthat::expect_true(is.data.frame(result),
                           info = "Output should be a data.frame or tibble even if the input is empty")
    testthat::expect_equal(nrow(result), 0,
                           info = "Output should have zero rows when MUSIC input is empty")
    testthat::expect_equal(ncol(result), length(expected_columns),
                           info = "Output must have 34 columns even if empty")
    testthat::expect_equal(colnames(result), expected_columns,
                           info = "Column names must match the specification even when MUSIC input is empty")
  })
  
})

