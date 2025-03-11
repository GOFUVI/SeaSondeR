library(testthat)

context('Tests for seasonder_exportRadialMetrics')

# 1. Caso de solución single

test_that('Single solution returns one row with correct values', {
  # Override the seasonder_getSeaSondeRCS_MUSIC function for testing
  seasonder_getSeaSondeRCS_MUSIC <- function(...) {
    music <- data.frame(retained_solution = "single",
                        radial_v = -20,
                        range = 3,
                        doppler_bin = 100,
                        eigen_values_ratio = 30,
                        signal_power_ratio = 2,
                        diag_off_diag_power_ratio = 1,
                        stringsAsFactors = FALSE)
    music$lonlat <- list(data.frame(lon = -122.75, lat = 37.93))
    music$eigen <- list(list(values = c(1e-10, 2e-10, 3e-10)))
    music$DOA_solutions <- list(list(single = list(bearing = 107, P = 1e-9)))
    return(music)
  }
  
  result <- seasonder_exportRadialMetrics()

  expect_equal(nrow(result), 1)
  expect_equal(result$MSEL, 1)
  expect_equal(result$LOND, -122.75)
  expect_equal(result$LATD, 37.93)
  expect_equal(result$VELU, -20)
  expect_equal(result$RNGE, 3)
  expect_equal(result$SPDC, 100)
  
  # For single, MSA1 and BEAR should equal the single solution bearing
  expect_equal(result$MSA1, 107)
  expect_equal(result$BEAR, 107)

  # MSP1 should be 10*log10(Mod(P)); here P=1e-9, so expect -90 dB
  expect_equal(result$MSP1, 10 * log10(1e-9))

  # Additional MUSIC columns
  expect_equal(result$MEGR, 30)
  expect_equal(result$MPKR, 2)
  expect_equal(result$MOFR, 1)
  expect_equal(result$MEI1, 1e-10)
  expect_equal(result$MEI2, 2e-10)
  expect_equal(result$MEI3, 3e-10)
})

# 2. Caso de solución dual

test_that('Dual solution returns two rows with correct values', {
  seasonder_getSeaSondeRCS_MUSIC <- function(...) {
    music <- data.frame(retained_solution = "dual",
                        radial_v = 10,
                        range = 4,
                        doppler_bin = 101,
                        eigen_values_ratio = 40,
                        signal_power_ratio = 3,
                        diag_off_diag_power_ratio = 2,
                        stringsAsFactors = FALSE)
    music$lonlat <- list(data.frame(lon = -100, lat = 40))
    music$eigen <- list(list(values = c(1e-8, 2e-8, 3e-8)))
    music$DOA_solutions <- list(list(dual = list(bearing = c(113, 128), P = c(1e-8, 1e-10))))
    return(music)
  }
  
  result <- seasonder_exportRadialMetrics()

  expect_equal(nrow(result), 2)
  
  # First dual solution row
  row1 <- result[1, ]
  expect_equal(row1$MSEL, 2)
  expect_equal(row1$MDA1, 113)
  expect_equal(row1$BEAR, 113)
  expect_equal(row1$MDP1, 10 * log10(1e-8))
  
  # Second dual solution row
  row2 <- result[2, ]
  expect_equal(row2$MSEL, 3)
  expect_equal(row2$MDA2, 128)
  expect_equal(row2$BEAR, 128)
  expect_equal(row2$MDP2, 10 * log10(1e-10))
  
  # Verify that common fields (location and others) are consistent across rows
  for (col in c('LOND', 'LATD', 'VELU', 'RNGE', 'SPDC', 'MEGR', 'MPKR', 'MOFR', 'MEI1', 'MEI2', 'MEI3')) {
    expect_equal(result[[col]][1], result[[col]][2])
  }
})

# 3. Caso con ambas soluciones (single y dual) en una misma fila

test_that('Both single and dual solutions in one MUSIC row return three rows', {
  seasonder_getSeaSondeRCS_MUSIC <- function(...) {
    music <- data.frame(retained_solution = "both",
                        radial_v = 5,
                        range = 10,
                        doppler_bin = 200,
                        eigen_values_ratio = 50,
                        signal_power_ratio = 5,
                        diag_off_diag_power_ratio = 3,
                        stringsAsFactors = FALSE)
    music$lonlat <- list(data.frame(lon = 0, lat = 0))
    music$eigen <- list(list(values = c(1e-5, 2e-5, 3e-5)))
    music$DOA_solutions <- list(
      list(single = list(bearing = 50, P = 1e-6),
           dual = list(bearing = c(60, 70), P = c(2e-6, 3e-6)))
    )
    return(music)
  }
  
  result <- seasonder_exportRadialMetrics()
  expect_equal(nrow(result), 3)
  
  # Single solution row
  single_row <- result[result$MSEL == 1, ]
  expect_equal(nrow(single_row), 1)
  expect_equal(single_row$MSA1, 50)
  expect_equal(single_row$BEAR, 50)
  expect_equal(single_row$MSP1, 10 * log10(1e-6))
  
  # Dual solution rows
  dual_rows <- result[result$MSEL %in% c(2, 3), ]
  expect_equal(nrow(dual_rows), 2)
  
  # First dual solution
  row_dual1 <- dual_rows[dual_rows$MSEL == 2, ]
  expect_equal(row_dual1$MDA1, 60)
  expect_equal(row_dual1$BEAR, 60)
  expect_equal(row_dual1$MDP1, 10 * log10(2e-6))
  
  # Second dual solution
  row_dual2 <- dual_rows[dual_rows$MSEL == 3, ]
  expect_equal(row_dual2$MDA2, 70)
  expect_equal(row_dual2$BEAR, 70)
  expect_equal(row_dual2$MDP2, 10 * log10(3e-6))
  
  # Verify common fields
  expect_equal(single_row$LOND, 0)
  expect_equal(single_row$LATD, 0)
})

# 4. Caso con información faltante

test_that('Missing DOA_solutions or missing P results in NA fields', {
  seasonder_getSeaSondeRCS_MUSIC <- function(...) {
    # Create a MUSIC table with two rows
    music <- data.frame(retained_solution = c("single", "dual"),
                        radial_v = c(15, 20),
                        range = c(7, 8),
                        doppler_bin = c(150, 151),
                        eigen_values_ratio = c(60, 70),
                        signal_power_ratio = c(4, 5),
                        diag_off_diag_power_ratio = c(2, 3),
                        stringsAsFactors = FALSE)
    music$lonlat <- list(data.frame(lon = -50, lat = 30), data.frame(lon = -51, lat = 31))
    music$eigen <- list(list(values = c(1e-7, 2e-7, 3e-7)), list(values = c(1e-7, 2e-7, 3e-7)))
    # First row: retained_solution is single but missing DOA_solutions$single
    # Second row: dual with DOA_solutions present but P is NULL
    music$DOA_solutions <- list(list(single = NULL), list(dual = list(bearing = c(80, 90), P = NULL)))
    return(music)
  }
  
  result <- seasonder_exportRadialMetrics()
  
  # For the first row (single missing DOA_solutions), expect no output row
  # Since the function does not add a row when the solution data is missing, we expect only the dual rows
  expect_equal(nrow(result), 2)
  
  dual_rows <- result[result$MSEL %in% c(2,3), ]
  expect_true(is.na(dual_rows$MDP1[dual_rows$MSEL == 2]))
  expect_true(is.na(dual_rows$MDP2[dual_rows$MSEL == 3]))
})

