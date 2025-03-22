#### Coding agent runs ####

# If local_redefs is not defined in the environment, define a simple version to override functions in the given environment.
if (!exists("local_redefs", mode = "function")) {
  local_redefs <- function(..., .env = globalenv()) {
    new_defs <- list(...)
    old_defs <- lapply(names(new_defs), function(nm) {
      if (exists(nm, envir = .env, inherits = FALSE)) get(nm, envir = .env, inherits = FALSE) else NULL
    })
    names(old_defs) <- names(new_defs)
    on.exit({
      for (nm in names(old_defs)) {
        if (!is.null(old_defs[[nm]])) assign(nm, old_defs[[nm]], envir = .env)
      }
    }, add = TRUE)
    for (nm in names(new_defs)) {
      assign(nm, new_defs[[nm]], envir = .env)
    }
  }
}

# Dummy definition for seasonder_getSeaSondeRCS_MUSIC in case it doesn't exist
if (!exists("seasonder_getSeaSondeRCS_MUSIC", mode = "function")) {
  seasonder_getSeaSondeRCS_MUSIC <- function(...) { stop("Not implemented") }
}

library(testthat)

# First, check that the required functions exist

test_that('Function existence check', {
  expect_true(exists('seasonder_exportRadialMetrics', mode = 'function'),
              info = 'seasonder_exportRadialMetrics should exist.')
  expect_true(exists('seasonder_getSeaSondeRCS_MUSIC', mode = 'function'),
              info = 'seasonder_getSeaSondeRCS_MUSIC should exist.')
})

# Tests for seasonder_exportRadialMetrics

describe('seasonder_exportRadialMetrics', {

  it('handles single solution correctly', {
    # Create a fake MUSIC table with a single solution
    music <- data.frame(
      radial_v = 10,
      range = 5,
      range_cell = 1,
      doppler_bin = 300,
      eigen_values_ratio = 0.5,
      signal_power_ratio = 0.8,
      diag_off_diag_power_ratio = 1.2,
      retained_solution = 'single',
      stringsAsFactors = FALSE
    )
    # Simulated location information
    music$lonlat <- list(data.frame(lon = 100, lat = 50))
    # Simulated DOA single solution (bearing of interest)
    music$DOA_solutions <- list(list(single = list(bearing = 123)))
    
    # Override seasonder_getSeaSondeRCS_MUSIC using local_redefs
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 1, info = 'Single solution should produce one row')
    expect_equal(res$MSEL, 1, info = 'MSEL should be 1 for single solution')
    expect_equal(res$MSA1, 123, info = 'MSA1 should be set to the single solution bearing')
    expect_equal(res$BEAR, 123, info = 'BEAR should be set to the single solution bearing')
    expect_equal(res$SPDC, 300, info = 'SPDC should be assigned from doppler_bin')
    expect_equal(res$MEGR, 0.5, info = 'MEGR should be assigned correctly')
    expect_equal(res$MPKR, 0.8, info = 'MPKR should be assigned correctly')
    expect_equal(res$MOFR, 1.2, info = 'MOFR should be assigned correctly')
    expect_equal(res$LOND, 100, info = 'LOND should be assigned from lonlat')
    expect_equal(res$LATD, 50, info = 'LATD should be assigned from lonlat')
    expect_equal(res$VELU, 10, info = 'VELU should be assigned from radial_v')
    expect_equal(res$VELV, 10, info = 'VELV should be assigned from radial_v')
  })

  it('handles dual solution correctly', {
    # Create a fake MUSIC table with a dual solution only
    music <- data.frame(
      radial_v = 20,
      range = 10,
      range_cell = 2,
      doppler_bin = 400,
      eigen_values_ratio = 0.55,
      signal_power_ratio = 0.85,
      diag_off_diag_power_ratio = 1.25,
      retained_solution = 'dual',
      stringsAsFactors = FALSE
    )
    music$lonlat <- list(data.frame(lon = 110, lat = 60))
    # Simulated dual solution with two bearings
    music$DOA_solutions <- list(list(dual = list(bearing = c(111, 222))))
    
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 2, info = 'Dual solution should produce two rows')
    res_sorted <- res[order(res$MSEL), ]
    expect_equal(res_sorted$MSEL[1], 2, info = 'First dual row should have MSEL equal to 2')
    expect_equal(res_sorted$MDA1[1], 111, info = 'MDA1 should be set to first dual bearing')
    expect_equal(res_sorted$BEAR[1], 111, info = 'BEAR should be set to first dual bearing')
    expect_equal(res_sorted$MSEL[2], 3, info = 'Second dual row should have MSEL equal to 3')
    expect_equal(res_sorted$MDA2[2], 222, info = 'MDA2 should be set to second dual bearing')
    expect_equal(res_sorted$BEAR[2], 222, info = 'BEAR should be set to second dual bearing')
    expect_equal(res_sorted$SPDC, rep(400, 2), info = 'SPDC should be assigned from doppler_bin')
    expect_equal(res_sorted$MEGR, rep(0.55, 2), info = 'MEGR should be assigned correctly')
    expect_equal(res_sorted$MPKR, rep(0.85, 2), info = 'MPKR should be assigned correctly')
    expect_equal(res_sorted$MOFR, rep(1.25, 2), info = 'MOFR should be assigned correctly')
    expect_equal(res_sorted$LOND, rep(110, 2), info = 'LOND should be assigned from lonlat')
    expect_equal(res_sorted$LATD, rep(60, 2), info = 'LATD should be assigned from lonlat')
    expect_equal(res_sorted$VELU, rep(20, 2), info = 'VELU should be assigned from radial_v')
    expect_equal(res_sorted$VELV, rep(20, 2), info = 'VELV should be assigned from radial_v')
  })

  it('handles both single and dual solutions together', {
    # Create a fake MUSIC table where both single and dual solutions are provided
    music <- data.frame(
      radial_v = 12,
      range = 6,
      range_cell = 4,
      doppler_bin = 320,
      eigen_values_ratio = 0.75,
      signal_power_ratio = 1.05,
      diag_off_diag_power_ratio = 1.45,
      retained_solution = 'dual',
      stringsAsFactors = FALSE
    )
    music$lonlat <- list(data.frame(lon = 120, lat = 70))
    music$DOA_solutions <- list(list(single = list(bearing = 333), dual = list(bearing = c(444, 555))))
    
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 3, info = 'Both single and dual solutions should produce three rows')
    single_row <- res[res$MSEL == 1, ]
    dual_row1 <- res[res$MSEL == 2, ]
    dual_row2 <- res[res$MSEL == 3, ]
    expect_equal(nrow(single_row), 1, info = 'There should be one single solution row')
    expect_equal(nrow(dual_row1), 1, info = 'There should be one dual solution row with MSEL 2')
    expect_equal(nrow(dual_row2), 1, info = 'There should be one dual solution row with MSEL 3')
    expect_equal(single_row$MSA1, 333, info = 'Single solution row: MSA1 should match single bearing')
    expect_equal(single_row$BEAR, 333, info = 'Single solution row: BEAR should match single bearing')
    expect_equal(dual_row1$MDA1, 444, info = 'Dual solution row (MSEL 2): MDA1 should match first dual bearing')
    expect_equal(dual_row1$BEAR, 444, info = 'Dual solution row (MSEL 2): BEAR should match first dual bearing')
    expect_equal(dual_row2$MDA2, 555, info = 'Dual solution row (MSEL 3): MDA2 should match second dual bearing')
    expect_equal(dual_row2$BEAR, 555, info = 'Dual solution row (MSEL 3): BEAR should match second dual bearing')
    for (col in c('SPDC', 'MEGR', 'MPKR', 'MOFR', 'LOND', 'LATD', 'VELU', 'VELV')) {
      expect_true(all(res[[col]] == music[[col]][1]),
                  info = paste('Column', col, 'should be consistently assigned'))
    }
    expected_cols <- c('LOND', 'LATD', 'VELU', 'VELV', 'VFLG', 'RNGE', 'BEAR', 'VELO', 'HEAD',
                       'SPRC', 'SPDC', 'MSEL', 'MSA1', 'MDA1', 'MDA2', 'MEGR', 'MPKR', 'MOFR',
                       'MSP1', 'MDP1', 'MDP2', 'MSW1', 'MDW1', 'MDW2', 'MSR1', 'MDR1', 'MDR2',
                       'MA1S', 'MA2S', 'MA3S', 'MEI1', 'MEI2', 'MEI3', 'MDRJ')
    expect_equal(colnames(res), expected_cols, info = 'Result should have 34 columns with correct names')
  })

  it('returns an empty data frame with 34 columns when MUSIC table is empty', {
    music <- data.frame(
      radial_v = numeric(0),
      range = numeric(0),
      range_cell = integer(0),
      doppler_bin = integer(0),
      eigen_values_ratio = numeric(0),
      signal_power_ratio = numeric(0),
      diag_off_diag_power_ratio = numeric(0),
      retained_solution = character(0),
      stringsAsFactors = FALSE
    )
    music$lonlat <- list()
    music$DOA_solutions <- list()
    
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 0, info = 'Empty MUSIC table should produce a data frame with 0 rows')
    expected_cols <- c('LOND', 'LATD', 'VELU', 'VELV', 'VFLG', 'RNGE', 'BEAR', 'VELO', 'HEAD',
                       'SPRC', 'SPDC', 'MSEL', 'MSA1', 'MDA1', 'MDA2', 'MEGR', 'MPKR', 'MOFR',
                       'MSP1', 'MDP1', 'MDP2', 'MSW1', 'MDW1', 'MDW2', 'MSR1', 'MDR1', 'MDR2',
                       'MA1S', 'MA2S', 'MA3S', 'MEI1', 'MEI2', 'MEI3', 'MDRJ')
    expect_equal(colnames(res), expected_cols, info = 'Empty data frame should have 34 columns with correct names')
  })

})


##### Run #####


#Acción a realizar: write_new_code.
#Detalles: Se ha definido la función local_redefs (si no existe) para sobrescribir funciones en el entorno global. Esta suite de tests redefine localmente la función seasonder_getSeaSondeRCS_MUSIC para simular distintos escenarios y verifica que la función seasonder_exportRadialMetrics produce la salida esperada. Por favor, ejecute esta suite de tests para confirmar que la función cumple con las especificaciones.
#
# If local_redefs is not defined in the environment, define a simple version to override functions in the given environment.
if (!exists("local_redefs", mode = "function")) {
  local_redefs <- function(..., .env = globalenv()) {
    new_defs <- list(...)
    old_defs <- lapply(names(new_defs), function(nm) {
      if (exists(nm, envir = .env, inherits = FALSE)) get(nm, envir = .env, inherits = FALSE) else NULL
    })
    names(old_defs) <- names(new_defs)
    on.exit({
      for (nm in names(old_defs)) {
        if (!is.null(old_defs[[nm]])) assign(nm, old_defs[[nm]], envir = .env)
      }
    }, add = TRUE)
    for (nm in names(new_defs)) {
      assign(nm, new_defs[[nm]], envir = .env)
    }
  }
}

# Dummy definition for seasonder_getSeaSondeRCS_MUSIC in case it doesn't exist
if (!exists("seasonder_getSeaSondeRCS_MUSIC", mode = "function")) {
  seasonder_getSeaSondeRCS_MUSIC <- function(...) { stop("Not implemented") }
}

library(testthat)

# First, check that the required functions exist

test_that('Function existence check', {
  expect_true(exists('seasonder_exportRadialMetrics', mode = 'function'),
              info = 'seasonder_exportRadialMetrics should exist.')
  expect_true(exists('seasonder_getSeaSondeRCS_MUSIC', mode = 'function'),
              info = 'seasonder_getSeaSondeRCS_MUSIC should exist.')
})

# Tests for seasonder_exportRadialMetrics

describe('seasonder_exportRadialMetrics', {

  it('handles single solution correctly', {
    # Create a fake MUSIC table with a single solution
    music <- data.frame(
      radial_v = 10,
      range = 5,
      range_cell = 1,
      doppler_bin = 300,
      eigen_values_ratio = 0.5,
      signal_power_ratio = 0.8,
      diag_off_diag_power_ratio = 1.2,
      retained_solution = 'single',
      stringsAsFactors = FALSE
    )
    # Simulated location information
    music$lonlat <- list(data.frame(lon = 100, lat = 50))
    # Simulated DOA single solution (bearing of interest)
    music$DOA_solutions <- list(list(single = list(bearing = 123)))
    
    # Override seasonder_getSeaSondeRCS_MUSIC using local_redefs
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 1, info = 'Single solution should produce one row')
    expect_equal(res$MSEL, 1, info = 'MSEL should be 1 for single solution')
    expect_equal(res$MSA1, 123, info = 'MSA1 should be set to the single solution bearing')
    expect_equal(res$BEAR, 123, info = 'BEAR should be set to the single solution bearing')
    expect_equal(res$SPDC, 300, info = 'SPDC should be assigned from doppler_bin')
    expect_equal(res$MEGR, 0.5, info = 'MEGR should be assigned correctly')
    expect_equal(res$MPKR, 0.8, info = 'MPKR should be assigned correctly')
    expect_equal(res$MOFR, 1.2, info = 'MOFR should be assigned correctly')
    expect_equal(res$LOND, 100, info = 'LOND should be assigned from lonlat')
    expect_equal(res$LATD, 50, info = 'LATD should be assigned from lonlat')
    expect_equal(res$VELU, 10, info = 'VELU should be assigned from radial_v')
    expect_equal(res$VELV, 10, info = 'VELV should be assigned from radial_v')
  })

  it('handles dual solution correctly', {
    # Create a fake MUSIC table with a dual solution only
    music <- data.frame(
      radial_v = 20,
      range = 10,
      range_cell = 2,
      doppler_bin = 400,
      eigen_values_ratio = 0.55,
      signal_power_ratio = 0.85,
      diag_off_diag_power_ratio = 1.25,
      retained_solution = 'dual',
      stringsAsFactors = FALSE
    )
    music$lonlat <- list(data.frame(lon = 110, lat = 60))
    # Simulated dual solution with two bearings
    music$DOA_solutions <- list(list(dual = list(bearing = c(111, 222))))
    
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 2, info = 'Dual solution should produce two rows')
    res_sorted <- res[order(res$MSEL), ]
    expect_equal(res_sorted$MSEL[1], 2, info = 'First dual row should have MSEL equal to 2')
    expect_equal(res_sorted$MDA1[1], 111, info = 'MDA1 should be set to first dual bearing')
    expect_equal(res_sorted$BEAR[1], 111, info = 'BEAR should be set to first dual bearing')
    expect_equal(res_sorted$MSEL[2], 3, info = 'Second dual row should have MSEL equal to 3')
    expect_equal(res_sorted$MDA2[2], 222, info = 'MDA2 should be set to second dual bearing')
    expect_equal(res_sorted$BEAR[2], 222, info = 'BEAR should be set to second dual bearing')
    expect_equal(res_sorted$SPDC, rep(400, 2), info = 'SPDC should be assigned from doppler_bin')
    expect_equal(res_sorted$MEGR, rep(0.55, 2), info = 'MEGR should be assigned correctly')
    expect_equal(res_sorted$MPKR, rep(0.85, 2), info = 'MPKR should be assigned correctly')
    expect_equal(res_sorted$MOFR, rep(1.25, 2), info = 'MOFR should be assigned correctly')
    expect_equal(res_sorted$LOND, rep(110, 2), info = 'LOND should be assigned from lonlat')
    expect_equal(res_sorted$LATD, rep(60, 2), info = 'LATD should be assigned from lonlat')
    expect_equal(res_sorted$VELU, rep(20, 2), info = 'VELU should be assigned from radial_v')
    expect_equal(res_sorted$VELV, rep(20, 2), info = 'VELV should be assigned from radial_v')
  })

  it('handles both single and dual solutions together', {
    # Create a fake MUSIC table where both single and dual solutions are provided
    music <- data.frame(
      radial_v = 12,
      range = 6,
      range_cell = 4,
      doppler_bin = 320,
      eigen_values_ratio = 0.75,
      signal_power_ratio = 1.05,
      diag_off_diag_power_ratio = 1.45,
      retained_solution = 'dual',
      stringsAsFactors = FALSE
    )
    music$lonlat <- list(data.frame(lon = 120, lat = 70))
    music$DOA_solutions <- list(list(single = list(bearing = 333), dual = list(bearing = c(444, 555))))
    
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 3, info = 'Both single and dual solutions should produce three rows')
    single_row <- res[res$MSEL == 1, ]
    dual_row1 <- res[res$MSEL == 2, ]
    dual_row2 <- res[res$MSEL == 3, ]
    expect_equal(nrow(single_row), 1, info = 'There should be one single solution row')
    expect_equal(nrow(dual_row1), 1, info = 'There should be one dual solution row with MSEL 2')
    expect_equal(nrow(dual_row2), 1, info = 'There should be one dual solution row with MSEL 3')
    expect_equal(single_row$MSA1, 333, info = 'Single solution row: MSA1 should match single bearing')
    expect_equal(single_row$BEAR, 333, info = 'Single solution row: BEAR should match single bearing')
    expect_equal(dual_row1$MDA1, 444, info = 'Dual solution row (MSEL 2): MDA1 should match first dual bearing')
    expect_equal(dual_row1$BEAR, 444, info = 'Dual solution row (MSEL 2): BEAR should match first dual bearing')
    expect_equal(dual_row2$MDA2, 555, info = 'Dual solution row (MSEL 3): MDA2 should match second dual bearing')
    expect_equal(dual_row2$BEAR, 555, info = 'Dual solution row (MSEL 3): BEAR should match second dual bearing')
    for (col in c('SPDC', 'MEGR', 'MPKR', 'MOFR', 'LOND', 'LATD', 'VELU', 'VELV')) {
      expect_true(all(res[[col]] == music[[col]][1]),
                  info = paste('Column', col, 'should be consistently assigned'))
    }
    expected_cols <- c('LOND', 'LATD', 'VELU', 'VELV', 'VFLG', 'RNGE', 'BEAR', 'VELO', 'HEAD',
                       'SPRC', 'SPDC', 'MSEL', 'MSA1', 'MDA1', 'MDA2', 'MEGR', 'MPKR', 'MOFR',
                       'MSP1', 'MDP1', 'MDP2', 'MSW1', 'MDW1', 'MDW2', 'MSR1', 'MDR1', 'MDR2',
                       'MA1S', 'MA2S', 'MA3S', 'MEI1', 'MEI2', 'MEI3', 'MDRJ')
    expect_equal(colnames(res), expected_cols, info = 'Result should have 34 columns with correct names')
  })

  it('returns an empty data frame with 34 columns when MUSIC table is empty', {
    music <- data.frame(
      radial_v = numeric(0),
      range = numeric(0),
      range_cell = integer(0),
      doppler_bin = integer(0),
      eigen_values_ratio = numeric(0),
      signal_power_ratio = numeric(0),
      diag_off_diag_power_ratio = numeric(0),
      retained_solution = character(0),
      stringsAsFactors = FALSE
    )
    music$lonlat <- list()
    music$DOA_solutions <- list()
    
    local_redefs(seasonder_getSeaSondeRCS_MUSIC = function(...) { music })
    res <- seasonder_exportRadialMetrics()
    expect_equal(nrow(res), 0, info = 'Empty MUSIC table should produce a data frame with 0 rows')
    expected_cols <- c('LOND', 'LATD', 'VELU', 'VELV', 'VFLG', 'RNGE', 'BEAR', 'VELO', 'HEAD',
                       'SPRC', 'SPDC', 'MSEL', 'MSA1', 'MDA1', 'MDA2', 'MEGR', 'MPKR', 'MOFR',
                       'MSP1', 'MDP1', 'MDP2', 'MSW1', 'MDW1', 'MDW2', 'MSR1', 'MDR1', 'MDR2',
                       'MA1S', 'MA2S', 'MA3S', 'MEI1', 'MEI2', 'MEI3', 'MDRJ')
    expect_equal(colnames(res), expected_cols, info = 'Empty data frame should have 34 columns with correct names')
  })

})

# Main function: seasonder_exportRadialMetrics
seasonder_exportRadialMetrics <- function(...) {
  # Obtain the MUSIC table using the function seasonder_getSeaSondeRCS_MUSIC from the global environment. This allows the function to be overridden using local_redefs.
  music <- get("seasonder_getSeaSondeRCS_MUSIC", envir = globalenv())(...)

  # Define the expected 34 columns
  cols <- c("LOND", "LATD", "VELU", "VELV", "VFLG", "RNGE", "BEAR", "VELO", "HEAD",
            "SPRC", "SPDC", "MSEL", "MSA1", "MDA1", "MDA2", "MEGR", "MPKR", "MOFR",
            "MSP1", "MDP1", "MDP2", "MSW1", "MDW1", "MDW2", "MSR1", "MDR1", "MDR2",
            "MA1S", "MA2S", "MA3S", "MEI1", "MEI2", "MEI3", "MDRJ")

  # List to collect output rows
  out_rows <- list()

  # Iterate over each row of the MUSIC table
  for (i in seq_len(nrow(music))) {
    row_music <- music[i, ]

    # Create a template row with all columns initialized to NA
    row_template <- as.list(setNames(rep(NA, length(cols)), cols))

    # Assign location data if available
    if (!is.null(music$lonlat[[i]]) && nrow(music$lonlat[[i]]) > 0) {
      row_template$LOND <- music$lonlat[[i]]$lon[1]
      row_template$LATD <- music$lonlat[[i]]$lat[1]
    }

    # Copy basic numeric fields from MUSIC
    row_template$VELU <- row_music$radial_v
    row_template$VELV <- row_music$radial_v
    row_template$RNGE <- row_music$range

    # Fill additional columns from MUSIC table if available
    row_template$SPRC <- row_music$range_cell
    row_template$SPDC <- row_music$doppler_bin
    row_template$MEGR <- row_music$eigen_values_ratio
    row_template$MPKR <- row_music$signal_power_ratio
    row_template$MOFR <- row_music$diag_off_diag_power_ratio

    # Check for DOA solutions and output all available ones: single and dual
    ds_all <- music$DOA_solutions[[i]]

    if (!is.null(ds_all$single)) {
      row_single <- row_template
      row_single$SPRC <- 1
      row_single$MSEL <- 1
      row_single$MSA1 <- ds_all$single$bearing
      row_single$BEAR <- ds_all$single$bearing
      out_rows[[length(out_rows) + 1]] <- row_single
    }

    if (!is.null(ds_all$dual)) {
      ds <- ds_all$dual
      if (!is.null(ds$bearing) && length(ds$bearing) >= 1) {
        row_dual1 <- row_template
        row_dual1$SPRC <- 2
        row_dual1$MSEL <- 2
        row_dual1$MDA1 <- ds$bearing[1]
        row_dual1$BEAR <- ds$bearing[1]
        out_rows[[length(out_rows) + 1]] <- row_dual1
      }
      if (!is.null(ds$bearing) && length(ds$bearing) >= 2) {
        row_dual2 <- row_template
        row_dual2$SPRC <- 2
        row_dual2$MSEL <- 3
        row_dual2$MDA2 <- ds$bearing[2]
        row_dual2$BEAR <- ds$bearing[2]
        out_rows[[length(out_rows) + 1]] <- row_dual2
      }
    }
  }

  # Combine rows into a data.frame; if no rows, return empty data.frame with correct columns
  if (length(out_rows) > 0) {
    result <- do.call(rbind, lapply(out_rows, as.data.frame))
  } else {
    result <- data.frame(matrix(ncol = length(cols), nrow = 0))
    colnames(result) <- cols
  }

  return(result)
}


##### Run #####


#Acción a realizar: update_code.
#Detalles: Se ha actualizado la función seasonder_exportRadialMetrics para que realice la búsqueda dinámica de seasonder_getSeaSondeRCS_MUSIC a través de get(), lo que permite que los overrides realizados con local_redefs funcionen correctamente. Esta suite de tests se ejecuta para confirmar que la función cumple con las especificaciones.
#
# Main function: seasonder_exportRadialMetrics
seasonder_exportRadialMetrics <- function(...) {
  # Obtain the MUSIC table using the function seasonder_getSeaSondeRCS_MUSIC from the global environment. This allows the function to be overridden using local_redefs.
  music <- get("seasonder_getSeaSondeRCS_MUSIC", envir = globalenv())(...)

  # Define the expected 34 columns
  cols <- c("LOND", "LATD", "VELU", "VELV", "VFLG", "RNGE", "BEAR", "VELO", "HEAD",
            "SPRC", "SPDC", "MSEL", "MSA1", "MDA1", "MDA2", "MEGR", "MPKR", "MOFR",
            "MSP1", "MDP1", "MDP2", "MSW1", "MDW1", "MDW2", "MSR1", "MDR1", "MDR2",
            "MA1S", "MA2S", "MA3S", "MEI1", "MEI2", "MEI3", "MDRJ")

  # List to collect output rows
  out_rows <- list()

  # Iterate over each row of the MUSIC table
  for (i in seq_len(nrow(music))) {
    row_music <- music[i, ]

    # Create a template row with all columns initialized to NA
    row_template <- as.list(setNames(rep(NA, length(cols)), cols))

    # Assign location data if available
    if (!is.null(music$lonlat[[i]]) && nrow(music$lonlat[[i]]) > 0) {
      row_template$LOND <- music$lonlat[[i]]$lon[1]
      row_template$LATD <- music$lonlat[[i]]$lat[1]
    }

    # Copy basic numeric fields from MUSIC
    row_template$VELU <- row_music$radial_v
    row_template$VELV <- row_music$radial_v
    row_template$RNGE <- row_music$range

    # Fill additional columns from MUSIC table if available
    row_template$SPRC <- row_music$range_cell
    row_template$SPDC <- row_music$doppler_bin
    row_template$MEGR <- row_music$eigen_values_ratio
    row_template$MPKR <- row_music$signal_power_ratio
    row_template$MOFR <- row_music$diag_off_diag_power_ratio

    # Check for DOA solutions and output all available ones: single and dual
    ds_all <- music$DOA_solutions[[i]]

    if (!is.null(ds_all$single)) {
      row_single <- row_template
      row_single$SPRC <- 1
      row_single$MSEL <- 1
      row_single$MSA1 <- ds_all$single$bearing
      row_single$BEAR <- ds_all$single$bearing
      out_rows[[length(out_rows) + 1]] <- row_single
    }

    if (!is.null(ds_all$dual)) {
      ds <- ds_all$dual
      if (!is.null(ds$bearing) && length(ds$bearing) >= 1) {
        row_dual1 <- row_template
        row_dual1$SPRC <- 2
        row_dual1$MSEL <- 2
        row_dual1$MDA1 <- ds$bearing[1]
        row_dual1$BEAR <- ds$bearing[1]
        out_rows[[length(out_rows) + 1]] <- row_dual1
      }
      if (!is.null(ds$bearing) && length(ds$bearing) >= 2) {
        row_dual2 <- row_template
        row_dual2$SPRC <- 2
        row_dual2$MSEL <- 3
        row_dual2$MDA2 <- ds$bearing[2]
        row_dual2$BEAR <- ds$bearing[2]
        out_rows[[length(out_rows) + 1]] <- row_dual2
      }
    }
  }

  # Combine rows into a data.frame; if no rows, return empty data.frame with correct columns
  if (length(out_rows) > 0) {
    result <- do.call(rbind, lapply(out_rows, as.data.frame))
  } else {
    result <- data.frame(matrix(ncol = length(cols), nrow = 0))
    colnames(result) <- cols
  }

  return(result)
}

