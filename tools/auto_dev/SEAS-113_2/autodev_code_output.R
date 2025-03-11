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
