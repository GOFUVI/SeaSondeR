# Main function: seasonder_exportRadialMetrics
seasonder_exportRadialMetrics <- function(...) {
  # Obtain the MUSIC table using seasonder_getSeaSondeRCS_MUSIC
  music <- seasonder_getSeaSondeRCS_MUSIC(...)

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
    if (!is.null(row_music$doppler_bin)) {
      row_template$SPDC <- row_music$doppler_bin
    }

    # For this version, additional columns from MEGR onward are not yet populated
    # Determine solution type
    retained <- as.character(row_music$retained_solution)
    if (retained == "single") {
      # Single solution: one output row
      row_template$SPRC <- 1
      row_template$MSEL <- 1
      ds <- music$DOA_solutions[[i]]$single
      if (!is.null(ds)) {
        row_template$MSA1 <- ds$bearing
      }
      out_rows[[length(out_rows) + 1]] <- row_template
    } else if (retained == "dual") {
      # Dual solution: two output rows
      ds <- music$DOA_solutions[[i]]$dual

      # First row: assign MSEL = 2 and first dual bearing to MDA1
      row1 <- row_template
      row1$SPRC <- 2
      row1$MSEL <- 2
      if (!is.null(ds) && length(ds$bearing) >= 1) {
        row1$MDA1 <- ds$bearing[1]
      }

      # Second row: assign MSEL = 3 and second dual bearing to MDA2
      row2 <- row_template
      row2$SPRC <- 2
      row2$MSEL <- 3
      if (!is.null(ds) && length(ds$bearing) >= 2) {
        row2$MDA2 <- ds$bearing[2]
      }

      out_rows[[length(out_rows) + 1]] <- row1
      out_rows[[length(out_rows) + 1]] <- row2
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

