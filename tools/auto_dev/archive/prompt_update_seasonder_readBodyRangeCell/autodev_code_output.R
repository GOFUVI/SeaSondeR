# Read Reduced Encoded Data from a Binary Connection
#
# This function reads an array of numbers from a binary connection using a custom command-based protocol.
# A block of data is processed according to its size specified in key$size. Within the block, the first byte read
# is a command byte that determines how the subsequent bytes are interpreted. The function updates a running "tracking
# value" based on the commands encountered and returns a vector of decoded numbers. The supported commands are:
#
#   0x9C: Read 4 bytes as an unsigned 32-bit integer.
#   0x94: Read one count byte, then (count+1) unsigned 32-bit integers.
#   0xAC: Read 3 bytes as a 24-bit signed integer; add its value to the current tracking value.
#   0xA4: Read one count byte, then (count+1) 24-bit signed integers; sequentially add each to the tracking value.
#   0x89: Read 1 byte as a signed 8-bit integer; add it to the tracking value.
#   0x8A: Read 2 bytes as a signed 16-bit integer; add it to the tracking value.
#   0x82: Read one count byte, then (count+1) signed 16-bit integers; sequentially add each to the tracking value.
#   0x81: Read one count byte, then (count+1) signed 8-bit integers; sequentially add each to the tracking value.

seasonder_read_reduced_encoded_data <- function(connection, key, endian = "big") {

  # Helper function to read a 32-bit unsigned integer from the binary connection.
  read_uint32 <- function(connection, endian) {
    bytes <- readBin(connection, what = "raw", n = 4, size = 1)
    if (endian == "big") {
      value <- as.integer(bytes[1]) * 256^3 +
        as.integer(bytes[2]) * 256^2 +
        as.integer(bytes[3]) * 256 +
        as.integer(bytes[4])
    } else {
      value <- as.integer(bytes[4]) * 256^3 +
        as.integer(bytes[3]) * 256^2 +
        as.integer(bytes[2]) * 256 +
        as.integer(bytes[1])
    }
    return(value)
  }

  # Helper function to read a 24-bit signed integer from the binary connection.
  read_sint24 <- function(connection, endian) {
    bytes <- readBin(connection, what = "integer", n = 3, size = 1, endian = endian, signed = FALSE)
    if (endian == "big") {
      value <- bytes[1] * 256^2 + bytes[2] * 256 + bytes[3]
    } else {
      value <- bytes[3] * 256^2 + bytes[2] * 256 + bytes[1]
    }
    if (value >= 8388608) value <- value - 16777216
    return(value)
  }

  .command_byte_9C <- function(connection, endian, tracking_value) {
    value <- read_uint32(connection, endian)
    return(value)
  }

  .command_byte_94 <- function(connection, endian, tracking_value) {
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    num_integers <- count + 1
    values <- sapply(seq_len(num_integers), function(i) {
      read_uint32(connection, endian)
    })
    return(values)
  }

  .command_byte_AC <- function(connection, endian, tracking_value) {
    delta <- read_sint24(connection, endian)
    new_value <- tracking_value + delta
    return(new_value)
  }

  .command_byte_A4 <- function(connection, endian, tracking_value) {
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    num_values <- count + 1
    results <- integer(num_values)
    for (i in seq_len(num_values)) {
      delta <- read_sint24(connection, endian)
      tracking_value <- tracking_value + delta
      results[i] <- tracking_value
    }
    return(results)
  }

  .command_byte_89 <- function(connection, endian, tracking_value) {
    x <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = TRUE)
    out <- tracking_value + x
    return(out)
  }

  .command_byte_8A <- function(connection, endian, tracking_value) {
    x <- readBin(connection, what = "integer", n = 1, size = 2, endian = endian, signed = TRUE)
    out <- tracking_value + x
    return(out)
  }

  .command_byte_82 <- function(connection, endian, tracking_value) {
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    n <- count + 1
    out <- purrr::accumulate(1:n, function(track, i) {
      val <- readBin(connection, what = "integer", n = 1, size = 2, endian = endian, signed = TRUE)
      track + val
    }, .init = tracking_value)
    out <- out[-1]
    return(out)
  }

  .command_byte_81 <- function(connection, endian, tracking_value) {
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    n <- count + 1
    out <- purrr::accumulate(1:n, function(track, i) {
      val <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = TRUE)
      track + val
    }, .init = tracking_value)
    out <- out[-1]
    return(out)
  }

  tracking_value <- 0L
  start_point <- seek(connection, origin = "current")
  final_point <- start_point + key$size
  out <- integer(0)

  while (seek(connection, origin = "current") < final_point) {
    command_byte <- readBin(connection, what = "raw", n = 1, size = 1, endian = endian)
    if (command_byte == as.raw(0x9C)) {
      cmd_function <- .command_byte_9C
    } else if (command_byte == as.raw(0x94)) {
      cmd_function <- .command_byte_94
    } else if (command_byte == as.raw(0xAC)) {
      cmd_function <- .command_byte_AC
    } else if (command_byte == as.raw(0xA4)) {
      cmd_function <- .command_byte_A4
    } else if (command_byte == as.raw(0x89)) {
      cmd_function <- .command_byte_89
    } else if (command_byte == as.raw(0x8A)) {
      cmd_function <- .command_byte_8A
    } else if (command_byte == as.raw(0x82)) {
      cmd_function <- .command_byte_82
    } else if (command_byte == as.raw(0x81)) {
      cmd_function <- .command_byte_81
    } else {
      seasonder_logAndAbort("Invalid command encountered")
    }
    result_piece <- cmd_function(connection, endian, tracking_value)
    out <- append(out, result_piece)
    tracking_value <- dplyr::last(out)
  }
  return(out)
}


# Updated seasonder_readBodyRangeCell: applies scaling if a 'scal' block is read
seasonder_readBodyRangeCell <- function(connection, specs, endian, specs_key_size = NULL){
  indx_read <- FALSE
  scaling_params <- NULL
  out <- list()

  while(TRUE){
    key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)
    if(!key$key %in% names(specs)){
      seek(connection, key$size, origin = "current")
    } else if(key$key == "END "){
      break
    } else if(key$key == "indx" && indx_read){
      seek(connection, -8, origin = "current")
      break
    } else if(key$key == "scal"){
      # Read scaling parameters from the "scal" block
      scaling_params <- seasonder_readCSSYFields(connection, purrr::chuck(specs, key$key), endian, parent_key = key)
      out <- append(out, list(scaling_params) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("cs1a","cs2a","cs3a","c13r","c13i","c23r","c23i","c12r","c12i", "csqf")){
      data_block <- seasonder_read_reduced_encoded_data(connection, key, endian)
      # Apply scaling if scaling parameters have been provided
      if(!is.null(scaling_params)){
        data_block <- seasonder_SeaSondeRCSSYApplyScaling(list(data_block),
                                                          fmax = scaling_params$fmax,
                                                          fmin = scaling_params$fmin,
                                                          fscale = scaling_params$fscale,
                                                          dbRef = scaling_params$dbRef)[[1]]
      }
      out <- append(out, list(data_block) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("csgn")){
      out <- append(out, list(seasonder_read_csign(connection, key)) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("asgn")){
      out <- append(out, list(seasonder_read_asign(connection, key)) %>% magrittr::set_names(key$key))
    } else {
      out <- append(out,
                    list(seasonder_readCSSYFields(connection, purrr::chuck(specs, key$key), endian, parent_key = key)) %>% magrittr::set_names(key$key)
      )
      if(key$key == "indx"){
        indx_read <- TRUE
      }
    }
  }
  return(out)
}

# Apply Scaling to SeaSondeRCSSY Data
seasonder_SeaSondeRCSSYApplyScaling <- function(values, fmax, fmin, fscale, dbRef) {
  if (!is.list(values)) {
    stop("The input 'values' must be a list of numeric vectors.")
  }
  if (!is.numeric(fmax) || !is.numeric(fmin) || !is.numeric(fscale) || !is.numeric(dbRef)) {
    stop("Parameters 'fmax', 'fmin', 'fscale', and 'dbRef' must be numeric.")
  }
  if (fscale == 0) {
    stop("'fscale' must not be zero.")
  }
  scaled_values <- lapply(values, function(vec) {
    if (!is.numeric(vec)) {
      stop("Each element in 'values' must be a numeric vector.")
    }
    sapply(vec, function(value) {
      if (value == 0xFFFFFFFF) {
        return(NaN)
      } else {
        intermediate <- value * (fmax - fmin) / fscale + fmin
        voltage <- 10^((intermediate + dbRef) / 10)
        return(voltage)
      }
    })
  })
  return(scaled_values)
}
