' Read Reduced Encoded Data from a Binary Connection
#'
#' This function reads an array of numbers from a binary connection using a custom command-based protocol.
#' A block of data is processed according to its size specified in \code{key$size}. Within the block, the first byte read
#' is a command byte that determines how the subsequent bytes are interpreted. The function updates a running "tracking
#' value" based on the commands encountered and returns a vector of decoded numbers. The supported commands are:
#'
#' \describe{
#'   \item{0x9C}{Read 4 bytes as an unsigned 32-bit integer.}
#'   \item{0x94}{Read one count byte, then (count+1) unsigned 32-bit integers.}
#'   \item{0xAC}{Read 3 bytes as a 24-bit signed integer; add its value to the current tracking value.}
#'   \item{0xA4}{Read one count byte, then (count+1) 24-bit signed integers; sequentially add each to the tracking value.}
#'   \item{0x89}{Read 1 byte as a signed 8-bit integer; add it to the tracking value.}
#'   \item{0x8A}{Read 2 bytes as a signed 16-bit integer; add it to the tracking value.}
#'   \item{0x82}{Read one count byte, then (count+1) signed 16-bit integers; sequentially add each to the tracking value.}
#'   \item{0x81}{Read one count byte, then (count+1) signed 8-bit integers; sequentially add each to the tracking value.}
#' }
#'
#' A 24-bit signed integer is computed by reading 3 bytes and then adjusting the value by subtracting 16777216 if the
#' computed value is greater than or equal to 8388608 to account for the two's complement representation.
#'
#' @param connection A binary connection from which the encoded data is read.
#' @param key A list containing a field \code{size} that indicates how many bytes of data to process.
#' @param endian A character string specifying the byte order; either \code{"big"} or \code{"little"}. The default is \code{"big"}.
#'
#' @return An integer vector containing the decoded numbers.
#'
#' @importFrom purrr accumulate
#' @importFrom dplyr last
#'
#' @examples
#' \dontrun{
#'   # Example for processing command 0x9C (unsigned 32-bit integer):
#'   # 0x9C is followed by 4 bytes that represent 1000 (big endian: 0x00, 0x00, 0x03, 0xE8).
#'   raw_vec <- as.raw(c(0x9C, 0x00, 0x00, 0x03, 0xE8))
#'   con <- rawConnection(raw_vec, open = "rb")
#'   key <- list(size = length(raw_vec))
#'   result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
#'   close(con)
#'   print(result)  # Expected output: 1000
#' }
seasonder_read_reduced_encoded_data <- function(connection, key, endian = "big") {

  # Helper function to read a 32-bit unsigned integer from the binary connection.
  # It reads 4 bytes and computes the integer value based on the specified endianness.
  read_uint32 <- function(connection, endian) {
    # Read 4 raw bytes from the connection.
    bytes <- readBin(connection, what = "raw", n = 4, size = 1)
    if (endian == "big") {
      # Compute the 32-bit value using big endian order.
      value <- as.integer(bytes[1]) * 256^3 +
        as.integer(bytes[2]) * 256^2 +
        as.integer(bytes[3]) * 256 +
        as.integer(bytes[4])
    } else {
      # Compute the 32-bit value using little endian order.
      value <- as.integer(bytes[4]) * 256^3 +
        as.integer(bytes[3]) * 256^2 +
        as.integer(bytes[2]) * 256 +
        as.integer(bytes[1])
    }
    return(value)
  }

  # Helper function to read a 24-bit signed integer from the binary connection.
  # It reads 3 bytes and, based on the specified endianness, returns the correctly signed integer.
  read_sint24 <- function(connection, endian) {
    # Read 3 bytes as unsigned integers.
    bytes <- readBin(connection, what = "integer", n = 3, size = 1, endian = endian, signed = FALSE)
    if (endian == "big") {
      # Compute using big endian order.
      value <- bytes[1] * 256^2 + bytes[2] * 256 + bytes[3]
    } else {
      # Compute using little endian order.
      value <- bytes[3] * 256^2 + bytes[2] * 256 + bytes[1]
    }
    # Adjust for negative numbers using two's complement.
    if (value >= 8388608) value <- value - 16777216
    return(value)
  }

  # Command 0x9C: Read 4 bytes as an unsigned 32-bit integer.
  .command_byte_9C <- function(connection, endian, tracking_value) {
    # Invoke helper to read the 32-bit unsigned integer and return its value.
    value <- read_uint32(connection, endian)
    return(value)
  }

  # Command 0x94: Read one count byte, then (count+1) unsigned 32-bit integers.
  .command_byte_94 <- function(connection, endian, tracking_value) {
    # Read the count byte (unsigned 8-bit integer).
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    num_integers <- count + 1  # Determine the number of integers to read.
    # Read each unsigned 32-bit integer sequentially.
    values <- sapply(seq_len(num_integers), function(i) {
      read_uint32(connection, endian)
    })
    return(values)
  }

  # Command 0xAC: Read 3 bytes as a 24-bit signed integer and add the result to the tracking value.
  .command_byte_AC <- function(connection, endian, tracking_value) {
    # Read the 24-bit signed integer delta.
    delta <- read_sint24(connection, endian)
    # Update tracking value and return the new value.
    new_value <- tracking_value + delta
    return(new_value)
  }

  # Command 0xA4: Read one count byte, then (count+1) 24-bit signed integers.
  # Each integer is added sequentially to the tracking value.
  .command_byte_A4 <- function(connection, endian, tracking_value) {
    # Read the count byte.
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    num_values <- count + 1  # Determine how many values are to be processed.
    results <- integer(num_values)  # Initialize the results vector.
    for (i in seq_len(num_values)) {
      # Read each 24-bit delta and update the tracking value.
      delta <- read_sint24(connection, endian)
      tracking_value <- tracking_value + delta
      results[i] <- tracking_value  # Store the updated tracking value.
    }
    return(results)
  }

  # Command 0x89: Read 1 byte as a signed 8-bit integer and add it to the tracking value.
  .command_byte_89 <- function(connection, endian, tracking_value) {
    # Read a signed 8-bit integer.
    x <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = TRUE)
    # Compute the new tracking value.
    out <- tracking_value + x
    return(out)
  }

  # Command 0x8A: Read 2 bytes as a signed 16-bit integer and add it to the tracking value.
  .command_byte_8A <- function(connection, endian, tracking_value) {
    # Read a signed 16-bit integer.
    x <- readBin(connection, what = "integer", n = 1, size = 2, endian = endian, signed = TRUE)
    # Update and return the tracking value.
    out <- tracking_value + x
    return(out)
  }

  # Command 0x82: Read one count byte, then (count+1) signed 16-bit integers.
  # Each value is used to update the tracking value via cumulative addition.
  .command_byte_82 <- function(connection, endian, tracking_value) {
    # Read the count byte.
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    n <- count + 1  # Determine the total number of 16-bit integers.
    # Accumulate tracking values for each integer read.
    out <- purrr::accumulate(1:n, function(track, i) {
      val <- readBin(connection, what = "integer", n = 1, size = 2, endian = endian, signed = TRUE)
      track + val
    }, .init = tracking_value)
    out <- out[-1]  # Remove the seed value.
    return(out)
  }

  # Command 0x81: Read one count byte, then (count+1) signed 8-bit integers.
  # Each value is added sequentially to update the tracking value.
  .command_byte_81 <- function(connection, endian, tracking_value) {
    # Read the count byte.
    count <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = FALSE)
    n <- count + 1  # Calculate the number of 8-bit integers to process.
    # Cumulatively add each 8-bit integer to the tracking value.
    out <- purrr::accumulate(1:n, function(track, i) {
      val <- readBin(connection, what = "integer", n = 1, size = 1, endian = endian, signed = TRUE)
      track + val
    }, .init = tracking_value)
    out <- out[-1]  # Exclude the initial tracking value.
    return(out)
  }

  # Initialize the tracking value to 0.
  tracking_value <- 0L
  # Get the current position in the connection; this marks the start of the data block.
  start_point <- seek(connection, origin = "current")
  # Compute the final position based on the size provided in key.
  final_point <- start_point + key$size
  # Initialize an empty vector to accumulate the decoded output.
  out <- integer(0)

  # Process the data block until the current position reaches the final position.
  while (seek(connection, origin = "current") < final_point) {
    # Read the next command byte from the connection.
    command_byte <- readBin(connection, what = "raw", n = 1, size = 1, endian = endian)

    # Select the appropriate command handler based on the command byte.
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
      # Log the error and abort processing when an invalid command is encountered.
      seasonder_logAndAbort("Invalid command encountered")
    }

    # Execute the appropriate command function and update the output.
    result_piece <- cmd_function(connection, endian, tracking_value)
    out <- append(out, result_piece)
    # Update the tracking value with the last value from the output vector.
    tracking_value <- dplyr::last(out)
  }

  # Return the decoded integer vector.
  return(out)
}



#' Read Complex Spectral Sign Information from a Connection
#'
#' This function reads a raw binary stream from a provided connection, expecting a specific format
#' that contains the sign bits for complex spectral values. The data is divided into 6 groups corresponding
#' to: \code{C13r}, \code{C13i}, \code{C23r}, \code{C23i}, \code{C12r}, and \code{C12i}.
#'
#' @param connection A binary connection to read raw bytes from.
#' @param key A list containing:
#'   \describe{
#'     \item{size}{An integer specifying the total number of bytes to be read. It must equal 6 times the number
#'       of bytes per group.}
#'     \item{key}{A string identifier (expected to be \code{"csign"}).}
#'   }
#'
#' @return A named list of 6 vectors. Each vector represents one group (e.g., \code{C13r}, \code{C13i}, etc.)
#'   and contains integers (0 or 1) corresponding to the bits (in little-endian order) extracted from the raw data.
#'
#' @details The function performs the following steps:
#'   \itemize{
#'     \item Reads \code{key$size} bytes from the specified connection.
#'     \item Checks if enough bytes were read.
#'     \item Ensures that the total number of bytes is divisible by 6, allowing equal distribution among the groups.
#'     \item Splits the raw byte vector into 6 groups based on the calculated number of bytes per group.
#'     \item Converts each byte into its 8-bit representation (using \code{rawToBits}) and flattens the result.
#'   }
#'
#'
#' @examples
#' \dontrun{
#' # Create a raw connection with sample data:
#' con <- rawConnection(as.raw(c(0x42, 0x29, 0xa3, 0xd7, 0xFF, 0x00)))
#' key <- list(size = 6, key = "csign")
#' result <- seasonder_read_csign(con, key)
#' print(result)
#' close(con)
#' }
seasonder_read_csign <- function(connection, key) {
  # Store the number of bytes to read based on the key list.
  total_bytes <- key$size

  # Read the specified number of raw bytes from the connection.
  raw_data <- readBin(connection, what = "raw", n = total_bytes)

  # Check if the actual number of bytes read is less than expected.
  if (length(raw_data) < total_bytes) {
    stop("Not enough bytes in connection")
  }

  # Validate that the total number of bytes is divisible by 6,
  # which is necessary to form 6 equal-sized groups corresponding to:
  # C13r, C13i, C23r, C23i, C12r, and C12i.
  if (total_bytes %% 6 != 0) {
    stop("Invalid total size: not divisible by 6")
  }

  # Calculate how many bytes correspond to each group.
  group_bytes_count <- total_bytes / 6

  # Define the names of the 6 groups in the expected order.
  group_names <- c("C13r", "C13i", "C23r", "C23i", "C12r", "C12i")

  # Initialize the result list with names set for each group.
  result <- setNames(vector("list", length(group_names)), group_names)

  # Loop through each group to extract and process the relevant bytes.
  for (i in seq_along(group_names)) {
    # Calculate the starting and ending indices for the current group's slice.
    start_index <- (i - 1) * group_bytes_count + 1
    end_index <- i * group_bytes_count

    # Extract the subset of raw bytes corresponding to the current group.
    group_raw <- raw_data[start_index:end_index]

    # Convert each byte in the group to its 8-bit binary representation.
    # rawToBits converts a byte to a vector of 8 bits (in little-endian order).
    # lapply applies this conversion to each byte, and unlist flattens the list to a vector.
    group_bits <- unlist(lapply(group_raw, function(byte) as.integer(rawToBits(byte))))

    # Store the converted bits in the result list for the current group.
    result[[i]] <- group_bits
  }

  # Return the list containing vectors of bits for each of the 6 groups.
  return(result)
}


#' Read Self Spectra Sign Information from a Connection
#'
#' This function reads a raw binary stream from a provided connection, expecting a specific format
#' that contains the sign bits for self spectra values. The data is divided into 3 groups corresponding
#' to: \code{cs1a}, \code{cs2a}, and \code{cs3a}.
#'
#' @param connection A binary connection to read raw bytes from.
#' @param key A list containing:
#'   \describe{
#'     \item{size}{An integer specifying the total number of bytes to be read. It must equal 3 times the number
#'       of bytes per group.}
#'     \item{key}{A string identifier (expected to be \code{"asign"}).}
#'   }
#'
#' @return A named list of 3 vectors. Each vector represents one group (i.e., \code{cs1a}, \code{cs2a}, \code{cs3a})
#'   and contains integers (0 or 1) corresponding to the bits (in little-endian order) extracted from the raw data.
#'
#' @details The function performs the following steps:
#'   \itemize{
#'     \item Reads \code{key$size} bytes from the specified connection.
#'     \item Verifies that the number of bytes read matches the expected size.
#'     \item Checks that the total number of bytes is divisible by 3, allowing equal distribution among the groups.
#'     \item Splits the raw byte vector into 3 groups based on the calculated number of bytes per group.
#'     \item Converts each byte into its 8-bit binary representation (using \code{rawToBits}) and flattens the results for each group.
#'   }
#'
seasonder_read_asign <- function(connection, key) {
  # Determine the total number of bytes to read from the connection based on key$size.
  total_bytes <- key$size

  # Read the specified number of raw bytes from the connection using readBin.
  raw_data <- readBin(connection, what = "raw", n = total_bytes)

  # Check if the number of bytes read is less than the expected total_bytes; if so, throw an error.
  if (length(raw_data) < total_bytes) {
    stop("Not enough bytes in connection")
  }

  # Validate that the total number of bytes is divisible by 3 to form 3 equal groups.
  if (total_bytes %% 3 != 0) {
    stop("Invalid total size: not divisible by 3")
  }

  # Calculate the number of bytes allocated for each group.
  group_bytes_count <- total_bytes / 3

  # Define the names for each of the three groups.
  group_names <- c("cs1a", "cs2a", "cs3a")

  # Initialize an empty list to store each group's bit vectors, assigning the group names.
  result <- setNames(vector("list", length(group_names)), group_names)

  # Loop over each group to extract its corresponding bytes and convert them to bit vectors.
  for (i in seq_along(group_names)) {
    # Calculate the start and end indices for the current group's slice in the raw_data vector.
    start_index <- (i - 1) * group_bytes_count + 1
    end_index <- i * group_bytes_count

    # Extract the raw bytes corresponding to the current group.
    group_raw <- raw_data[start_index:end_index]

    # Convert each byte in the group to its 8-bit binary representation using rawToBits,
    # then convert the raw bits to integers and flatten the resulting list into a vector.
    group_bits <- unlist(lapply(group_raw, function(byte) as.integer(rawToBits(byte))))

    # Store the resulting bit vector in the result list at the corresponding group name.
    result[[i]] <- group_bits
  }

  # Return the list containing the bit vectors for each group.
  return(result)
}

seasonder_readCSSYFields <- function(connection, specs, endian, parent_key= NULL){
  variable_char_types <- purrr::map_lgl(specs, \(x) x$type == "CharX")
  if(any(variable_char_types)){
    variable_char_types_index <- which(variable_char_types)
    specs <- purrr::reduce(variable_char_types_index, \(spcs_so_far, i){
      spcs_so_far[[i]]$type <-  glue::glue("Char{size}", size = parent_key$size)
      spcs_so_far
    }, .init = specs )

  }
  # browser(expr = parent_key$key == "scal")
  out <-  seasonder_readSeaSondeCSFileBlock(specs, connection, endian)

  return(out)

}


#' Apply Scaling to SeaSondeRCSSY Data
#'
#' This function applies scaling to each vector of integer values contained in the list `values` by converting them to floating point
#' voltage values using a specified scaling procedure. For each integer value:
#'   - If the value equals 0xFFFFFFFF, it returns NaN;
#'   - Otherwise, it computes an intermediate value using the formula:
#'         intermediate = value * (fmax - fmin) / fscale + fmin
#'     and then converts it to a voltage via:
#'         voltage = 10^((intermediate + dbRef) / 10)
#'
#' The function processes each vector in the input list and returns a new list having the same structure, but with each value converted
#' into its corresponding voltage value. It also performs several validations regarding input types and values.
#'
#' @param values A list of numeric vectors containing integer values to be scaled. Each vector is expected to contain values read
#'        from a binary CSSY values block.
#' @param fmax A numeric value representing the maximum scaling value. Used to compute the linear scaling factor.
#' @param fmin A numeric value representing the minimum scaling value. Acts as an offset for the scaling.
#' @param fscale A numeric value representing the scaling factor. Must not be zero as it determines the divisor in the scaling formula.
#' @param dbRef A numeric value representing the decibel reference to be added before the voltage conversion step.
#'
#' @return A list with the same structure as `values`, where each numeric vector has been transformed to a vector of floating point
#' voltage values. Special integer values equal to 0xFFFFFFFF are converted to NaN.
#'
#' @examples
#' # Example usage:
#' values <- list(c(1000, 0xFFFFFFFF, 2000))
#' scaled <- seasonder_SeaSondeRCSSYApplyScaling(values, fmax = 5, fmin = 0, fscale = 1000, dbRef = -20)
#' print(scaled)
#'
#' @details
#' The scaling process performs the following steps for each input value:
#'   1. Checks whether the value equals 0xFFFFFFFF. If so, it returns NaN immediately because this value indicates a
#'      missing or invalid measurement.
#'   2. Otherwise, it computes the intermediate scaled value by applying a linear transformation:
#'         intermediate = value * (fmax - fmin) / fscale + fmin
#'   3. Finally, it converts the intermediate value to a voltage using:
#'         voltage = 10^((intermediate + dbRef) / 10)
#'
#' The function includes input validation to ensure that `values` is a list, and that `fmax`, `fmin`, `fscale`, and `dbRef`
#' are numeric. It also checks that no element in `values` is non-numeric and that `fscale` is non-zero to prevent division errors.
seasonder_SeaSondeRCSSYApplyScaling <- function(values, fmax, fmin, fscale, dbRef) {

  # Validate that 'values' is a list
  if (!is.list(values)) {
    stop("The input 'values' must be a list of numeric vectors.")
  }

  # Validate that the scaling parameters and reference are numeric
  if (!is.numeric(fmax) || !is.numeric(fmin) || !is.numeric(fscale) || !is.numeric(dbRef)) {
    stop("Parameters 'fmax', 'fmin', 'fscale', and 'dbRef' must be numeric.")
  }

  # Prevent division by zero by ensuring fscale is not zero
  if (fscale == 0) {
    stop("'fscale' must not be zero.")
  }

  # Process each vector in the input list 'values'
  scaled_values <- lapply(values, function(vec) {
    # Validate that the current element is a numeric vector
    if (!is.numeric(vec)) {
      stop("Each element in 'values' must be a numeric vector.")
    }

    # Apply scaling to each value in the numeric vector
    sapply(vec, function(value) {
      # Check if the current value equals the special marker 0xFFFFFFFF (indicating missing/invalid data)
      if (value == 0xFFFFFFFF) {
        # Return NaN for invalid measurement
        return(NaN)
      } else {
        # Compute the intermediate scaled value using the linear transformation
        intermediate <- value * (fmax - fmin) / fscale + fmin

        # Convert the intermediate value to voltage using the decibel conversion formula
        voltage <- 10^((intermediate + dbRef) / 10)

        # Return the computed voltage value
        return(voltage)
      }
    })
  })

  # Return the list containing the scaled voltage vectors, preserving the structure of 'values'
  return(scaled_values)
}


#' Read a Body Range Cell and Apply Scaling if Required
#'
#' This function processes a block of keys from a binary connection according to a given specification ('specs').
#' Each key is read using seasonder_readSeaSondeCSFileBlock and processed based on its name.
#'
#' Key processing details:
#'   - 'scal': Reads scaling parameters (fmax, fmin, fscale, dbRef) via seasonder_readCSSYFields and stores
#'             them for use in scaling subsequent reduced data blocks.
#'
#'   - Reduced data keys (e.g., 'cs1a', 'cs2a', etc.): Reads the data using seasonder_read_reduced_encoded_data.
#'     If scaling parameters were set by a preceding 'scal' block, the raw data is transformed into voltage values
#'     using seasonder_SeaSondeRCSSYApplyScaling; otherwise, the raw data is returned as is.
#'
#'   - Other keys such as 'csgn' and 'asgn' invoke their own specialized read functions.
#'
#' The function terminates when the 'END ' key is encountered or when a repeated key (e.g., 'indx') signals the end of
#' the block.
#'
#' @param connection A binary connection from which keys and data are read.
#' @param specs A list that defines the expected keys and their formats.
#' @param endian A string specifying the byte order ("big" or "little").
#' @param specs_key_size Optional specification for the key size block.
#' @return A list with elements named after the keys read. For reduced data blocks, each element contains either
#'         raw decoded data or scaled voltage values if a 'scal' block was applied.
seasonder_readBodyRangeCell <- function(connection, specs, dbRef, endian = "big", specs_key_size = NULL){
  indx_read <- FALSE       # Flag indicating whether 'indx' has been encountered
  scaling_params <- NULL   # Storage for scaling parameters read from a 'scal' block
  out <- list()

  while(TRUE){
    key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)
    if(!key$key %in% names(specs)){
      # Skip keys not defined in the specifications
      seek(connection, key$size, origin = "current")
    } else if(key$key == "END "){
      # End-of-block marker encountered
      break
    } else if(key$key == "indx" && indx_read){
      # A repeated 'indx' indicates end of the block; rewind if necessary
      seek(connection, -8, origin = "current")
      break
    } else if(key$key == "scal"){
      # 'scal' block encountered: read and store scaling parameters
      scaling_params <- seasonder_readCSSYFields(connection, purrr::chuck(specs, key$key),
                                                 endian, parent_key = key)
      out <- append(out, list(scaling_params) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("cs1a", "cs2a", "cs3a", "c13r", "c13i", "c23r", "c23i", "c12r", "c12i", "csqf")){
      # Reduced data block: read the block; apply scaling if parameters are available
      data_block <- seasonder_read_reduced_encoded_data(connection, key, endian)
      if(!is.null(scaling_params)){
        data_block <- seasonder_SeaSondeRCSSYApplyScaling(list(data_block),
                                                          fmax = scaling_params$fmax,
                                                          fmin = scaling_params$fmin,
                                                          fscale = scaling_params$fscale,
                                                          dbRef = dbRef)[[1]]
      }
      out <- append(out, list(data_block) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("csgn")){
      # Process complex spectral sign information
      out <- append(out, list(seasonder_read_csign(connection, key)) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("asgn")){
      # Process self spectra sign information
      out <- append(out, list(seasonder_read_asign(connection, key)) %>% magrittr::set_names(key$key))
    } else {
      # For all other keys, process them as simple field blocks
      out <- append(out, list(seasonder_readCSSYFields(connection, purrr::chuck(specs, key$key),
                                                       endian, parent_key = key)) %>%
                      magrittr::set_names(key$key))
      if(key$key == "indx"){
        indx_read <- TRUE
      }
    }
  }
  return(out)
}





seasonder_readCSSYBody <- function(connection, specs, size, dbRef, endian = "big", specs_key_size = NULL){

  end_point <- seek(connection) + size

  out <- list()
  while(seek(connection) < end_point){

    out <- append(out, list(seasonder_readBodyRangeCell(connection, specs,  dbRef,endian = endian, specs_key_size = specs_key_size)))

  }

  return(out)
}

seasonder_readCSSYLims <- function(connection, n_values, endian = "big") {

  # Read n_values of 32-bit unsigned integers
  uint_values <- readBin(connection, what = "integer", n = n_values, size = 4, endian = endian, signed = FALSE)

  lims_matrix <- matrix(uint_values, ncol = 4, byrow = TRUE)
  colnames(lims_matrix) <- c("LeftBraggLeftLimit", "LeftBraggRightLimit", "RightBraggLeftLimit", "RightBraggRightLimit")
  return(lims_matrix)
}

#' Read CSSY File Header
#'
#' This function reads the header section of a CSSY file from a binary connection. The CSSY file header
#' contains a set of key blocks formatted according to the SeaSonde CSSY specification. The header section
#' is processed recursively and terminates when one of the following conditions is met:
#' \itemize{
#'   \item A key with name "BODY" is encountered. In this case, the connection is rewound by 8 bytes
#'         to allow subsequent processing of the body.
#'   \item A key that is not defined in \code{current_specs} but is already present in the
#'         \code{keys_so_far} vector is encountered (indicative of repeated keys), which triggers termination.
#' }
#'
#' When no subkeys are specified in \code{current_specs} (i.e. \code{current_specs} comprises only
#' simple field definitions), the function delegates the processing to \code{seasonder_readCSSYFields}.
#'
#' @param connection A binary connection from which to read the CSSY file header.
#' @param current_specs A list representing the specification for the header; may contain nested subkeys.
#' @param endian A character string indicating the byte order for reading numeric values ("big" or "little").
#' @param parent_key (Optional) A list with information from the parent key block, used when processing nested keys.
#' @param keys_so_far A character vector of keys already processed, used to avoid recursive loops. Defaults to c("CSSY", "HEAD").
#' @param specs_key_size A specification for reading the key size block, often obtained from YAML specs.
#'
#' @return A list containing the parsed CSSY header information. The returned list may be empty if a termination
#'         condition is encountered.
#'
#' @details The function processes the CSSY header recursively:
#' \itemize{
#'   \item If \code{current_specs} contains only field definitions, \code{seasonder_readCSSYFields} is called.
#'   \item When a key named "BODY" is encountered, it signifies the beginning of the body section; the
#'         function rewinds the connection 8 bytes and stops processing further keys.
#'   \item If a key is encountered that is not defined in \code{current_specs} but is already present in
#'         \code{keys_so_far}, the function also rewinds the connection 8 bytes and terminates header reading.
#'   \item Otherwise, the function updates \code{keys_so_far}, handles special cases (e.g., key "cs4h"), and
#'         calls itself recursively to process nested keys.
#' }
#'
#' @importFrom purrr map_lgl chuck reduce
#' @importFrom magrittr set_names
#' @import glue
#'
#' @examples
#' \dontrun{
#'   con <- file("path/to/file.cssy", "rb")
#'   specs <- seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"), "header")
#'   header <- seasonder_readCSSYHeader(con, specs, endian = "big")
#'   close(con)
#' }
seasonder_readCSSYHeader <- function(connection, current_specs, endian = "big", parent_key = NULL, keys_so_far = c("CSSY", "HEAD"), specs_key_size = NULL){
  # Initialize an empty output list for accumulating header values
  out <- list()

  # Determine if the current specifications define subkeys or only simple fields
  has_subkeys <- !all(purrr::map_lgl(current_specs, \(x) "type" %in% names(x)))

  if(!has_subkeys){
    # When there are no subkeys, delegate reading to seasonder_readCSSYFields
    out <- seasonder_readCSSYFields(connection, current_specs, endian, parent_key)
  } else {
    # Read the next key block from the binary connection
    key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)

    # Termination condition: if the key is "BODY", rewind 8 bytes and terminate header reading
    if(key$key == "BODY"){
      seek(connection, -8, origin = "current")
      return(out)
    } else if(!key$key %in% names(current_specs) && !key$key %in% keys_so_far){
      # If key is unknown (not in current_specs and not already seen), skip it by advancing connection by key$size bytes
      seek(connection, key$size, origin = "current")
    } else if(!key$key %in% names(current_specs) && key$key %in% keys_so_far){
      # If key is not defined in current_specs but already encountered, rewind 8 bytes and finish header reading
      seek(connection, -8, origin = "current")
      return(out)
    } else {
      # Update keys_so_far to include the names defined in the current specification
      keys_so_far <- unique(c(keys_so_far, names(current_specs)))
      if(key$key == "cs4h"){
        # Special handling: for key 'cs4h', read the CS file header using its respective specifications
        CSHSpecs <- seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CS"), "header")
        out <- list(seasonder_readSeaSondeCSFileHeader(CSHSpecs, connection, endian)) %>% magrittr::set_names(key$key)
      } else if(key$key %in% c("alim","wlim")){

        out <- seasonder_readCSSYHeader(connection, purrr::chuck(current_specs, key$key), endian, parent_key = key, keys_so_far = keys_so_far, specs_key_size = specs_key_size)

        out$lims <- seasonder_readCSSYLims(connection, out$nRange,endian = endian)

        out <- list(out) %>% magrittr::set_names(key$key)

      }else{
        # Recursively process the key using the sub-specs defined in current_specs
        out <- list(seasonder_readCSSYHeader(connection, purrr::chuck(current_specs, key$key), endian, parent_key = key, keys_so_far = keys_so_far, specs_key_size = specs_key_size)) %>% magrittr::set_names(key$key)
      }
    }
    # Continue recursive processing of the header for any remaining keys
    out <- c(out, seasonder_readCSSYHeader(connection, current_specs, endian, keys_so_far = keys_so_far, specs_key_size = specs_key_size))
  }

  return(out)
}


#' Transform CSSY Header to SeaSondeRCS Header
#'
#' This helper function extracts the 'cs4h' component from a CSSY header, removes it from the original header,
#' and embeds the remaining header information within the 'header_cssy' field of the CS header.
#'
#' @param header A list representing the CSSY header. Must contain a 'cs4h' component.
#' @return A transformed header where the primary CS header is taken from 'cs4h' and the remaining CSSY header fields
#'         are stored in the 'header_cssy' element.
seasonder_CSSY2CSHeader <- function(header) {
  if (is.null(header$cs4h)) {
    seasonder_logAndAbort("CSSY header does not contain a cs4h component")
  }
  header_cs <- header$cs4h  # Extract the valid CS header
  header_cssy <- header    # Copy the original header
  header_cssy$cs4h <- NULL  # Remove the cs4h component from the original header
  header_cs$header_cssy <- header_cssy  # Embed the remaining header
  return(header_cs)
}

#' Transform CSSY Body to SeaSonde CS Data Structure
#'
#' This function converts the body structure of a CSSY file into a list of matrices that conform
#' to the data structure required for creating a SeaSondeRCS object. The conversion is performed
#' by mapping specific fields:
#' \describe{
#'   \item{SSA1, SSA2, SSA3}{Matrices are built using the numeric vectors found in the \code{cs1a}, \code{cs2a}
#'          and \code{cs3a} fields respectively.}
#'   \item{CS12, CS13, CS23}{Each complex cross-spectra matrix is formed by combining the real parts
#'          from \code{c12r}, \code{c13r} and \code{c23r} with the corresponding imaginary parts
#'          from \code{c12i}, \code{c13i} and \code{c23i}.}
#'   \item{QC}{The quality control matrix is obtained directly from the \code{csqf} field.}
#' }
#'
#' Each row in the output matrices corresponds to the index provided by \code{cell$indx$index} in the input list.
#'
#' @param body A list representing the body of a CSSY file. Each element of the list is expected to be a
#'   cell containing the following fields: \code{indx} (which includes an \code{index}), \code{cs1a}, \code{cs2a}, \code{cs3a},
#'   \code{c12r}, \code{c12i}, \code{c13r}, \code{c13i}, \code{c23r}, \code{c23i} and \code{csqf}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{SSA1}{A numeric matrix containing self-spectra from \code{cs1a}.}
#'   \item{SSA2}{A numeric matrix containing self-spectra from \code{cs2a}.}
#'   \item{SSA3}{A numeric matrix containing self-spectra from \code{cs3a}.}
#'   \item{CS12}{A complex matrix formed by pairing \code{c12r} (real) and \code{c12i} (imaginary).}
#'   \item{CS13}{A complex matrix formed by pairing \code{c13r} (real) and \code{c13i} (imaginary).}
#'   \item{CS23}{A complex matrix formed by pairing \code{c23r} (real) and \code{c23i} (imaginary).}
#'   \item{QC}{A numeric matrix containing the quality control data from \code{csqf}.}
#' }
#'
#' @details
#' The function first determines the maximum index among the cells in the body, which defines the number of rows
#' for the matrices. Then, it calculates the number of columns for each matrix based on the length of the corresponding
#' vectors from the first cell where they appear. Finally, each cell's data is inserted into the appropriate row
#' of the matrices as indicated by the cell's \code{indx$index} value.
#'
#' @examples
#' \dontrun{
#'   # Example with a single cell
#'   cell <- list(
#'     indx  = list(index = 1),
#'     cs1a  = c(1, 2, 3),
#'     cs2a  = c(4, 5, 6),
#'     cs3a  = c(7, 8, 9),
#'     c12r  = c(10, 11, 12),
#'     c12i  = c(13, 14, 15),
#'     c13r  = c(16, 17, 18),
#'     c13i  = c(19, 20, 21),
#'     c23r  = c(22, 23, 24),
#'     c23i  = c(25, 26, 27),
#'     csqf  = c(28, 29, 30)
#'   )
#'   body <- list(cell)
#'   transformed <- seasonder_CSSY2CSData(body)
#'   print(transformed)
#' }
#'
#' @export

seasonder_CSSY2CSData <- function(body) {
  # Validate the input to ensure it's a non-empty list
  if (!is.list(body) || length(body) == 0) stop("Invalid body: must be a non-empty list")

  # Extract the row indices from each cell using cell$indx$index
  indices <- sapply(body, function(cell) cell$indx$index)
  max_index <- max(indices)

  # Helper function: for a given field, determine the number of columns based on the first cell that contains that field
  get_ncols <- function(field) {
    for (cell in body) {
      if (!is.null(cell[[field]])) {
        return(length(cell[[field]]))
      }
    }
    return(0)
  }

  # Determine the number of columns for self spectra (SSA), quality control (QC) and cross-spectra (for CS matrices)
  ncols_SSA1 <- get_ncols("cs1a")
  ncols_SSA2 <- get_ncols("cs2a")
  ncols_SSA3 <- get_ncols("cs3a")
  ncols_QC   <- get_ncols("csqf")
  ncols_CS12 <- get_ncols("c12r")  # assume c12r and c12i have same length
  ncols_CS13 <- get_ncols("c13r")
  ncols_CS23 <- get_ncols("c23r")

  # Create empty matrices for each required field. Each matrix will have 'max_index' rows (provided by cell$indx$index)
  # and a number of columns as determined by the helper function above.
  SSA1 <- matrix(NA_real_, nrow = max_index, ncol = ncols_SSA1)
  SSA2 <- matrix(NA_real_, nrow = max_index, ncol = ncols_SSA2)
  SSA3 <- matrix(NA_real_, nrow = max_index, ncol = ncols_SSA3)
  QC   <- matrix(NA_real_, nrow = max_index, ncol = ncols_QC)

  # Initialize complex matrices for cross spectra
  CS12 <- matrix(NA_complex_, nrow = max_index, ncol = ncols_CS12)
  CS13 <- matrix(NA_complex_, nrow = max_index, ncol = ncols_CS13)
  CS23 <- matrix(NA_complex_, nrow = max_index, ncol = ncols_CS23)

  # Iterate over each cell and assign the corresponding data to the appropriate row in the matrices
  for (cell in body) {
    row <- cell$indx$index
    if (!is.null(cell$cs1a)) SSA1[row, ] <- cell$cs1a
    if (!is.null(cell$cs2a)) SSA2[row, ] <- cell$cs2a
    if (!is.null(cell$cs3a)) SSA3[row, ] <- cell$cs3a
    if (!is.null(cell$csqf)) QC[row, ]   <- cell$csqf

    # For cross spectra, combine the real and imaginary parts to create complex numbers
    if (!is.null(cell$c12r) && !is.null(cell$c12i)) {
      CS12[row, ] <- complex(real = cell$c12r, imaginary = cell$c12i)
    }
    if (!is.null(cell$c13r) && !is.null(cell$c13i)) {
      CS13[row, ] <- complex(real = cell$c13r, imaginary = cell$c13i)
    }
    if (!is.null(cell$c23r) && !is.null(cell$c23i)) {
      CS23[row, ] <- complex(real = cell$c23r, imaginary = cell$c23i)
    }
  }

  # Return a list containing all the matrices required for a SeaSondeRCS object
  list(
    SSA1 = SSA1,
    SSA2 = SSA2,
    SSA3 = SSA3,
    CS12 = CS12,
    CS13 = CS13,
    CS23 = CS23,
    QC   = QC
  )
}


seasonder_readSeaSondeRCSSYFile <- function(filepath, specs_path = seasonder_defaultSpecsFilePath("CSSY"), endian = "big"){

  # Set up error handling parameters with function name, error class, and file path
  conditions_params <- list(
    calling_function = "seasonder_readSeaSondeRCSSYFile",
    class = "seasonder_read_cs_file_error",
    seasonder_cs_filepath = filepath
  )

  # Retrieve YAML specifications for the key size block from the CSSY specifications file
  specs <- seasonder_readYAMLSpecs(specs_path)

  # Attempt to open the file in binary mode ("rb") with warnings suppressed
  connection <- rlang::try_fetch(
    suppressWarnings(file(filepath, "rb")),
    error = function(e) {
      # Abort if the file connection cannot be opened, including the error message
      rlang::inject(
        seasonder_logAndAbort(
          glue::glue("Could no open connection to file {filepath %||% ''}. Reason: {conditionMessage(e)}."),
          !!!conditions_params,
          parent = e
        )
      )
    }
  )
  # Ensure the file connection is closed when the function exits
  on.exit(close(connection), add = TRUE)


  specs_key_size <- purrr::chuck(specs, "key_size_block")


  file_key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)

  file_specs <- specs %>% purrr::chuck(file_key$key)

  header_key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)

  header_specs <- file_specs %>% purrr::chuck(header_key$key)




  header <- seasonder_readCSSYHeader(connection, header_specs,endian, specs_key_size = specs_key_size)

  dbRef <- header$dbrf$dBmReference

  header <- seasonder_CSSY2CSHeader(header)  # Transform CSSY header to valid CS header
  body_key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)

  body_specs <- file_specs %>% purrr::chuck(body_key$key)





  body <- seasonder_readCSSYBody(connection, body_specs, size = body_key$size, dbRef = dbRef, endian = endian, specs_key_size = specs_key_size)

  data <- seasonder_CSSY2CSData(body)

  cs_list <- list(header = header , data = data)

  out <- seasonder_createSeaSondeRCS.list(cs_list)

  return(out)
}
