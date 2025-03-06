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

seasonder_readCSSYFields <- function(connection, specs, endian, parent_key){
  variable_char_types <- purrr::map_lgl(specs, \(x) x$type == "CharX")
  if(any(variable_char_types)){
    variable_char_types_index <- which(variable_char_types)
    specs <- purrr::reduce(variable_char_types_index, \(spcs_so_far, i){
      spcs_so_far[[i]]$type <-  glue::glue("Char{size}", size = parent_key$size)
      spcs_so_far
    }, .init = specs )

  }
  out <-  seasonder_readSeaSondeCSFileBlock(specs, connection, endian)

  return(out)

}

seasonder_readBodyRangeCell <- function(connection, specs, endian, specs_key_size = NULL){
  indx_read <- FALSE
  out <- list()

  while(TRUE){
    key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)
    if(!key$key %in% names(specs)){
      seek(connection,key$size,origin = "current")

    }else if(key$key == "END "){
      break
    }else if(key$key == "indx" && indx_read){
      seek(connection,-8,origin = "current")
      break
    }else if(key$key %in% c("cs1a","cs2a","cs3a","c13r","c13i","c23r","c23i","c12r","c12i", "csqf")){

      out <- append(out,list(seasonder_read_reduced_encoded_data(connection, key, endian))  %>% magrittr::set_names(key$key))

    }else if(key$key %in% c("csgn")){
      out <- append(out,list(seasonder_read_csign(connection, key))  %>% magrittr::set_names(key$key))



    }else if(key$key %in% c("asgn")){
      out <- append(out,list(seasonder_read_asign(connection, key))  %>% magrittr::set_names(key$key))



    }else{
      out <- append(out,
                    list(seasonder_readCSSYFields(connection, purrr::chuck(specs, key$key), endian, parent_key = key )) %>% magrittr::set_names(key$key)
      )
      if(key$key == "indx"){
        indx_read <- TRUE
      }

    }
  }
  return(out)
}

seasonder_readCSSYBody <- function(connection, specs, size, endian = "big", specs_key_size = NULL){
  end_point <- seek(connection) + size

  out <- list()
  while(seek(connection) < end_point){
    out <- append(out, list(seasonder_readBodyRangeCell(connection, specs, endian, specs_key_size = specs_key_size)))
  }

  return(out)
}

seasonder_readCSSYHeader <- function(connection, current_specs, endian, parent_key = NULL, keys_so_far =c("CSSY","HEAD"), specs_key_size = NULL){
  out <- list()

  has_subkeys <- !all(purrr::map_lgl(current_specs, \(x)"type" %in% names(x)))
  if(!has_subkeys){

    out <- seasonder_readCSSYFields(connection, current_specs, endian, parent_key)

  }else{


    key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)
    if(!key$key %in% names(current_specs) && !key$key %in% keys_so_far){
      browser()
      seek(connection,key$size,origin = "current")

    }else if(!key$key %in% names(current_specs) && key$key %in% keys_so_far){
      seek(connection,-8,origin = "current")
      return(out)

    }else{

      keys_so_far <- unique(c(keys_so_far, names(current_specs)))
      if(key$key == "cs4h"){

        CSHSpecs <- seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CS"),"header")
        out <-  list(seasonder_readSeaSondeCSFileHeader(CSHSpecs, connection, endian)) %>% magrittr::set_names(key$key)
      }else{

        out <- list(seasonder_readCSSYHeader(connection, purrr::chuck(current_specs, key$key), endian, parent_key = key, keys_so_far = keys_so_far)) %>% magrittr::set_names(key$key)

      }



    }

    out <- c(out,seasonder_readCSSYHeader(connection, current_specs, endian, keys_so_far = keys_so_far))
  }


  return(out)
}

seasonder_readSeaSondeCSSYFile <- function(filepath, specs_path = seasonder_defaultSpecsFilePath("CSSY"), endian = "big"){

  # Set up error handling parameters with function name, error class, and file path
  conditions_params <- list(
    calling_function = "seasonder_readSeaSondeCSSYFile",
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

  body_key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)

  body_specs <- file_specs %>% purrr::chuck(body_key$key)




  body <- seasonder_readCSSYBody(connection, body_specs, size = body_key$size, endian, specs_key_size = specs_key_size)
  browser()


  return(out)
}
