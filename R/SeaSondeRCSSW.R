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
#' @return A named list of 3 vectors, each containing bits as integers (0 or 1) for self spectra sign data.
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
#' @examples
#' # Example usage:
#' con <- rawConnection(as.raw(c(0xFF, 0x00, 0xAA, 0x55, 0xCC, 0x33, 0x77, 0x88, 0x99)))
#' key <- list(size = 9, key = "asign")
#' result <- SeaSondeR:::seasonder_CSSW_read_asign(con, key)
#' print(result)
#' close(con)
seasonder_CSSW_read_asign <- function(connection, key) {
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
  result <- stats::setNames(vector("list", length(group_names)), group_names)

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

#' Read CSSW Fields
#'
#' Processes a block of keys from the binary connection according to provided specifications.
#'
#' @param connection A binary connection.
#' @param specs A list specifying the expected keys.
#' @param endian A character indicating byte order.
#' @param parent_key Optional parent key information.
#' @return A named list as returned by seasonder_readSeaSondeCSFileBlock consistent with the provided specifications.
#'
#' @examples
#' # Example usage: read a single UInt8 field
#' con <- rawConnection(as.raw(c(5)))
#' specs <- list(
#'   field1 = list(
#'     type = "UInt8",
#'     qc_fun = "qc_check_unsigned",
#'     qc_params = list()
#'   )
#' )
#' result <- SeaSondeR:::seasonder_readCSSWFields(con, specs, endian = "big")
#' print(result)
#' close(con)
seasonder_readCSSWFields <- function(connection, specs, endian, parent_key= NULL){
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


#' Apply Scaling to SeaSondeRCSSW Data
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
#'        from a binary CSSW values block.
#' @param fmax A numeric value representing the maximum scaling value. Used to compute the linear scaling factor.
#' @param fmin A numeric value representing the minimum scaling value. Acts as an offset for the scaling.
#' @param fscale A numeric value representing the scaling factor. Must not be zero as it determines the divisor in the scaling formula.
#' @param dbRef A numeric value representing the decibel reference to be added before the voltage conversion step.
#' @param computeVoltage A logical value indicating whether to compute the voltage from the scaled values. If FALSE, it returns the intermediate scaled values.
#'
#' @return A list with the same structure as `values`, where each numeric vector has been transformed to a vector of floating point
#' voltage values. Special integer values equal to 0xFFFFFFFF are converted to NaN.
#'
#' @examples
#' # Example usage:
#' values <- list(c(1000, 0xFFFFFFFF, 2000))
#' # Use triple colon to call internal function
#' scaled <- SeaSondeR:::seasonder_SeaSondeRCSSWApplyScaling(values, fmax = 5, fmin = 0, fscale = 1000,
#' dbRef = -20)
#' print(scaled)
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
seasonder_SeaSondeRCSSWApplyScaling <- function(values, fmax, fmin, fscale, dbRef, computeVoltage = TRUE) {

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
if(computeVoltage){
        # Convert the intermediate value to voltage using the decibel conversion formula
        voltage <- 10^((intermediate + dbRef) / 10)

        # Return the computed voltage value
        return(voltage)
}else{
  return(intermediate)
}
      }
    })
  })

  # Return the list containing the scaled voltage vectors, preserving the structure of 'values'
  return(scaled_values)
}




#' Read a Body Range Cell and Apply Scaling if Required
#'
#' This function processes a block of keys from a binary connection according to a provided specification
#' ('specs'). Each key is interpreted by reading it with \code{seasonder_readSeaSondeCSFileBlock} and processing
#' it based on its key name. The key processing follows these rules:
#'
#' - **Scaling Block ('scal')**: Reads scaling parameters (fmax, fmin, fscale, dbRef) using
#'   \code{seasonder_readCSSWFields} and stores them for later use.
#'
#' - **Reduced Data Blocks (e.g., 'cs1a', 'cs2a', 'cs3a', 'c13m', 'c13a', etc.)**:
#'   Reads the block using \code{seasonder_read_reduced_encoded_data}. If scaling parameters were set by a
#'   preceding 'scal' block, the raw data is converted to voltage values using \code{seasonder_SeaSondeRCSSWApplyScaling};
#'   otherwise, the raw data is returned.
#'
#' - **Other Keys (e.g., 'csgn' and 'asgn')**: These keys invoke their specialized read functions for processing.
#'
#' The function continues reading keys until it detects the 'END ' marker or a repeated 'indx' key, which signals
#' the end of the block.
#'
#' @param connection A binary connection from which keys and data are read.
#' @param specs A list defining the expected keys and their formats.
#' @param dbRef A numeric value providing the dB reference used in scaling.
#' @param endian A string specifying the byte order ("big" or "little"). Defaults to "big".
#' @param specs_key_size Optional specification for the key size block.
#'
#' @return A list with elements named after the keys read. For reduced data blocks, each element contains either
#'         the raw decoded data or the scaled voltage values if a 'scal' block had been applied.
#'
#' @examples
#' # Example: use real specifications with a minimal raw cell
#' spec_file <- SeaSondeR:::seasonder_defaultSpecsFilePath("CSSW")
#' specs_key_size <- SeaSondeR:::seasonder_readYAMLSpecs(spec_file, "key_size_block")
#' body_specs <- SeaSondeR:::seasonder_readYAMLSpecs(spec_file, c("CSSW", "BODY"))
#' # Build a minimal raw cell: 'END ' marker and zero payload size
#' raw_data <- c(charToRaw("END "), as.raw(c(0, 0, 0, 0)))
#' con <- rawConnection(raw_data)
#' result <- SeaSondeR:::seasonder_readCSSWBodyRangeCell(
#'   con,
#'   body_specs,
#'   dbRef = -20,
#'   endian = "big",
#'   specs_key_size = specs_key_size
#' )
#' print(result)
#' close(con)
seasonder_readCSSWBodyRangeCell <- function(connection, specs, dbRef, endian = "big", specs_key_size = NULL){
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
      scaling_params <- seasonder_readCSSWFields(connection, purrr::chuck(specs, key$key),
                                                 endian, parent_key = key)
      out <- append(out, list(scaling_params) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("cs1a", "cs2a", "cs3a", "c13m", "c13a", "c23m", "c23a", "c12m", "c12a", "csqf")){
      # Reduced data block: read the block; apply scaling if parameters are available
      data_block <- seasonder_read_reduced_encoded_data(connection, key, endian)
      if(!is.null(scaling_params)){
        data_block <- seasonder_SeaSondeRCSSWApplyScaling(list(data_block),
                                                          fmax = scaling_params$fmax,
                                                          fmin = scaling_params$fmin,
                                                          fscale = scaling_params$fscale,
                                                          dbRef = dbRef,
                                                          computeVoltage = !key$key %in% c("c13a","c12a","c23a"))[[1]]
      }
      out <- append(out, list(data_block) %>% magrittr::set_names(key$key))
    } else if(key$key %in% c("asgn")){
      # Process self spectra sign information
      out <- append(out, list(seasonder_CSSW_read_asign(connection, key)) %>% magrittr::set_names(key$key))
    } else {
      # For all other keys, process them as simple field blocks
      out <- append(out, list(seasonder_readCSSWFields(connection, purrr::chuck(specs, key$key),
                                                       endian, parent_key = key)) %>%
                      magrittr::set_names(key$key))
      if(key$key == "indx"){
        indx_read <- TRUE
      }
    }
  }

  return(out)
}


#' Read CSSW Body
#'
#' Reads the body section of a CSSW file, processing each cell block until the designated endpoint.
#'
#' @param connection A binary connection from which the body is read.
#' @param specs A list specifying the body keys and formats.
#' @param size The total number of bytes to read for the body section.
#' @param dbRef Numeric decibel reference used for scaling.
#' @param endian A character specifying byte order.
#' @param specs_key_size Optional specification for the key size block.
#' @return A list of processed body cells with applied sign corrections.
#'
#' @examples
#' # Example: read one minimal cell using real CSSW specifications
#' spec_file <- SeaSondeR:::seasonder_defaultSpecsFilePath("CSSW")
#' specs_key_size <- SeaSondeR:::seasonder_readYAMLSpecs(spec_file, "key_size_block")
#' body_specs <- SeaSondeR:::seasonder_readYAMLSpecs(spec_file, c("CSSW", "BODY"))
#' # Build a minimal raw cell: 'END ' marker and zero payload size
#' raw_data <- c(charToRaw("END "), as.raw(c(0, 0, 0, 0)))
#' con <- rawConnection(raw_data)
#' result <- SeaSondeR:::seasonder_readCSSWBody(
#'   con,
#'   body_specs,
#'   size = length(raw_data),
#'   dbRef = -20,
#'   endian = "big",
#'   specs_key_size = specs_key_size
#' )
#' print(result)
#' close(con)
seasonder_readCSSWBody <- function(connection, specs, size, dbRef, endian = "big", specs_key_size = NULL){

  end_point <- seek(connection) + size

  out <- list()
  while(seek(connection) < end_point){

    out <- append(out, list(seasonder_readCSSWBodyRangeCell(connection, specs,  dbRef,endian = endian, specs_key_size = specs_key_size)))

  }
  out <- seasonder_applyCSSWSigns(out)
  return(out)
}

#' Read CSSW Limits
#'
#' Reads a specified number of 32-bit unsigned integers from a binary connection and reshapes them into a matrix representing CSSW limits.
#'
#' @param connection A binary connection.
#' @param n_values The number of 32-bit unsigned integers to read.
#' @param endian A string specifying byte order ("big" or "little").
#' @return A numeric matrix with four columns: LeftBraggLeftLimit, LeftBraggRightLimit, RightBraggLeftLimit, and RightBraggRightLimit.
#'
#' @examples
#' # Example usage:
#' con <- rawConnection(as.raw(rep(0x01, 16)))
#' lims <- seasonder_readCSSWLims(con, 4, endian = "big")
#' print(lims)
#' close(con)
seasonder_readCSSWLims <- function(connection, n_values, endian = "big") {

  # Read n_values of 32-bit unsigned integers
  uint_values <- readBin(connection, what = "integer", n = n_values, size = 4, endian = endian, signed = FALSE)

  lims_matrix <- matrix(uint_values, ncol = 4, byrow = TRUE)
  colnames(lims_matrix) <- c("LeftBraggLeftLimit", "LeftBraggRightLimit", "RightBraggLeftLimit", "RightBraggRightLimit")
  return(lims_matrix)
}

#' Read CSSW File Header
#'
#' This function reads the header section of a CSSW file from a binary connection. The CSSW file header
#' contains a set of key blocks formatted according to the SeaSonde CSSW specification. The header section
#' is processed recursively and terminates when one of the following conditions is met:
#' \itemize{
#'   \item A key with name "BODY" is encountered. In this case, the connection is rewound by 8 bytes
#'         to allow subsequent processing of the body.
#'   \item A key that is not defined in \code{current_specs} but is already present in the
#'         \code{keys_so_far} vector is encountered (indicative of repeated keys), which triggers termination.
#' }
#'
#' When no subkeys are specified in \code{current_specs} (i.e. \code{current_specs} comprises only
#' simple field definitions), the function delegates the processing to \code{seasonder_readCSSWFields}.
#'
#' @param connection A binary connection from which to read the CSSW file header.
#' @param current_specs A list representing the specification for the header; may contain nested subkeys.
#' @param endian A character string indicating the byte order for reading numeric values ("big" or "little").
#' @param parent_key (Optional) A list with information from the parent key block, used when processing nested keys.
#' @param keys_so_far A character vector of keys already processed, used to avoid recursive loops. Defaults to c("CSSW", "HEAD").
#' @param specs_key_size A specification for reading the key size block, often obtained from YAML specs.
#'
#' @return A list containing the parsed CSSW header information. The returned list may be empty if a termination
#'         condition is encountered.
#'
#' @details The function processes the CSSW header recursively:
#' \itemize{
#'   \item If \code{current_specs} contains only field definitions, \code{seasonder_readCSSWFields} is called.
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
#' # Example: read the CSSW file header using real specifications
#' spec_file <- SeaSondeR:::seasonder_defaultSpecsFilePath("CSSW")
#' specs_key_size <- SeaSondeR:::seasonder_readYAMLSpecs(spec_file, "key_size_block")
#' header_specs <- SeaSondeR:::seasonder_readYAMLSpecs(spec_file, c("CSSW", "HEAD"))
#' con <- file(system.file("css_data/CSS_TORA_2024_04_04_070000.csr", package = "SeaSondeR"), "rb", raw = TRUE)
#' header <- SeaSondeR:::seasonder_readCSSWHeader(
#'   con,
#'   header_specs,
#'   endian = "big",
#'   specs_key_size = specs_key_size
#' )
#' print(header)
#' close(con)
seasonder_readCSSWHeader <- function(connection, current_specs, endian = "big", parent_key = NULL, keys_so_far = c("CSSW", "HEAD"), specs_key_size = NULL){
  # Initialize an empty output list for accumulating header values
  out <- list()

  # Determine if the current specifications define subkeys or only simple fields
  has_subkeys <- !all(purrr::map_lgl(current_specs, \(x) "type" %in% names(x)))

  if(!has_subkeys){
    # When there are no subkeys, delegate reading to seasonder_readCSSWFields
    out <- seasonder_readCSSWFields(connection, current_specs, endian, parent_key)
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

        out <- seasonder_readCSSWHeader(connection, purrr::chuck(current_specs, key$key), endian, parent_key = key, keys_so_far = keys_so_far, specs_key_size = specs_key_size)

        out$lims <- seasonder_readCSSWLims(connection, out$nRange*4,endian = endian)

        out <- list(out) %>% magrittr::set_names(key$key)

      }else{
        # Recursively process the key using the sub-specs defined in current_specs
        out <- list(seasonder_readCSSWHeader(connection, purrr::chuck(current_specs, key$key), endian, parent_key = key, keys_so_far = keys_so_far, specs_key_size = specs_key_size)) %>% magrittr::set_names(key$key)
      }
    }
    # Continue recursive processing of the header for any remaining keys
    out <- c(out, seasonder_readCSSWHeader(connection, current_specs, endian, keys_so_far = keys_so_far, specs_key_size = specs_key_size))
  }

  return(out)
}


#' Transform CSSW Header to SeaSonde CS Header
#'
#' Extracts the 'cs4h' component from a CSSW header and reorganizes the remaining header information under 'header_csr'.
#'
#' @param header A list representing the CSSW header, which must contain a 'cs4h' component.
#' @return A transformed list representing a valid SeaSonde CS header with embedded CSSW header information.
#'
#' @examples
#' # Example usage:
#' header <- list(cs4h = list(field = 1), someField = 42)
#' cs_header <- SeaSondeR:::seasonder_CSSW2CSHeader(header)
#' print(cs_header)
seasonder_CSSW2CSHeader <- function(header) {
  if (is.null(header$cs4h)) {
    seasonder_logAndAbort("CSSW header does not contain a cs4h component")
  }
  header_cs <- header$cs4h  # Extract the valid CS header
  header_csr <- header    # Copy the original header
  header_csr$cs4h <- NULL  # Remove the cs4h component from the original header
  header_cs$header_csr <- header_csr  # Embed the remaining header
  return(header_cs)
}

#' Transform CSSW Body to SeaSonde CS Data Structure
#'
#' This function converts the body structure of a CSSW file into a list of matrices that conform
#' to the data structure required for creating a SeaSondeRCS object. The conversion is performed
#' by mapping specific fields:
#' \describe{
#'   \item{SSA1, SSA2, SSA3}{Matrices are built using the numeric vectors found in the \code{cs1a}, \code{cs2a}
#'          and \code{cs3a} fields respectively.}
#'   \item{CS12, CS13, CS23}{Each complex cross-spectra matrix is formed by combining the real parts
#'          from \code{c12m}, \code{c13m} and \code{c23m} with the corresponding imaginary parts
#'          from \code{c12a}, \code{c13a} and \code{c23a}.}
#'   \item{QC}{The quality control matrix is obtained directly from the \code{csqf} field.}
#' }
#'
#' Each row in the output matrices corresponds to the index provided by \code{cell$indx$index} in the input list.
#'
#' @param body A list representing the body of a CSSW file. Each element of the list is expected to be a
#'   cell containing the following fields: \code{indx} (which includes an \code{index}), \code{cs1a}, \code{cs2a}, \code{cs3a},
#'   \code{c12m}, \code{c12a}, \code{c13m}, \code{c13a}, \code{c23m}, \code{c23a} and \code{csqf}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{SSA1}{A numeric matrix containing self-spectra from \code{cs1a}.}
#'   \item{SSA2}{A numeric matrix containing self-spectra from \code{cs2a}.}
#'   \item{SSA3}{A numeric matrix containing self-spectra from \code{cs3a}.}
#'   \item{CS12}{A complex matrix formed by pairing \code{c12m} (real) and \code{c12a} (imaginary).}
#'   \item{CS13}{A complex matrix formed by pairing \code{c13m} (real) and \code{c13a} (imaginary).}
#'   \item{CS23}{A complex matrix formed by pairing \code{c23m} (real) and \code{c23a} (imaginary).}
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
#'   # Example with a single cell
#'   cell <- list(
#'     indx  = list(index = 1),
#'     cs1a  = c(1, 2, 3),
#'     cs2a  = c(4, 5, 6),
#'     cs3a  = c(7, 8, 9),
#'     c12m  = c(10, 11, 12),
#'     c12a  = c(13, 14, 15),
#'     c13m  = c(16, 17, 18),
#'     c13a  = c(19, 20, 21),
#'     c23m  = c(22, 23, 24),
#'     c23a  = c(25, 26, 27),
#'     csqf  = c(28, 29, 30)
#'   )
#'   body <- list(cell)
#'   transformed <- seasonder_CSSW2CSData(body)
#'   print(transformed)
#'
#' @export

seasonder_CSSW2CSData <- function(body) {
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
  ncols_CS12 <- get_ncols("c12m")  # assume c12m and c12a have same length
  ncols_CS13 <- get_ncols("c13m")
  ncols_CS23 <- get_ncols("c23m")

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
    if (!is.null(cell$c12m) && !is.null(cell$c12a)) {
      CS12[row, ] <- complex(real = cell$c12m * cos(cell$c12a * pi / 180),imaginary = cell$c12m * sin(cell$c12a * pi / 180))
    }
    if (!is.null(cell$c13m) && !is.null(cell$c13a)) {
      CS13[row, ] <- complex(real = cell$c13m * cos(cell$c13a * pi / 180),imaginary = cell$c13m * sin(cell$c13a * pi / 180))
    }
    if (!is.null(cell$c23m) && !is.null(cell$c23a)) {
      CS23[row, ] <- complex(real = cell$c23m * cos(cell$c23a * pi / 180),imaginary = cell$c23m * sin(cell$c23a * pi / 180))
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

#' Apply CSSW Sign Corrections
#'
#' Applies sign corrections to both cross-spectra and auto-spectra fields within a list of CSSW data cells.
#'
#' @param cs_data A list of CSSW data cells, where each cell may include fields for cross-spectra ('c12m', 'c12a', 'c13m', 'c13a', 'c23m', 'c23a') and auto-spectra ('cs1a', 'cs2a', 'cs3a') signs.
#' @return The modified list of CSSW data cells with sign corrections applied.
#'
#' @examples
#' # Example usage:
#' cs_data <- list(
#'   list(csgn = list(c12m = 1, c12a = 0, c13m = 1, c13a = 0, c23m = 1, c23a = 0),
#'        c12m = c(1,2), c12a = c(0,0),
#'        cs1a = c(3,4))
#' )
#' corrected <- SeaSondeR:::seasonder_applyCSSWSigns(cs_data)
#' print(corrected)
seasonder_applyCSSWSigns <- function(cs_data) {
  for (i in seq_along(cs_data)) {
    cell <- cs_data[[i]]

    # Apply auto-spectra sign correction if 'asgn' exists
    if (!is.null(cell$asgn)) {
      auto_fields <- c("cs1a", "cs2a", "cs3a")
      for (field in auto_fields) {
        if (!is.null(cell[[field]])) {
          asgn <- cell$asgn[[field]] *-2 +1
          cell[[field]] <- cell[[field]] * asgn
        }
      }
    }

    cs_data[[i]] <- cell
  }
  return(cs_data)
}

#' Read SeaSonde RCSSW File and Create SeaSondeRCS Object
#'
#' This function reads a SeaSonde RCSSW file from a specified file path and parses its content
#' into a SeaSondeRCS object. The file is processed by reading its header and body sections using
#' CSSW specifications provided via a YAML file.
#'
#' @param filepath A character string specifying the path to the SeaSonde RCSSW file.
#' @param specs_path A character string specifying the path to the YAML file containing CSSW specifications.
#'   Defaults to the output of \code{seasonder_defaultSpecsFilePath("CSSW")}.
#' @param endian A character string indicating the byte order used in the file. Defaults to \code{"big"}.
#'
#' @return A SeaSondeRCS object containing the parsed header and data.
#'
#' @details
#' The function executes the following steps:
#' \enumerate{
#'   \item Sets up error handling parameters specific to the function.
#'   \item Retrieves YAML specifications for the key size block from the CSSW spec file.
#'   \item Attempts to open the file in binary mode ("rb") with warnings suppressed.
#'   \item Reads the file key and uses it to extract file specs.
#'   \item Reads the header key, retrieves header specs, and parses the CSSW header.
#'   \item Converts the CSSW header into a valid SeaSondeRCS header.
#'   \item Reads the body key, retrieves body specs, and parses the CSSW body.
#'   \item Transforms the CSSW body into a SeaSondeRCS data structure.
#'   \item Combines the header and data into a SeaSondeRCS object.
#' }
#'
#' @examples
#'   # Assuming "path/to/file.rcssy" is a valid SeaSonde RCSSW file and the specifications file exists:
#'   cs_obj <- seasonder_readSeaSondeRCSSWFile("path/to/file.rcssy")
#'
#'   # Inspect the resulting SeaSondeRCS object:
#'   print(attr(cs_obj, "header"))
#'   print(attr(cs_obj, "data"))
#'
seasonder_readSeaSondeRCSSWFile <- function(filepath, specs_path = seasonder_defaultSpecsFilePath("CSSW"), endian = "big"){

  # Set up error handling parameters with function name, error class, and file path
  conditions_params <- list(
    calling_function = "seasonder_readSeaSondeRCSSWFile",
    class = "seasonder_read_cs_file_error",
    seasonder_cs_filepath = filepath
  )

  # Retrieve YAML specifications for the key size block from the CSSW specifications file
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



  header <- seasonder_readCSSWHeader(connection, header_specs,endian, specs_key_size = specs_key_size)

  dbRef <- header$dbrf$dBmReference

  header <- seasonder_CSSW2CSHeader(header)  # Transform CSSW header to valid CS header
  body_key <- seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian)

  body_specs <- file_specs %>% purrr::chuck(body_key$key)





  body <- seasonder_readCSSWBody(connection, body_specs, size = body_key$size, dbRef = dbRef, endian = endian, specs_key_size = specs_key_size)

data <- seasonder_CSSW2CSData(body)

cs_list <- list(header = header , data = data)

out <- seasonder_createSeaSondeRCS.list(cs_list)


  return(out)
}
