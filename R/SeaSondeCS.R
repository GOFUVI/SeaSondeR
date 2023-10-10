#' @export
create_SeaSondeCS <- function(){

}

#' @export
seasonder_readSeaSondeCSFile <- function(filepath){


  # Crear conexión y manejar endianidad
  ...

  # Obtener especificaciones y comprobar versión




  # Leer encabezado
  header <- seasonder_readSeaSondeCSFileHeader(connection)

  # Leer datos (podrías tener otro helper para esto)
  data <- seasonder_readSeaSondeCSFileData(connection, header)

  # Retornar lista con encabezado y datos
  list(header = header, data = data)

}



seasonder_readSeaSondeCSFileData <- function(connection,header){

}



seasonder_int_to_raw <- function(x){

  out <- as.raw(packBits(c(t(matrix(as.integer(strsplit(bit64::as.bitstring(bit64::as.integer64(x)),"")[[1]]),ncol=8,byrow = T)[,8:1])),"raw"))

  out

}

#' Convert a Raw Vector to a 64-bit Integer
#'
#' This function converts a raw vector to a 64-bit integer,
#' handling both signed and unsigned conversions.
#'
#' @param r A raw vector to be converted.
#' @param signed Logical, indicating whether the conversion should consider the value as signed (default is FALSE for unsigned).
#' @return A 64-bit integer representation of the raw vector.
#' @examples
#' r <- as.raw(c(0x12,0x34,0x56,0x78,0x90,0xAB,0xCD,0xEF))
#' seasonder_raw_to_int(r, signed=TRUE)
seasonder_raw_to_int <- function(r,signed=F){
  # Convert raw values to bits and collapse into a single bit string.
  bit_str <- sapply(r,FUN = function(x) rev(rawToBits(x))) %>% as.integer() %>% paste0(collapse="")
  class(bit_str) <-"bitstring"

  # Convert the bit string into a 64-bit integer.
  int_val <-   bit64::as.integer64(bit_str)

  return(int_val)

}



#' Read a CSField from a Binary Connection
#'
#' This function reads specific data types from a binary connection,
#' supporting various types including integer, float, double, complex, and strings.
#'
#' @param con A connection object to a binary file.
#' @param type A character string identifying the type of data to read.
#' @param endian A character string indicating the byte order. Options are "big" and "little" (default is "big").
#' @return The value read from the connection.
#' @examples
#' con <- rawConnection(as.raw(c(0x12)))
#' seasonder_readCSField(con, "UInt8")
#' @seealso seasonder_raw_to_int For converting raw to 64-bit integers.
seasonder_readCSField <- function(con, type, endian="big") {

  # Check if the connection is open. If not, raise an error.
  open_con <- try(isOpen(con),silent = T)
  if (!inherits(open_con,"try-error")) {
    if(!open_con){
      seasonder_logAndAbort("Connection is not open.")
    }
  }else{
    seasonder_logAndAbort("Connection is not open.")
  }

  # Helper function to read values from the connection. This function takes care of potential
  # reading errors and uses a specific format and byte size to read the values.
  read_values <- function(bytes, format, n=1) {
    res <- tryCatch(readBin(con, what=format, n=n, size=bytes, endian=endian),
                    error = function(e) {
                      seasonder_logAndMessage("Error while reading value.", "error")
                      NULL
                    })
    return(res)
  }



  # Check if it's CharN
  if (grepl("^Char[0-9]+$", type)) {

    # Read N characters
    char_length <- as.integer(sub("^Char", "", type))


    chars <- read_values(1, "raw",char_length)
    out <- rawToChar(chars)
    return(out)
  }

  # Determine the data type specified and read it from the connection accordingly.
  # If the data type is not recognized, raise an error.
  switch(type,
         "UInt8"   = as.integer(read_values(1, "raw")),
         "SInt8"   = as.integer(read_values(1, "integer")),
         "UInt16"  = as.integer(read_values(2, "int")),
         "SInt16"  = as.integer(read_values(2, "int")),
         "UInt32"  = as.integer(read_values(4, "int")),
         "SInt32"  = as.integer(read_values(4, "int")),
         "Float"   = as.numeric(read_values(4, "double")),
         "Double"  = as.numeric(read_values(8, "double")),
         "UInt64"  = {
           v <- read_values(1, "raw", n=8)
           seasonder_raw_to_int(v,signed=FALSE)
         },
         "SInt64"  = {
           v <- read_values(1, "raw", n=8)
           seasonder_raw_to_int(v,signed=TRUE)
         },
         "Complex" = {
           # Read real and imaginary parts separately and then combine them into a complex number.
           real_part <- as.numeric(read_values(4, "double"))
           imag_part <- as.numeric(read_values(4, "double"))
           complex(real = real_part, imaginary = imag_part)
         },
         "String"  = {
           # Read characters until a null terminator (0) is encountered.
           chars <- NULL
           repeat {
             char <- read_values(1, "raw")
             if (char == as.raw(0)) break
             chars <- c(chars, char)
           }

           rawToChar(do.call(c, list(chars)))
         },
         {
           # Handle unrecognized data types.
           seasonder_logAndMessage(glue::glue("Type Unknown: '{type}'."),"error")
           return(NULL)
         }
  )
}

#' Read and Quality Control a Single Field
#'
#' This auxiliary function reads a field from a binary file using a provided specification and
#' applies a quality control function on the retrieved data. The expectations and functioning of the
#' quality control functions are described in detail in the documentation for `seasonder_readSeaSondeCSFileBlock`.
#'
#' @param field_spec A list containing the specifications for the field to read.
#'   It should contain:
#'   * `type`: the type of data to read, passed to `seasonder_readCSField`.
#'   * `qc_fun`: the name of a quality control function. As detailed in `seasonder_readSeaSondeCSFileBlock`,
#'     this function should be present in the shared environment `seasonder_the` and must accept
#'     `field_value` as its first argument, followed by any other arguments specified in `qc_params`.
#'   * `qc_params`: a list of additional parameters to pass to the quality control function. See
#'     `seasonder_readSeaSondeCSFileBlock` for detailed expectations of the QC function behavior.
#' @param connection A connection to the binary file.
#' @param endian A character string indicating the byte order. Options are "big" and "little" (default is "big").
#'
#' @return The value of the field after quality control. Can be the original value, a transformed value,
#'   or NULL if the value fails quality control. The exact behavior of the quality control function, including
#'   the handling of NULL values, is detailed in `seasonder_readSeaSondeCSFileBlock`.
#'
read_and_qc_field <- function(field_spec, connection, endian="big") {
  # Extract the field type from the specifications
  field_type <- field_spec$type
  # Extract the quality control function name from the specifications
  qc_fun_name <- field_spec$qc_fun
  # Extract the quality control parameters from the specifications
  qc_params <- field_spec$qc_params

  # Read the field using the helper function
  field_value <- seasonder_readCSField(connection, field_type, endian=endian)

  # Apply quality control
  # Get the quality control function from the shared environment
  qc_fun <- seasonder_the$qc_functions[[qc_fun_name]]
  # Call the quality control function with the field value and the specified parameters
  field_value_after_qc <- do.call(qc_fun, c(list(field_value), qc_params))

  # Return the value after quality control
  return(field_value_after_qc)
}



#' Read and Apply Quality Control to a Block of Fields
#'
#' Reads a block of fields from a binary file based on provided specifications. Each field is read
#' and then processed with a specified quality control function.
#'
#' @param spec A named list of specifications for fields to read. Each specification should be in
#'   the form:
#'   list(type = "data_type", qc_fun = "qc_function_name", qc_params = list(param1 = value1, ...))
#'   Where:
#'   * `type`: is the data type to read, which will be passed to `seasonder_readCSField`.
#'   * `qc_fun`: is the name of a quality control function. This function should be present in the
#'     shared environment `seasonder_the` and must accept `field_value` as its first argument,
#'     followed by any other arguments specified in `qc_params`.
#'   * `qc_params`: is a list of additional parameters to pass to the quality control function.
#' @param connection A connection to the binary file.
#' @param endian A character string indicating the byte order. Options are "big" and "little" (default is "big").
#'
#' @details
#' The quality control (QC) functions (`qc_fun`) specified within `spec` play a pivotal role in ensuring the
#' reliability of the data that's read. Here's the expected behavior of these QC functions:
#'
#' - **Input**:
#'   * `field_value`: Value of the field that has been read from the binary file using the `seasonder_readCSField` function.
#'   * `...`: Additional parameters specified in `qc_params` that are passed to `qc_fun` for quality control.
#'
#' - **Functioning**:
#'   The QC function receives a read value and performs checks or transformations based on defined rules or parameters.
#'
#'   * **On QC failure**:
#'     - The QC function itself is responsible for determining the action to take. It can log an error, return a default
#'       value, impute the value, and more.
#'     - For critical errors, the QC function could halt the execution. However, note that logging is managed by the QC
#'       function and won't necessarily halt execution in every case.
#'   * **On success**:
#'     The QC function will return the value (either unchanged or transformed).
#'
#' - **Output**:
#'   Value that has been validated or transformed based on quality control rules.
#'
#' - **Additional Notes**:
#'   - The action on QC failure is directly implemented within the QC function.
#'   - Reading errors are managed by the `seasonder_readCSField` function, which returns NULL in the case of an error. It
#'     is up to the QC function to decide what to do if it receives a NULL.
#'
#' @return A named list where each entry corresponds to a field that has been read. Each key is
#'   the field name, and its associated value is the data for that field after quality control.
#'
seasonder_readSeaSondeCSFileBlock <- function(spec, connection,endian="big") {
  # Use purrr::map to apply the read_and_qc_field function to each field specification
  results <- purrr::map(spec, \(field_spec) {
    out <- try(read_and_qc_field(field_spec=field_spec,connection=connection, endian=endian))
    if(inherits(out,"try-error")){
      out <- NULL
    }
    out
  })

  # Return the results
  return(results)
}


seasonder_check_specs <- function(specs, fields){
  fields %>% purrr::walk(\(field){
    if(is.null(purrr::pluck(specs,field))){
      seasonder_logAndAbort(glue::glue("Specifications for field '{field}' not provided"))
    }
  })
}

#' Read SeaSonde File Header (Version 1)
#'
#' Reads the header of a SeaSonde file (Version 1) based on the provided specifications.
#' Transforms the date-time fields and returns the results.
#'
#' @param specs A list containing specifications for reading the file.
#' @param connection Connection object to the file.
#' @param endian Character string specifying the endianness. Default is "big".
#'
#' @return A list with the read and transformed results.
seasonder_readSeaSondeCSFileHeaderV1 <- function(specs, connection, endian = "big", prev_data = NULL) {

  # Step 1: Specification Validation
  # This step ensures that the provided specs contain the necessary information
  # for the fields "nCsFileVersion", "nDateTime", and "nV1Extent".
  seasonder_check_specs(specs, c("nCsFileVersion","nDateTime","nV1Extent"))

  # Step 2: Field Reading
  # This step reads the specified fields from the connection (e.g., file)
  # based on the provided specs and returns the results as a list.
  results <- seasonder_readSeaSondeCSFileBlock(specs, connection, endian)

  # Step 3: Data Transformation
  # The date-time field "nDateTime" is read as an integer. This step converts it
  # to a POSIXct object, using "1904-01-01" as the origin, and sets the time zone to UTC.
  # The reason for the origin "1904-01-01" is specific to SeaSonde data formats.
  results$nDateTime <- as.POSIXct(results$nDateTime, origin="1904-01-01", tz="UTC")

  # Return the final results, including the transformed date-time field.
  return(results)
}


#' Read SeaSonde File Header (Version 2)
#'
#' Reads the header of a SeaSonde file (Version 2) based on the provided specifications.
#'
#' @param specs A list containing specifications for reading the file.
#' @param connection Connection object to the file.
#' @param endian Character string specifying the endianness. Default is "big".
#'
#' @return A list with the read results.
seasonder_readSeaSondeCSFileHeaderV2 <- function(specs, connection, endian = "big", prev_data = NULL) {

  # Step 1: Specification Validation
  # This step ensures that the provided specs contain the necessary information
  # for the fields "nCsKind" and "nV2Extent".
  seasonder_check_specs(specs, c("nCsKind","nV2Extent"))

  # Step 2: Field Reading
  # This step reads the specified fields from the connection (e.g., file)
  # based on the provided specs and returns the results as a list.
  results <- seasonder_readSeaSondeCSFileBlock(specs, connection, endian)

  # Return the read results.
  return(results)
}

#' Read SeaSonde File Header (Version 3)
#'
#' Reads the header of a SeaSonde file (Version 3) based on the provided specifications.
#' Adds nRangeCells, nDopplerCells, and nFirstRangeCell as constant values to the results.
#'
#' @param specs A list containing specifications for reading the file.
#' @param connection Connection object to the file.
#' @param endian Character string specifying the endianness. Default is "big".
#'
#' @return A list with the read results.
#' @export
seasonder_readSeaSondeCSFileHeaderV3 <- function(specs, connection, endian = "big", prev_data = NULL) {

  # Step 1: Specification Validation
  # This step ensures that the provided specs contain the necessary information
  # for the fields "nSiteCodeName" and "nV3Extent".
  seasonder_check_specs(specs, c("nSiteCodeName","nV3Extent"))

  # Step 2: Field Reading
  # This step reads the specified fields from the connection (e.g., file)
  # based on the provided specs and returns the results as a list.
  results <- seasonder_readSeaSondeCSFileBlock(specs, connection, endian)

  # Step 3: Data Addition
  # Add constant values to the results.
  results$nRangeCells <- 31L
  results$nDopplerCells <- 512L
  results$nFirstRangeCell <- 1L

  # Return the final results
  return(results)
}

#' Read SeaSonde File Header (Version 4)
#'
#' Reads the header of a SeaSonde file (Version 4) based on the provided specifications.
#' Transforms the CenterFreq field and returns the results.
#'
#' @param specs A list containing specifications for reading the file.
#' @param connection Connection object to the file.
#' @param endian Character string specifying the endianness. Default is "big".
#'
#' @return A list with the read and transformed results.
#' @export
seasonder_readSeaSondeCSFileHeaderV4 <- function(specs, connection, endian = "big", prev_data = NULL) {

  # Step 1: Specification Validation
  # This step ensures that the provided specs contain the necessary information.
  required_fields <- c("nCoverMinutes", "bDeletedSource", "bOverrideSrcInfo",
                       "fStartFreqMHz", "fRepFreqHz", "fBandwidthKHz", "bSweepUp",
                       "nDopplerCells", "nRangeCells", "nFirstRangeCell",
                       "fRangeCellDistKm", "nV4Extent")
  seasonder_check_specs(specs, required_fields)

  # Step 2: Field Reading
  # This step reads the specified fields from the connection (e.g., file)
  # based on the provided specs and returns the results as a list.
  results <- seasonder_readSeaSondeCSFileBlock(specs, connection, endian)

  # Step 3: Data Transformation
  # Calculate CenterFreq using the provided formula.
  results$CenterFreq <- results$fStartFreqMHz + results$fBandwidthKHz/2 * -2^(results$bSweepUp == 0)

  # Return the final results, including the CenterFreq.
  return(results)
}

#' Read SeaSonde File Header (Version 5)
#'
#' Reads the header of a SeaSonde file (Version 5) based on the provided specifications.
#' Performs applicable transformations and returns the results.
#'
#' @param specs A list containing specifications for reading the file.
#' @param connection Connection object to the file.
#' @param endian Character string specifying the endianness. Default is "big".
#'
#' @return A list with the read and transformed results.
#' @export
seasonder_readSeaSondeCSFileHeaderV5 <- function(specs, connection, endian = "big", prev_data = NULL) {

  # Step 1: Specification Validation
  seasonder_check_specs(specs, c("nOutputInterval", "nCreateTypeCode", "nCreatorVersion", "nActiveChannels",
                                 "nSpectraChannels", "nActiveChanBits", "nV5Extent"))

  # Step 2: Field Reading
  results <- seasonder_readSeaSondeCSFileBlock(specs, connection, endian)

  # Step 3: Data Transformation
  # Convert the bits into a vector of active antennas
  results$ActiveChannels <- which(intToBits(results$nActiveChanBits)[1:32] == TRUE)

  # Return the final results.
  return(results)
}

#' Read Version 6 Block Data
#'
#' This function reads and processes regular and repeated blocks of data
#' based on provided specifications. Regular blocks are read directly, while
#' repeated blocks are processed recursively based on a set of loops provided
#' in the specifications.
#'
#' @param specs A list. Specifications detailing the structure and content of the data blocks.
#'              Contains variable names, types, quality check functions, and other related attributes.
#'              For repeated blocks, a 'repeat' key is added which details the loop structure and
#'              nested specifications.
#' @param connection A connection object. Represents the connection to the data source. It's passed
#'                   to the lower-level reading function.
#' @param endian A character string. Specifies the byte order to be used. Default is "big".
#'               Passed to the lower-level reading function.
#' @param prev_data A list. Previous data or metadata that might be required to inform the reading
#'                  process, such as loop lengths for repeated blocks. Default is NULL.
#' @param remaining_loops A character vector. Details the remaining loops to be processed for
#'                        repeated blocks. Internally used for recursive processing. Default is NULL.
#'                        If provided, it should always be in sync with the repeat specifications.
#' @return A list. Contains the read and processed data based on the provided specifications.
#'         Regular variables are returned at the top level. Repeated blocks are nested lists with
#'         'loop' and 'data' keys detailing the loop variable and corresponding data.
#' @importFrom purrr list_transpose map
#'
#' @export
readV6BlockData <- function(specs, connection, endian="big", prev_data=NULL, remaining_loops=NULL) {

  # browser(expr= "nReceiverModel" %in% names(specs))

  # If there are remaining loops to process, handle the repeated block recursively
  if(length(remaining_loops) > 0) {
    # Get the current loop variable and its repetition count from prev_data
    loop_var <- remaining_loops[1]
    num_repeats <- prev_data[[loop_var]]

    # Update the list of remaining loops by removing the current one
    remaining_loops <- remaining_loops[-1]

    # Initialize a list to store data from repeated blocks
    repeated_data <- vector("list", num_repeats)

    # Recursively call readV6BlockData for each iteration of the current loop
    for (i in seq_len(num_repeats)) {
      repeated_data[[i]] <- readV6BlockData(specs, connection, endian, prev_data, remaining_loops)
    }

    # Transpose the list to group by variable rather than by loop iteration
    repeated_data <- purrr::list_transpose(repeated_data)

    # Add loop information to the data
    repeated_data %<>% purrr::map(\(x) list(loop = loop_var, data = x))

    return(repeated_data)
  }

  # If there are no remaining loops, handle the regular block
  # Filter out repeated block specifications
  regular_specs <- specs[names(specs) != "repeat"]

  # Initialize an output list
  out <- list()

  # If there are regular specs, read the regular block data
  if (length(regular_specs) > 0) {
    regular_block <- seasonder_readSeaSondeCSFileBlock(regular_specs, connection, endian)
    out <- c(out, regular_block)
  }

  # If the specs contain a "repeat" key, handle the repeated block
  if("repeat" %in% names(specs)) {
    repeat_specs <- specs[["repeat"]]
    remaining_loops <- repeat_specs$how_many

    # Recursively call readV6BlockData for the repeated block
    repeat_result <- readV6BlockData(repeat_specs$what, connection, endian, prev_data, remaining_loops)

    # Merge the repeated block data into the output
    out <- c(out, repeat_result)
  }

  # Return the merged regular and repeated block data
  return(out)
}

#' Read SeaSonde CS File Header V6
#'
#' This function reads the header of a SeaSonde CS File Version 6.
#' It sequentially reads blocks based on the provided specifications and returns the read data.
#'
#' @param specs A list of specifications for reading the file header. It should contain three main elements:
#' `nCS6ByteSize`, `block_spec`, and `blocks`, each containing further specifications for reading various parts of the header.
#' @param connection A connection object to the SeaSonde CS file.
#' @param endian The byte order for reading the file. Default is "big".
#' @param prev_data Previous data, if any, that might affect the current reading. Default is NULL.
#'
#' @return A list containing the read data, organized based on the block keys.
#'
#' @export
seasonder_readSeaSondeCSFileHeaderV6 <- function(specs, connection, endian = "big", prev_data = NULL) {

  # Step 1: Specification Validation
  seasonder_check_specs(specs, c("nCS6ByteSize","block_spec"))

  # Step 2: Field Reading
  nCS6ByteSize <- seasonder_readSeaSondeCSFileBlock(specs["nCS6ByteSize"], connection, endian)$nCS6ByteSize
  results <- list()

  # Continue reading as long as there are bytes left in the CS6 Byte Size
  while (nCS6ByteSize > 0) {
    # Read the block key and block data size
    block <- seasonder_readSeaSondeCSFileBlock(specs$block_spec, connection, endian)

    # If the block key is recognized:
    if (!is.null(specs$blocks[[block$nBlockKey]])) {
      # Read block data using readV6BlockData
      block_data <- readV6BlockData(specs$blocks[[block$nBlockKey]], connection, endian, prev_data)

      # Apply transformations if they exist
      if (!is.null(seasonder_the$transform_functions[[block$nBlockKey]])) {
        block_data <- seasonder_the$transform_functions[[block$nBlockKey]](block_data)
      }

      # Store the results
      results[[block$nBlockKey]] <- block_data
    } else {
      # If the block key is not recognized, skip bytes as per the block data size
      seek(connection, block$nBlockDataSize, origin = "current")
    }

    # Subtract the current read size from nCS6ByteSize
    nCS6ByteSize <- nCS6ByteSize - 8 - block$nBlockDataSize
  }

  # Return the results
  return(results)
}




##' Process a Specific Version of the SeaSonde File Header
#'
#' This function processes a specified version of the SeaSonde file header. It identifies the
#' appropriate header function for the given version, processes the header, and then updates
#' the accumulating pool of header data. Specifically:
#'
#' 1. For fields in the current header that overlap with the accumulated pool, the
#'    current header's values overwrite those in the pool.
#' 2. Fields that are unique to the current header are appended to the pool.
#'
#' @section Assumptions:
#' This function assumes that the desired version-specific `seasonder_readSeaSondeCSFileHeaderV*`
#' functions are available in the global environment.
#'
#' @param pool List. An accumulating list of processed headers from prior versions.
#' @param version Integer. The specific version of the header to be processed. E.g., for version 3,
#'        the function `seasonder_readSeaSondeCSFileHeaderV3` should be present.
#' @param specs List. Header specifications for each version. Each entry should correspond to
#'        a version number and contain the required information to process that version's header.
#' @param connection Connection object. The file connection pointing to the SeaSonde file.
#' @param endian Character string. Specifies the byte order for reading data. Can be "big" (default)
#'        or "little". Use the appropriate value depending on the system architecture and the
#'        file's source.
#'
#' @return List. A combination of the initial `pool` and the processed header for the given `version`.
#'         Fields in the current header will overwrite or append to the pool as described above.
#'
process_version_header <- function(pool, version, specs, connection, endian = "big", prev_data = NULL) {
  # Construct the function name based on the provided version
  function_name <- paste0("seasonder_readSeaSondeCSFileHeaderV", version)

  # Access the appropriate function from the global environment
  header_function <- get(function_name)

  # Process the current version header
  current_header <- header_function(specs[[paste0("V", version)]], connection, endian,  prev_data = prev_data)

  # Overwrite overlapping fields from the pool with the current header
  overlapping_keys <- intersect(names(pool), names(current_header))
  pool[overlapping_keys] <- current_header[overlapping_keys]

  # Add new fields from the current header to the pool
  new_keys <- setdiff(names(current_header), names(pool))
  pool <- c(pool, current_header[new_keys])

  return(pool)
}


#' Read the SeaSonde CS File Header
#'
#' This function reads and processes the header of a SeaSonde CS file. It initially reads
#' the general header (Version 1) to determine the file version. Subsequent headers are processed
#' based on the file version.
#'
#' @param specs List of header specifications for each version.
#' @param connection The file connection.
#' @param endian Character string indicating the byte order, either "big" (default) or "little".
#'
#' @return A combined list of all processed headers up to the file version.
#'
seasonder_readSeaSondeCSFileHeader <- function(specs, connection, endian = "big") {
  # Read the general header (Version 1)
  header_v1 <- seasonder_readSeaSondeCSFileHeaderV1(specs$V1, connection, endian)

  # Extract the file version to determine subsequent headers to process
  file_version <- header_v1$nCsFileVersion

  # Create a list of header versions to process
  versions_to_process <- 2:file_version

  # Reduce the list of versions to process them sequentially
  header_pool <- purrr::reduce(versions_to_process, \(pool, version){

    out <- process_version_header(pool = pool, version = version, specs = specs, connection = connection, endian = endian, prev_data = pool)

    out
  }, .init = header_v1)

  return(header_pool)
}


#### Transform functions ####

seasonder_the$transform_functions <- list()

seasonder_the$transform_functions[["TIME"]] <- function(x) {
  x$nTimeMark %<>% factor(levels=c(0L,1L,2L),labels=c("start","center time","end time"))
  x
}

seasonder_the$transform_functions[["RCVI"]] <- function(x) {
  x$nReceiverModel %<>% factor(levels=c(0L,1L,2L,3L,4L,5L),labels=c("Unknown", "Awg3/Rcvr2 Chassis AC", "Awg3/Rcvr2 Chassis DC", "AllInOne", "Awg4 Chassis AC","Awg4 Chassis DC"))
  x$nRxAntennaModel %<>% factor(levels=c(0L,1L,2L,3L,4L,5L),labels=c("Unknown", "Box Loops","2","3", "Dome Loops","TR Dome Loops"))

  x
}


seasonder_the$transform_functions[["GLRM"]] <- function(x) {
  x$nReceiverModel %<>% factor(levels=c(0L,1L,2L,3L,4L),labels=c("Off","Point","Range","Range&Point", "SubDCOnly"))
  x
}


seasonder_the$transform_functions[["SUPI"]] <- function(x) {
  x$nMethod %<>% factor(levels=c(0L,1L),labels=c("Off","Normal"))
  x$nMode %<>% factor(levels=c(0L,1L,2L,3L),labels=c("Light","Heavy","MaxLight","MaxHeavy"))
  x$nDebugMode %<>% factor(levels=c(0L,1L),labels=c("Off","On"))
  x
}



seasonder_the$transform_functions[["FWIN"]] <- function(x) {

  x$nRangeWindowType %<>% factor(levels=c(0L,1L,2L,3L),labels=c("None","Blackman", "Hamming", "Tukey"))
  x$nDopplerWindowType %<>% factor(levels=c(0L,1L,2L,3L),labels=c("None","Blackman", "Hamming", "Tukey"))
  x
}

seasonder_the$transform_functions[["IQAP"]] <- function(x) {

  x$nRangeWindowType %<>% factor(levels=c(0L,1L,2L),labels=c("Off","Measured","Corrected"))

  x
}

seasonder_the$transform_functions[["FILL"]] <- function(x) {

  x$nRangeMethod %<>% factor(levels=c(0L,1L,2L),labels=c("None", "Linear", "FFTPadding"))
  x$nDopplerMethod %<>% factor(levels=c(0L,1L,2L),labels=c("None", "Linear", "FFTPadding"))

  x
}

seasonder_the$transform_functions[["BRGR"]] <- function(x) {

  x$nBraggReject$data %<>% factor(levels=c(0L,1L,2L,3L),labels=c("OK", "RejectNegBragg", "RejectPosBragg", "RejectBoth"))


  x
}


#### QC functions ####

seasonder_the$qc_functions <- list()

#' Quality Control - Check Type
#'
#' This function verifies if a given value is of the expected type.
#'
#' @param field_value The value whose type needs to be checked.
#' @param expected_type The expected type of the field_value.
#'
#' @return The original field_value if it matches the expected_type; otherwise, an error is raised.
#'
#'
#' @examples
#' qc_check_type(100L, "integer") # Returns 100L
#' qc_check_type("100", "integer") # Raises error
qc_check_type <- function(field_value, expected_type) {
  if (!inherits(field_value, expected_type)) {
    seasonder_logAndAbort(glue::glue("QC Error: Value does not have the expected type: {expected_type}"))
  }
  return(field_value)
}




#' Quality Control - Check Range and Type
#'
#' This function verifies if a given value lies within a specified range
#' and matches the expected type, if provided.
#'
#' @param field_value The value to be checked.
#' @param min Minimum allowable value for field_value.
#' @param max Maximum allowable value for field_value.
#' @param expected_type (optional) The expected type of the field_value. Default is NULL.
#'
#' @return The original field_value if it's within range and matches the expected_type; otherwise, an error is raised.
#'
#'
#' @examples
#' qc_check_range(15L, 1, 32, "integer") # Returns 15L
#' qc_check_range(40L, 1, 32, "integer") # Raises error
#' qc_check_range("15", 1, 32, "integer") # Raises error
#' qc_check_range(15L, 1, 32) # Returns 15L
qc_check_range <- function(field_value, min, max, expected_type = NULL) {
  # Si se proporciona un tipo esperado, verifica el tipo antes de comprobar el rango
  if (!is.null(expected_type)) {
    field_value <- qc_check_type(field_value, expected_type)
  }

  if (field_value < min || field_value > max) {
    seasonder_logAndAbort(glue::glue("QC Error: Value out of range. Expected between {min} and {max}"))
  }
  return(field_value)
}

seasonder_load_qc_functions <- function(){

  seasonder_the$qc_functions[["qc_check_type"]] <- qc_check_type
  seasonder_the$qc_functions[["qc_check_range"]] <- qc_check_range

}
seasonder_load_qc_functions()
