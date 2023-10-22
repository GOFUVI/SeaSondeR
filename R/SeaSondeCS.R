


#' @export
create_SeaSondeCS <- function(){

}

#' Validate SeaSondeR CS File Data
#'
#' This function performs multiple validation checks on a provided CS file in the SeaSondeR system.
#' It checks the file for various conditions to determine if it meets the SeaSondeR standards.
#'
#' @param filepath A character string indicating the path to the CS file to validate.
#' @param header A list containing header information of the CS file.
#'
#' @details The function performs the following validation checks:
#' \enumerate{
#'   \item Verifies that the file size is greater than 10 bytes.
#'   \item Validates the `nCsFileVersion` field in the header to ensure it's between 1 and 32.
#'   \item Depending on the `nCsFileVersion`, verifies the appropriate file size, and the extent of various version headers (`nV1Extent`, `nV2Extent`, etc.).
#'   \item Validates the `nRangeCells` and `nDopplerCells` fields to ensure they are within permissible ranges.
#'   \item Depending on the `nCsKind` value, validates the file size against expected sizes based on `nRangeCells`, `nSpectraChannels`, and `nDopplerCells`.
#' }
#'
#' @section Error Management and Conditions:
#'
#' As described abobe, the function validates multiple conditions. Each error condition is associated with a specific error message and an error class \code{seasonder_validate_cs_file_error}.
#' Any violations of these conditions will invoke the \code{\link{seasonder_logAndAbort}} function, logging the error and aborting the process.
#'
#'
#' @return NULL invisibly. The function mainly serves to validate and will stop execution and log an error using `seasonder_logAndAbort` if any condition fails.
#'
#' @seealso \code{\link{seasonder_logAndAbort}}
#' @references Cross Spectra File Format Version 6. CODAR. 2016
seasonder_validateCSFileData <- function(filepath, header) {

  conditions_params <- list(calling_function = "seasonder_validateCSFileData",class = "seasonder_validate_cs_file_error",seasonder_cs_filepath = filepath, seasonder_cs_header = header)

  # Validations
  file_size <- file.info(filepath)$size

  header_size <- header$nV1Extent + 10

  if (file_size <= 10) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file size {file_size} in file {filepath}."),!!!conditions_params))

  if (header$nCsFileVersion < 1 || header$nCsFileVersion > 32) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid nCsFileVersion {header$nCsFileVersion} in file {filepath}."),!!!conditions_params))

  if (header$nCsFileVersion == 1 && (file_size <= 10 || header$nV1Extent < 0)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file for version 1 in file {filepath}. size: {file_size}, header extent V1 {header$nV1Extent}."),!!!conditions_params))

  if (header$nCsFileVersion == 2 && (file_size <= 16 || header$nV1Extent < 6 || header$nV2Extent < 0)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file for version 2  in file {filepath}. size: {file_size}, header extent V1 {header$nV1Extent}, V2 {header$nV2Extent}."),!!!conditions_params))

  if (header$nCsFileVersion == 3 && (file_size <= 24 || header$nV1Extent < 14 || header$nV2Extent < 8 || header$nV3Extent < 0)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file for version 3 in file {filepath}. size: {file_size}, header extent V1 {header$nV1Extent}, V2 {header$nV2Extent}, V3 {header$nV3Extent}."),!!!conditions_params))

  if (header$nCsFileVersion == 4 && (file_size <= 72 || header$nV1Extent < 62 || header$nV2Extent < 56 || header$nV3Extent < 48 || header$nV4Extent < 0)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file for version 4 in file {filepath}. size: {file_size}, header extent V1 {header$nV1Extent}, V2 {header$nV2Extent}, V3 {header$nV3Extent}, V4 {header$nV4Extent}."),!!!conditions_params))

  if (header$nCsFileVersion >= 5 && (file_size <= 100 || header$nV1Extent < 90 || header$nV2Extent < 84 || header$nV3Extent < 76 || header$nV4Extent < 28 || header$nV5Extent < 0)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file for version >= 5 in file {filepath}. size: {file_size}, header extent V1 {header$nV1Extent}, V2 {header$nV2Extent}, V3 {header$nV3Extent}, V4 {header$nV4Extent}, V5 {header$nV5Extent}."),!!!conditions_params))

  if (header$nCsFileVersion >= 6 && header$nV5Extent < header$nCS6ByteSize + 4) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file for version >= 6 in file {filepath}. size: {file_size}, header extent V5 {header$nV5Extent}, V6 {header$nCS6ByteSize}."),!!!conditions_params))


  if (header$nRangeCells <= 0 || header$nRangeCells > 8192 || header$nDopplerCells <= 0 || header$nDopplerCells > 32768) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid nRangeCells or nDopplerCells in file {filepath}. nRangeCells: {header$nRangeCells}, nDopplerCells: {header$nDopplerCells}."),!!!conditions_params))


  if (header$nCsKind == 1 && file_size < (header_size + header$nRangeCells * header$nSpectraChannels * header$nDopplerCells * 36)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file size for nCsKind 1 in file {filepath}. Expected >= {(header_size + header$nRangeCells * header$nSpectraChannels * header$nDopplerCells * 36)}, actual: {file_size}."),!!!conditions_params))

  if (header$nCsKind == 2 && file_size < (header_size + header$nRangeCells * header$nSpectraChannels * header$nDopplerCells * 40)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file size for nCsKind 2 in file {filepath}. Expected >= {(header_size + header$nRangeCells * header$nSpectraChannels * header$nDopplerCells * 40)}, actual: {file_size}."),!!!conditions_params))

  return(NULL)

}

seasonder_skip_cs_file <- function(cond) invokeRestart("seasonder_skip_cs_file",cond)

#' Read SeaSonde Cross Spectra (CS) File
#'
#' This function reads and processes a SeaSonde CS file, extracting both its header and data.
#'
#' @param filepath A character string specifying the path to the SeaSonde CS file.
#' @param specs_path A character string specifying the path to the YAML specifications for the CS file.
#'
#' @details
#' The function starts by establishing a connection to the CS file specified by \code{filepath}.
#' It then reads the necessary metadata and header specifications from the \code{specs_path}.
#' Based on the CS file version determined from its header, it applies specific adjustments
#' to the header data. After processing the header, the function validates the CS file data
#' using \code{\link{seasonder_validateCSFileData}} and then reads the data itself via
#' \code{\link{seasonder_readSeaSondeCSFileData}}.
#'
#' @section Error Management:
#' This function utilizes the `rlang` package to manage errors and provide detailed and structured error messages:
#'
#' \strong{Error Classes}:
#' \itemize{
#'   \item \code{seasonder_read_cs_file_error}: An error class that indicates a general problem when attempting to read the SeaSonde CS file.
#'   \item \code{seasonder_cs_file_skipped}: Condition indicating that the processing of a CS file was skipped due to an error.
#' }
#'
#' \strong{Error Cases}:
#' \itemize{
#'   \item Failure to open a connection to the file.
#'   \item Unsupported version found in the specs file.
#'   \item Any other error that can arise from dependent functions such as `seasonder_readSeaSondeCSFileHeader` and `seasonder_readSeaSondeCSFileData`.
#' }
#'
#' \strong{Restart Options}:
#' This function provides a structured mechanism to recover from errors during its execution using the `rlang::withRestarts` function. The following restart option is available:
#'
#' \describe{
#'   \item{\code{seasonder_skip_cs_file(cond)}}{This allows for the graceful handling of file reading errors. If this restart is invoked, the function will log an error message indicating that the processing of a specific CS file was skipped and will return a list with `header=NULL` and `data=NULL`. The restart takes one argument: \code{cond} (the condition or error that occurred).
#'   \itemize{
#'     \item \strong{Usage}: In a custom condition handler, you can call \code{seasonder_skip_cs_file(cond)} to trigger this restart and skip the processing of the current CS file.
#'     \item \strong{Effect}: If invoked, the function logs an error message detailing the reason for skipping the file and then returns a list with both the header and data set to NULL.
#'   }}
#'}
#' @return A list containing two components:
#' \itemize{
#'   \item \code{header}: A list containing the processed header information of the CS file.
#'   \item \code{data}: A list containing the processed data of the CS file. The structure
#'     of this list depends on the content of the CS file and can contain components such as
#'     `SSA*`, `CSxy`, and `QC`.
#' }
#'
#' @seealso
#' \code{\link{seasonder_validateCSFileData}},
#' \code{\link{seasonder_readSeaSondeCSFileHeader}},
#' \code{\link{seasonder_readSeaSondeCSFileData}},
#' \code{\link{seasonder_logAndMessage}},
#' \code{\link{seasonder_logAndAbort}}
#'
#' @references Cross Spectra File Format Version 6. CODAR. 2016
#' @export
#'
seasonder_readSeaSondeCSFile <- function(filepath, specs_path) {

  conditions_params <- list(calling_function = "seasonder_readSeaSondeCSFile",class="seasonder_read_cs_file_error",seasonder_cs_filepath=filepath,seasonder_cs_specs_path=specs_path)

  withRestarts(
    seasonder_skip_cs_file=function(cond){
      seasonder_logAndMessage(glue::glue("An issue happened while processing the file {cond$seasonder_cs_filepath %||% ''}, skipping. Issue: {conditionMessage(cond)}"),"error",calling_function = "seasonder_readSeaSondeCSFile",class="seasonder_cs_file_skipped",parent=cond)
      return(list(header=NULL,data=NULL))
    },
    {


      connection <- rlang::try_fetch(
        suppressWarnings(file(filepath, "rb")),
        error = function(e) {
          rlang::inject(seasonder_logAndAbort(glue::glue("Could no open connection to file {filepath %||% ''}. Reason: {conditionMessage(e)}."),!!!conditions_params,parent=e))
        }
      )
      on.exit(close(connection), add = TRUE)

      specs_metadata <- seasonder_readYAMLSpecs(specs_path, "metadata")
      specs_header <- seasonder_readYAMLSpecs(specs_path, "header")


      yaml_version <- specs_metadata$version
      if (!(yaml_version %in% seasonder_the$valid_yaml_seasondecs_versions)) {
        rlang::inject(seasonder_logAndAbort(glue::glue("Unsupported version {yaml_version} found in specs file {specs_path}."),!!!conditions_params))
      }

      header <- seasonder_readSeaSondeCSFileHeader(specs_header, connection)

      if (header$nCsFileVersion < 4) {
        header$nRangeCells <- 32
        header$nDopplerCells <- 512
      }

      if (header$nCsFileVersion < 5) {
        header$nSpectraChannels <- 3
      }

      seasonder_validateCSFileData(filepath, header)


      data <-  seasonder_readSeaSondeCSFileData(connection, header)



      return(list(header = header, data = data))

    })

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

#' @export
seasonder_skip_cs_field <- function(cond,value) invokeRestart("seasonder_skip_cs_field",cond,value)

#' Read a CSField from a Binary Connection
#'
#' This function reads specific data types from a binary connection,
#' supporting various types including integer, float, double, complex, and strings.
#'
#' @param con A connection object to a binary file.
#' @param type A character string identifying the type of data to read.
#' @param endian A character string indicating the byte order. Options are "big" and "little" (default is "big").
#' @return The value read from the connection.
#' @importFrom rlang !!!
#' @examples
#' \dontrun{
#' con <- rawConnection(as.raw(c(0x12)))
#' seasonder_readCSField(con, "UInt8")
#' }
#' @seealso seasonder_raw_to_int For converting raw to 64-bit integers.
#'
#'
#' @section Supported Data Types:
#' This function provides support for reading a variety of data types from a binary connection. The following data types are recognized and can be used for the \code{type} argument:
#'
#' \describe{
#'   \item{\code{CharN}}{Reads N characters from the connection where N is a positive integer. For example, \code{Char5} would read five characters.}
#'
#'   \item{\code{UInt8}}{Reads an 8-bit unsigned integer.}
#'
#'   \item{\code{SInt8}}{Reads an 8-bit signed integer.}
#'
#'   \item{\code{UInt16}}{Reads a 16-bit unsigned integer.}
#'
#'   \item{\code{SInt16}}{Reads a 16-bit signed integer.}
#'
#'   \item{\code{UInt32}}{Reads a 32-bit unsigned integer.}
#'
#'   \item{\code{SInt32}}{Reads a 32-bit signed integer.}
#'
#'   \item{\code{Float}}{Reads a single-precision floating-point number.}
#'
#'   \item{\code{Double}}{Reads a double-precision floating-point number.}
#'
#'   \item{\code{UInt64}}{Reads a 64-bit unsigned integer.}
#'
#'   \item{\code{SInt64}}{Reads a 64-bit signed integer.}
#'
#'   \item{\code{Complex}}{Reads a complex number by separately reading the real and imaginary parts, which are each represented as double-precision floating-point numbers.}
#'
#'   \item{\code{String}}{Reads a null-terminated string.}
#'}
#'
#' If the provided \code{type} does not match any of the supported data types, the function raises an error.
#'
#' @section Error Management:
#' This function utilizes the `rlang` package to manage errors and provide detailed and structured error messages:
#'
#' \strong{Error Classes}:
#' \itemize{
#'   \item \code{seasonder_cs_field_reading_error}: General error related to reading a CSField from the binary connection.
#'   \item \code{seasonder_cs_field_skipped}: Condition that indicates a CSField was skipped due to a reading error.
#' }
#'
#' \strong{Error Cases}:
#' \itemize{
#'   \item Connection is not open.
#'   \item Error while reading value from connection.
#'   \item Read value of length 0 from connection (likely reached end of file).
#'   \item Unrecognized data type specified.
#' }
#'
#' \strong{Restart Options}:
#' This function provides a structured mechanism to recover from errors during its execution using the `rlang::withRestarts` function. The following restart option is available:
#'
#' \describe{
#'   \item{\code{seasonder_skip_cs_field(cond, value)}}{This allows for the graceful handling of reading errors. If this restart is invoked, the function will log an error message indicating that a specific CSField reading was skipped and will return the value specified. The restart takes two arguments: \code{cond} (the condition or error that occurred) and \code{value} (the value to return if this CSField reading is skipped). To invoke this restart during a condition or error, you can use the helper function \code{seasonder_skip_cs_field(cond, value)}.
#'   \itemize{
#'     \item \strong{Usage}: In a custom condition handler, you can call \code{seasonder_skip_cs_field(cond, yourDesiredReturnValue)} to trigger this restart and skip the current CSField reading.
#'     \item \strong{Effect}: If invoked, the function logs an error message detailing the reason for skipping, and then returns the value specified in the restart function call.
#'   }}
#'}
seasonder_readCSField <- function(con, type, endian="big") {

  # Parameters used for error messages and logging.
  conditions_params <- list(calling_function = "seasonder_readCSField",class="seasonder_cs_field_reading_error",seasonder_cs_field_type=type,seasonder_cs_endian=endian)

  out <- withRestarts(seasonder_skip_cs_field=function(cond,value){
    # Log the error message and skip the CS field.
    seasonder_logAndMessage(glue::glue("Skipping CS field, returning {value}: {conditionMessage(cond)}"), "error", calling_function = "seasonder_readCSField", class="seasonder_cs_field_skipped", parent=cond, seasonder_cs_field_value=value)
    return(value)
  },
  {
    # Ensure the connection is open before proceeding.
    open_con <- try(isOpen(con), silent = T)
    if (!inherits(open_con, "try-error")) {
      if(!open_con) {
        rlang::inject(seasonder_logAndAbort("Connection is not open.", !!!conditions_params))
      }
    } else {
      rlang::inject(seasonder_logAndAbort("Connection is not open.", !!!conditions_params,parent=attr(open_con,"condition")))
    }

    # Helper function to safely read from the connection.
    read_values <- function(bytes, format, n=1) {
      res <- rlang::try_fetch({
        out <- readBin(con, what=format, n=n, size=bytes, endian=endian)
        # If nothing is read, it could be the end of the file.
        if(length(out) == 0) {
          rlang::abort("Read value of length 0. Possibly reached end of file.")
        }
        out
      },
      error = function(e) {
        rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading value: {conditionMessage(e)}"), !!!conditions_params, parent=e))
      })

      return(res)
    }

    # Process data types that involve reading character strings.
    if (grepl("^Char[0-9]+$", type)) {
      # Extract the number of characters to read.
      char_length <- as.integer(sub("^Char", "", type))
      chars <- read_values(1, "raw", char_length)
      out <- rawToChar(chars)
      return(out)
    }

    # Determine and read the specific data type from the connection.
    switch(type,
           "UInt8" = as.integer(read_values(1, "raw")),
           "SInt8" = as.integer(read_values(1, "integer")),
           "UInt16" = as.integer(read_values(2, "int")),
           "SInt16" = as.integer(read_values(2, "int")),
           "UInt32" = as.integer(read_values(4, "int")),
           "SInt32" = as.integer(read_values(4, "int")),
           "Float" = as.numeric(read_values(4, "numeric")),
           "Double" = as.numeric(read_values(8, "double")),
           "UInt64" = {
             v <- read_values(1, "raw", n=8)
             seasonder_raw_to_int(v, signed=FALSE)
           },
           "SInt64" = {
             v <- read_values(1, "raw", n=8)
             seasonder_raw_to_int(v, signed=TRUE)
           },
           "Complex" = {
             # Read both the real and imaginary components.
             real_part <- as.numeric(read_values(4, "double"))
             imag_part <- as.numeric(read_values(4, "double"))
             complex(real = real_part, imaginary = imag_part)
           },
           "String" = {
             # Keep reading characters until a null terminator is found.
             chars <- NULL
             repeat {
               char <- read_values(1, "raw")
               if (char == as.raw(0)) break
               chars <- c(chars, char)
             }
             rawToChar(do.call(c, list(chars)))
           },
           {
             # Raise an error for unknown data types.
             rlang::inject(seasonder_logAndAbort(glue::glue("Type Unknown: '{type}'."), !!!conditions_params))
           })
  })
  return(out)
}


#' @export
seasonder_rerun_qc_with_fun <- function(cond,qc_fun) invokeRestart("seasonder_rerun_qc_with_fun",cond,qc_fun)

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
#'
#' @return The value of the field after quality control. Can be the original value, a transformed value,
#'   or NULL if the value fails quality control. The exact behavior of the quality control function, including
#'   the handling of NULL values, is detailed in `seasonder_readSeaSondeCSFileBlock`.
#'
#'
#' @section Error Management:
#'
#' The `read_and_qc_field` function uses the `rlang` package to manage errors and conditions
#' in a structured way. This documentation section details the classes of errors/conditions
#' generated, the cases considered, and the restart options provided.
#'
#' \strong {Error/Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_cs_field_skipped}: Condition that indicates a CSField was skipped during reading.
#'   \item \code{seasonder_cs_field_qc_fun_rerun}: Condition that indicates a rerun of the quality control function was triggered.
#'   \item \code{seasonder_cs_field_qc_fun_not_defined_error}: Error raised when the quality control function specified is not found in the shared environment `seasonder_the`.
#'   \item \code{seasonder_cs_field_qc_fun_error}: Error raised when an issue occurs while applying the quality control function.
#' }
#'
#' \strong {Error/Condition Cases}:
#' \itemize{
#'   \item If a CSField is skipped during reading, the condition \code{seasonder_cs_field_skipped} is used to skip QC and then is re-signaled.
#'   \item If an alternate QC is rerun using the \code{seasonder_rerun_qc_with_fun} restart, the condition \code{seasonder_cs_field_qc_fun_rerun} is signaled.
#'   \item If the quality control function specified is not found in the shared environment `seasonder_the`, the error \code{seasonder_cs_field_qc_fun_not_defined_error} is raised.
#'   \item If there's an issue applying the quality control function, the error \code{seasonder_cs_field_qc_fun_error} is raised.
#' }
#'
#' \strong {Restart Options}:
#' The function provides structured mechanisms to recover from errors/conditions during its execution using `withRestarts`. The following restart options are available:
#'
#' \itemize{
#'   \item \code{seasonder_rerun_qc_with_fun}: Allows for rerunning QC with an alternate function.
#'     \itemize{
#'     \item \strong{Usage}: In a custom condition handler, you can call \code{seasonder_rerun_qc_with_fun(cond, alternateQCfunction)} to trigger this restart and run an alternate QC using \code{alternateQCfunction}. \code{alternateQCfunction} will be used as follows alternateQCfunction(x) being x the value. No extra parameters are passed.
#'     \item \strong{Effect}: If invoked, the function logs an info message detailing the reason of the rerun, and then returns the value returned by \code{alternateQCfunction}.
#'     }
#' }
#'
#' It's also important to note that within `read_and_qc_field`, the function `seasonder_readCSField` is used. This function has its own error management and restart options, which are detailed in its documentation.
#'
read_and_qc_field <- function(field_spec, connection, endian="big") {

  # Parameters used for error messages and logging.
  conditions_params <- list(calling_function = "read_and_qc_field")

  # Extract the field type from the specifications
  field_type <- field_spec$type
  # Extract the quality control function name from the specifications
  qc_fun_name <- field_spec$qc_fun
  # Extract the quality control parameters from the specifications
  qc_params <- field_spec$qc_params



  # Read the field using the helper function
  field_skipped <- FALSE

  field_value <- rlang::try_fetch(seasonder_readCSField(connection, field_type, endian=endian),
                                  seasonder_cs_field_skipped=function(cond){
                                    field_skipped <<-TRUE
                                    rlang::cnd_signal(cond)
                                    return(cond$seasonder_cs_field_value)

                                  }
  )
  if(!field_skipped){

    field_value_after_qc <-  withRestarts(
      seasonder_rerun_qc_with_fun=function(cond,qc_fun){
        value <- cond$seasonder_value
        rlang::inject(seasonder_logAndMessage(glue::glue("Rerunning QC on value {value}."),"info",class="seasonder_cs_field_qc_fun_rerun",!!!conditions_params,parent=cond))
        return(qc_fun(value))

      },
      {

        # Apply quality control

        if(!qc_fun_name %in% names(seasonder_the$qc_functions)){
          rlang::inject(seasonder_logAndAbort(glue::glue("QC function '{qc_fun_name}' not defined."),!!!conditions_params,class="seasonder_cs_field_qc_fun_not_defined_error",seasonder_value=field_value))
        }

        # Get the quality control function from the shared environment
        qc_fun <- seasonder_the$qc_functions[[qc_fun_name]]

        # Call the quality control function with the field value and the specified parameters
        field_value_after_qc <- rlang::try_fetch(
          rlang::inject(qc_fun(field_value,!!!qc_params)),
          error=function(e) rlang::inject(seasonder_logAndAbort(glue::glue("An issue happened while applying QC function '{qc_fun_name}'."),!!!conditions_params,class="seasonder_cs_field_qc_fun_error",seasonder_value=field_value,parent=e))
        )
        field_value_after_qc
      })
    # Return the value after quality control
    return(field_value_after_qc)
  }else{

    return(field_value)
  }
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
  results <- withCallingHandlers(
    purrr::map(spec, \(field_spec)
               read_and_qc_field(field_spec=field_spec,connection=connection, endian=endian)),
    purrr_error_indexed=function(err){
      parent_err <- err$parent

      parent_err$message <- glue::glue("Field {names(spec)[err$location]}: {err$parent$message}")

      rlang::cnd_signal(parent_err)

    })




  # Return the results
  return(results)
}


seasonder_check_specs <- function(specs, fields){

  fields %>% purrr::walk(\(field){
    if(is.null(purrr::pluck(specs,field))){
      seasonder_logAndAbort(glue::glue("Specifications for field '{field}' not provided"),calling_function = "seasonder_check_specs",class="spsr_field_specification_missing_error")
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
  if (length(remaining_loops) > 0) {
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
  if ("repeat" %in% names(specs)) {
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

#' @export
seasonder_v6_skip_transformation <- function(cond,value) invokeRestart("seasonder_v6_skip_transformation",cond,value)


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
#' @section Error Management and Conditions:
#'
#' The \code{seasonder_readSeaSondeCSFileHeaderV6} function contains multiple layers of error and condition management to ensure robust data reading and appropriate error reporting.
#'
#' \strong{Error and Condition Classes}:
#'
#' The function might raise the following conditions:
#'
#' \itemize{
#' \item \code{seasonder_v6_block_transformacion_skipped}: Triggered when a transformation for a specific block is skipped.
#' \item \code{seasonder_v6_transform_function_error}: Triggered when there's an error while applying the transformation function for a V6 header block.
#' \item \code{seasonder_v6_skip_block_error}: Triggered when there's an error while skipping a block.
#'}
#'
#' \string{Error Cases}:
#'
#' The following are the scenarios when errors or conditions are raised:
#'
#'\itemize{
#' \item Transformation Failure: If there's a recognized block key and the transformation function associated with it fails.
#' \item Error in Transformation Function Application: If there's an error while applying the transformation function for a recognized V6 header block.
#' \item Error in Skipping Block: If there's an error while skipping a block when the block key is not recognized.
#'}
#'
#' \strong{Restart Options}:
#'
#' The function provides the following restart option:
#'
#' \code{seasonder_v6_skip_transformation}: This restart allows users to skip the transformation for a specific block and instead return the provided value.
#'
#' \strong{Effects of Restart Options}:
#'
#' Using the \code{seasonder_v6_skip_transformation} restart:
#' \itemize{
#' \item The error message gets logged.
#' \item The transformation that caused the error gets skipped.
#' \item The provided value for that block is returned.
#'}
#'
#' Proper error management ensures the integrity of the reading process and provides detailed feedback to users regarding issues and potential resolutions.
#'
#' @export
seasonder_readSeaSondeCSFileHeaderV6 <- function(specs, connection, endian = "big", prev_data = NULL) {
  conditions_params <- list(calling_function = "seasonder_readSeaSondeCSFileHeaderV6")
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
        block_data <- withRestarts(
          seasonder_v6_skip_transformation = function(cond, value){
            # Log the error message and skip the CS field.
            rlang::inject(seasonder_logAndMessage(glue::glue("Skipping transformation for block '{block$nBlockKey}', returning provided value: {conditionMessage(cond)}"), "error",!!!conditions_params, class = "seasonder_v6_block_transformacion_skipped", parent = cond, new_seasonder_block_data = value))
            return(value)
          },
          rlang::try_fetch(
            seasonder_the$transform_functions[[block$nBlockKey]](block_data),
            error = function(err){
              rlang::inject(seasonder_logAndAbort(glue::glue("Error while applying transform function for V6 header block '{block$nBlockKey}': {conditionMessage(err)}"),!!!conditions_params, class = "seasonder_v6_transform_function_error",parent = err, seasonder_block_data = block_data))
            })
        )
      }

      # Store the results
      results[[block$nBlockKey]] <- block_data
    } else {
      # If the block key is not recognized, skip bytes as per the block data size
      rlang::try_fetch(
        seek(connection, where = block$nBlockDataSize, origin = "current", rw = "read"),
        error = function(err){
          rlang::inject(seasonder_logAndAbort(glue::glue("Error while skipping block '{block$nBlockKey}': {conditionMessage(err)}"),!!!conditions_params, parent = err, class = "seasonder_v6_skip_block_error"))
        })
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
  withCallingHandlers(
    header_v1 <- seasonder_readSeaSondeCSFileHeaderV1(specs$V1, connection, endian),
    error=function(err){
      err$message <- glue::glue("Header version 1: {conditionMessage(err)}")

      rlang::cnd_signal(err)
    }
  )

  # Extract the file version to determine subsequent headers to process
  file_version <- header_v1$nCsFileVersion

  # Create a list of header versions to process
  versions_to_process <- 2:file_version



  # Reduce the list of versions to process them sequentially
  header_pool <- withCallingHandlers(
    purrr::reduce(versions_to_process, \(pool, version){

      out <- process_version_header(pool = pool, version = version, specs = specs, connection = connection, endian = endian, prev_data = pool)

      out
    }, .init = header_v1),
    purrr_error_indexed=function(err){

      parent_err <- err$parent

      parent_err$message <- glue::glue("Header version {versions_to_process[err$location]}: {conditionMessage(err$parent)}")

      rlang::cnd_signal(parent_err)
    })

  return(header_pool)
}




#' Read SeaSonde Cross Spectra (CS) File Data
#'
#' This function reads the SeaSonde CS file data based on the provided header information.
#' The CS file data includes the antenna voltage squared self spectra (`SSA*`) and the
#' antenna cross spectra (`CSxy`). Additionally, a quality matrix (`QC`) is read when the header's
#' `nCsKind` is greater than or equal to 2.
#'
#' @param connection A connection object to the CS file.
#' @param header A list containing the header information. This is typically the output
#'   of the `seasonder_readSeaSondeCSFileHeader` function.
#' @param endian Character string indicating the byte order. Options are "big" (default) or "little".
#'
#' @details
#' - `SSA*`: Represents the Antenna * voltage squared self spectra. These are matrices
#'   where each row corresponds to a range and each column to a Doppler cell.
#' - `CSxy`: Represents the cross spectra between two antennas x and y. These are complex matrices.
#' - `QC`: Quality matrix with values ranging from zero to one. A value less than one indicates
#'   that the SpectraAverager skipped some data during averaging.
#'
#' @return A list containing the matrices for `SSA*`, `CSxy`, and `QC` (when applicable).
#'
#' @examples
#' \dontrun{
#'   con <- file("path_to_CS_file", "rb")
#'   header <- seasonder_readSeaSondeCSFileHeader(specs, con)
#'   data <- seasonder_readSeaSondeCSFileData(con, header)
#'   close(con)
#' }
#' @export
seasonder_readSeaSondeCSFileData <- function(connection, header, endian="big") {
  conditions_params <- list(calling_function = "seasonder_readSeaSondeCSFileData",class = "seasonder_cs_data_reading_error")

  # Extracting information from the header
  nRanges <- header$nRangeCells
  nDoppler <- header$nDopplerCells
  nCSKind <- header$nCsKind

  # Initialize matrices for the spectra
  out <- list(
    SSA1 = matrix(rep(NA_real_, nRanges * nDoppler), ncol = nDoppler, byrow = TRUE),
    SSA2 = matrix(rep(NA_real_, NA_real_, nRanges * nDoppler), ncol = nDoppler, byrow = TRUE),
    SSA3 = matrix(rep(NA_real_, NA_real_, nRanges * nDoppler), ncol = nDoppler, byrow = TRUE),
    CS12 = matrix(rep(NA_real_, complex(real = NA_real_, imaginary = NA_real_), nRanges * nDoppler), ncol = nDoppler, byrow = TRUE),
    CS13 = matrix(rep(NA_real_, complex(real = NA_real_, imaginary = NA_real_), nRanges * nDoppler), ncol = nDoppler, byrow = TRUE),
    CS23 = matrix(rep(NA_real_, complex(real = NA_real_, imaginary = NA_real_), nRanges * nDoppler), ncol = nDoppler, byrow = TRUE),
    QC = matrix(rep(NA_real_, NA_real_, nRanges * nDoppler), ncol = nDoppler, byrow = TRUE)
  )

  # Helper function to read complex vectors
  read_complex_vector <- function(connection, n, endian) {
    cplx_data <- readBin(connection, what = "numeric", n = n * 2, size = 4, endian = endian)

    complex(real = cplx_data[rep(c(TRUE, FALSE), n/2)], imaginary = cplx_data[rep(c(FALSE, TRUE), n/2)])
  }

  # Read data for each range
  for (i in seq_len(nRanges)) {

    out$SSA1[i,] <- rlang::try_fetch(error = function(cond){
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading SSA1 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "SSA1"))
    },
    readBin(connection, what = "numeric", n = nDoppler, size = 4, endian = endian))

    out$SSA2[i,] <- rlang::try_fetch(error = function(cond){
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading SSA2 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "SSA2"))
    },
    readBin(connection, what = "numeric", n = nDoppler, size = 4, endian = endian))

    out$SSA3[i,] <- rlang::try_fetch(error = function(cond){
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading SSA3 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "SSA3"))
    },
    readBin(connection, what = "numeric", n = nDoppler, size = 4, endian = endian))

    out$CS12[i,] <- rlang::try_fetch(error = function(cond){
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading CS12 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "CS12"))
    },
    read_complex_vector(connection, nDoppler, endian))
    out$CS13[i,] <-  rlang::try_fetch(error = function(cond){
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading CS13 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "CS13"))
    },
    read_complex_vector(connection, nDoppler, endian))

    out$CS23[i,] <-  rlang::try_fetch(error = function(cond){
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading CS23 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "CS23"))
    },
    read_complex_vector(connection, nDoppler, endian))
    if (nCSKind >= 2) {
      out$QC[i,] <- rlang::try_fetch(error = function(cond){
        rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading QC data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "QC"))
      },
      readBin(connection, what = "numeric", n = nDoppler, size = 4, endian = endian))
    }
  }
  return(out)
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


# Conditions

cs_file_reading_error <- function(e,filepath=NULL,calling_function=NULL){

  message <- glue::glue("An issue happened while processing the file {filepath}. Issue: {as.character(e)}")

  seasonder_logAndAbort(message,calling_function = calling_function,parent=e,filepath=filepath,class="cs_file_reading_issure")


}
