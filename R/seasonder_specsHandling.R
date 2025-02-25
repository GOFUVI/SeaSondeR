# Set valid YAML versions for seasondecs and seasondecssy specifications
seasonder_the$valid_yaml_seasondecs_versions <- c("1.0.0")  # Valid version for seasondecs YAML specs
seasonder_the$valid_yaml_seasondecssy_versions <- c("1.0.0")  # Valid version for seasondecssy YAML specs

# Define the file paths for the specifications.
# Maps the specification type ("CS" or "CSSY") to its corresponding YAML file located within the SeaSondeR package.
seasonder_the$specs_filepaths <- list(
  "CS" = system.file("specs", "CS_V1.yaml", package = "SeaSondeR"),
  "CSSY" = system.file("specs", "CSSY_V1.yaml", package = "SeaSondeR")
)

#' Get the Default Specifications File Path
#'
#' This function returns the default file path for the specifications YAML file corresponding
#' to the provided type. The type must be one of the names defined in the default paths (i.e., "CS" or "CSSY").
#'
#' @param type A character string specifying the type of specifications file. Default is "CS".
#'
#' @return A character string representing the full path to the YAML specifications file.
#'
#' @importFrom rlang arg_match
#'
#' @examples
#' \dontrun{
#'   # Retrieve the default CS specifications file path
#'   cs_specs_path <- seasonder_defaultSpecsFilePath("CS")
#'
#'   # Retrieve the default CSSY specifications file path
#'   cssy_specs_path <- seasonder_defaultSpecsFilePath("CSSY")
#' }
seasonder_defaultSpecsFilePath <- function(type = "CS") {
  # Retrieve the list of default specifications file paths from the shared environment
  default_paths <- seasonder_the$specs_filepaths

  # Match the provided type against the allowed names in the default paths list
  rlang::arg_match(type, names(default_paths))

  # Extract the file path corresponding to the specified type
  out <- default_paths[[type]]

  # Return the obtained file path
  return(out)
}



#' Read Specifications from a YAML File
#'
#' This function reads a YAML file containing specifications, handles potential
#' reading errors, and extracts specific information based on a provided path.
#'
#' @param file_path A string. The path to the YAML file.
#' @param path A character vector. Represents the path within the YAML file to
#'   access the desired information. For example, to access fields of version V2
#'   of the header, the path would be \code{c("header", "versions", "V2")}.
#'
#' @return A list. The information extracted from the YAML file based on the
#'   provided path.
#' @seealso \code{\link[yaml]{read_yaml}} for the underlying YAML reading.
#' @seealso \code{\link[purrr]{pluck}} for the data extraction mechanism used.
#'
#' @section Error Handling:
#' The function has built-in error handling which aborts the function's execution and logs
#' detailed error messages in case of:
#' \itemize{
#'  \item File not found.
#'  \item Error in reading the YAML content.
#'  \item If the read YAML content is not of list type.
#'  \item If no data is found for the provided path in the YAML content.
#' }
#' Errors generated are of class \code{"seasonder_read_yaml_file_error"}.
#' Detailed error information including the file path and path within the file
#' is provided. For logging and aborting, this function utilizes the
#' \code{\link[=seasonder_logAndAbort]{seasonder_logAndAbort}} function.
#'
#' @importFrom yaml read_yaml
#'
#' @examples
#' \dontrun{
#' # Assuming a YAML file named "example.yaml" exists with appropriate content
#' result <- seasonder_readYAMLSpecs("example.yaml", c("header", "versions", "V2"))
#' print(result)
#' }
#'
seasonder_readYAMLSpecs <- function(file_path, path = rlang::zap()) {

  conditions_params <- list(calling_function = "seasonder_readYAMLSpecs",class="seasonder_read_yaml_file_error",seasonder_yaml_file_path=file_path,seasonder_yaml_specs_path=path)
  # Check if the file exists
  if (!file.exists(file_path)) {
    rlang::inject(seasonder_logAndAbort(glue::glue("File '{file_path %||% ''}' not found."),!!!conditions_params))
  }

  # Read the content from the YAML file
  yaml_content <- rlang::try_fetch({
    yaml::read_yaml(file_path)
  }, error = function(e) {
    rlang::inject(seasonder_logAndAbort(glue::glue("Reading error. The file '{file_path %||% ''}' might not be a valid YAML. Reason: {conditionMessage(e)}"),!!!conditions_params,parent=e))
  })

  # If the content is not a list, throw an error
  if (!is.list(yaml_content)) {
    rlang::inject(seasonder_logAndAbort(glue::glue("Invalid YAML structure in file '{file_path}'."),!!!conditions_params))
  }

  extracted_data<- yaml_content
  if(!rlang::is_zap(path) && is.character(path)){
    # Extract the desired data based on the given path
    extracted_data <- purrr::pluck(yaml_content, !!!path)

    # If no data is found for the provided path, throw an error
    if (is.null(extracted_data)) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Invalid specs path '{path}' for file '{file_path}'."),!!!conditions_params))
    }

  }

  return(extracted_data)
}


#' Determine the Spectra File Type
#'
#' This function identifies the type of a spectra file (either "CS" or "CSSY") by reading its header block based on YAML specifications.
#' It first attempts to read a key size block using the CSSY specifications, and if that fails, it reopens the file and tries to read the CS header block.
#'
#' @param filepath A character string specifying the path to the spectra file.
#' @param endian A character string indicating the file's byte order ("big" by default).
#'
#' @return A character string representing the spectra file type ("CS" or "CSSY").
#'
#' @details
#' The function sets up error handling parameters and uses YAML specifications retrieved via
#' \code{seasonder_readYAMLSpecs} and \code{seasonder_defaultSpecsFilePath}. It opens the file in binary read mode
#' and ensures the connection is closed upon exit. If reading the key size block fails, it reopens the file to try
#' reading the CS header block. The final file type is determined by the key returned from the file block.
#'
#' @importFrom rlang try_fetch inject inherits_any
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#'   # Determine the spectra file type for a given file path
#'   file_type <- seasonder_find_spectra_file_type("path/to/spectra_file.bin")
#'   print(file_type)
#' }
seasonder_find_spectra_file_type <- function(filepath, endian = "big") {
  # Set up error handling parameters with function name, error class, and file path
  conditions_params <- list(
    calling_function = "seasonder_find_spectra_file_type",
    class = "seasonder_read_cs_file_error",
    seasonder_cs_filepath = filepath
  )

  # Initialize output type to "CS" by default
  out <- "CS"

  # Retrieve YAML specifications for the key size block from the CSSY specifications file
  specs_key_size <- seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"), "key_size_block")

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

  # Try to read the file block using the CSSY key size specifications
  file_id <- rlang::try_fetch(
    seasonder_readSeaSondeCSFileBlock(specs_key_size, connection, endian),
    error = function(e) {
      # If reading the CSSY block fails, attempt to reopen the file
      connection <- rlang::try_fetch(
        suppressWarnings(file(filepath, "rb")),
        error = function(e) {
          # Abort if reconnection fails
          rlang::inject(
            seasonder_logAndAbort(
              glue::glue("Could no open connection to file {filepath %||% ''}. Reason: {conditionMessage(e)}."),
              !!!conditions_params,
              parent = e
            )
          )
        }
      )
      # Ensure the new connection is closed on exit
      on.exit(close(connection), add = TRUE)

      # Retrieve YAML specifications for the CS header block (version V1)
      CS_header_specs <- seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CS"), c("header","V1"))

      # Attempt to read the CS header block; use try() to capture any errors silently
      V1_header <- try(seasonder_readSeaSondeCSFileBlock(CS_header_specs, connection, endian), silent = TRUE)

      # Determine if the CS header was read successfully (i.e., no error occurred)
      is_CS <- !rlang::inherits_any(V1_header, "try-error")

      # If successful, return a list with key "CS"; otherwise, propagate the error
      if (is_CS) return(list(key = "CS")) else e
    }
  )

  # Use a switch to determine the output based on the key read from the file block
  out <- switch(
    file_id$key,
    CSSY = "CSSY",
    CS = "CS",
    # If the key is not recognized, abort with an error message
    rlang::inject(
      seasonder_logAndAbort(
        glue::glue("Spectra file type not recognized. File: {filepath %||% ''}."),
        !!!conditions_params
      )
    )
  )

  # Return the determined spectra file type
  return(out)
}

#' Get the Default Specifications Path for a Spectra File
#'
#' This function returns the default YAML specifications file path corresponding to a given spectra file.
#' It first determines the file type by analyzing the file content and then retrieves the associated default
#' specifications path.
#'
#' @param filepath A character string specifying the path to the spectra file.
#' @param endian A character string indicating the file's byte order ("big" by default).
#'
#' @return A character string representing the default YAML specifications file path for the detected file type.
#'
#' @details
#' The function leverages \code{seasonder_find_spectra_file_type} to determine whether the file is of type "CS" or "CSSY".
#' It then uses \code{seasonder_defaultSpecsFilePath} to obtain the corresponding default specifications path.
#'
#' @seealso \code{\link{seasonder_find_spectra_file_type}}, \code{\link{seasonder_defaultSpecsFilePath}}
#'
#' @examples
#' \dontrun{
#'   # Get the default specifications path for a given spectra file
#'   specs_path <- seasonder_defaultSpecsPathForFile("path/to/spectra_file.bin")
#'   print(specs_path)
#' }
seasonder_defaultSpecsPathForFile <- function(filepath, endian = "big") {
  # Determine the file type ("CS" or "CSSY") by analyzing the spectra file
  file_type <- seasonder_find_spectra_file_type(filepath, endian = endian)

  # Retrieve the default specifications file path based on the detected file type
  specs_path <- seasonder_defaultSpecsFilePath(type = file_type)

  # Return the specifications file path
  return(specs_path)
}
