seasonder_the$valid_yaml_seasondecs_versions <- c("1.0.0")


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
seasonder_readYAMLSpecs <- function(file_path, path) {

  conditions_params <- list(calling_function = "seasonder_readYAMLSpecs",class="seasonder_read_yaml_file_error",seasonder_yaml_file_path=file_path,seasonder_yaml_specs_path=path)
  # Check if the file exists
  if (!file.exists(file_path)) {
    rlang::inject(seasonder_logAndAbort(glue::glue("File '{file_path %||% ''}' not found."),!!!conditions_params))
  }

  # Read the content from the YAML file
  yaml_content <- rlang::try_fetch({
    yaml::read_yaml(file_path)
  }, error = function(e) {
    rlang::inject(seasonder_logAndAbort(glue::glue("Reading error. The file '{file_path %||% ''}' might not be a valid YAML. Reason: {condMessage(e)}"),!!!conditions_params,parent=e))
  })

  # If the content is not a list, throw an error
  if (!is.list(yaml_content)) {
    rlang::inject(seasonder_logAndAbort(glue::glue("Invalid YAML structure in file '{file_path}'."),!!!conditions_params))
  }

  # Extract the desired data based on the given path
  extracted_data <- purrr::pluck(yaml_content, !!!path)

  # If no data is found for the provided path, throw an error
  if (is.null(extracted_data)) {
    rlang::inject(seasonder_logAndAbort(glue::glue("Invalid specs path '{path}' for file '{file_path}'."),!!!conditions_params))
  }

  return(extracted_data)
}


