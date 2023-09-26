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
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming a YAML file named "example.yaml" exists with appropriate content
#' result <- seasonder_readYAMLSpecs("example.yaml", c("header", "versions", "V2"))
#' print(result)
#' }
#'
seasonder_readYAMLSpecs <- function(file_path, path) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    seasonder_logAndAbort("File not found.")
  }

  # Read the content from the YAML file
  yaml_content <- tryCatch({
    yaml::read_yaml(file_path)
  }, error = function(e) {
    seasonder_logAndAbort("Reading error. The file might not be a valid YAML.")
  })

  # If the content is not a list, throw an error
  if (!is.list(yaml_content)) {
    seasonder_logAndAbort("Invalid YAML structure.")
  }

  # Check the YAML version
  yaml_version <- yaml_content$metadata$version
  if (!(yaml_version %in% seasonder_the$valid_yaml_seasondecs_versions)) {
    seasonder_logAndAbort("Unsupported version.")
  }

  # Extract the desired data based on the given path
  extracted_data <- purrr::pluck(yaml_content, !!!path)

  # If no data is found for the provided path, throw an error
  if (is.null(extracted_data)) {
    seasonder_logAndAbort("Invalid path.")
  }

  return(extracted_data)
}

