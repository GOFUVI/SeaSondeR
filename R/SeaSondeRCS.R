#### SeaSondeRCS ####



#' Create a New SeaSondeRCS Object
#'
#' This function constructs a new SeaSondeRCS object with the provided header, data, and version information.
#'
#' @param header A list containing header information for the SeaSondeRCS object.
#' @param data A list containing the data fields for the SeaSondeRCS object.
#'
#' @seealso
#' \code{\link{seasonder_setSeaSondeRCS_header}}
#' \code{\link{seasonder_setSeaSondeRCS_data}}
#'
#' @return A SeaSondeRCS object with the specified header, data, and version.
#'
#'
new_SeaSondeRCS <- function(header, data, seasonder_apm_object = NULL, doppler_interpolation = 1L) {



  out <- structure(list(header = list(),
                        data = list()),
                   version = 1, # An integer indicating the version of the SeaSondeRCS object. Current is 1.
                   ProcessingSteps = character(0),
                   FOR_data = list(),
                   MUSIC_data = list(),
                   NoiseLevel = numeric(0),
                   APM = seasonder_apm_object,
                   interpolated_doppler_cells_index = integer(0),
                   class = "SeaSondeRCS")




  out %<>% seasonder_setSeaSondeRCS_header(header)
  out %<>% seasonder_setSeaSondeRCS_data(data)

  out %<>% seasonder_setSeaSondeRCS_doppler_interpolation(doppler_interpolation)




  out %<>% seasonder_setSeaSondeRCS_FOR_parameters(list())
  out %<>% seasonder_setSeaSondeRCS_FOR(seasonder_initSeaSondeRCS_FOR(out))

  out %<>% seasonder_initMUSICData()



  return(out)
}

# TODO: update docs

#' Create a SeaSondeRCS object
#'
#' This function creates a SeaSondeRCS object either from a file path or directly from a list containing header and data.
#'
#' @param x Either a character string specifying the path to the SeaSonde CS file or a list with header and data.
#' @param specs_path A character string specifying the path to the YAML specifications for the CS file. Only used if x is a character string.
#'
#' @return A SeaSondeRCS object.
#' @seealso
#' \code{\link{new_SeaSondeRCS}}
#' \code{\link{seasonder_readSeaSondeCSFile}} (for character inputs)
#' \code{\link{seasonder_setSeaSondeRCS_ProcessingSteps}}
#'
#'
#' @export
seasonder_createSeaSondeRCS <- function(x, specs_path = NULL, ...) {
  UseMethod("seasonder_createSeaSondeRCS")
}

#' @export
seasonder_createSeaSondeRCS.list <- function(x, specs_path = NULL, ...) {

  # Creating the SeaSondeRCS object
  out <- new_SeaSondeRCS(x$header, x$data)

  out %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_creation_step_text("list"))

  return(out)
}

#' @export
seasonder_createSeaSondeRCS.character <- function(x, specs_path = system.file("specs","CS_V1.yaml",package = "SeaSondeR"), ...) {
  # Checking if the file exists
  if (!file.exists(x)) {
    seasonder_logAndAbort(glue::glue("File '{x}' does not exist."), calling_function = "seasonder_createSeaSondeRCS.character", class = "seasonder_CS_file_not_found_error")
  }

  # Reading the SeaSonde CS file
  result <- seasonder_readSeaSondeCSFile(x, specs_path)

  # Creating the SeaSondeRCS object
  out <- new_SeaSondeRCS(result$header, result$data, ...)

  out %<>% seasonder_setSeaSondeRCS_ProcessingSteps(SeaSondeRCS_creation_step_text(x))

  return(out)
}

seasonder_SeaSondeRCS_dataMatrix_dimensionNames <- function(nRanges, nDoppler) {

  dimension_names <- list(sprintf("range_%03d",1:nRanges),sprintf("doppler_%03d",0:(nDoppler - 1)))

  return(dimension_names)

}

new_SeaSondeRCS_SSMatrix <- function(nRanges, nDoppler, name = NULL, data = NULL) {

  dimension_names <- seasonder_SeaSondeRCS_dataMatrix_dimensionNames(nRanges, nDoppler)

  data <- data %||% rep(NA_real_, nRanges * nDoppler)


  matrix <- matrix(data, ncol = nDoppler, byrow = TRUE, dimnames = dimension_names)

  out <- structure(matrix,
                   name = name,
                   class = "SeaSondeRCS_SSMatrix")

  return(out)
}


new_SeaSondeRCS_QCMatrix <- function(nRanges, nDoppler, name = NULL, data = NULL) {

  dimension_names <- seasonder_SeaSondeRCS_dataMatrix_dimensionNames(nRanges, nDoppler)

  data <- data %||% rep(NA_real_, nRanges * nDoppler)


  matrix <- matrix(data, ncol = nDoppler, byrow = TRUE, dimnames = dimension_names)

  out <- structure(matrix,
                   name = name,
                   class = "SeaSondeRCS_QCMatrix")

  return(out)
}


new_SeaSondeRCS_CSMatrix <- function(nRanges, nDoppler, name = NULL, data = NULL) {

  dimension_names <- seasonder_SeaSondeRCS_dataMatrix_dimensionNames(nRanges, nDoppler)

  data <- data %||% rep(complex(real = NA_real_, imaginary = NA_real_), nRanges * nDoppler)


  matrix <- matrix(data, ncol = nDoppler, byrow = TRUE, dimnames = dimension_names)

  out <- structure(matrix,
                   name = name,
                   class = "SeaSondeRCS_CSMatrix")

  return(out)
}

#' Initialize Cross-Spectra Data Structure for SeaSondeR
#'
#' This function initializes a data structure for storing cross-spectra data
#' related to SeaSonde radar measurements. It creates a list of matrices,
#' each corresponding to different components of the SeaSonde data.
#'
#' @param nRanges Integer, number of range cells in the radar measurement.
#'        Specifies the number of rows in each matrix.
#' @param nDoppler Integer, number of Doppler bins in the radar measurement.
#'        Specifies the number of columns in each matrix.
#'
#' @return A list containing matrices for different cross-spectra components:
#'         \itemize{
#'           \item \code{SSA1}: Matrix for SSA1 component, filled with \code{NA_real_}.
#'           \item \code{SSA2}: Matrix for SSA2 component, filled with \code{NA_real_}.
#'           \item \code{SSA3}: Matrix for SSA3 component, filled with \code{NA_real_}.
#'           \item \code{CS12}: Matrix for CS12 component, complex numbers with \code{NA_real_} real and imaginary parts.
#'           \item \code{CS13}: Matrix for CS13 component, complex numbers with \code{NA_real_} real and imaginary parts.
#'           \item \code{CS23}: Matrix for CS23 component, complex numbers with \code{NA_real_} real and imaginary parts.
#'           \item \code{QC}: Quality control matrix, filled with \code{NA_real_}.
#'         }
seasonder_initCSDataStructure <- function(nRanges, nDoppler) {


  list(
    SSA1 = new_SeaSondeRCS_SSMatrix(nRanges, nDoppler, name = "SSA1"),
    SSA2 = new_SeaSondeRCS_SSMatrix(nRanges, nDoppler, name = "SSA2"),
    SSA3 = new_SeaSondeRCS_SSMatrix(nRanges, nDoppler, name = "SSA3"),
    CS12 = new_SeaSondeRCS_CSMatrix(nRanges, nDoppler, name = "CS12"),
    CS13 = new_SeaSondeRCS_CSMatrix(nRanges, nDoppler, name = "CS13"),
    CS23 = new_SeaSondeRCS_CSMatrix(nRanges, nDoppler, name = "CS23"),
    QC = new_SeaSondeRCS_QCMatrix(nRanges, nDoppler, name = "QC")
  )

}

seasonder_initSeaSondeRCS_FORFromHeader <- function(seasonder_cs_obj, FOR) {

  out <- FOR

  nRanges <- seasonder_getnRangeCells(seasonder_cs_obj)

  nNegBraggLeftIndex <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "nNegBraggLeftIndex")$data %||% rep(0,nRanges)

  if (any(nNegBraggLeftIndex > 0)) {
    nNegBraggRightIndex <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj,"nNegBraggRightIndex")$data %||% rep(0,nRanges)

    if (any(nNegBraggRightIndex > 0 & nNegBraggLeftIndex > 0)) {
      out <-  1:nRanges %>% purrr::reduce(\(result,i) {
        left_index <- nNegBraggLeftIndex[i]
        right_index <- nNegBraggRightIndex[i]

        if (left_index > 0 && right_index > 0 && left_index <= right_index) {
          result[[i]]$negative_FOR <- seq(left_index, right_index)
        }
        return(result)
      },.init = out)
    }

  }

  nPosBraggLeftIndex <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj,"nPosBraggLeftIndex")$data %||% rep(0,nRanges)

  if (any(nPosBraggLeftIndex > 0)) {
    nPosBraggRightIndex <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj,"nPosBraggRightIndex")$data %||% rep(0,nRanges)

    if (any(nPosBraggRightIndex > 0 & nPosBraggLeftIndex > 0)) {
      out <-  1:nRanges %>% purrr::reduce(\(result,i) {
        left_index <- nPosBraggLeftIndex[i]
        right_index <- nPosBraggRightIndex[i]

        if (left_index > 0 && right_index > 0 && left_index <= right_index) {
          result[[i]]$positive_FOR <- seq(left_index, right_index)
        }
        return(result)
      },.init = out)
    }

  }

  return(out)

}


seasonder_initSeaSondeRCS_FOR <- function(seasonder_cs_obj) {

  nRanges <- seasonder_getnRangeCells(seasonder_cs_obj)

  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  dim_names <- seasonder_SeaSondeRCS_dataMatrix_dimensionNames(nRanges = nRanges, nDoppler = nDoppler)

  out <- rep(list(list(negative_FOR = integer(0), positive_FOR = integer(0))),  nRanges)

  names(out) <- dim_names[[1]]

  out <- seasonder_initSeaSondeRCS_FORFromHeader(seasonder_cs_obj, out)




  return(out)


}




##### Validation #####

#' Validate ProcessingSteps Attribute for a SeaSondeRCS Object
#'
#' This function validates if the provided ProcessingSteps is a character vector.
#'
#' @param steps The character vector to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRCS_ProcessingSteps <- function(steps) {
  if (!is.character(steps)) {
    seasonder_logAndAbort("ProcessingSteps must be a character vector.", calling_function = "validate_SeaSondeRCS_ProcessingSteps")

  }
  return(TRUE)
}

#' Validate the Header of CrossSpectra Data
#'
#' This function validates the structure of a header list that is expected to
#' represent the metadata for a cross spectra file. It checks if the header is
#' indeed a list and whether mandatory elements, such as the number of range cells
#' and the number of Doppler cells, are present.
#'
#' @param header A list representing the header metadata of a cross spectra file.
#'
#' @section Details:
#' The function primarily checks for two conditions:
#' - Whether the provided header argument is a list.
#' - Whether the nRangeCells and nDopplerCells are present in the header.
#'
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage conditions and provide
#' detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_CS_header_is_not_a_list}: Triggered when the header parameter is not a list.
#'   \item \code{seasonder_CS_missing_nRange_nDoppler_error}: Triggered when either nRangeCells or nDopplerCells is missing from the header.
#' }
#'
#' \strong{Condition Cases}:
#' \itemize{
#'   \item When the header is not a list, the function throws an error with the class `seasonder_CS_header_is_not_a_list`.
#'   \item If either nRangeCells or nDopplerCells is missing, an error with the class `seasonder_CS_missing_nRange_nDoppler_error` is thrown.
#' }
#'
#' @return Invisible NULL if the header structure is valid. Otherwise, an error is thrown.
#'
#' @export
seasonder_validateCSHeaderStructure <- function(header) {
  # TODO: test, document, vignette
  if (!rlang::is_list(header)) {
    seasonder_logAndAbort(glue::glue("The 'header' parameter must be a list"), calling_function = "seasonder_setSeaSondeRCS_Header", class = "seasonder_CS_header_is_not_a_list", seasonder_header = header)
  }

  # Checking if nRanges and nDoppler are present in the header
  if (is.null(header$nRangeCells) || is.null(header$nDopplerCells)) {
    seasonder_logAndAbort(glue::glue("The 'nRangeCells' or 'nDopplerCells' are not present in the header data."), calling_function = "seasonder_setSeaSondeRCS_Header", class = "seasonder_CS_missing_nRange_nDoppler_error", seasonder_nRange = header$nRangeCells, seasonder_nDoppler = header$nDopplerCells)
  }

  invisible(NULL)
}


#' Validate the Data Structure of CrossSpectra Data
#'
#' This function checks the validity of the `data` structure for CrossSpectra (CS) data. It ensures that all required fields are present,
#' the dimensions of the matrices are correct based on `nRanges` and `nDoppler`, and that the types of the data fields are as expected.
#'
#' @param data A list representing the CrossSpectra (CS) data. It should contain fields "SSA1", "SSA2", "SSA3", "CS12", "CS13", "CS23", and "QC".
#' @param nRanges An integer specifying the expected number of range cells.
#' @param nDoppler An integer specifying the expected number of Doppler cells.
#'
#' @details
#' The function expects the following structure for the `data` list:
#' \itemize{
#'   \item `SSA1`, `SSA2`, `SSA3`, `QC`: Matrices with numeric values, with dimensions `nRanges` x `nDoppler`.
#'   \item `CS12`, `CS13`, `CS23`: Matrices with complex values, with dimensions `nRanges` x `nDoppler`.
#' }
#'
#' @section Error Management:
#' This function utilizes the `rlang` package to manage errors and provide detailed and structured error messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_CS_data_structure_validation_error}: An error class indicating a problem with the data structure of the CrossSpectra (CS) data.
#' }
#'
#' \strong{Condition Cases}:
#' \itemize{
#'   \item Missing fields in the data.
#'   \item Incorrect dimensions for the matrices in the data.
#'   \item Incorrect data type for the fields in the data.
#' }
#'
#' @return Invisible NULL if the data structure is valid. Otherwise, an error is thrown.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- list(
#'   SSA1 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
#'   SSA2 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
#'   # ... other fields
#' )
#' seasonder_validateCSDataStructure(data, 10, 20)
#' }
#'
seasonder_validateCSDataStructure <- function(data, nRanges, nDoppler) {

  # Prepare a list of parameters to be passed in case an error is encountered.
  # This provides detailed information about the context of the error.
  conditions_params = list(calling_function = "seasonder_validateDataStructure",
                           class = "seasonder_CS_data_structure_validation_error",
                           seasonder_data = data,
                           seasonder_nRanges = nRanges,
                           seasonder_nDoppler = nDoppler)

  # Define the required fields that should be present in the data.
  required_fields <- c("SSA1", "SSA2", "SSA3", "CS12", "CS13", "CS23", "QC")

  # Loop over each required field to check its existence, dimensions, and data type.
  for (field in required_fields) {

    # Check if the current field exists in the data.
    if (!field %in% names(data)) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Missing field '{field}' in data."), !!!conditions_params))
    }

    # Check if the dimensions of the matrix for the current field match the expected nRanges and nDoppler.
    if (dim(data[[field]])[1] != nRanges || dim(data[[field]])[2] != nDoppler) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Incorrect dimensions for field '{field}' in data."), !!!conditions_params))
    }

    # Check the data type of the current field.
    # SSA1, SSA2, SSA3, and QC should contain numeric values.
    # CS12, CS13, and CS23 should contain complex values.
    if (field %in% c("SSA1", "SSA2", "SSA3", "QC")) {
      if (!is.numeric(data[[field]])) {
        rlang::inject(seasonder_logAndAbort(glue::glue("Field '{field}' should contain numeric values."), !!!conditions_params))
      }
    } else {
      if (!is.complex(data[[field]])) {
        rlang::inject(seasonder_logAndAbort(glue::glue("Field '{field}' should contain complex values."), !!!conditions_params))
      }
    }
  }
}


##### Doppler interpolation #####

seasonder_SeaSondeRCSInterpolateDoppler <- function(seasonder_cs_obj){

  out <- seasonder_cs_obj

data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)

data_matrices_names <- c("SSA1", "SSA2", "SSA3", "CS12", "CS13", "CS23")

doppler_interpolation <- seasonder_getSeaSondeRCS_doppler_interpolation(seasonder_cs_obj)


nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)
nRanges <- seasonder_getnRangeCells(seasonder_cs_obj)

interpolated_data <- seasonder_initCSDataStructure(nRanges = nRanges, nDoppler = nDoppler)


# 1, int,2,int,3 n*2-1
# 1-> 1 (i-1)*2 +1 = i*2 -2 +1 = i * 2 -1
# 2 -> 3 i * 2 -1
# 3 -> 5 i * 2 -1
# 1, int, int ,2,int, int ,3 n*3-2
# 1 -> 1 (i-1)*3 +1 = i *3 -3 +1 = i*3 -2
# 2 -> 4 i*3 -2
# 3 -> 7 i* 3-2
# 1, int, int, int ,2,int, int, int ,3 n*4-3


index_mapping <- data.frame(original=1:ncol(data[[1]]), mapped=(0:(ncol(data[[1]])-1))*doppler_interpolation  +1)

interpolated_cells <- dplyr::setdiff(1:nDoppler, index_mapping$mapped
)

interpolated_data %<>% purrr::map2(names(.), \(matrix, name){

  if(!name %in% names(data)){
    seasonder_logAndAbort(glue::glue("{name} is not a data matrix name."), calling_function = "seasonder_SeaSondeRCSInterpolateDoppler")
  }



  original_matrix <- data[[name]]

  matrix[,index_mapping$mapped] <- original_matrix[,index_mapping$original]

  if(name == "QC"){
    matrix[,interpolated_cells] <- -1L
  }else{
   matrix <-  1:nrow(matrix) %>% purrr::reduce(\(matrix_so_far,i){

      data <- matrix_so_far[i,,drop = TRUE]

      if(!rlang::is_complex(data)){
        data <- zoo::na.approx(data)
      }else{


        data <- complex(real= zoo::na.approx(pracma::Real(data)),
                imaginary= zoo::na.approx(pracma::Imag(data)))

      }

      matrix_so_far[i,] <- data

      matrix_so_far

    }, .init=matrix)

  }


matrix

})


out %<>% seasonder_setSeaSondeRCS_interpolated_doppler_cells_index(interpolated_cells)

out %<>% seasonder_setSeaSondeRCS_data(interpolated_data)



return(out)
}

##### Setters #####

#' Setter for header
#'
#' @param seasonder_cs_obj SeaSondeRCS object
#' @param header new value
#'
#' @seealso
#' \code{\link{seasonder_validateCSHeaderStructure}}
#'
#' @export
seasonder_setSeaSondeRCS_header <- function(seasonder_cs_obj, header) {
  # TODO: test, document, vignette
  seasonder_validateCSHeaderStructure(header)

  out <- seasonder_cs_obj

  out[["header"]] <- header

  return(out)
}

#' Setter for data
#'
#' @param seasonder_cs_obj SeaSondeRCS object
#' @param data new value
#'
#' @seealso
#' \code{\link{seasonder_validateCSDataStructure}}
#'
#' @export
seasonder_setSeaSondeRCS_data <- function(seasonder_cs_obj, data) {
  # TODO: test, document, vignette
  nRangeCells <- seasonder_getnRangeCells(seasonder_cs_obj)
  nDopplerCells <- seasonder_getnDopplerCells(seasonder_cs_obj)

  seasonder_validateCSDataStructure(data,nRanges = nRangeCells, nDoppler = nDopplerCells)

  out <- seasonder_cs_obj

  out[["data"]] <- data

  return(out)
}


#' Setter for ProcessingSteps
#'
#' @param seasonder_cs_obj SeaSondeRCS object
#' @param processing_steps new value
#' @param append append the new step or replace previous steps? Default: TRUE
#'
#' @export
seasonder_setSeaSondeRCS_ProcessingSteps <- function(seasonder_cs_obj, processing_steps,append = TRUE) {



  if (append) {
    steps <-  seasonder_getSeaSondeRCS_ProcessingSteps(seasonder_cs_obj)
    processing_steps <- c(steps,processing_steps)
  }
  validate_SeaSondeRCS_ProcessingSteps(processing_steps)
  out <- seasonder_cs_obj

  attr(out,"ProcessingSteps") <- processing_steps


  return(out)
}

seasonder_setSeaSondeRCS_APM <- function(seasonder_cs_object, seasonder_apm_object){

  # TODO: Valiate APM obj


  attr(seasonder_cs_object, "APM") <- seasonder_apm_object

  return(seasonder_cs_object)

}

seasonder_setSeaSondeRCS_doppler_interpolation <- function(seasonder_cs_object, doppler_interpolation){

  # TODO: Valiate doppler_interpolation (must be 1L, 2L, 3L or 4L, also check the number of final doppler bins SEAS-72). The default is 1 (no interpolation)


  attr(seasonder_cs_object, "doppler_interpolation") <- doppler_interpolation


  if(doppler_interpolation > 1L){

    seasonder_cs_object %<>% seasonder_SeaSondeRCSInterpolateDoppler()

  }

  return(seasonder_cs_object)

}


seasonder_setSeaSondeRCS_interpolated_doppler_cells_index <- function(seasonder_cs_object, interpolated_doppler_cells_index){

  # TODO: Valiate interpolated_doppler_cells_index (should be integer in the range of 1: (nDopplerCells))


  attr(seasonder_cs_object, "interpolated_doppler_cells_index") <- interpolated_doppler_cells_index

  return(seasonder_cs_object)

}



##### Getters #####

#' Getter for header
#'
#' @param seasonder_cs_obj SeaSondeRCS object
#'
#' @importFrom rlang %||%
#'
#' @export
seasonder_getSeaSondeRCS_header <- function(seasonder_cs_obj) {
  # TODO: test, document, vignette
  out <- seasonder_cs_obj[["header"]] %||% list()

  return(out)
}




#' Convert SeaSondeRCS Object to JSON
#'
#' This function extracts the header data from a `seasonder_cs_obj`, representing a SeaSondeRCS object, and converts it into a JSON format. Optionally, it can write this JSON data to a specified file path.
#'
#' @param seasonder_cs_obj A SeaSondeRCS object from which the header data will be extracted.
#' @param path Optional path to a file where the JSON output should be saved. If provided, the function will write the JSON data to this file. If NULL, the function will only return the JSON data as a string without writing it to a file.
#'
#' @return A character string in JSON format representing the header data of the provided SeaSondeRCS object. If a path is provided, the function also writes this data to the specified file.
#'
#' @export
#'
#' @seealso
#' \code{\link{seasonder_createSeaSondeRCS}}, \code{\link{seasonder_getSeaSondeRCS_header}}
#'
#' @note
#' If a path is provided and there is an issue writing to the file, the function logs an error message using `seasonder_logAndMessage` and returns the JSON data as a string.
seasonder_asJSONSeaSondeRCSHeader <- function(seasonder_cs_obj, path = NULL) {

  header <- seasonder_getSeaSondeRCS_header(seasonder_cs_obj)

  out <- jsonlite::toJSON(header, pretty = TRUE)

  if (!is.null(path)) {
    rlang::try_fetch(jsonlite::write_json(header, path, pretty = TRUE, auto_unbox = TRUE),
                     error = function(e) {
                       seasonder_logAndMessage(glue::glue("Error while trying to write JSON to path {path}"), "error", calling_function = "seasonder_asJSONSeaSondeRCSHeader", class = "seasonder_write_JSON_error", seasonder_path = path, seasonder_JSON = out, seasonder_cs_obj = seasonder_cs_obj)
                     })
  }

  return(out)
}

#' Convert SeaSondeRCS Object to JSON
#'
#' This function extracts the data from a `seasonder_cs_obj`, representing a SeaSondeRCS object, and converts it into a JSON format. Optionally, it can write this JSON data to a specified file path.
#'
#' @param seasonder_cs_obj A SeaSondeRCS object from which the data will be extracted.
#' @param path Optional path to a file where the JSON output should be saved. If provided, the function will write the JSON data to this file. If NULL, the function will only return the JSON data as a string without writing it to a file.
#'
#' @return A character string in JSON format representing the data of the provided SeaSondeRCS object. If a path is provided, the function also writes this data to the specified file.
#'
#' @export
#'
#' @seealso
#' \code{\link{seasonder_createSeaSondeRCS}}, \code{\link{seasonder_getSeaSondeRCS_data}}
#'
#' @note
#' If a path is provided and there is an issue writing to the file, the function logs an error message using `seasonder_logAndMessage` and returns the JSON data as a string.
seasonder_asJSONSeaSondeRCSData <- function(seasonder_cs_obj, path = NULL) {

  data <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj)

  out <- jsonlite::toJSON(data, pretty = TRUE)

  if (!is.null(path)) {
    rlang::try_fetch(jsonlite::write_json(data, path, pretty = TRUE, auto_unbox = TRUE),
                     error = function(e) {
                       seasonder_logAndMessage(glue::glue("Error while trying to write JSON to path {path}"), "error", calling_function = "seasonder_asJSONSeaSondeRCSHeader", class = "seasonder_write_JSON_error", seasonder_path = path, seasonder_JSON = out, seasonder_cs_obj = seasonder_cs_obj)
                     })
  }

  return(out)
}


seasonder_getSeaSondeRCS_APM <- function(seasonder_cs_object){

  out <- attr(seasonder_cs_object, "APM", exact = T)

  return(out)

}


seasonder_getSeaSondeRCS_doppler_interpolation <- function(seasonder_cs_object){

  out <- attr(seasonder_cs_object, "doppler_interpolation", exact = T)

  return(out)

}

seasonder_getSeaSondeRCS_interpolated_doppler_cells_index <- function(seasonder_cs_object){

  out <- attr(seasonder_cs_object, "interpolated_doppler_cells_index", exact = T)

  return(out)

}

###### Data ######
#' Getter for data
#'
#' @param seasonder_cs_obj SeaSondeRCS object
#'
#' @seealso
#' \code{\link{seasonder_getnRangeCells}}
#' \code{\link{seasonder_getnDopplerCells}}
#' \code{\link{seasonder_initCSDataStructure}}
#'
#' @importFrom rlang %||%
#'
#' @export
seasonder_getSeaSondeRCS_data <- function(seasonder_cs_obj) {
  # TODO: test, document, vignette
  out <- seasonder_cs_obj[["data"]]

  if (is.null(out)) {

    nRangeCells <- seasonder_getnRangeCells(seasonder_cs_obj)
    nDopplerCells <- seasonder_getnDopplerCells(seasonder_cs_obj)
    if (!is.null(nRangeCells) && nRangeCells > 0 && !is.null(nDopplerCells) && nDopplerCells > 0) {
      out <- seasonder_initCSDataStructure(nRanges = nRangeCells, nDoppler = nDopplerCells)
    }

  }

  return(out)
}


seasonder_getSeaSondeRCS_dataMatrix <- function(seasonder_cs_obj, matrix_name) {

  matrix_name %in% c("SSA1","SSA2","SSA3","CS12","CS13","CS23","QC") || seasonder_logAndAbort(glue::glue("Unknown data matrix name '{matrix_name}'"),calling_function = "matrix_name", class = "seasonder_unknown_data_matrix_name", seasonder_matrix_name = matrix_name)

  matrix <- seasonder_getSeaSondeRCS_data(seasonder_cs_obj = seasonder_cs_obj)[[matrix_name]]

  return(matrix)

}

#' returns the power spectrum of an antenna
seasonder_getSeaSondeRCS_antenna_SSdata <- function(seasonder_cs_obj, antenna) {

  matrix_name <- paste0("SSA",antenna)

  matrix <- seasonder_getSeaSondeRCS_dataMatrix(seasonder_cs_obj = seasonder_cs_obj, matrix_name = matrix_name)

  return(matrix)

}

seasonder_extractSeaSondeRCS_distRanges_from_SSdata <- function(SSmatrix, dist_ranges) {

  # TODO: check that dist_ranges is in the matrix range

  sliced_SSmatrix <- SSmatrix[dist_ranges,, drop = FALSE]

  return(sliced_SSmatrix)

}



seasonder_extractSeaSondeRCS_dopplerRanges_from_SSdata <- function(SSmatrix, doppler_cells) {


  # TODO: check that doppler_cells is in the matrix range

  sliced_SSmatrix <- SSmatrix[,doppler_cells, drop = FALSE]

  return(sliced_SSmatrix)

}


#'  returns a list of power spectra for each combination of antenna, dist_range and doppler_range
seasonder_getSeaSondeRCS_SelfSpectra <- function(seasonder_cs_obj, antennae, dist_ranges = NULL, doppler_ranges = NULL, dist_in_km = FALSE, collapse = FALSE) {


  out <- list()



  doppler_ranges <- doppler_ranges %||% list(all_doppler = range(seq_len(seasonder_getnDopplerCells(seasonder_cs_obj))))

  dist_ranges <- dist_ranges %||% list(all_ranges = range(seq_len(seasonder_getnRangeCells(seasonder_cs_obj))))


  if (!rlang::is_list(dist_ranges)) {
    dist_ranges <- list(dist_ranges)
  }


  if (!rlang::is_list(doppler_ranges)) {
    doppler_ranges <- list(doppler_ranges)
  }


  if (!rlang::is_named(antennae)) {
    antennae %<>% magrittr::set_names(sprintf("A%d",as.integer(antennae)))
  }

  if (!rlang::is_named(dist_ranges)) {
    dist_ranges %<>% magrittr::set_names(sprintf("dist_range_%d",1:length(dist_ranges)))
  }

  if (!rlang::is_named(doppler_ranges)) {
    doppler_ranges %<>% magrittr::set_names(sprintf("doppler_range_%d",1:length(doppler_ranges)))
  }

  # TODO: option for all antennae, all dist_ranges and all doppler_ranges
  # TODO: wrappers for antenna + dist_ranges, antenna + doppler ranges, disr_ranges + doppler ranges, dist_ranges, antenna and doppler ranges.


  SSMatrices <- antennae %>% purrr::map(\(antenna) seasonder_getSeaSondeRCS_antenna_SSdata(seasonder_cs_obj,antenna))

  # Slice dist_ranges

  if (dist_in_km) {
    dist_ranges %<>% purrr::map(\(dists) {

      dists <- seasonder_rangeCellsDists2RangeNumber(seasonder_cs_obj, dists)

      dists[1] <- floor(dists[1])

      dists[2] <- ceiling(dists[2])

      return(dists)

    })

  }




  out <- SSMatrices %>% purrr::map(\(SSmatrix) {

    sliced_matrix <- dist_ranges %>% purrr::map(\(dists) {

      dist_slice <- seasonder_extractSeaSondeRCS_distRanges_from_SSdata(SSmatrix = SSmatrix, dist_ranges = seq(dists[1],dists[2]))


      dist_doppler_slice <- doppler_ranges  %>% purrr::map(\(doppler_cells) {

        doppler_slice <- seasonder_extractSeaSondeRCS_dopplerRanges_from_SSdata(SSmatrix = dist_slice, doppler_cells = seq(doppler_cells[1], doppler_cells[2]))




        return(doppler_slice)

      })
      return(dist_doppler_slice)
    })

    return(sliced_matrix)
  })



  if (collapse) {
    out %<>% purrr::list_flatten(name_spec = "{outer}:{inner}") %>% purrr::list_flatten(name_spec = "{outer}:{inner}")
  }



  return(out)
}


###### Metadata ######

#' Getter for ProcessingSteps
#'
#' @param seasonder_cs_obj SeaSonderCS object
#'
#' @export
seasonder_getSeaSondeRCS_ProcessingSteps <- function(seasonder_cs_obj) {
  return(attributes(seasonder_cs_obj)$ProcessingSteps)
}

#' Get the version value from a SeaSondeRCS object
#'
#' @param seasonder_obj A SeaSondeRCS object.
#' @return The version value.
#' @export
seasonder_getVersion.SeaSondeRCS <- function(seasonder_obj) {

  attr(seasonder_obj,"version",exact = TRUE)
}




###### Header Fields ######
#' Retrieve a value from the SeaSondeRCS header by a specific path
#'
#' This function retrieves a specific value from the SeaSondeRCS object's header based on the provided path.
#' The path can be a single field name or a list of nested field names.
#'
#' @param seasonder_obj A SeaSondeRCS object.
#' @param path A character vector specifying the field or nested fields to retrieve.
#'
#' @return The value at the specified path in the header. If the path is not found, NULL is returned and a warning is thrown.
#'
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage errors and conditions, and provide detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_SeaSonderCS_field_not_found_in_header}: Indicates that the specified path was not found in the header.
#' }
#'
#' \strong{Condition Cases}:
#' \itemize{
#'   \item Field or nested fields specified by the path are not found in the header.
#' }
#'
#' @export
seasonder_getCSHeaderByPath <- function(seasonder_obj, path) {

  header <- seasonder_getSeaSondeRCS_header(seasonder_obj)

  # Use purrr::pluck to extract the value from the header
  result <- rlang::inject(purrr::pluck(header, !!!path))

  # If the result is NULL, log a warning
  if (is.null(result)) {
    path_str <- paste0(path, collapse = "/")
    warning_msg <- glue::glue("Field '{path_str}' not found in header.", path_str = path_str)
    seasonder_logAndMessage(warning_msg, "error", calling_function = "seasonder_getCSHeaderByPath", class = "seasonder_SeaSonderCS_field_not_found_in_header")
  }

  return(result)
}

seasonder_getSeaSondeRCS_headerField <- function(seasonder_cs_obj,field) {

  header <- seasonder_getSeaSondeRCS_header(seasonder_cs_obj)

  value <- purrr:::list_flatten(header,name_spec = "{inner}") %>% purrr::pluck(field)

  return(value)

}

#' Get the nRangeCells value from a SeaSondeRCS object
#'
#' @param seasonder_obj A SeaSondeRCS object.
#' @return The nRangeCells value.
#' @export
seasonder_getnRangeCells <- function(seasonder_obj) {
  return(seasonder_getSeaSondeRCS_headerField(seasonder_obj, "nRangeCells"))
}

#' Get the nDopplerCells value from a SeaSondeRCS object
#'
#' @param seasonder_obj A SeaSondeRCS object.
#' @return The nDopplerCells value.
#' @export
seasonder_getnDopplerCells <- function(seasonder_obj) {

  out <- seasonder_getSeaSondeRCS_headerField(seasonder_obj, "nDopplerCells")

  doppler_interpolation <- seasonder_getSeaSondeRCS_doppler_interpolation(seasonder_obj) %||% 1L

  if(doppler_interpolation > 1L){
    out <- (out -1) * doppler_interpolation +1
  }



  return(out)
}

seasonder_getCellsDistKm <- function(seasonder_cs_obj) {
  return(seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "CellsDistKm"))
}

seasonder_getCenterFreqMHz <- function(seasonder_cs_obj) {
  return(seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "CenterFreq"))
}

###### + Derived parameters ######

seasonder_getReceiverGain_dB <- function(seasonder_cs_obj) {



  receiver_gain <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "fReferenceGainDB") %||% -34.2 # dB


  return(receiver_gain)

}


seasonder_getCenterDopplerBin <- function(seasonder_cs_obj) {

  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  center_bin <- nDoppler / 2

  return(center_bin)


}

seasonder_getRadarWaveLength <- function(seasonder_cs_obj) {

  CenterFreq <- seasonder_getCenterFreqMHz(seasonder_cs_obj)*1000000

  c <- constants::syms$c0

  l <- c/(CenterFreq) # (m/s)/(Hz) = m

  return(l)


}

seasonder_getRadarWaveNumber <- function(seasonder_cs_obj) {


  l <- seasonder_getRadarWaveLength(seasonder_cs_obj)

  k <- 2*pi/l

  return(k)
}




seasonder_getBraggWaveLength <- function(seasonder_cs_obj) {

  l <- seasonder_getRadarWaveLength(seasonder_cs_obj)

  lB <- l/2

  return(lB)


}

seasonder_getBraggDopplerAngularFrequency <- function(seasonder_cs_obj) {

  k <- seasonder_getRadarWaveNumber(seasonder_cs_obj = seasonder_cs_obj)

  wb <- sqrt(2*constants::syms$gn*k) / (2*pi) * c(-1,1)

  return(wb)

}

seasonder_getDopplerSpectrumResolution <- function(seasonder_cs_obj) {

  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  SweepRate <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "fRepFreqHz")

  spectral_resolution <- SweepRate/nDoppler

  return(spectral_resolution)

}

seasonder_getBraggLineBins <- function(seasonder_cs_obj) {

  bins <- seasonder_NormalizedDopplerFreq2Bins(seasonder_cs_obj, c(-1,1))
  return(bins)

}

#' Get Doppler Bins Frequency
#'
#' This function calculates the frequency limits for each Doppler bin within a SeaSonde Cross Spectrum (CS) object. It can return frequencies either in their original Hz values or normalized by the second Bragg frequency. The frequencies are calculated as the high limit of each Doppler bin interval, similar to what is displayed in SpectraPlotterMap.
#'
#' @param seasonder_cs_obj A SeaSonde Cross Spectrum (CS) object created by `seasonder_createSeaSondeRCS()`. This object contains the necessary metadata and spectral data to compute Doppler bin frequencies.
#' @param normalized A logical value indicating if the returned frequencies should be normalized by the second Bragg frequency. When `TRUE`, frequencies are divided by the second Bragg frequency, returning dimensionless values relative to it. Default is `FALSE`, returning frequencies in Hz.
#'
#' @return A numeric vector of frequencies representing the high limit of each Doppler bin interval. If `normalized` is TRUE, these frequencies are dimensionless values relative to the second Bragg frequency; otherwise, they are in Hz.
#'
#' @details The function internally utilizes several helper functions such as `seasonder_getCenterDopplerBin()`, `seasonder_getnDopplerCells()`, and `seasonder_getDopplerSpectrumResolution()` to calculate the Doppler bin frequencies. Furthermore, when normalization is requested, it uses `seasonder_getBraggDopplerAngularFrequency()` to obtain the second Bragg frequency for normalization purposes.
#'
#' @importFrom dplyr last
seasonder_getDopplerBinsFrequency <- function(seasonder_cs_obj, normalized = FALSE) {

  center_bin <- seasonder_getCenterDopplerBin(seasonder_cs_obj) # Freq 0

  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  spectra_res <- seasonder_getDopplerSpectrumResolution(seasonder_cs_obj)

  frequencies <- (seq(1,nDoppler) - center_bin) * spectra_res


  if (normalized) {

    bragg_freq <- seasonder_getBraggDopplerAngularFrequency(seasonder_cs_obj)[2]


    frequencies <- frequencies / bragg_freq


  }

  return(frequencies)


}



#' Calculate Radial Velocities for Each Doppler Bin
#'
#' Computes the radial velocities for each Doppler bin interval's high boundary
#' for a SeaSonde radar cross-section (CS) object, as typically visualized in
#' SpectraPlotterMap. This function utilizes the Doppler shift frequency alongside
#' the radar's wave number and Bragg frequency to transform frequency measurements
#' into radial velocities. The calculation is grounded on the relationship
#' between the Doppler shift frequency and the velocity of the surface currents
#' within the radar's field of view.
#'
#' Specifically, the radial velocity \(v\) for each Doppler bin is calculated using the formula:
#' \[
#' v = \frac{\text{Freq} - \text{BraggFreq}}{2 \cdot k_0}
#' \]
#' where \(v\) is the radial velocity, \(\text{Freq}\) is the Doppler shift frequency for the bin, \(\text{BraggFreq}\) is the Bragg
#' frequency (negative for frequencies below 0 and positive for frequencies equal or above 0), and \(k_0\) is the radar wave number
#' divided by \(2\pi\).
#'
#' @param seasonder_cs_obj A SeaSondeRCS object created using `seasonder_createSeaSondeRCS`. This object
#'        contains the necessary data for calculating the Doppler bins frequencies and, subsequently, radial velocities.
#'
#' @return A numeric vector containing the radial velocities (in m/s) for each
#' Doppler bin, calculated for the high boundary of each Doppler bin interval.
#' The velocities provide insight into the scatterers' radial movement within the
#' radar's observation area.
#'
#' @seealso \code{\link{seasonder_getDopplerBinsFrequency}},
#'          \code{\link{seasonder_getBraggDopplerAngularFrequency}},
#'          \code{\link{seasonder_getRadarWaveNumber}}
seasonder_getBinsRadialVelocity <- function(seasonder_cs_obj) {

  freq <- seasonder_getDopplerBinsFrequency(seasonder_cs_obj)

  bragg_freq <- seasonder_getBraggDopplerAngularFrequency(seasonder_cs_obj)


  k0 <- seasonder_getRadarWaveNumber(seasonder_cs_obj)/(2*pi)

  v <- c((freq[freq < 0]  - bragg_freq[1])/(2*k0),(freq[freq >= 0]  - bragg_freq[2])/(2*k0))


  return(v)


}

#' Calculate Radial Velocity Resolution
#'
#' Computes the radial velocity resolution for a SeaSonde radar cross-section (CS) object.
#' This measurement indicates the smallest change in velocity that the radar can
#' discern between different targets or scatterers within its observation area.
#' The calculation is based on the Doppler spectrum resolution and the radar wave
#' number, providing a crucial parameter for analyzing the radar's capability to
#' distinguish between velocities.
#'
#' The radial velocity resolution (\(v_{res}\)) is determined using the formula:
#' \[
#' v_{res} = \frac{\text{SpectraRes}}{2 \cdot k_0}
#' \]
#' where \(v_{res}\) is the radial velocity resolution, \(\text{SpectraRes}\) is
#' the Doppler spectrum resolution, and \(k_0\) is the radar wave number divided
#' by \(2\pi\). This formula reflects the relationship between the
#' frequency resolution of the radar's Doppler spectrum and the corresponding
#' velocity resolution, taking into account the wave number which is a fundamental
#' characteristic of the radar system.
#'
#' @param seasonder_cs_obj A SeaSondeRCS object created using `seasonder_createSeaSondeRCS`. This object
#'        contains the necessary data to calculate the Doppler spectrum resolution and, subsequently, the
#'        radial velocity resolution.
#'
#' @return A single numeric value representing the radial velocity resolution in meters per second (m/s),
#'         indicating the radar's ability to differentiate between closely spaced velocities.
#'
#' @seealso \code{\link{seasonder_getDopplerSpectrumResolution}},
#'          \code{\link{seasonder_getRadarWaveNumber}}
seasonder_getRadialVelocityResolution <- function(seasonder_cs_obj) {

  spectra_res <- seasonder_getDopplerSpectrumResolution(seasonder_cs_obj)

  k0 <- seasonder_getRadarWaveNumber(seasonder_cs_obj)/(2*pi)

  vel_res <- spectra_res / (2*k0)


  return(vel_res)
}

##### Utils #####

seasonder_rangeCellsDists2RangeNumber <- function(seasonder_cs_obj,cells_dists) {

  # TODO: check that the cs file version is at least V4

  fRangeCellDistKm <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "fRangeCellDistKm")

  nFirstRangeCell <- seasonder_getSeaSondeRCS_headerField(seasonder_cs_obj, "nFirstRangeCell")

  # NOTE: based on File_Cross_Spectra_V6 page 4

  range_numbers <- cells_dists/fRangeCellDistKm - nFirstRangeCell + 1

  return(range_numbers)
}

seasonder_SelfSpectra2dB <- function(seasonder_cs_obj, spectrum_values) {

  receiver_gain <- seasonder_getReceiverGain_dB(seasonder_cs_obj)

  spectrum_dB <- 10 * log10(abs(spectrum_values)) - receiver_gain

  return(spectrum_dB)

}


seasonder_Bins2NormalizedDopplerFreq <- function(seasonder_cs_obj, bins) {

  normalized_doppler_freqs <- seasonder_getDopplerBinsFrequency(seasonder_cs_obj, normalized = TRUE)

  return(normalized_doppler_freqs[bins])


}

seasonder_NormalizedDopplerFreq2Bins <- function(seasonder_cs_obj, doppler_values) {

  normalized_doppler_freqs <- seasonder_getDopplerBinsFrequency(seasonder_cs_obj, normalized = TRUE)

  delta_freq <- normalized_doppler_freqs %>% diff()

  boundaries <- c(normalized_doppler_freqs[1] - delta_freq[1], normalized_doppler_freqs)

  bins <- findInterval(doppler_values,boundaries, rightmost.closed = T, all.inside = F, left.open = T)

  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  bins[bins < 1 | bins > nDoppler] <- NA_integer_



  return(bins)

}


seasonder_DopplerFreq2Bins <- function(seasonder_cs_obj, doppler_values) {

  doppler_freqs <- seasonder_getDopplerBinsFrequency(seasonder_cs_obj, normalized = FALSE)

  delta_freq <- seasonder_getDopplerSpectrumResolution(seasonder_cs_obj)

  boundaries <- c(doppler_freqs[1] - delta_freq, doppler_freqs)

  bins <- findInterval(doppler_values,boundaries, rightmost.closed = T, all.inside = F,left.open = T)

  nDoppler <- seasonder_getnDopplerCells(seasonder_cs_obj)

  bins[bins < 1 | bins > nDoppler] <- NA_integer_



  return(bins)

}


seasonder_Bins2DopplerFreq <- function(seasonder_cs_obj, bins) {

  doppler_freqs <- seasonder_getDopplerBinsFrequency(seasonder_cs_obj, normalized = FALSE)

  return(doppler_freqs[bins])



}

seasonder_DopplerFreq2NormalizedDopplerFreq <- function(seasonder_cs_obj, doppler_values) {

  bins <- seasonder_DopplerFreq2Bins(seasonder_cs_obj, doppler_values)

  normalized_doppler_freq <- seasonder_Bins2NormalizedDopplerFreq(seasonder_cs_obj, bins)


  return(normalized_doppler_freq)



}


seasonder_NormalizedDopplerFreq2DopplerFreq <- function(seasonder_cs_obj, doppler_values) {

  bins <- seasonder_NormalizedDopplerFreq2Bins(seasonder_cs_obj, doppler_values)

  doppler_freq <- seasonder_Bins2DopplerFreq(seasonder_cs_obj, bins)


  return(doppler_freq)



}

seasonder_SwapDopplerUnits <- function(seasonder_cs_obj, values, in_units, out_units) {



  doppler_units_options <- c("normalized doppler frequency","bins","doppler frequency")

  in_units %in% doppler_units_options || seasonder_logAndAbort(glue::glue("in_units is '{in_units}', but should be one of {paste0(doppler_units_options, collapse=', ')}"),calling_function = "seasonder_SwapDopplerUnits")

  out_units %in% doppler_units_options || seasonder_logAndAbort(glue::glue("out_units is '{out_units}', but should be one of {paste0(doppler_units_options, collapse=', ')}"),calling_function = "seasonder_SwapDopplerUnits")

  if (in_units == out_units) {

    return(values)
  }

  swap_functions <- list("normalized doppler frequency" = list("bins" = seasonder_NormalizedDopplerFreq2Bins,
                                                               "doppler frequency" = seasonder_NormalizedDopplerFreq2DopplerFreq),
                         "bins" = list("normalized doppler frequency" = seasonder_Bins2NormalizedDopplerFreq,
                                       "doppler frequency" = seasonder_Bins2DopplerFreq),
                         "doppler frequency" = list("bins" = seasonder_DopplerFreq2Bins,
                                                    "normalized doppler frequency" = seasonder_DopplerFreq2NormalizedDopplerFreq)
  )

  swap_fun <- swap_functions[[in_units]][[out_units]]

  out <- swap_fun(seasonder_cs_obj, values)

  return(out)


}



##### Plot #####


seasonder_SeaSondeRCS_plotSelfSpectrum <- function(seasonder_cs_obj, antenna, range_dist, doppler_units = "normalized doppler frequency", plot_FORs = FALSE) {

  SS <- NULL

  spectrum <- seasonder_getSeaSondeRCS_SelfSpectra(seasonder_cs_obj = seasonder_cs_obj, antennae = antenna,dist_ranges = c(range_dist[1],range_dist[1]), collapse = TRUE)[[1]] %>% t() %>% as.data.frame() %>% magrittr::set_colnames("SS")

  doppler_values <- seasonder_SwapDopplerUnits(seasonder_cs_obj,seasonder_getDopplerBinsFrequency(seasonder_cs_obj), in_units = "doppler frequency", out_units = doppler_units)

  spectrum %<>% dplyr::mutate(doppler = doppler_values, SS = seasonder_SelfSpectra2dB(seasonder_cs_obj, SS))


  Bragg_freq <- seasonder_getBraggDopplerAngularFrequency(seasonder_cs_obj)

  if (doppler_units == "normalized doppler frequency") {
    Bragg_freq <- c(-1,1)
  }

  out <- ggplot2::ggplot(spectrum, ggplot2::aes(y = SS, x = doppler)) + ggplot2::geom_line() + ggplot2::geom_vline(xintercept = Bragg_freq, color = "red") + ggplot2::theme_bw()



  if (plot_FORs) {


    smoothed_spectrum <- seasonder_getSeaSondeRCS_FOR_SS_Smoothed(seasonder_cs_obj)[range_dist,, drop = TRUE]


    if (!is.null(smoothed_spectrum)) {

      smoothed_data <- data.frame(SS =  seasonder_SelfSpectra2dB(seasonder_cs_obj,smoothed_spectrum), doppler = doppler_values)

      out <- out + ggplot2::geom_line(data = smoothed_data, color = "orange", size = 1)
    }



    FOR <- seasonder_getSeaSondeRCS_FOR(seasonder_cs_obj)[[range_dist]]

    if (!is.null(FOR)) {

      FOR %<>% unlist()
      FOR <- seasonder_SwapDopplerUnits(seasonder_cs_obj, FOR, "bins", doppler_units)
      FOR_data <- data.frame(xintercept = FOR)

      out <- out + ggplot2::geom_vline(data = FOR_data, ggplot2::aes(xintercept = xintercept), color = "blue", alpha = 0.1)


    }




    noise_level <- seasonder_getSeaSondeRCS_NoiseLevel(seasonder_cs_obj, dB = T)[range_dist] %>% magrittr::set_names(NULL)

    reference_noise_normalized_limits <- seasonder_getSeaSondeRCS_FOR_reference_noise_normalized_limits(seasonder_cs_obj)

    if (!is.null(noise_level) && !is.null(reference_noise_normalized_limits)) {

      positive_noise_range <- seasonder_SwapDopplerUnits(seasonder_cs_obj, reference_noise_normalized_limits, in_units = "normalized doppler frequency", out_units = doppler_units)

      negative_noise_range <- seasonder_SwapDopplerUnits(seasonder_cs_obj,-1 * reference_noise_normalized_limits, in_units = "normalized doppler frequency", out_units = doppler_units)

      positive_noise_data <- data.frame(SS = noise_level, doppler = c(positive_noise_range))
      negative_noise_data <- data.frame(SS = noise_level, doppler = c(negative_noise_range))
      out <- out + ggplot2::geom_line(data = positive_noise_data, color = "red", size = 2) + ggplot2::geom_line(data = negative_noise_data, color = "red", size = 2)

    }

  }

  return(out)

}




#### Processing_steps ####

#' Generate Creation Step Text
#'
#' This function generates a text message indicating the time an CS object was created based on the current system time and the provided file path.
#'
#' @param file_path A character string specifying the path to the file.
#'
#' @return
#' A character string with the formatted message indicating the time of creation and the file path.
#'
SeaSondeRCS_creation_step_text <- function(file_path) {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: Created from {file_path}.")
}


#### Read CS File ####


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
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage conditions and provide detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_validate_cs_file_error}: An error class that indicates a validation requirement was not met.
#' }
#'
#' \strong{Condition Cases}:
#' \itemize{
#'   \item Failure on any validation test.
#' }
#'
#' @return NULL invisibly. The function mainly serves to validate and will stop execution and log an error using `seasonder_logAndAbort` if any condition fails.
#'
#'
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

  # CODAR documentation is not correct, they are double counting the number of spectra channels when they multiply by nSpectraChannels and by 36 or 40
  if (header$nCsKind == 1 && file_size < (header_size + header$nRangeCells *  header$nDopplerCells * 36)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file size for nCsKind 1 in file {filepath}. Expected >= {(header_size + header$nRangeCells  * header$nDopplerCells * 36)}, actual: {file_size}."),!!!conditions_params))



  if (header$nCsKind == 2 && file_size < (header_size + header$nRangeCells *  header$nDopplerCells * 40)) rlang::inject(seasonder_logAndAbort(glue::glue("Invalid file size for nCsKind 2 in file {filepath}. Expected >= {(header_size + header$nRangeCells* header$nDopplerCells * 40)}, actual: {file_size}."),!!!conditions_params))

  return(NULL)

}

#' Skip SeaSonde Cross Spectra (CS) File Reading
#'
#' This function serves as a restart for `seasonder_readSeaSondeCSFile`. When invoked, it provides a
#' mechanism to gracefully handle file reading errors by logging an error message and skipping the current file processing.
#'
#' @param cond The condition or error that occurred during the file reading process. This is used
#' to log a detailed error message indicating the reason for skipping the file.
#'
#' @details
#' This function is meant to be used within a custom condition handler. When a problematic condition
#' arises during the processing of a SeaSonde CS file, you can call `seasonder_skip_cs_file(cond)` to
#' trigger this restart, which allows for a graceful degradation by logging an error message and skipping the file.
#'
#' The effect of invoking this restart is twofold:
#' 1. An error message detailing the reason for skipping the file is logged.
#' 2. The calling function (`seasonder_readSeaSondeCSFile`) will immediately return a list with `header = NULL` and `data = NULL`.
#'
#'
#' @return If invoked, the function returns a list with both `header` and `data` set to NULL.
#' @export
seasonder_skip_cs_file <- function(cond) {
  invokeRestart("seasonder_skip_cs_file",cond)
}

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
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage conditions and provide detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_read_cs_file_error}: An error class that indicates a general problem when attempting to read the SeaSonde CS file.
#'   \item \code{seasonder_cs_file_skipped}: Condition indicating that the processing of a CS file was skipped due to an error.
#' }
#'
#' \strong{Condition Cases}:
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
#'   \item{\code{seasonder_skip_cs_file(cond)}}{This allows for the graceful handling of file reading errors. If this restart is invoked, the function will log an error message indicating that the processing of a specific CS file was skipped and will return a list with `header = NULL` and `data = NULL`. The restart takes one argument: \code{cond} (the condition or error that occurred).
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
#' \code{\link{seasonder_skip_cs_file}},
#' \code{\link{seasonder_validateCSFileData}},
#' \code{\link{seasonder_readSeaSondeCSFileHeader}},
#' \code{\link{seasonder_readSeaSondeCSFileData}},
#' \code{\link{seasonder_readYAMLSpecs}}
#'
#' @references Cross Spectra File Format Version 6. CODAR. 2016
#' @export
#'
seasonder_readSeaSondeCSFile <- function(filepath, specs_path) {

  conditions_params <- list(calling_function = "seasonder_readSeaSondeCSFile",class = "seasonder_read_cs_file_error",seasonder_cs_filepath = filepath,seasonder_cs_specs_path = specs_path)

  withRestarts(
    seasonder_skip_cs_file = function(cond) {
      seasonder_logAndMessage(glue::glue("An issue happened while processing the file {cond$seasonder_cs_filepath %||% ''}, skipping. Issue: {conditionMessage(cond)}"),"error",calling_function = "seasonder_readSeaSondeCSFile",class = "seasonder_cs_file_skipped",parent = cond)
      return(list(header = NULL,data = NULL))
    },
    {


      connection <- rlang::try_fetch(
        suppressWarnings(file(filepath, "rb")),
        error = function(e) {
          rlang::inject(seasonder_logAndAbort(glue::glue("Could no open connection to file {filepath %||% ''}. Reason: {conditionMessage(e)}."),!!!conditions_params,parent = e))
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








#' Convert an integer to raw bytes using a 64-bit representation
#'
#' This function converts an integer to a raw byte representation using a 64-bit (8-byte) format.
#' It leverages the `bit64` package to handle the 64-bit integer representation and conversion.
#'
#' @param x An integer to be converted to raw bytes.
#'
#' @return A raw vector representing the 64-bit format of the provided integer.
#'
#' @details
#' The function follows these steps:
#' 1. Convert the integer to a 64-bit format using `bit64::as.integer64`.
#' 2. Convert the 64-bit integer to a bit string.
#' 3. Split the bit string into individual bits.
#' 4. Reorder the bits into groups of 8, reversing the order within each group.
#' 5. Convert the reordered bits back to raw bytes.
#'
#'
#' @examples
#' \dontrun{
#' int_val <- 1234567890
#' raw_val <- seasonder_int_to_raw(int_val)
#' cat(rawToChar(raw_val))
#' }
#'
#' @importFrom bit64 as.integer64
seasonder_int_to_raw <- function(x) {

  # Convert the integer to a 64-bit bitstring
  x_bitstr <- bit64::as.bitstring(bit64::as.integer64(x))

  # Split the bitstring into individual bits
  x_bits <- strsplit(x_bitstr, "")[[1]]

  # Convert the bits to integers and reorder them
  x_integers <- as.integer(x_bits)
  x_integers_mtrx_rev <- matrix(x_integers, ncol = 8, byrow = TRUE)[, 8:1]
  x_integers_mtrx_rev_transposed <- t(x_integers_mtrx_rev)

  # Flatten the matrix into a vector
  x_int_vect <- c(x_integers_mtrx_rev_transposed)

  # Convert the bit sequence to raw bytes
  x_packed <- packBits(x_int_vect, "raw")
  out <- as.raw(x_packed)

  return(out)
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
#' \dontrun{
#' r <- as.raw(c(0x12,0x34,0x56,0x78,0x90,0xAB,0xCD,0xEF))
#' seasonder_raw_to_int(r, signed = TRUE)
#' }
seasonder_raw_to_int <- function(r,signed = F) {
  # Convert raw values to bits and collapse into a single bit string.
  bit_str <- sapply(r,FUN = function(x) rev(rawToBits(x))) %>% as.integer() %>% paste0(collapse = "")
  class(bit_str) <- "bitstring"

  # Convert the bit string into a 64-bit integer.
  int_val <-   bit64::as.integer64(bit_str)

  return(int_val)

}

#' Skip Reading a CSField and Return a Specified Value
#'
#' This function is a convenience mechanism to invoke the `seasonder_skip_cs_field` restart option. It can be used in custom condition handlers when reading a CSField from a binary connection encounters an error or condition. When called, it indicates the intention to skip reading the current CSField and return a specific value.
#'
#' @param cond A condition or error that occurred while reading the CSField.
#' @param value The desired return value to use in place of the CSField reading that encountered an error.
#'
#' @details
#' During the execution of the `seasonder_readCSField` function, errors or conditions can occur. To provide a structured mechanism to handle such cases, the function utilizes the `rlang::withRestarts` mechanism, offering a restart option named `seasonder_skip_cs_field`. This restart allows the function to gracefully handle reading errors by logging a relevant error message and returning a specified value.
#'
#' The `seasonder_skip_cs_field` function provides an easy way to invoke this restart. When called within a custom condition handler, it signals the intention to skip the current CSField reading due to an error and specifies a return value.
#'
#' @return Returns the value specified by the `value` parameter.
#'
#'
#' @export
seasonder_skip_cs_field <- function(cond,value) {
  invokeRestart("seasonder_skip_cs_field",cond,value)
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
#' @importFrom rlang !!!
#' @examples
#' \dontrun{
#' con <- rawConnection(as.raw(c(0x12)))
#' seasonder_readCSField(con, "UInt8")
#' }
#' @seealso
#' \code{\link{seasonder_skip_cs_field}},
#' \code{\link{seasonder_raw_to_int}}
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
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage conditions and provide detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_cs_field_reading_error}: General error related to reading a CSField from the binary connection.
#'   \item \code{seasonder_cs_field_skipped}: Condition that indicates a CSField was skipped due to a reading error.
#' }
#'
#' \strong{Condition Cases}:
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
seasonder_readCSField <- function(con, type, endian = "big") {

  # Parameters used for error messages and logging.
  conditions_params <- list(calling_function = "seasonder_readCSField",class = "seasonder_cs_field_reading_error",seasonder_cs_field_type = type,seasonder_cs_endian = endian)

  out <- withRestarts(seasonder_skip_cs_field = function(cond,value) {
    # Log the error message and skip the CS field.
    seasonder_logAndMessage(glue::glue("Skipping CS field, returning {value}: {conditionMessage(cond)}"), "error", calling_function = "seasonder_readCSField", class = "seasonder_cs_field_skipped", parent = cond, seasonder_cs_field_value = value)
    return(value)
  },
  {
    # Ensure the connection is open before proceeding.
    open_con <- try(isOpen(con), silent = T)
    if (!inherits(open_con, "try-error")) {
      if (!open_con) {
        rlang::inject(seasonder_logAndAbort("Connection is not open.", !!!conditions_params))
      }
    } else {
      rlang::inject(seasonder_logAndAbort("Connection is not open.", !!!conditions_params,parent = attr(open_con,"condition")))
    }

    # Helper function to safely read from the connection.
    read_values <- function(bytes, format, n = 1, signed = T) {
      res <- rlang::try_fetch({
        out <- readBin(con, what = format, n = n, size = bytes, endian = endian, signed = signed)
        # If nothing is read, it could be the end of the file.
        if (length(out) == 0) {
          rlang::abort("Read value of length 0. Possibly reached end of file.")
        }
        out
      },
      error = function(e) {
        rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading value: {conditionMessage(e)}"), !!!conditions_params, parent = e))
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
           "UInt8" = as.integer(read_values(1, "raw", signed = F)),
           "SInt8" = as.integer(read_values(1, "integer")),
           "UInt16" = as.integer(read_values(2, "int", signed = F)),
           "SInt16" = as.integer(read_values(2, "int")),
           "UInt32" = bitops::bitAnd(read_values(4, "integer"),0xFFFFFFFF),
           "SInt32" = as.integer(read_values(4, "int")),
           "Float" = as.numeric(read_values(4, "numeric")),
           "Double" = as.numeric(read_values(8, "double")),
           "UInt64" = {
             v <- read_values(1, "raw", n = 8, signed = F)
             seasonder_raw_to_int(v, signed = FALSE)
           },
           "SInt64" = {
             v <- read_values(1, "raw", n = 8)
             seasonder_raw_to_int(v, signed = TRUE)
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


#' Structured Restart for Quality Control
#'
#' Provides a structured restart mechanism to rerun the quality control (QC) function
#' with an alternative function during the execution of `read_and_qc_field`.
#' This allows for a flexible error recovery strategy when the initial QC function fails
#' or is deemed inadequate.
#'
#' This function is meant to be used within custom condition handlers for the
#' `read_and_qc_field` function.
#'
#' @param cond The condition object captured during the execution of the
#' `read_and_qc_field` function.
#' @param qc_fun An alternate quality control function to apply. This function should accept
#' the value from the field as its sole argument and return a QC-applied value.
#'
#' @return The value returned by the alternate QC function `qc_fun`.
#' @export
seasonder_rerun_qc_with_fun <- function(cond,qc_fun) {
  invokeRestart("seasonder_rerun_qc_with_fun",cond,qc_fun)
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
#'
#' @return The value of the field after quality control. Can be the original value, a transformed value,
#'   or NULL if the value fails quality control. The exact behavior of the quality control function, including
#'   the handling of NULL values, is detailed in `seasonder_readSeaSondeCSFileBlock`.
#'
#'
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage conditions and provide detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{seasonder_cs_field_skipped}: Condition that indicates a CSField was skipped during reading.
#'   \item \code{seasonder_cs_field_qc_fun_rerun}: Condition that indicates a rerun of the quality control function was triggered.
#'   \item \code{seasonder_cs_field_qc_fun_not_defined_error}: Error raised when the quality control function specified is not found in the shared environment `seasonder_the`.
#'   \item \code{seasonder_cs_field_qc_fun_error}: Error raised when an issue occurs while applying the quality control function.
#' }
#'
#' \strong{Condition Cases}:
#' \itemize{
#'   \item If a CSField is skipped during reading, the condition \code{seasonder_cs_field_skipped} is used to skip QC and then is re-signaled.
#'   \item If an alternate QC is rerun using the \code{seasonder_rerun_qc_with_fun} restart, the condition \code{seasonder_cs_field_qc_fun_rerun} is signaled.
#'   \item If the quality control function specified is not found in the shared environment `seasonder_the`, the error \code{seasonder_cs_field_qc_fun_not_defined_error} is raised.
#'   \item If there's an issue applying the quality control function, the error \code{seasonder_cs_field_qc_fun_error} is raised.
#' }
#'
#' \strong{Restart Options}:
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
#' @seealso
#' \code{\link{seasonder_rerun_qc_with_fun}},
#' \code{\link{seasonder_readCSField}}
#'
#' It's also important to note that within `read_and_qc_field`, the function `seasonder_readCSField` is used. This function has its own error management and restart options, which are detailed in its documentation.
#'

read_and_qc_field <- function(field_spec, connection, endian = "big") {

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

  field_value <- rlang::try_fetch(seasonder_readCSField(connection, field_type, endian = endian),
                                  seasonder_cs_field_skipped = function(cond) {
                                    field_skipped <<- TRUE
                                    rlang::cnd_signal(cond)
                                    return(cond$seasonder_cs_field_value)

                                  }
  )
  if (!field_skipped) {

    field_value_after_qc <-  withRestarts(
      seasonder_rerun_qc_with_fun = function(cond,qc_fun) {
        value <- cond$seasonder_value
        rlang::inject(seasonder_logAndMessage(glue::glue("Rerunning QC on value {value}."),"info",class = "seasonder_cs_field_qc_fun_rerun",!!!conditions_params,parent = cond))
        return(qc_fun(value))

      },
      {

        # Apply quality control

        if (!qc_fun_name %in% names(seasonder_the$qc_functions)) {
          rlang::inject(seasonder_logAndAbort(glue::glue("QC function '{qc_fun_name}' not defined."),!!!conditions_params,class = "seasonder_cs_field_qc_fun_not_defined_error",seasonder_value = field_value))
        }

        # Get the quality control function from the shared environment
        qc_fun <- seasonder_the$qc_functions[[qc_fun_name]]

        # Call the quality control function with the field value and the specified parameters
        field_value_after_qc <- rlang::try_fetch(
          rlang::inject(qc_fun(field_value,!!!qc_params)),
          error = function(e) rlang::inject(seasonder_logAndAbort(glue::glue("An issue happened while applying QC function '{qc_fun_name}'."),!!!conditions_params,class = "seasonder_cs_field_qc_fun_error",seasonder_value = field_value,parent = e))
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
#' @seealso
#' \code{\link{read_and_qc_field}}
#'
#' @return A named list where each entry corresponds to a field that has been read. Each key is
#'   the field name, and its associated value is the data for that field after quality control.

seasonder_readSeaSondeCSFileBlock <- function(spec, connection,endian = "big") {
  # Use purrr::map to apply the read_and_qc_field function to each field specification
  results <- withCallingHandlers(
    purrr::map(spec, \(field_spec)
               read_and_qc_field(field_spec = field_spec,connection = connection, endian = endian)),
    purrr_error_indexed = function(err) {
      parent_err <- err$parent

      parent_err$message <- glue::glue("Field {names(spec)[err$location]}: {err$parent$message}")

      rlang::cnd_signal(parent_err)

    })




  # Return the results
  return(results)
}


#' Validate Field Specifications
#'
#' This function checks if the provided specifications (`specs`) contain entries for all the required fields listed in `fields`.
#'
#' @param specs A list containing field specifications.
#' @param fields A character vector of field names to be checked in the `specs`.
#'
#' @details
#' The function iterates over each field in the `fields` vector and checks if there is an associated entry in the `specs` list.
#' If any field is missing, an error is thrown using `seasonder_logAndAbort` indicating the missing field specification.
#'
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage conditions, and provide detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#'   \item \code{spsr_field_specification_missing_error}: This error is thrown when a required field specification is missing from the `specs` list.
#' }
#'
#' \strong{Condition Cases}:
#' \itemize{
#'   \item Required field specification is missing.
#' }
#'
#'
#' @examples
#' \dontrun{
#' specs <- list(field1 = "spec1", field2 = "spec2")
#' fields <- c("field1", "field2", "field3")
#' seasonder_check_specs(specs, fields) # Throws an error since spec for 'field3' is missing
#' }
#'
seasonder_check_specs <- function(specs, fields) {

  # Use purrr::walk to iterate over each field and check its presence in the specs
  fields %>% purrr::walk(function(field) {
    # Check if the field is present in the specs
    if (is.null(purrr::pluck(specs, field))) {
      # If not, throw an error indicating the missing field specification
      seasonder_logAndAbort(glue::glue("Specifications for field '{field}' not provided"),
                            calling_function = "seasonder_check_specs",
                            class = "spsr_field_specification_missing_error")
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
#' @param prev_data previous header data
#'
#' @seealso
#' \code{\link{seasonder_check_specs}}
#' \code{\link{seasonder_readSeaSondeCSFileBlock}}
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

  results$nDateTime <- as.POSIXct(results$nDateTime, origin = "1904-01-01", tz = "UTC")

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
#' @param prev_data previous header data
#'
#' @seealso
#' \code{\link{seasonder_check_specs}}
#' \code{\link{seasonder_readSeaSondeCSFileBlock}}
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
#' @param prev_data previous header data
#'
#' @seealso
#' \code{\link{seasonder_check_specs}}
#' \code{\link{seasonder_readSeaSondeCSFileBlock}}
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
#' @param prev_data previous header data
#'
#' @seealso
#' \code{\link{seasonder_check_specs}}
#' \code{\link{seasonder_readSeaSondeCSFileBlock}}
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

  results$CenterFreq <- results$fStartFreqMHz + (results$fBandwidthKHz/1000)/2 * -1^(results$bSweepUp == 0)

  results$CellsDistKm <- (seq(1:results$nRangeCells) - 1 + results$nFirstRangeCell) * results$fRangeCellDistKm

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
#' @param prev_data previous header data
#'
#' @seealso
#' \code{\link{seasonder_check_specs}}
#' \code{\link{seasonder_readSeaSondeCSFileBlock}}
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
#' @seealso
#' \code{\link{readV6BlockData}}
#'
#'
#' @export
readV6BlockData <- function(specs, connection, endian = "big", prev_data = NULL, remaining_loops = NULL) {

  # browser(expr = "nReceiverModel" %in% names(specs))

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

#' Trigger Restart for Skipping Transformation
#'
#' This function provides a mechanism to invoke a restart during the reading and
#' transformation process of the SeaSonde CS File Version 6 header. It allows users
#' to skip transformations that may have caused errors and proceed with a provided value.
#'
#' @param cond The condition object that triggered the restart.
#' @param value The provided value to be used when the transformation is skipped.
#'
#' @details
#' This function specifically triggers the `seasonder_v6_skip_transformation` restart
#' that allows for skipping a block transformation in the reading process of the
#' SeaSonde CS File Version 6 header. When triggered, it logs an error message,
#' skips the problematic transformation, and returns the provided value for the block.
#'
#' @section Integration with SeaSonde CS File Reading:
#'
#' The restart mechanism of this function is integrated within the
#' \code{seasonder_readSeaSondeCSFileHeaderV6} function. If an error occurs during
#' the transformation process of a specific block, the restart provides users with
#' an option to skip the problematic transformation and proceed with a fallback value.
#'
#'
#' @return This function does not have a standard return value. Instead, it triggers a restart
#' that can be caught by an enclosing context to handle the error and decide how to proceed.
#'
#' @export
seasonder_v6_skip_transformation <- function(cond, value) {
  invokeRestart("seasonder_v6_skip_transformation", cond, value)
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
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage conditions and provide detailed and structured condition messages:
#'
#' \strong{Condition Classes}:
#' \itemize{
#' \item \code{seasonder_v6_block_transformacion_skipped}: Triggered when a transformation for a specific block is skipped.
#' \item \code{seasonder_v6_transform_function_error}: Triggered when there's an error while applying the transformation function for a V6 header block.
#' \item \code{seasonder_v6_skip_block_error}: Triggered when there's an error while skipping a block.
#'}
#'
#' \strong{Condition Cases}:
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
#' @seealso
#' \code{\link{seasonder_check_specs}}
#' \code{\link{seasonder_readSeaSondeCSFileBlock}}
#' \code{\link{readV6BlockData}}
#' \code{\link{seasonder_v6_skip_transformation}}
#'
#'
#' @export
seasonder_readSeaSondeCSFileHeaderV6 <- function(specs, connection, endian = "big", prev_data = NULL) {
  conditions_params <- list(calling_function = "seasonder_readSeaSondeCSFileHeaderV6")
  # Step 1: Specification Validation
  seasonder_check_specs(specs, c("nCS6ByteSize","block_spec"))

  # Step 2: Field Reading
  nCS6ByteSize <- seasonder_readSeaSondeCSFileBlock(specs["nCS6ByteSize"], connection, endian)$nCS6ByteSize
  results <- list(nCS6ByteSize = nCS6ByteSize)

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
          seasonder_v6_skip_transformation = function(cond, value) {
            # Log the error message and skip the CS field.
            rlang::inject(seasonder_logAndMessage(glue::glue("Skipping transformation for block '{block$nBlockKey}', returning provided value: {conditionMessage(cond)}"), "error", !!!conditions_params, class = "seasonder_v6_block_transformacion_skipped", parent = cond, new_seasonder_block_data = value))
            return(value)
          },
          rlang::try_fetch(
            seasonder_the$transform_functions[[block$nBlockKey]](block_data),
            error = function(err) {
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
        error = function(err) {
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
#' @param prev_data previous header data
#'        or "little". Use the appropriate value depending on the system architecture and the
#'        file's source.
#'
#' @seealso
#' \code{\link{seasonder_readSeaSondeCSFileHeaderV2}}
#' \code{\link{seasonder_readSeaSondeCSFileHeaderV3}}
#' \code{\link{seasonder_readSeaSondeCSFileHeaderV4}}
#' \code{\link{seasonder_readSeaSondeCSFileHeaderV5}}
#' \code{\link{seasonder_readSeaSondeCSFileHeaderV6}}
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
#' @seealso
#' \code{\link{seasonder_readSeaSondeCSFileHeaderV1}}
#' \code{\link{process_version_header}}
#'
#' @return A combined list of all processed headers up to the file version.
#'
seasonder_readSeaSondeCSFileHeader <- function(specs, connection, endian = "big") {
  # Read the general header (Version 1)
  withCallingHandlers({
    header_v1 <- seasonder_readSeaSondeCSFileHeaderV1(specs$V1, connection, endian)
    },
    error = function(err) {
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
    purrr::reduce(versions_to_process, \(pool, version) {

      out <- process_version_header(pool = pool, version = version, specs = specs, connection = connection, endian = endian, prev_data = pool)

      out
    }, .init = header_v1),
    purrr_error_indexed = function(err) {

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
#' @section Condition Management:
#' This function utilizes the `rlang` package to manage errors and conditions, providing detailed and structured messages:
#'
#' \strong{Error Classes}:
#' \itemize{
#'   \item \code{"seasonder_cs_data_reading_error"}: This error is thrown when there is a problem reading the CS file data. This could be due to issues with the connection object or the file itself.
#'   \item \code{"seasonder_cs_missing_header_info_error"}: Thrown if essential header information such as `nRangeCells`, `nDopplerCells`, or `nCsKind` is missing or invalid.
#' }
#'
#' \strong{Error Cases}:
#' \itemize{
#'   \item Connection object is not properly opened or is invalid.
#'   \item Header information is incomplete or improperly formatted.
#'   \item File read operations fail due to incorrect data size, type, or unexpected end of file.
#'   \item Non-numeric values encountered where numeric spectra data is expected.
#' }
#'
#' @return A list containing the matrices for `SSA*`, `CSxy`, and `QC` (when applicable).
#'
#'
#' @export
seasonder_readSeaSondeCSFileData <- function(connection, header, endian = "big") {
  conditions_params <- list(calling_function = "seasonder_readSeaSondeCSFileData",class = "seasonder_cs_data_reading_error")

  # Extracting information from the header
  nRanges <- header$nRangeCells
  nDoppler <- header$nDopplerCells
  nCSKind <- header$nCsKind

  # Initialize matrices for the spectra
  out <- seasonder_initCSDataStructure(nRanges, nDoppler)

  # Helper function to read complex vectors
  read_complex_vector <- function(connection, n, endian) {
    cplx_data <- readBin(connection, what = "numeric", n = n * 2, size = 4, endian = endian)

    complex(real = cplx_data[rep(c(TRUE, FALSE), n/2)], imaginary = cplx_data[rep(c(FALSE, TRUE), n/2)])
  }

  # Read data for each range
  for (i in seq_len(nRanges)) {

    out$SSA1[i,] <- rlang::try_fetch(error = function(cond) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading SSA1 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "SSA1"))
    },
    readBin(connection, what = "numeric", n = nDoppler, size = 4, endian = endian))

    out$SSA2[i,] <- rlang::try_fetch(error = function(cond) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading SSA2 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "SSA2"))
    },
    readBin(connection, what = "numeric", n = nDoppler, size = 4, endian = endian))

    out$SSA3[i,] <- rlang::try_fetch(error = function(cond) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading SSA3 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "SSA3"))
    },
    readBin(connection, what = "numeric", n = nDoppler, size = 4, endian = endian))

    out$CS12[i,] <- rlang::try_fetch(error = function(cond) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading CS12 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "CS12"))
    },
    read_complex_vector(connection, nDoppler, endian))
    out$CS13[i,] <-  rlang::try_fetch(error = function(cond) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading CS13 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "CS13"))
    },
    read_complex_vector(connection, nDoppler, endian))

    out$CS23[i,] <-  rlang::try_fetch(error = function(cond) {
      rlang::inject(seasonder_logAndAbort(glue::glue("Error while reading CS23 data for range cell {i}: {conditionMessage(cond)}"),!!!conditions_params, parent = cond, seasonder_range_cell = i,seasonder_data_component = "CS23"))
    },
    read_complex_vector(connection, nDoppler, endian))
    if (nCSKind >= 2) {
      out$QC[i,] <- rlang::try_fetch(error = function(cond) {
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
  x$nTimeMark %<>% factor(levels = c(0L,1L,2L),labels = c("start","center time","end time"))
  x
}

seasonder_the$transform_functions[["RCVI"]] <- function(x) {
  x$nReceiverModel %<>% factor(levels = c(0L,1L,2L,3L,4L,5L),labels = c("Unknown", "Awg3/Rcvr2 Chassis AC", "Awg3/Rcvr2 Chassis DC", "AllInOne", "Awg4 Chassis AC","Awg4 Chassis DC"))
  x$nRxAntennaModel %<>% factor(levels = c(0L,1L,2L,3L,4L,5L),labels = c("Unknown", "Box Loops","2","3", "Dome Loops","TR Dome Loops"))

  x
}


seasonder_the$transform_functions[["GLRM"]] <- function(x) {
  x$nReceiverModel %<>% factor(levels = c(0L,1L,2L,3L,4L),labels = c("Off","Point","Range","Range&Point", "SubDCOnly"))
  x
}


seasonder_the$transform_functions[["SUPI"]] <- function(x) {
  x$nMethod %<>% factor(levels = c(0L,1L),labels = c("Off","Normal"))
  x$nMode %<>% factor(levels = c(0L,1L,2L,3L),labels = c("Light","Heavy","MaxLight","MaxHeavy"))
  x$nDebugMode %<>% factor(levels = c(0L,1L),labels = c("Off","On"))
  x
}



seasonder_the$transform_functions[["FWIN"]] <- function(x) {

  x$nRangeWindowType %<>% factor(levels = c(0L,1L,2L,3L),labels = c("None","Blackman", "Hamming", "Tukey"))
  x$nDopplerWindowType %<>% factor(levels = c(0L,1L,2L,3L),labels = c("None","Blackman", "Hamming", "Tukey"))
  x
}

seasonder_the$transform_functions[["IQAP"]] <- function(x) {

  x$nRangeWindowType %<>% factor(levels = c(0L,1L,2L),labels = c("Off","Measured","Corrected"))

  x
}

seasonder_the$transform_functions[["FILL"]] <- function(x) {

  x$nRangeMethod %<>% factor(levels = c(0L,1L,2L),labels = c("None", "Linear", "FFTPadding"))
  x$nDopplerMethod %<>% factor(levels = c(0L,1L,2L),labels = c("None", "Linear", "FFTPadding"))

  x
}

seasonder_the$transform_functions[["BRGR"]] <- function(x) {

  x$nBraggReject$data %<>% factor(levels = c(0L,1L,2L,3L),labels = c("OK", "RejectNegBragg", "RejectPosBragg", "RejectBoth"))


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

#' Quality Control Check for Unsigned Values
#'
#' This function performs a quality control check to ensure that a given field value
#' is an unsigned number (i.e., a non-negative number). Optionally, it can also
#' check if the field value matches a specified data type before performing the
#' unsigned check.
#'
#' @param field_value The value to be checked. The function verifies if this value
#'        is non-negative. It can be of any type but is typically expected to be a
#'        numeric value.
#' @param expected_type An optional parameter specifying the expected data type of
#'        `field_value`. If provided, the function first checks if `field_value`
#'        matches the expected type before verifying if it is unsigned. Default is NULL,
#'        which means no type check is performed.
#'
#' @return Returns the `field_value` if it passes the checks: it is of the expected
#'         type (if `expected_type` is not NULL) and is non-negative. If any of the
#'         checks fail, the function logs an error message and aborts execution.
qc_check_unsigned <- function(field_value,  expected_type = NULL) {

  if (!is.null(expected_type)) {
    field_value <- qc_check_type(field_value, expected_type)
  }

  if (field_value < 0) {
    seasonder_logAndAbort(glue::glue("QC Error: Value is negative. Expected unsigned value"))
  }
  return(field_value)
}

seasonder_load_qc_functions <- function() {

  seasonder_the$qc_functions[["qc_check_type"]] <- qc_check_type
  seasonder_the$qc_functions[["qc_check_range"]] <- qc_check_range
  seasonder_the$qc_functions[["qc_check_unsigned"]] <- qc_check_unsigned
}
seasonder_load_qc_functions()


