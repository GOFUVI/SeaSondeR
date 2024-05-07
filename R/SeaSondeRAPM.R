#' Create a SeaSondeRAPM Object
#'
#' This function creates a SeaSondeRAPM object to store antenna pattern calibration data.
#'
#' @param calibration_matrix A 2 x b complex matrix, where b is the number of bearings for calibration.
#' @param ... Additional named attributes that will be passed to \code{\link{seasonder_initializeAttributesSeaSondeRAPM}}.
#'
#' @return A SeaSondeRAPM object containing a complex matrix with class attribute 'SeaSondeRAPM' and
#'         additional attributes for metadata. Row names are set "A13" and "A23" and column names are set to be the values in BEAR.
#'
#' @details
#' The function performs the following operations:
#' 1. Validates the \code{calibration_matrix} with code{\link{seasonder_validateCalibrationMatrixSeaSondeRAPM}}.
#' 2. Initializes all other attributes either with default or user-provided values.
#' 3. Merges the initialized attributes into \code{calibration_matrix}.
#' 4. Sets the object's class to 'SeaSondeRAPM'.
#'
#' For more details on the attributes, see \code{\link{seasonder_initializeAttributesSeaSondeRAPM}}.
#'
#' @seealso
#' \code{\link{seasonder_validateCalibrationMatrixSeaSondeRAPM}},
#' \code{\link{seasonder_initializeAttributesSeaSondeRAPM}}
#'
#' @examples
#' cal_matrix <- matrix(complex(real = c(1, 2), imaginary = c(3, 4)), nrow = 2)
#' apm <- seasonder_createSeaSondeRAPM(cal_matrix)
#'
#' apm <- seasonder_createSeaSondeRAPM(cal_matrix,Creator="Me")
#' print(attr(apm,"Creator"))
#' # [1] "Me"
#'
#' @importFrom rlang %||%
#' @importFrom magrittr %>% %<>%
#' @export
seasonder_createSeaSondeRAPM <- function(calibration_matrix = matrix(complex(real=NA_real_, imaginary=NA_real_), nrow = 2, ncol = 0),...) {

  # Validate calibration matrix
  seasonder_validateCalibrationMatrixSeaSondeRAPM(calibration_matrix)

  rownames(calibration_matrix) <- c("A13", "A23", "A33")

  # Initialize attributes
  attributes_list <- seasonder_initializeAttributesSeaSondeRAPM(calibration_matrix,...)

  colnames(calibration_matrix) <- attributes_list$BEAR

  calibration_matrix <- attributes_list %>% purrr::reduce2(names(attributes_list),\(cal_matrix_so_far,attribute,attribute_name) {
    setter_fun <- get(glue::glue("seasonder_setSeaSondeRAPM_{attribute_name}"))
    cal_matrix_so_far %<>% setter_fun(new_value=attribute)

  },.init = calibration_matrix)


  # Assign class
  class(calibration_matrix) <- c("SeaSondeRAPM",class(calibration_matrix))

  attr(calibration_matrix,"version") <- 1

  seasonder_logAndMessage("seasonder_createSeaSondeRAPM: APM object created successfully.", "info")

  return(calibration_matrix)
}


#' Initialize Attributes for a SeaSondeRAPM Object
#'
#' This function initializes attributes for a SeaSondeRAPM object, including metadata and properties.
#'
#' @param calibration_matrix A 2 x b complex matrix, where b is the number of bearings for calibration.
#' @param ... Additional named attributes that may override the defaults.
#'
#' @return A list containing initialized attributes for a SeaSondeRAPM object.
#'
#' @details
#' The function initializes the following attributes:
#' - \code{quality_matrix}: A 2 x b complex matrix for quality data, where b is the number of bearings.
#' - \code{BEAR}: A numeric vector for bearings (degrees).
#' - \code{Type}: Character string for antenna pattern type.
#' - \code{Creator}: Object creator name. Default is an empty character vector.
#' - \code{SiteName}: Site name (not the same as SiteCode). Default is an empty character vector.
#' - \code{SiteOrigin}: Numeric vector with two elements representing the Station GPS location. Default is \code{c(0,0)}.
#' - \code{FileName}: Default is an empty character vector.
#' - \code{CreateTimeStamp}: APM file creation time. Default is current system date and time.
#' - \code{ProcessingSteps}: Processing steps applied to this object. Default is an empty character vector.
#' - \code{AmplitudeFactors}: Numeric vector with two elements for the amplitude factors. Default is \code{c(0,0)}.
#' - \code{AntennaBearing}: Default is an empty numeric vector.
#' - \code{StationCode}: 4-character station code. Default is an empty character vector.
#' - \code{BearingResolution}: In degrees. Default is an empty numeric vector.
#' - \code{Smoothing}: Numeric vector indicating smoothing applied to the antenna pattern. Default is an empty numeric vector.
#' - \code{CommentLine}: Metadata lines in the data file not matching any other attribute. Default is an empty character vector.
#' - \code{FileID}: File's UUID. Default is an empty character vector.
#' - \code{PhaseCorrections}: Numeric vector with two elements for phase corrections. Default is \code{c(0,0)}.
#'
#' Each attribute has a corresponding setter and getter function, following the naming pattern \code{seasonder_set/getSeaSondeRAPM_*}, where * is the name of the attribute. For example, to set the 'Type' attribute, you would use \code{seasonder_setSeaSondeRAPM_Type(seasonde_apm_obj,new_value)}. To get the same, you would use \code{seasonder_getSeaSondeRAPM_Type(seasonde_apm_obj,new_value)}.
#'
#' Please see \code{\link{seasonder_validateAttributesSeaSondeRAPM}} for details in attributes validation.
#'
#' @seealso
#' \code{\link{seasonder_createSeaSondeRAPM}},
#' \code{\link{seasonder_validateAttributesSeaSondeRAPM}}
#'
#' @examples
#' # Create a calibration_matrix
#' cal_matrix <- matrix(complex(real = c(1, 2), imaginary = c(3, 4)), nrow = 2, ncol = 2)
#'
#' # Initialize attributes with default settings
#' attr_list <- seasonder_initializeAttributesSeaSondeRAPM(calibration_matrix = cal_matrix)
#' str(attr_list)
#'
#' # Initialize attributes with custom 'Type'
#' attr_list_custom <- seasonder_initializeAttributesSeaSondeRAPM(calibration_matrix = cal_matrix,
#' Type = "Custom Pattern")
#' str(attr_list_custom)
#'
#' @importFrom magrittr %<>%
#' @export
seasonder_initializeAttributesSeaSondeRAPM <- function(calibration_matrix,...) {



  defaults <- list(
    quality_matrix = matrix(complex(real=NA_real_, imaginary=NA_real_), nrow=2, ncol=ncol(calibration_matrix)),
    BEAR = numeric(ncol(calibration_matrix)),
    Type = character(0),
    Creator = character(0),
    SiteName = character(0),
    SiteOrigin = numeric(2),
    FileName = character(0),
    CreateTimeStamp = Sys.time(),
    ProcessingSteps = character(0),
    AmplitudeFactors = numeric(2),
    AntennaBearing = numeric(0),
    StationCode = character(0),
    BearingResolution = numeric(0),
    Smoothing = numeric(0),
    CommentLine = character(0),
    FileID = character(0),
    PhaseCorrections = numeric(2)
  )

  out <- do.call(rlang::dots_list,c(rlang::list2(...),defaults,list(.homonyms = "first")))

  out <- out[names(defaults)]

  return(out)

}

#### Processing_steps ####

#' Generate Creation Step Text
#'
#' This function generates a text message indicating the time an APM object was created based on the current system time and the provided file path.
#'
#' @param file_path A character string specifying the path to the file.
#'
#' @return
#' A character string with the formatted message indicating the time of creation and the file path.
#'
SeaSondeRAPM_creation_step_text <- function(file_path) {
  # Use glue to format the message with the current system time and the provided file path
  glue::glue("{Sys.time()}: Created from {file_path}.")
}


#### Validation ####


#' Validate Calibration Matrix for a SeaSondeRAPM Object
#'
#' This function validates the input calibration_matrix to ensure it meets the required specifications
#' for use in a SeaSondeRAPM object.
#'
#' @param matrix A 2 x b complex matrix for calibration, where b is the number of bearings.
#'
#' @return TRUE if the matrix is valid. The function will stop execution and display an error message if the matrix is invalid.
#'
#' @details
#' The function performs the following validation checks:
#' 1. Confirms that the input is a matrix.
#' 2. Verifies that the matrix has exactly two rows.
#' 3. Checks that the matrix contains only complex numbers.
#'
#' If any of these validation steps fail, the function will log a fatal error and stop the execution using \code{rlang::abort}.
#'
#' @seealso
#' \code{\link{seasonder_createSeaSondeRAPM}}
#'
#' @examples
#' # Invalid matrix: not a matrix
#' invalid_matrix1 <- c(1, 2)
#' # This will cause an error
#' # seasonder_validateCalibrationMatrixSeaSondeRAPM(invalid_matrix1)
#'
#' # Invalid matrix: not two rows
#' invalid_matrix2 <- matrix(complex(real = c(1, 2, 3), imaginary = c(4, 5, 6)), nrow = 3)
#' # This will cause an error
#' # seasonder_validateCalibrationMatrixSeaSondeRAPM(invalid_matrix2)
#'
#' # Valid matrix
#' valid_matrix <- matrix(complex(real = c(1, 2), imaginary = c(3, 4)), nrow = 2)
#' seasonder_validateCalibrationMatrixSeaSondeRAPM(valid_matrix)  # No error
#'
#' @export
seasonder_validateCalibrationMatrixSeaSondeRAPM <- function(matrix) {
  # Implement validation logic here

  if (!is.matrix(matrix)) {
    seasonder_logAndMessage("seasonder_validateCalibrationMatrixSeaSondeRAPM: Input calibration_matrix must be a matrix.", "fatal")
    rlang::abort("seasonder_validateCalibrationMatrixSeaSondeRAPM: Input calibration_matrix must be a matrix.")
  }

  if (nrow(matrix) != 3) {
    seasonder_logAndMessage("seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must have three rows.", "fatal")
    rlang::abort("seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must have three rows.")
  }

  if (!is.complex(matrix)) {
    seasonder_logAndMessage("seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must contain complex numbers.", "fatal")
    rlang::abort("seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must contain complex numbers.")
  }

  TRUE
}

#' Validate Attributes for a SeaSondeRAPM Object
#'
#' This function validates the attributes of a given SeaSondeRAPM object to ensure they meet the required specifications.
#'
#' @param seasonde_apm_obj A SeaSondeRAPM object whose attributes are to be validated.
#'
#' @return TRUE if all attributes are valid. The function will stop execution and display an error message if any of the attributes are invalid.
#'
#' @details
#' The function performs validation on the following attributes of the SeaSondeRAPM object:
#' - quality_matrix
#' - BEAR
#' - Type
#' - Creator
#' - SiteName
#' - SiteOrigin
#' - FileName
#' - CreateTimeStamp
#' - ProcessingSteps
#' - AmplitudeFactors
#' - AntennaBearing
#' - StationCode
#' - BearingResolution
#' - Smoothing
#' - CommentLine
#' - FileID
#' - PhaseCorrections
#'
#' It internally calls specific validation functions for each of these attributes. If any of the attributes are found to be invalid, the function will stop execution and display an error message.
#'
#' For more details on the attributes, see \code{\link{seasonder_initializeAttributesSeaSondeRAPM}}.
#'
#' @seealso
#' \code{\link{validate_SeaSondeRAPM_quality_matrix}},
#' \code{\link{validate_SeaSondeRAPM_BEAR}},
#' \code{\link{validate_SeaSondeRAPM_Type}},
#' \code{\link{validate_SeaSondeRAPM_Creator}},
#' \code{\link{validate_SeaSondeRAPM_SiteName}},
#' \code{\link{validate_SeaSondeRAPM_SiteOrigin}},
#' \code{\link{validate_SeaSondeRAPM_FileName}},
#' \code{\link{validate_SeaSondeRAPM_CreateTimeStamp}},
#' \code{\link{validate_SeaSondeRAPM_ProcessingSteps}},
#' \code{\link{validate_SeaSondeRAPM_AmplitudeFactors}},
#' \code{\link{validate_SeaSondeRAPM_AntennaBearing}},
#' \code{\link{validate_SeaSondeRAPM_StationCode}},
#' \code{\link{validate_SeaSondeRAPM_BearingResolution}},
#' \code{\link{validate_SeaSondeRAPM_Smoothing}},
#' \code{\link{validate_SeaSondeRAPM_CommentLine}},
#' \code{\link{validate_SeaSondeRAPM_FileID}},
#' \code{\link{validate_SeaSondeRAPM_PhaseCorrections}}
#'
#' @examples
#' # Create a mock SeaSondeRAPM object with valid attributes
#' mock_matrix <- matrix(complex(real = c(1, 2), imaginary = c(3, 4)), nrow = 2)
#' mock_apm <- seasonder_createSeaSondeRAPM(mock_matrix)
#'
#' # Validate attributes
#' valid <- seasonder_validateAttributesSeaSondeRAPM(mock_apm)
#' print(valid)  # Should print TRUE if all attributes are valid
#'
#'@export
seasonder_validateAttributesSeaSondeRAPM <- function(seasonde_apm_obj) {
  validate_SeaSondeRAPM_quality_matrix(attributes(seasonde_apm_obj)$quality_matrix,seasonde_apm_obj)
  validate_SeaSondeRAPM_BEAR(attributes(seasonde_apm_obj)$BEAR,seasonde_apm_obj)
  validate_SeaSondeRAPM_Type(attributes(seasonde_apm_obj)$Type)
  validate_SeaSondeRAPM_Creator(attributes(seasonde_apm_obj)$Creator)
  validate_SeaSondeRAPM_SiteName(attributes(seasonde_apm_obj)$SiteName)
  validate_SeaSondeRAPM_SiteOrigin(attributes(seasonde_apm_obj)$SiteOrigin)
  validate_SeaSondeRAPM_FileName(attributes(seasonde_apm_obj)$FileName)
  validate_SeaSondeRAPM_CreateTimeStamp(attributes(seasonde_apm_obj)$CreateTimeStamp)
  validate_SeaSondeRAPM_ProcessingSteps(attributes(seasonde_apm_obj)$ProcessingSteps)
  validate_SeaSondeRAPM_AmplitudeFactors(attributes(seasonde_apm_obj)$AmplitudeFactors)
  validate_SeaSondeRAPM_AntennaBearing(attributes(seasonde_apm_obj)$AntennaBearing)
  validate_SeaSondeRAPM_StationCode(attributes(seasonde_apm_obj)$StationCode)
  validate_SeaSondeRAPM_BearingResolution(attributes(seasonde_apm_obj)$BearingResolution)
  validate_SeaSondeRAPM_Smoothing(attributes(seasonde_apm_obj)$Smoothing)
  validate_SeaSondeRAPM_CommentLine(attributes(seasonde_apm_obj)$CommentLine)
  validate_SeaSondeRAPM_FileID(attributes(seasonde_apm_obj)$FileID)
  validate_SeaSondeRAPM_PhaseCorrections(attributes(seasonde_apm_obj)$PhaseCorrections)

  return(TRUE)
}

#' Validate quality_matrix Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided quality_matrix is a 2-row complex matrix.
#' It also checks if the number of columns matches that of the calibration_matrix in the given SeaSondeRAPM object.
#'
#' @param matrix The matrix to be validated.
#' @param seasonde_apm_obj The SeaSondeRAPM object for compatibility check.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_quality_matrix <- function(matrix,seasonde_apm_obj) {
  if (!is.matrix(matrix) || nrow(matrix) != 3 || !is.complex(matrix)) {
    msg <- "validate_SeaSondeRAPM_quality_matrix: The quality_matrix must be a 2-row complex matrix."
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }

  if (ncol(matrix) != ncol(seasonde_apm_obj)) {
    msg <- glue::glue("validate_SeaSondeRAPM_quality_matrix: The quality_matrix must be a {ncol(seasonde_apm_obj)}-row complex matrix, same as the calibration_matrix. Currently has {ncol(matrix)} columns.")
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }

  return(TRUE)
}

#' Validate BEAR Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided BEAR is a numeric vector and if its length
#' matches the number of columns in the calibration_matrix of the given SeaSondeRAPM object.
#' It also validates that the bearings are between -180 and 180 degrees.
#'
#' @param vector The numeric vector to be validated.
#' @param seasonde_apm_obj The SeaSondeRAPM object for compatibility check.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_BEAR <- function(vector,seasonde_apm_obj) {
  if (!is.numeric(vector)) {
    msg <- "validate_SeaSondeRAPM_BEAR: BEAR must be a numeric vector."
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }

  if (length(vector) != ncol(seasonde_apm_obj)) {
    msg <- glue::glue("validate_SeaSondeRAPM_BEAR: BEAR must be a numeric vector of length {ncol(seasonde_apm_obj)}, matching the number of columns of the calibration matrix. Currently is of length {length(vector)}.")
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }

  if (any(!dplyr::between(vector,-180,180))) {
    msg <- glue::glue("validate_SeaSondeRAPM_BEAR: BEAR must be a numeric vector of values between -180 and 180.")
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }

  return(TRUE)
}

#' Validate Type Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided Type is a character string.
#'
#' @param type The character string to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_Type <- function(type) {
  if (!is.character(type)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_Type: Type must be a character string.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_Type: Type must be a character string.")
  }
  return(TRUE)
}

#' Validate Creator Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided Creator is a character string.
#'
#' @param creator The character string to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_Creator <- function(creator) {
  if (!is.character(creator)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_Creator: Creator must be a character string.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_Creator: Creator must be a character string.")
  }
  return(TRUE)
}

#' Validate SiteName Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided SiteName is a character string.
#'
#' @param site_name The character string to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_SiteName <- function(site_name) {
  if (!is.character(site_name)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_SiteName: SiteName must be a character string.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_SiteName: SiteName must be a character string.")
  }
  return(TRUE)
}

#' Validate SiteOrigin Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided SiteOrigin is a numeric vector of length 2.
#'
#' @param site_origin The numeric vector to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_SiteOrigin <- function(site_origin) {

  if (!is.numeric(site_origin) | length(site_origin) !=2) {
    msg <- glue::glue("validate_SeaSondeRAPM_SiteOrigin: SiteOrigin must be a numeric vector of length 2. Current length is {length(vector)}")
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }

  return(TRUE)
}


#' Validate FileName Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided FileName is a character string.
#'
#' @param file_name The character string to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_FileName <- function(file_name) {
  if (!is.character(file_name)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_FileName: FileName must be a character string.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_FileName: FileName must be a character string.")
  }
  return(TRUE)
}

#' Validate CreateTimeStamp Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided CreateTimeStamp is a POSIXct Date object.
#'
#' @param timestamp The Date object to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_CreateTimeStamp <- function(timestamp) {

  if (!inherits(timestamp, "POSIXct")) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_CreateTimeStamp: CreateTimeStamp must be a Date object.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_CreateTimeStamp: CreateTimeStamp must be a Date object.")
  }
  return(TRUE)
}

#' Validate ProcessingSteps Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided ProcessingSteps is a character vector.
#'
#' @param steps The character vector to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_ProcessingSteps <- function(steps) {
  if (!is.character(steps)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_ProcessingSteps: ProcessingSteps must be a character vector.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_ProcessingSteps: ProcessingSteps must be a character vector.")
  }
  return(TRUE)
}

#' Validate AmplitudeFactors Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided AmplitudeFactors is a numeric vector of length 2.
#'
#' @param factors The numeric vector to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_AmplitudeFactors <- function(factors) {
  if (!is.numeric(factors) | length(factors) !=2) {
    msg <- glue::glue("validate_SeaSondeRAPM_AmplitudeFactors: AmplitudeFactors must be a numeric vector of length 2. Current length is {length(vector)}")
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }
  return(TRUE)
}

#' Validate AntennaBearing Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided AntennaBearing is a numeric value.
#'
#' @param bearing The numeric value to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_AntennaBearing <- function(bearing) {
  if (!is.numeric(bearing)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_AntennaBearing: AntennaBearing must be a numeric value.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_AntennaBearing: AntennaBearing must be a numeric value.")
  }
  return(TRUE)
}

#' Validate StationCode Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided StationCode is an empty character string or a 4-character string of length 1.
#'
#' @param code The character string to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_StationCode <- function(code) {
  if (!is.character(code)) {
    msg <- glue::glue("validate_SeaSondeRAPM_StationCode: StationCode must be a character string. Provided value is {code}")
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }
  if (length(code)>0) {
    if (length(code)==1) {
      if (nchar(code) !=4) {
        msg <- glue::glue("validate_SeaSondeRAPM_StationCode: StationCode must have 4 characters. Provided value is {code}.")
        seasonder_logAndMessage(msg, "fatal")
        rlang::abort(msg)
      }
    }else{
      msg <- glue::glue("validate_SeaSondeRAPM_StationCode: StationCode must have length 0 or 1. Provided value is {length(code)}.")
      seasonder_logAndMessage(msg, "fatal")
      rlang::abort(msg)
    }
  }
  return(TRUE)
}

#' Validate BearingResolution Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided BearingResolution is a numeric value.
#'
#' @param resolution The numeric value to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_BearingResolution <- function(resolution) {
  if (!is.numeric(resolution)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_BearingResolution: BearingResolution must be a numeric value.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_BearingResolution: BearingResolution must be a numeric value.")
  }
  return(TRUE)
}


#' Validate Smoothing Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided Smoothing is a numeric value.
#'
#' @param smoothing The numeric value to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_Smoothing <- function(smoothing) {
  if (!is.numeric(smoothing)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_Smoothing: Smoothing must be a numeric value.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_Smoothing: Smoothing must be a numeric value.")
  }
  return(TRUE)
}

#' Validate CommentLine Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided CommentLine is a character string.
#'
#' @param comment The character string to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_CommentLine <- function(comment) {
  if (!is.character(comment)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_CommentLine: CommentLine must be a character string.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_CommentLine: CommentLine must be a character string.")
  }
  return(TRUE)
}

#' Validate FileID Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided FileID is a unique character string.
#'
#' @param id The unique character string to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_FileID <- function(id) {
  if (!is.character(id)) {
    seasonder_logAndMessage("validate_SeaSondeRAPM_FileID: FileID must be a unique character string.", "fatal")
    rlang::abort("validate_SeaSondeRAPM_FileID: FileID must be a unique character string.")
  }
  return(TRUE)
}

#' Validate PhaseCorrections Attribute for a SeaSondeRAPM Object
#'
#' This function validates if the provided PhaseCorrections attribute is a numeric
#' vector of length 2.
#'
#' @param corrections The numeric vector to be validated.
#' @return Returns TRUE if the validation passes.
validate_SeaSondeRAPM_PhaseCorrections <- function(corrections) {
  if (!is.numeric(corrections) | length(corrections) !=2) {
    msg <- glue::glue("validate_SeaSondeRAPM_PhaseCorrections: PhaseCorrections must be a numeric vector of length 2. Current length is {length(corrections)}")
    seasonder_logAndMessage(msg, "fatal")
    rlang::abort(msg)
  }

  return(TRUE)
}



####' Getters y Setters ####


#' Get the version value from a SeaSondeRAPM object
#'
#' @param seasonder_obj A SeaSondeRAPM object.
#' @return The version value.
#' @export
seasonder_getVersion.SeaSondeRAPM <- function(seasonder_obj) {

  attr(seasonder_obj,"version",exact = TRUE)
}

#' Getter for quality_matrix
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_quality_matrix <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$quality_matrix)
}

#' Setter for quality_matrix
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_quality_matrix <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_quality_matrix(new_value,seasonde_apm_obj)
  modified_obj <- seasonde_apm_obj

  colnames(new_value) <- colnames(seasonde_apm_obj)
  rownames(new_value) <- rownames(seasonde_apm_obj)

  attributes(modified_obj)$quality_matrix <- new_value
  return(modified_obj)
}


#' Getter for BEAR
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_BEAR <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$BEAR)
}

#' Setter for BEAR
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_BEAR <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_BEAR(new_value,seasonde_apm_obj)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$BEAR <- new_value
  return(modified_obj)
}

#' Getter for PhaseCorrections
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_PhaseCorrections <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$PhaseCorrections)
}

#' Setter for PhaseCorrections
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_PhaseCorrections <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_PhaseCorrections(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$PhaseCorrections <- new_value
  return(modified_obj)
}

#' Getter for Type
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_Type <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$Type)
}


#' Setter for Type
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_Type <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_Type(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$Type <- new_value
  return(modified_obj)
}


#' Getter for Creator
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_Creator <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$Creator)
}

#' Setter for Creator
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_Creator <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_Creator(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$Creator <- new_value
  return(modified_obj)
}



#' Getter for SiteName
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_SiteName <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$SiteName)
}

#' Setter for SiteName
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_SiteName <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_SiteName(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$SiteName <- new_value
  return(modified_obj)
}



#' Getter for SiteOrigin
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_SiteOrigin <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$SiteOrigin)
}


#' Setter for SiteOrigin
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_SiteOrigin <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_SiteOrigin(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$SiteOrigin <- new_value
  return(modified_obj)
}


#' Getter for FileName
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_FileName <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$FileName)
}


#' Setter for FileName
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_FileName <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_FileName(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$FileName <- new_value
  return(modified_obj)
}


#' Getter for CreateTimeStamp
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_CreateTimeStamp <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$CreateTimeStamp)
}


#' Setter for CreateTimeStamp
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_CreateTimeStamp <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_CreateTimeStamp(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$CreateTimeStamp <- new_value
  return(modified_obj)
}


#' Getter for ProcessingSteps
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_ProcessingSteps <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$ProcessingSteps)
}


#' Setter for ProcessingSteps
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#' @param append append the new step or replace previous steps? Default: TRUE
#'
#' @export
seasonder_setSeaSondeRAPM_ProcessingSteps <- function(seasonde_apm_obj, new_value,append=TRUE) {



  if (append) {
    steps <-  seasonder_getSeaSondeRAPM_ProcessingSteps(seasonde_apm_obj)
    new_value <- c(steps,new_value)
  }
  validate_SeaSondeRAPM_ProcessingSteps(new_value)
  modified_obj <- seasonde_apm_obj

  attributes(modified_obj)$ProcessingSteps <- new_value


  return(modified_obj)
}


#' Getter for AmplitudeFactors
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_AmplitudeFactors <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$AmplitudeFactors)
}


#' Setter for AmplitudeFactors
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_AmplitudeFactors <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_AmplitudeFactors(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$AmplitudeFactors <- new_value
  return(modified_obj)
}


#' Getter for AntennaBearing
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_AntennaBearing <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$AntennaBearing)
}


#' Setter for AntennaBearing
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_AntennaBearing <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_AntennaBearing(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$AntennaBearing <- new_value
  return(modified_obj)
}


#' Getter for StationCode
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_StationCode <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$StationCode)
}


#' Setter for StationCode
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_StationCode <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_StationCode(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$StationCode <- new_value
  return(modified_obj)
}


#' Getter for BearingResolution
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_BearingResolution <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$BearingResolution)
}

#' Setter for BearingResolution
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_BearingResolution <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_BearingResolution(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$BearingResolution <- new_value
  return(modified_obj)
}



#' Getter for Smoothing
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_Smoothing <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$Smoothing)
}

#' Setter for Smoothing
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_Smoothing <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_Smoothing(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$Smoothing <- new_value
  return(modified_obj)
}


#' Getter for CommentLine
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_CommentLine <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$CommentLine)
}

#' Setter for CommentLine
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_CommentLine <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_CommentLine(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$CommentLine <- new_value
  return(modified_obj)
}

#' Setter for FileID
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#' @param new_value new value
#'
#' @export
seasonder_setSeaSondeRAPM_FileID <- function(seasonde_apm_obj, new_value) {
  validate_SeaSondeRAPM_FileID(new_value)
  modified_obj <- seasonde_apm_obj
  attributes(modified_obj)$FileID <- new_value
  return(modified_obj)
}



#' Getter for FileID
#'
#' @param seasonde_apm_obj SeaSonderAPM object
#'
#' @export
seasonder_getSeaSondeRAPM_FileID <- function(seasonde_apm_obj) {
  return(attributes(seasonde_apm_obj)$FileID)
}


#### File Reading ####

#' Read a Row from a Matrix Represented as Text Lines
#'
#' This function reads a row of numbers from a matrix, which is represented
#' as an array of text lines. It's used to facilitate reading data from
#' SeaSonde APM files.
#'
#' @param lines The array of lines, each representing part of the row.
#' @param start The start index of the lines to read from.
#' @param number_of_lines_to_read The number of lines to read to form the row.
#' @return Returns a numeric vector containing the row values.
read_matrix_row <- function(lines, start,number_of_lines_to_read) {
  row_str <- paste(lines[start:(start + number_of_lines_to_read -1)], collapse = " ")
  row_str <- as.numeric(unlist(stringr::str_split(stringr::str_squish(row_str), " ")))
}

#' Parse a Metadata Line from a SeaSonde APM File
#'
#' This function takes a single line from a SeaSonde APM file and parses it into
#' a named attribute and its corresponding value.
#'
#' @param line The line of text to parse.
#' @return Returns a list containing the attribute name and its value.
parse_metadata_line <- function(line) {
  components <- unlist(strsplit(line, "!"))
  value_str <- stringr::str_squish(components[1])
  attribute_str <- stringr::str_squish(components[2])

  attribute_name <- switch(attribute_str,
                           "Amplitude Factors" = "AmplitudeFactors",
                           "Antenna Bearing" = "AntennaBearing",
                           "Site Code" = "StationCode",
                           "Site Lat Lon" = "SiteOrigin",
                           "Degree Resolution" = "BearingResolution",
                           "Degree Smoothing" = "Smoothing",
                           "Date Year Mo Day Hr Mn Sec" = "CreateTimeStamp",
                           "UUID" = "FileID",
                           "Phase Corrections" = "PhaseCorrections",
                           "Unknown")

  value <- switch(attribute_name,
                  "AmplitudeFactors" = as.numeric(unlist(strsplit(value_str, " "))),
                  "AntennaBearing" = as.numeric(value_str),
                  "StationCode" = value_str,
                  "SiteOrigin" = as.numeric(unlist(strsplit(value_str, " "))),
                  "BearingResolution" = as.numeric(value_str),
                  "Smoothing" = as.numeric(value_str),
                  "CreateTimeStamp" = as.POSIXct(value_str, format="%Y %m %d  %H %M %S"),
                  "FileID" = value_str,
                  "PhaseCorrections" = as.numeric(unlist(strsplit(value_str, " "))),
                  value_str)

  return(list(attribute_name = attribute_name, value = value))
}

#' Read and Parse a SeaSonde APM File
#'
#' This function reads a SeaSonde APM file and returns a SeaSondeRAPM object containing
#' the parsed data.
#'
#' @param file_path The path to the SeaSonde APM file to read.
#' @param override_antenna_bearing If not NULL, overrides the Antenna Bearing data in the file
#' @param ... Additional arguments passed to the object creation function (See \code{\link{seasonder_createSeaSondeRAPM}} for more details).
#' @return Returns a SeaSondeRAPM object containing the parsed data.
#' @export
#' @importFrom magrittr %<>%
#' @seealso \code{\link{seasonder_createSeaSondeRAPM}}
#' @seealso \code{\link{seasonder_validateAttributesSeaSondeRAPM}}
seasonder_readSeaSondeRAPMFile <- function(file_path, override_antenna_bearing = NULL, ...) {

  lines <- readLines(file_path)
  num_angles <- as.integer(lines[1])


  number_of_lines_to_read <- ceiling(num_angles / 7)
  BEAR_start <- 2
  A13R_start  <- BEAR_start + number_of_lines_to_read
  A13RQ_start  <- A13R_start + number_of_lines_to_read
  A13I_start  <- A13RQ_start + number_of_lines_to_read
  A13IQ_start  <- A13I_start + number_of_lines_to_read
  A23R_start  <- A13IQ_start + number_of_lines_to_read
  A23RQ_start  <- A23R_start + number_of_lines_to_read
  A23I_start  <- A23RQ_start + number_of_lines_to_read
  A23IQ_start  <- A23I_start + number_of_lines_to_read

  BEAR <- read_matrix_row(lines,BEAR_start,number_of_lines_to_read)
  A13R <- read_matrix_row(lines,A13R_start,number_of_lines_to_read)
  A13RQ <- read_matrix_row(lines,A13RQ_start,number_of_lines_to_read)
  A13I <- read_matrix_row(lines,A13I_start,number_of_lines_to_read)
  A13IQ <- read_matrix_row(lines,A13IQ_start,number_of_lines_to_read)
  A23R <- read_matrix_row(lines,A23R_start,number_of_lines_to_read)
  A23RQ <- read_matrix_row(lines,A23RQ_start,number_of_lines_to_read)
  A23I <- read_matrix_row(lines,A23I_start,number_of_lines_to_read)
  A23IQ <- read_matrix_row(lines,A23IQ_start,number_of_lines_to_read)


  # Concatenar las líneas y luego dividirlas en ángulos
  angle_str <- paste(lines[2:(2 + number_of_lines_to_read -1)], collapse = " ")
  angles <- as.numeric(unlist(stringr::str_split(stringr::str_squish(angle_str), " ")))


  calibration_matrix <- matrix(0, nrow = 2, ncol = num_angles)

  A13 <- complex(real=A13R,imaginary = A13I)
  A23 <- complex(real=A23R,imaginary = A23I)
  A13Q <- complex(real=A13RQ,imaginary = A13IQ)
  A23Q <- complex(real=A23RQ,imaginary = A23IQ)
  A33 <- complex(real=rep(1,length(A13R)), imaginary=rep(0,length(A13I)))
  A33Q <- complex(real=rep(0,length(A13R)), imaginary=rep(0,length(A13I)))

  calibration_matrix <- matrix(c(A13,A23,A33),nrow=3,byrow = TRUE)
  quality_matrix <- matrix(c(A13Q,A23Q,A33Q),nrow=3,byrow = TRUE)

  out <- seasonder_createSeaSondeRAPM(calibration_matrix = calibration_matrix,quality_matrix=quality_matrix,BEAR=BEAR,...)

  out <- seasonder_setSeaSondeRAPM_quality_matrix(out,new_value = quality_matrix)

  metadata_start <- A23IQ_start + number_of_lines_to_read
  metadata_lines <- lines[(metadata_start):length(lines)]
  comment_lines <- character(0)
  metadata_list <- lapply(metadata_lines, parse_metadata_line)

  for (meta in metadata_list) {
    attribute_name <- meta$attribute_name
    value <- meta$value
    if(attribute_name == "AntennaBearing"){
      value <- override_antenna_bearing %||% value
    }

    if (attribute_name == "Unknown") {
      comment_lines <- c(comment_lines, value)
    } else {
      setter_fun <- get(glue::glue("seasonder_setSeaSondeRAPM_{attribute_name}"))
      out <- setter_fun(out, new_value=value)
    }
  }

  # Si hay comentarios, añadirlos al objeto
  if (length(comment_lines) > 0) {
    comment_str <- paste(comment_lines, collapse = "; ")
    setter_fun <- get("seasonder_setSeaSondeRAPM_CommentLine")
    out <- setter_fun(out, new_value = comment_str)
  }


  out <- seasonder_setSeaSondeRAPM_FileName(out,basename(file_path))


  out %<>% seasonder_setSeaSondeRAPM_ProcessingSteps(SeaSondeRAPM_creation_step_text(file_path))

  # Validar los atributos después de la lectura
  seasonder_validateAttributesSeaSondeRAPM(out)

  return(out)
}
