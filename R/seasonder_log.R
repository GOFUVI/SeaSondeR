# Initialize a variable 'logs_enabled' within the 'seasonder_the' environment
# and set its value to TRUE. This variable will be used to control whether
# logging is performed when using various functions in the SeaSondeR package.
seasonder_the$logs_enabled <- TRUE

#' Enable log recording in SeaSondeR
#'
#' This function enables log recording in the SeaSondeR package.
#' Once enabled, various SeaSondeR functions will output logs.
#'
#' @return NULL
#' @export
#' @examples
#' seasonder_enableLogs()
seasonder_enableLogs <- function() seasonder_the$logs_enabled <- TRUE

#' Disable log recording in SeaSondeR
#'
#' This function disables log recording in the SeaSondeR package.
#' Once disabled, various SeaSondeR functions will no longer output logs.
#'
#' @return NULL
#' @export
#' @examples
#' seasonder_disableLogs()
seasonder_disableLogs <- function() seasonder_the$logs_enabled <- FALSE

#' Check if log recording is enabled in SeaSondeR
#'
#' This function checks whether log recording is currently enabled
#' in the SeaSondeR package.
#'
#' @return Logical indicating whether logs are enabled or disabled.
#' @export
#' @examples
#' seasonder_areLogsEnabled()
seasonder_areLogsEnabled <- function() seasonder_the$logs_enabled




#' seasonder_log function
#'
#' This function creates a logging message and signals a seasonder_log condition.
#'
#' @param message A character string indicating the message to be logged.
#' @param level A character string that defines the level of the log. It can be "info", "error", or "fatal". Default is "info".
#' @export
#'
#' @examples
#' seasonder_log("This is an info message")
#' seasonder_log("This is an error message", "error")
#' seasonder_log("This is a fatal message", "fatal")
seasonder_log <- function(message, level=c("info","error","fatal")){
  # match level to one of the allowed inputs
  level <- match.arg(level)

  # signal a condition with the message
  rlang::signal(message, "seasonder_log", level=level)
}

#' seasonder_log_archiver function
#'
#' This function writes the logged message to a specific path based on the level of the log.
#'
#' @param object A list object that contains a 'level' and 'message' elements indicating the level and the message of the log respectively.
#' @param log_path A character string indicating the default path where the logs will be stored. Default is a temporary file.
#' @param log_info_path A character string indicating the path where the 'info' level logs will be stored. Default is 'log_path'.
#' @param log_error_path A character string indicating the path where the 'error' level logs will be stored. Default is 'log_info_path'.
#' @param log_fatal_path A character string indicating the path where the 'fatal' level logs will be stored. Default is 'log_error_path'.
#' @return the log object
#' @export
#'
#' @examples
#' log <- list(level = "info", message = "This is an info message")
#' seasonder_log_archiver(log)
#' log <- list(level = "error", message = "This is an error message")
#' seasonder_log_archiver(log)
#' log <- list(level = "fatal", message = "This is a fatal message")
#' seasonder_log_archiver(log)
seasonder_log_archiver <- function(object, log_path=tempfile(), log_info_path=log_path, log_error_path=log_info_path, log_fatal_path=log_error_path){

  # switch function to decide which function to call based on the log level
  log_fun <- switch(object$level,
                    "info" = function(x) write(glue::glue("[INFO] {Sys.time()}: {object$message}"), log_info_path, append = TRUE),
                    "error" = function(x) write(glue::glue("[ERROR] {Sys.time()}: {object$message}"), log_error_path, append = TRUE),
                    "fatal" = function(x) write(glue::glue("[FATAL] {Sys.time()}: {object$message}"), log_fatal_path, append = TRUE)
  )

  # execute the selected log function
  log_fun(object$level)

  # return the log path
  return(object)
}

#' seasonder_archive_expression_log function
#'
#' This function wraps the provided expression `expr` in a tryCatch block and archives the log information
#' whenever a 'seasonder_log' condition is signaled within the evaluated expression. The log message is archived
#' by calling `seasonder_log_archiver` with the signaled condition `cond` and any additional parameters.
#'
#' @param expr An expression to be evaluated. This expression should signal a 'seasonder_log' condition using `seasonder_log` function when needed.
#' @param ... Additional parameters passed to `seasonder_log_archiver` function.
#' @return The result of evaluating the input expression `expr`.
#' @export
seasonder_archive_expression_log <- function(expr, ...){
  # tryCatch block captures any errors or warnings generated within the expr
  # withCallingHandlers ensures that any 'seasonder_log' conditions signaled within expr are captured and passed to seasonder_log_archiver
  tryCatch(
    withCallingHandlers(expr, seasonder_log = function(cond) seasonder_log_archiver(cond, ...))
  )
}




#' Log and Inform Message in SeaSondeR
#'
#' This function logs a message to the SeaSondeR logging system and also informs the message to the console.
#' It prefixes the message with the name of the calling function.
#'
#' @param msg A character string indicating the message to be logged and informed.
#' @param log_level A character string indicating the level of the log ("info", "error", "fatal"). Default is "info".
#' @return An invisible NULL. The function modifies the shared environment `seasonder_the` in place if logs are enabled, and informs the message if messages are enabled.
#' @export
#' @examples
#' \dontrun{
#' my_function <- function() {
#'   seasonder_logAndMessage("This is a message", "info")
#' }
#' my_function()
#' }
#'
seasonder_logAndMessage <- function(msg,log_level="info") {


  # Get the name of the calling function

  calling_function <- sys.call(-1)[[1]]

  full_msg <- msg


  full_msg <- try({

    calling_function <- as.character(calling_function)
    full_msg <- stringr::str_remove(msg,paste0(calling_function[1],":"))

    # Construct the full message with the prefix
    paste0(calling_function[1], ": ", full_msg)
  },silent = TRUE)

  if(inherits(full_msg,"try-error")){
    full_msg <- msg
  }

  if (seasonder_areMessagesEnabled() & log_level=="info") {
    rlang::inform(full_msg)
  }

  if (log_level=="error") {
    rlang::warn(full_msg)
  }

  if (seasonder_areLogsEnabled()) {
    seasonder_log(full_msg, log_level)
  }
}


#' Log and Abort Message in SeaSondeR
#'
#' This function logs a message to the SeaSondeR logging system and also aborts the execution.
#' It prefixes the abort message with the name of the calling function.
#'
#' @param msg A character string indicating the message
#' @return An invisible NULL. The function modifies the shared environment `seasonder_the` in place if logs are enabled.
#' @export
#' @examples
#' \dontrun{
#' my_function <- function() {
#'   seasonder_logAndAbort("This is a message")
#' }
#' my_function()
#' }
#'
seasonder_logAndAbort <- function(msg) {

  log_level <- "fatal"

  # Get the name of the calling function

  calling_function <- sys.call(-1)[[1]]

  full_msg <- msg


  full_msg <- try({
    calling_function <- as.character(calling_function)
    full_msg <- stringr::str_remove(msg,paste0(calling_function[1],":"))

    # Construct the full message with the prefix
    paste0(calling_function[1], ": ", full_msg)
  },silent = TRUE)

  if(inherits(full_msg,"try-error")){
    full_msg <- msg
  }

  rlang::abort(full_msg)


  if (seasonder_areLogsEnabled()) {
    seasonder_log(full_msg, log_level)
  }
}
