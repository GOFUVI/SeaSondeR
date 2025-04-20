# Shared Environment for SeaSondeR Logs
#
# Update the shared environment to stores log messages
# across different functions within the SeaSondeR package.
seasonder_the$log <- character(0)

# Initialize a variable 'logs_enabled' within the 'seasonder_the' environment
# and set its value to TRUE. This variable will be used to control whether
# logging is performed when using various functions in the SeaSondeR package.
seasonder_the$logs_enabled <- TRUE

#' Enable log recording in SeaSondeR
#'
#' This function enables log recording in the SeaSondeR package.
#' Once enabled, various SeaSondeR functions will output logs.
#'
#' @return Invisibly returns TRUE, indicating that log recording has been enabled.
#' @export
#' @examples
#'   seasonder_enableLogs()
seasonder_enableLogs <- function() seasonder_the$logs_enabled <- TRUE

#' Disable log recording in SeaSondeR
#'
#' This function disables log recording in the SeaSondeR package.
#' Once disabled, various SeaSondeR functions will no longer output logs.
#'
#' @return Invisibly returns FALSE, indicating that log recording has been disabled.
#' @export
#' @examples
#'   seasonder_disableLogs()
seasonder_disableLogs <- function() seasonder_the$logs_enabled <- FALSE

#' Check if log recording is enabled in SeaSondeR
#'
#' This function checks whether log recording is currently enabled
#' in the SeaSondeR package.
#'
#' @return Logical indicating whether logs are enabled or disabled.
#' @export
#' @examples
#'   seasonder_areLogsEnabled()
seasonder_areLogsEnabled <- function() seasonder_the$logs_enabled

seasonder_appendLog <- function(log_str) {

  seasonder_the$log <- append(seasonder_the$log,log_str)

  invisible(seasonder_the$log)
}

seasonder_verifyLog <- function(message,level) {
  # match level to one of the allowed inputs
  level <- match.arg(level,c("info","error","fatal"))

  invisible(TRUE)
}

seasonder_logStr <- function(message,level) {

  out <-  switch(level,
                 "info" = glue::glue("[INFO] {Sys.time()}: {message}"),
                 "error" = glue::glue("[ERROR] {Sys.time()}: {message}"),
                 "fatal" = glue::glue("[FATAL] {Sys.time()}: {message}")
  )

  return(out)
}


#' Retrieve the Last Logs
#'
#' This function fetches the most recent log entries from the global log variable `seasonder_the$log`.
#'
#' @param n An integer specifying the number of recent log entries to retrieve.
#'
#' @return A character vector of the `n` most recent log entries from the global log.
#' @export
#' @examples
#'   head(seasonder_getLog())
seasonder_getLog <- function(n=100) {

  utils::tail(seasonder_the$log,n = n)

}

#' seasonder_log function
#'
#' This function creates a logging message and signals a seasonder_log condition.
#'
#' @param message A character string indicating the message to be logged.
#' @param level A character string that defines the level of the log. It can be "info", "error", or "fatal". Default is "info".
#'
#' @return Invisibly returns the generated log message string.
#' @export
#'
#' @examples
#'   seasonder_log("This is an info message")
#'   seasonder_log("This is an error message", "error")
#'   seasonder_log("This is a fatal message", "fatal")
seasonder_log <- function(message, level="info") {

  if (seasonder_areLogsEnabled()) {
    seasonder_verifyLog(message,level)
    log_str <- seasonder_logStr(message,level)

    seasonder_appendLog(log_str)

    # signal a condition with the message
    rlang::signal(log_str, "seasonder_log", level = level)
  }
}

#' Archive Log Entries
#'
#' Archives log entries based on their levels: INFO, ERROR, or FATAL. If paths are not provided,
#' temporary files will be used.
#'
#' @param log_path Path to the main log file.
#' @param log_info_path Path to the INFO level log file.
#' @param log_error_path Path to the ERROR level log file.
#' @param log_fatal_path Path to the FATAL level log file.
#'
#' @return When temporary files are used, returns a character string with the main log file path; otherwise, returns an invisible value indicating that logs were archived.
#' @export
#' @examples
#'   seasonder_logArchiver()
seasonder_logArchiver <- function(log_path=NULL, log_info_path=log_path, log_error_path=log_info_path, log_fatal_path=log_error_path) {

  temp_file <- FALSE

  if (all(purrr::map_lgl(c(log_path,log_info_path,log_error_path,log_fatal_path),is.null))) {
    log_path <- log_info_path <- log_error_path <- log_fatal_path <- tempfile()
    temp_file <- TRUE
  }


  seasonder_the$log %>% purrr::walk(\(log_entry) {
    level <- stringr::str_extract(log_entry,"^\\[(INFO|ERROR|FATAL)\\]",group = 1)

    # switch function to decide which function to call based on the log level
    log_fun <- switch(level,
                      "INFO" = function(x) if (!is.null(log_info_path)) write(log_entry, log_info_path, append = TRUE),
                      "ERROR" = function(x) if (!is.null(log_error_path)) write(log_entry, log_error_path, append = TRUE),
                      "FATAL" = function(x) if (!is.null(log_fatal_path)) write(log_entry, log_fatal_path, append = TRUE)
    )

    # execute the selected log function
    try(log_fun(level))

  })



  # return the log path
  if (temp_file) {
    return(log_path)
  }else{
    return(invisible())
  }
}


#' Log and Inform Message in SeaSondeR
#'
#' This function logs a message to the SeaSondeR logging system and also informs the message to the console.
#' It prefixes the message with the name of the calling function.
#'
#' @param msg A character string indicating the message to be logged and informed.
#' @param log_level A character string indicating the level of the log ("info", "error", "fatal"). Default is "info".
#' @param calling_function Function where the condition occurred. If NULL (default), the code determines the caller.
#' @param ... Additional arguments passed to `rlang::inform` (if log_level="info") or `rlang::warn` (if log_level="error").
#'
#' @return Invisibly returns no value; used solely for its side effects of logging and messaging.
#' @export
#' @examples
#' my_function <- function() {
#'   seasonder_logAndMessage("This is a message", "info")
#' }
#' my_function()
#'
seasonder_logAndMessage <- function(msg, log_level="info", calling_function=NULL, ...) {


  # Get the name of the calling function

  if (is.null(calling_function)) {
    calling_function <- sys.call(-1)[[1]]
  }

  full_msg <- msg


  full_msg <- try({

    calling_function <- as.character(calling_function)
    full_msg <- stringr::str_remove(msg,paste0(calling_function[1],":"))

    # Construct the full message with the prefix
    paste0(calling_function[1], ": ", full_msg)
  },silent = TRUE)

  if (inherits(full_msg,"try-error")) {
    full_msg <- msg
  }

  if (seasonder_areMessagesEnabled() && log_level == "info") {
    rlang::inform(full_msg,...)
  }

  if (log_level == "error") {
    rlang::warn(full_msg,...)
  }

  if (seasonder_areLogsEnabled()) {
    seasonder_log(full_msg, log_level)
  }
}


#' Log and Abort Message in SeaSondeR
#'
#' This function logs a message to the SeaSondeR logging system and aborts execution.
#' It prefixes the abort message with the name of the calling function.
#'
#' @param msg A character string indicating the message.
#' @param calling_function Function where the condition occurred. If NULL (default), the code determines the caller.
#' @param ... Additional arguments passed to `rlang::abort`.
#'
#' @return This function does not return as it always aborts execution.
#' @export
#' @examples
#' my_function <- function() {
#'   seasonder_logAndAbort("This is a message")
#' }
#' # Demonstrate abort without stopping execution
#' try(my_function(), silent = TRUE)
#'
seasonder_logAndAbort <- function(msg, calling_function=NULL, ...) {

  log_level <- "fatal"

  # Get the name of the calling function
  if (is.null(calling_function)) {
    calling_function <- sys.call(-1)[[1]]
  }
  full_msg <- msg


  full_msg <- try({
    calling_function <- as.character(calling_function)
    full_msg <- stringr::str_remove(msg,paste0(calling_function[1],":"))

    # Construct the full message with the prefix
    paste0(calling_function[1], ": ", full_msg)
  },silent = TRUE)

  if (inherits(full_msg,"try-error")) {
    full_msg <- msg
  }

  rlang::abort(full_msg,...)


  if (seasonder_areLogsEnabled()) {
    seasonder_log(full_msg, log_level)
  }
}

#' Split Logs Based on Time Thresholds
#'
#' The function splits the log entries into blocks based on time gaps between timestamps. The threshold
#' for splitting can be provided or calculated based on the gaps in the log timestamps.
#'
#' @param threshold The time difference threshold for splitting the logs. If NULL, it's calculated.
#' @param threshold_factor Multiplicative factor applied to the calculated threshold.
#' @param threshold_quantile Quantile used for threshold calculation if `threshold` is NULL.
#' @param min_threshold_secs Minimum threshold in seconds.
#'
#' @return A list of log blocks, each block being a vector of log entries.
#' @importFrom lubridate ymd_hms
#' @export
#' @examples
#' # Enable logging
#' seasonder_enableLogs()
#' # Log some messages
#' seasonder_log("First log entry", "info")
#' Sys.sleep(0.1)
#' seasonder_log("Second log entry", "info")
#' # Split logs into blocks (using a 1-second threshold)
#' blocks <- seasonder_splitLog(threshold = as.difftime(1, units = "secs"))
#' print(blocks)
seasonder_splitLog <- function(threshold=NULL, threshold_factor=4, threshold_quantile=0.9, min_threshold_secs=10) {

  time_block <- NULL

  log <- seasonder_the$log


  timestamps <- stringr::str_extract(log,"^\\[(?:INFO|ERROR|FATAL)\\] (\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}\\.\\d*):",group = 1)
  timestamps <- lubridate::ymd_hms(timestamps)


  df <- data.frame(timestamps, log)

  time_gaps <- difftime(timestamps, stats::lag(timestamps, default = dplyr::first(timestamps)))

  if (is.null(threshold)) {

    threshold <- stats::quantile(time_gaps,c(threshold_quantile)) %>%  magrittr::multiply_by(threshold_factor) %>% min(difftime(min_threshold_secs,0))
  }



  blocks <- df %>%
    dplyr::arrange(timestamps) %>%
    dplyr::mutate(
      time_gaps = time_gaps,
      time_block = cumsum(as.numeric(time_gaps) > as.numeric(threshold))
    ) %>%
    dplyr::group_by(time_block) %>%
    dplyr::group_split() %>%
    purrr::map(\(block) dplyr::pull(block,"log"))


  return(blocks)

}


#' Retrieve the Last Log Entry
#'
#' This function fetches and splits the log entries, then returns the last entry.
#'
#' @param ... Arguments passed to `seasonder_splitLog`.
#'
#' @return A character vector representing the last log entry.
#' @export
#' @examples
#' # Enable logging
#' seasonder_enableLogs()
#' # Log a test message
#' seasonder_log("Test log entry", "info")
#' # Retrieve the last log entry
#' seasonder_lastLog()
seasonder_lastLog <- function(...) {
  seasonder_splitLog(...) %>% dplyr::last()
}
