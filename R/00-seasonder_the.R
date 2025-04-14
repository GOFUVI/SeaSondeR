# Create a new environment object called 'seasonder_the' with no parent environment.
# This environment will be used to store variables that are shared across different
# functions within the SeaSondeR package.
seasonder_the <- new.env(parent=emptyenv())

#### Messaging ####

# Initialize a variable 'messages_enabled' within the 'seasonder_the' environment
# and set its value to TRUE. This variable will be used to control whether
# informational messages are displayed when using various functions in the SeaSondeR package.
seasonder_the$messages_enabled <- TRUE


#' Enable message logging in SeaSondeR
#'
#' This function enables message logging in the SeaSondeR package.
#' Once enabled, various SeaSondeR functions will output informational
#' messages.
#'
#' @return logical TRUE indicating that message logging was enabled.
#' @export
#' @examples
#' \dontrun{
#'   seasonder_enableMessages()
#' }
seasonder_enableMessages <- function() seasonder_the$messages_enabled <- TRUE

#' Disable message logging in SeaSondeR
#'
#' This function disables message logging in the SeaSondeR package.
#' Once disabled, various SeaSondeR functions will no longer output
#' informational messages.
#'
#' @return logical FALSE indicating that message logging was disabled.
#' @export
#' @examples
#' \dontrun{
#'   seasonder_disableMessages()
#' }
seasonder_disableMessages <- function() seasonder_the$messages_enabled <- FALSE

#' Check if message logging is enabled in SeaSondeR
#'
#' This function checks whether message logging is currently enabled.
#'
#' @return Logical value indicating whether messages are enabled.
#' @export
#' @examples
#' \dontrun{
#'   seasonder_areMessagesEnabled()
#' }
seasonder_areMessagesEnabled <- function() seasonder_the$messages_enabled


#### Debug points ####


seasonder_the$debug_points_enabled <- c("none")

#' Enable debug points in SeaSondeR
#'
#' This function adds one or more debug points to the list of enabled debug points.
#'
#' @param debug_points A character vector of debug point names to enable.
#' @return Updated character vector of enabled debug points.
#' @export
#' @examples
#' \dontrun{
#'   seasonder_enable_debug_points("example_debug")
#' }
seasonder_enable_debug_points <- function(debug_points) {
  seasonder_the$debug_points_enabled <- c(seasonder_the$debug_points_enabled, debug_points)
}

#' Get enabled debug points in SeaSondeR
#'
#' This function returns the currently enabled debug points.
#'
#' @return A character vector of enabled debug points.
#' @export
#' @examples
#' \dontrun{
#'   seasonder_get_enabled_debug_points()
#' }
seasonder_get_enabled_debug_points <- function() seasonder_the$debug_points_enabled

#' Check if a debug point is enabled in SeaSondeR
#'
#' This function checks whether the provided debug point is enabled.
#'
#' @param debug_point A character string specifying the debug point.
#' @return TRUE if the debug point is enabled, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#'   seasonder_is_debug_point_enabled("example_debug")
#' }
seasonder_is_debug_point_enabled <- function(debug_point){
  debug_point %in% seasonder_get_enabled_debug_points()
}

#' Disable all debug points in SeaSondeR
#'
#' This function resets the debug points to the default state ("none").
#'
#' @return A character vector containing only the default debug point "none".
#' @export
#' @examples
#' \dontrun{
#'   seasonder_disable_all_debug_points()
#' }
seasonder_disable_all_debug_points <- function(){
  seasonder_the$debug_points_enabled <- c("none")
}


#### Config ####


seasonder_the$config <- list()


