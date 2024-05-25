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
#' @return NULL
#' @export
#' @examples
#' seasonder_enableMessages()
seasonder_enableMessages <- function() seasonder_the$messages_enabled <- TRUE

#' Disable message logging in SeaSondeR
#'
#' This function disables message logging in the SeaSondeR package.
#' Once disabled, various SeaSondeR functions will no longer output
#' informational messages.
#'
#' @return NULL
#' @export
#' @examples
#' seasonder_disableMessages()
seasonder_disableMessages <- function() seasonder_the$messages_enabled <- FALSE

#' Check if message logging is enabled in SeaSondeR
#'
#' This function checks whether message logging is currently enabled
#' in the SeaSondeR package.
#'
#' @return Logical indicating whether messages are enabled or disabled.
#' @export
#' @examples
#' seasonder_areMessagesEnabled()
seasonder_areMessagesEnabled <- function() seasonder_the$messages_enabled


#### Debug points ####


seasonder_the$debug_points_enabled <- c("none")

#' @export
seasonder_enable_debug_points <- function(debug_points) {
  seasonder_the$debug_points_enabled <- c(seasonder_the$debug_points_enabled, debug_points)
}

#' @export
seasonder_get_enabled_debug_points <- function() seasonder_the$debug_points_enabled

#' @export
seasonder_is_debug_point_enabled <- function(debug_point){

  debug_point %in% seasonder_get_enabled_debug_points()


}
