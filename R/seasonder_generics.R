#' Get the version value from a SeaSondeR object
#'
#' @param seasonder_obj A SeaSondeR object.
#' @return The version value.
#' @examples 
#' # Assuming `object` is a valid object
#' value <- seasonder_getVersion(object)
#' print(value)
#' @export
seasonder_getVersion <- function(seasonder_obj) {
  UseMethod("seasonder_getVersion")

}
