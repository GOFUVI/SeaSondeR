#' Get the version value from a SeaSondeR object
#'
#' @param seasonder_obj A SeaSondeR object.
#' @return The version value.
#' @export
seasonder_getVersion <- function(seasonder_obj){
  UseMethod("seasonder_getVersion")

}
