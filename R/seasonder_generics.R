#' Get the version value from a SeaSondeR object
#'
#' @param seasonder_obj A SeaSondeR object.
#' @return The version value.
 #' @examples
 #' # Get version from a SeaSondeRCS object
 #' cs_file <- system.file("css_data/CSS_TORA_24_04_04_0700.cs", package = "SeaSondeR")
 #' apm_file <- system.file("css_data/MeasPattern.txt", package = "SeaSondeR")
 #' apm_obj <- seasonder_readSeaSondeRAPMFile(apm_file)
 #' cs_obj <- seasonder_createSeaSondeRCS(cs_file, seasonder_apm_object = apm_obj)
 #' value <- seasonder_getVersion(cs_obj)
 #' print(value)
 #' @export
seasonder_getVersion <- function(seasonder_obj) {
  UseMethod("seasonder_getVersion")

}
