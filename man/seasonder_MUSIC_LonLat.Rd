% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS_MUSIC.R
\name{seasonder_MUSIC_LonLat}
\alias{seasonder_MUSIC_LonLat}
\title{Map MUSIC Bearings to Geographic Coordinates}
\usage{
seasonder_MUSIC_LonLat(seasonder_cs_object)
}
\arguments{
\item{seasonder_cs_object}{A \code{SeaSondeRCS} object containing MUSIC detection data.}
}
\value{
A \code{SeaSondeRCS} object with updated MUSIC data, including geographic coordinates for each detection.
}
\description{
This function calculates geographic coordinates (latitude and longitude) for each MUSIC detection based on the range and direction of arrival (DOA) bearings from a \code{SeaSondeRCS} object.
}
\details{
This function performs the following operations:
\enumerate{
\item Retrieves MUSIC data and original geographic coordinates (latitude and longitude) from the \code{seasonder_cs_object}. If these coordinates are not available, the origin is derived from the associated Antenna Pattern (APM) data.
\item Converts DOA bearings from MUSIC detections into geographic bearings using the APM object.
\item Computes latitude and longitude for each MUSIC detection based on the range and geographic bearings using \code{\link{seasonder_computeLonLatFromOriginDistBearing}}
\item Updates the \code{seasonder_cs_object} with the newly computed coordinates.
}
}
\examples{
\dontrun{
# Assuming `seasonder_cs_obj` is a valid `SeaSondeRCS` object
updated_obj <- seasonder_MUSIC_LonLat(seasonder_cs_obj)
}
}
\seealso{
\itemize{
\item \code{\link{seasonder_getSeaSondeRCS_MUSIC}}
\item \code{\link{seasonder_getSeaSondeRCS_APM}}
\item \code{\link{seasonder_MUSICBearing2GeographicalBearing}}
\item \code{\link{seasonder_computeLonLatFromOriginDistBearing}}
}
}
