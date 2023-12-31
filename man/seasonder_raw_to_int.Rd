% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS.R
\name{seasonder_raw_to_int}
\alias{seasonder_raw_to_int}
\title{Convert a Raw Vector to a 64-bit Integer}
\usage{
seasonder_raw_to_int(r, signed = F)
}
\arguments{
\item{r}{A raw vector to be converted.}

\item{signed}{Logical, indicating whether the conversion should consider the value as signed (default is FALSE for unsigned).}
}
\value{
A 64-bit integer representation of the raw vector.
}
\description{
This function converts a raw vector to a 64-bit integer,
handling both signed and unsigned conversions.
}
\examples{
\dontrun{
r <- as.raw(c(0x12,0x34,0x56,0x78,0x90,0xAB,0xCD,0xEF))
seasonder_raw_to_int(r, signed=TRUE)
}
}
