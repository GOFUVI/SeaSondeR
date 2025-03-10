% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCSSY.R
\name{seasonder_read_asign}
\alias{seasonder_read_asign}
\title{Read Self Spectra Sign Information from a Connection}
\usage{
seasonder_read_asign(connection, key)
}
\arguments{
\item{connection}{A binary connection to read raw bytes from.}

\item{key}{A list containing:
\describe{
\item{size}{An integer specifying the total number of bytes to be read. It must equal 3 times the number
of bytes per group.}
\item{key}{A string identifier (expected to be \code{"asign"}).}
}}
}
\value{
A named list of 3 vectors. Each vector represents one group (i.e., \code{cs1a}, \code{cs2a}, \code{cs3a})
and contains integers (0 or 1) corresponding to the bits (in little-endian order) extracted from the raw data.
}
\description{
This function reads a raw binary stream from a provided connection, expecting a specific format
that contains the sign bits for self spectra values. The data is divided into 3 groups corresponding
to: \code{cs1a}, \code{cs2a}, and \code{cs3a}.
}
\details{
The function performs the following steps:
\itemize{
\item Reads \code{key$size} bytes from the specified connection.
\item Verifies that the number of bytes read matches the expected size.
\item Checks that the total number of bytes is divisible by 3, allowing equal distribution among the groups.
\item Splits the raw byte vector into 3 groups based on the calculated number of bytes per group.
\item Converts each byte into its 8-bit binary representation (using \code{rawToBits}) and flattens the results for each group.
}
}
