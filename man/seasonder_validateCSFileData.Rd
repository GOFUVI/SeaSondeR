% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS.R
\name{seasonder_validateCSFileData}
\alias{seasonder_validateCSFileData}
\title{Validate SeaSondeR CS File Data}
\usage{
seasonder_validateCSFileData(filepath, header)
}
\arguments{
\item{filepath}{A character string indicating the path to the CS file to validate.}

\item{header}{A list containing header information of the CS file.}
}
\value{
NULL invisibly. The function mainly serves to validate and will stop execution and log an error using \code{seasonder_logAndAbort} if any condition fails.
}
\description{
This function performs multiple validation checks on a provided CS file in the SeaSondeR system.
It checks the file for various conditions to determine if it meets the SeaSondeR standards.
}
\details{
The function performs the following validation checks:
\enumerate{
\item Verifies that the file size is greater than 10 bytes.
\item Validates the \code{nCsFileVersion} field in the header to ensure it's between 1 and 32.
\item Depending on the \code{nCsFileVersion}, verifies the appropriate file size, and the extent of various version headers (\code{nV1Extent}, \code{nV2Extent}, etc.).
\item Validates the \code{nRangeCells} and \code{nDopplerCells} fields to ensure they are within permissible ranges.
\item Depending on the \code{nCsKind} value, validates the file size against expected sizes based on \code{nRangeCells}, \code{nSpectraChannels}, and \code{nDopplerCells}.
}
}
\section{Condition Management}{

This function utilizes the \code{rlang} package to manage conditions and provide detailed and structured condition messages:

\strong{Condition Classes}:
\itemize{
\item \code{seasonder_validate_cs_file_error}: An error class that indicates a validation requirement was not met.
}

\strong{Condition Cases}:
\itemize{
\item Failure on any validation test.
}
}

\references{
Cross Spectra File Format Version 6. CODAR. 2016
}
