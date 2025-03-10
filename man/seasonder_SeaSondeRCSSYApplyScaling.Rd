% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCSSY.R
\name{seasonder_SeaSondeRCSSYApplyScaling}
\alias{seasonder_SeaSondeRCSSYApplyScaling}
\title{Apply Scaling to SeaSondeRCSSY Data}
\usage{
seasonder_SeaSondeRCSSYApplyScaling(values, fmax, fmin, fscale, dbRef)
}
\arguments{
\item{values}{A list of numeric vectors containing integer values to be scaled. Each vector is expected to contain values read
from a binary CSSY values block.}

\item{fmax}{A numeric value representing the maximum scaling value. Used to compute the linear scaling factor.}

\item{fmin}{A numeric value representing the minimum scaling value. Acts as an offset for the scaling.}

\item{fscale}{A numeric value representing the scaling factor. Must not be zero as it determines the divisor in the scaling formula.}

\item{dbRef}{A numeric value representing the decibel reference to be added before the voltage conversion step.}
}
\value{
A list with the same structure as \code{values}, where each numeric vector has been transformed to a vector of floating point
voltage values. Special integer values equal to 0xFFFFFFFF are converted to NaN.
}
\description{
This function applies scaling to each vector of integer values contained in the list \code{values} by converting them to floating point
voltage values using a specified scaling procedure. For each integer value:
\itemize{
\item If the value equals 0xFFFFFFFF, it returns NaN;
\item Otherwise, it computes an intermediate value using the formula:
intermediate = value * (fmax - fmin) / fscale + fmin
and then converts it to a voltage via:
voltage = 10^((intermediate + dbRef) / 10)
}
}
\details{
The function processes each vector in the input list and returns a new list having the same structure, but with each value converted
into its corresponding voltage value. It also performs several validations regarding input types and values.

The scaling process performs the following steps for each input value:
\enumerate{
\item Checks whether the value equals 0xFFFFFFFF. If so, it returns NaN immediately because this value indicates a
missing or invalid measurement.
\item Otherwise, it computes the intermediate scaled value by applying a linear transformation:
intermediate = value * (fmax - fmin) / fscale + fmin
\item Finally, it converts the intermediate value to a voltage using:
voltage = 10^((intermediate + dbRef) / 10)
}

The function includes input validation to ensure that \code{values} is a list, and that \code{fmax}, \code{fmin}, \code{fscale}, and \code{dbRef}
are numeric. It also checks that no element in \code{values} is non-numeric and that \code{fscale} is non-zero to prevent division errors.
}
\examples{
# Example usage:
values <- list(c(1000, 0xFFFFFFFF, 2000))
scaled <- seasonder_SeaSondeRCSSYApplyScaling(values, fmax = 5, fmin = 0, fscale = 1000, dbRef = -20)
print(scaled)

}
