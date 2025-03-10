% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS_FOR.R
\name{seasonder_setSeaSondeRCS_FOR_MAXP.bin}
\alias{seasonder_setSeaSondeRCS_FOR_MAXP.bin}
\title{Set Maximum Power Bin Indices for First Order Region (FOR)}
\usage{
seasonder_setSeaSondeRCS_FOR_MAXP.bin(seasonder_cs_obj, FOR_MAXP.bin)
}
\arguments{
\item{seasonder_cs_obj}{A \code{SeaSondeRCS} object to which the \code{MAXP.bin} values will be assigned.}

\item{FOR_MAXP.bin}{A list containing the Doppler bin indices of the maximum power for each range cell.}
}
\value{
The updated \code{SeaSondeRCS} object with the \code{MAXP.bin} values stored in the \code{FOR_data} attribute.
}
\description{
This function assigns the Doppler bin indices corresponding to the maximum power (\code{MAXP.bin})
for each range cell in the First Order Region (FOR) to the \code{SeaSondeRCS} object.
}
\details{
The maximum power bin (\code{MAXP.bin}) represents the Doppler bin index at which the highest spectral power
was detected in the first-order region. This information is used to refine first-order boundary detection.

\strong{Validation Considerations:}
\itemize{
\item The function does not currently validate the format of \code{FOR_MAXP.bin}.
\item Future improvements should ensure that \code{FOR_MAXP.bin} consists of integer values corresponding to Doppler bins.
}
}
\examples{
\dontrun{
# Assign maximum power bin indices to a SeaSondeRCS object
cs_obj <- seasonder_setSeaSondeRCS_FOR_MAXP.bin(cs_obj, MAXP_bin_values)
}
}
\seealso{
\itemize{
\item \code{\link{seasonder_findFORNulls}} for computing \code{MAXP.bin}.
\item \code{\link{seasonder_setSeaSondeRCS_FOR_MAXP}} for setting maximum power values.
}
}
