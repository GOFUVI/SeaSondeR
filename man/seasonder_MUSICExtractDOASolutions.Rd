% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS_MUSIC.R
\name{seasonder_MUSICExtractDOASolutions}
\alias{seasonder_MUSICExtractDOASolutions}
\title{Extract Direction of Arrival (DOA) Solutions Using MUSIC Algorithm}
\usage{
seasonder_MUSICExtractDOASolutions(projections)
}
\arguments{
\item{projections}{A matrix of projections where each column corresponds to a set of MUSIC spectra for single and dual solutions.
The matrix should have the attribute \code{"bearings"} indicating the corresponding bearing angles in degrees.}
}
\value{
A list containing the extracted single and dual DOA solutions, each with:
\itemize{
\item \code{bearing}: The bearing(s) corresponding to the detected peak(s).
\item \code{a}: The associated antenna pattern matrix values for the detected peak(s).
\item \code{peak_resp}: The response levels at the detected peak(s) in dB.
}
}
\description{
This function analyzes projection data using the Multiple Signal Classification (MUSIC) algorithm
to identify Direction of Arrival (DOA) solutions for radar signals. It implements the methodology
described by Paolo and Terril (2007) for HF radar signal analysis.
}
\details{
The function performs the following steps:
\enumerate{
\item Reverses the distances for single and dual solution projections to enhance peak detectability.
\item Detects peaks in the reversed single solution projection, retaining the highest peak.
\item Detects peaks in the reversed dual solution projection, retaining the two highest peaks.
\item Maps the identified peak positions back to their corresponding bearings.
}

The identification of DOA solutions using MUSIC relies on the inversion of spectral distances, as detailed in Paolo and Terril (2007),
to emphasize potential peaks corresponding to source directions.
}
\examples{
\dontrun{
projections <- matrix(runif(100), nrow = 2, ncol = 50)
attr(projections, "bearings") <- seq(0, 359, length.out = 50)
result <- seasonder_MUSICExtractDOASolutions(projections)
print(result)
}

}
\references{
Paolo, S., & Terril, E. (2007). Detection and characterization of signals in HF radar cross-spectra using the MUSIC algorithm.
\emph{Journal of Atmospheric and Oceanic Technology}.
}
\seealso{
\code{\link{seasonder_MUSICExtractPeaks}}, \code{\link{pracma::findpeaks}}
}
