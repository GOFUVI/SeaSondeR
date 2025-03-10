% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS.R
\name{seasonder_Bins2NormalizedDopplerFreq}
\alias{seasonder_Bins2NormalizedDopplerFreq}
\title{Convert Doppler Bins to Normalized Doppler Frequency}
\usage{
seasonder_Bins2NormalizedDopplerFreq(seasonder_cs_obj, bins)
}
\arguments{
\item{seasonder_cs_obj}{A \code{SeaSondeR} cross-spectral object containing Doppler bin metadata.}

\item{bins}{A numeric vector specifying the Doppler bin indices.}
}
\value{
A numeric vector of normalized Doppler frequencies corresponding to the specified bins.
}
\description{
This function retrieves the normalized Doppler frequencies corresponding to the specified bins in a given \code{SeaSondeR} object.
}
\details{
This function first retrieves the Doppler bin frequencies in normalized form using \code{\link{seasonder_getDopplerBinsFrequency}}. It then selects the normalized Doppler frequencies corresponding to the specified bin indices.

\strong{Normalized Doppler Frequency Calculation:}
The normalized Doppler frequency is typically defined as:
\deqn{f_{norm} = \frac{f_{doppler}}{f_{bragg}}}
where:
\itemize{
\item \( f_{norm} \) is the normalized Doppler frequency,
\item \( f_{doppler} \) is the Doppler frequency of a given bin,
\item \( f_{bragg} \) is the Bragg frequency, computed based on radar wavelength.
}
}
\examples{
\dontrun{
# Convert specific Doppler bins to normalized Doppler frequency
cs_obj <- seasonder_createSeaSondeRCS(...)
bins <- c(10, 20, 30)
normalized_freqs <- seasonder_Bins2NormalizedDopplerFreq(cs_obj, bins)
print(normalized_freqs)
}
}
\seealso{
\code{\link{seasonder_getDopplerBinsFrequency}} for retrieving Doppler bin frequencies.
}
