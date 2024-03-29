% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS.R
\name{seasonder_getDopplerBinsFrequency}
\alias{seasonder_getDopplerBinsFrequency}
\title{Get Doppler Bins Frequency}
\usage{
seasonder_getDopplerBinsFrequency(seasonder_cs_obj, normalized = FALSE)
}
\arguments{
\item{seasonder_cs_obj}{A SeaSonde Cross Spectrum (CS) object created by \code{seasonder_createSeaSondeRCS()}. This object contains the necessary metadata and spectral data to compute Doppler bin frequencies.}

\item{normalized}{A logical value indicating if the returned frequencies should be normalized by the second Bragg frequency. When \code{TRUE}, frequencies are divided by the second Bragg frequency, returning dimensionless values relative to it. Default is \code{FALSE}, returning frequencies in Hz.}
}
\value{
A numeric vector of frequencies representing the high limit of each Doppler bin interval. If \code{normalized} is TRUE, these frequencies are dimensionless values relative to the second Bragg frequency; otherwise, they are in Hz.
}
\description{
This function calculates the frequency limits for each Doppler bin within a SeaSonde Cross Spectrum (CS) object. It can return frequencies either in their original Hz values or normalized by the second Bragg frequency. The frequencies are calculated as the high limit of each Doppler bin interval, similar to what is displayed in SpectraPlotterMap.
}
\details{
The function internally utilizes several helper functions such as \code{seasonder_getCenterDopplerBin()}, \code{seasonder_getnDopplerCells()}, and \code{seasonder_getDopplerSpectrumResolution()} to calculate the Doppler bin frequencies. Furthermore, when normalization is requested, it uses \code{seasonder_getBraggDopplerAngularFrequency()} to obtain the second Bragg frequency for normalization purposes.
}
