% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS.R
\name{seasonder_getRadialVelocityResolution}
\alias{seasonder_getRadialVelocityResolution}
\title{Calculate Radial Velocity Resolution}
\usage{
seasonder_getRadialVelocityResolution(seasonder_cs_obj)
}
\arguments{
\item{seasonder_cs_obj}{A SeaSondeRCS object created using \code{seasonder_createSeaSondeRCS}. This object
contains the necessary data to calculate the Doppler spectrum resolution and, subsequently, the
radial velocity resolution.}
}
\value{
A single numeric value representing the radial velocity resolution in meters per second (m/s),
indicating the radar's ability to differentiate between closely spaced velocities.
}
\description{
Computes the radial velocity resolution for a SeaSonde radar cross-section (CS) object.
This measurement indicates the smallest change in velocity that the radar can
discern between different targets or scatterers within its observation area.
The calculation is based on the Doppler spectrum resolution and the radar wave
number, providing a crucial parameter for analyzing the radar's capability to
distinguish between velocities.
}
\details{
The radial velocity resolution (\(v_{res}\)) is determined using the formula:
[
v_{res} = \frac{\text{SpectraRes}}{2 \cdot k_0}
]
where \(v_{res}\) is the radial velocity resolution, \(\text{SpectraRes}\) is
the Doppler spectrum resolution, and \(k_0\) is the radar wave number divided
by \(2\pi\). This formula reflects the relationship between the
frequency resolution of the radar's Doppler spectrum and the corresponding
velocity resolution, taking into account the wave number which is a fundamental
characteristic of the radar system.
}
\seealso{
\code{\link{seasonder_getDopplerSpectrumResolution}},
\code{\link{seasonder_getRadarWaveNumber}}
}
