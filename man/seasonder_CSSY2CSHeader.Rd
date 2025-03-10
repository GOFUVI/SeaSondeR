% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCSSY.R
\name{seasonder_CSSY2CSHeader}
\alias{seasonder_CSSY2CSHeader}
\title{Transform CSSY Header to SeaSondeRCS Header}
\usage{
seasonder_CSSY2CSHeader(header)
}
\arguments{
\item{header}{A list representing the CSSY header. Must contain a 'cs4h' component.}
}
\value{
A transformed header where the primary CS header is taken from 'cs4h' and the remaining CSSY header fields
are stored in the 'header_cssy' element.
}
\description{
This helper function extracts the 'cs4h' component from a CSSY header, removes it from the original header,
and embeds the remaining header information within the 'header_cssy' field of the CS header.
}
