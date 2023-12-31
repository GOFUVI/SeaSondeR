% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS.R
\name{readV6BlockData}
\alias{readV6BlockData}
\title{Read Version 6 Block Data}
\usage{
readV6BlockData(
  specs,
  connection,
  endian = "big",
  prev_data = NULL,
  remaining_loops = NULL
)
}
\arguments{
\item{specs}{A list. Specifications detailing the structure and content of the data blocks.
Contains variable names, types, quality check functions, and other related attributes.
For repeated blocks, a 'repeat' key is added which details the loop structure and
nested specifications.}

\item{connection}{A connection object. Represents the connection to the data source. It's passed
to the lower-level reading function.}

\item{endian}{A character string. Specifies the byte order to be used. Default is "big".
Passed to the lower-level reading function.}

\item{prev_data}{A list. Previous data or metadata that might be required to inform the reading
process, such as loop lengths for repeated blocks. Default is NULL.}

\item{remaining_loops}{A character vector. Details the remaining loops to be processed for
repeated blocks. Internally used for recursive processing. Default is NULL.
If provided, it should always be in sync with the repeat specifications.}
}
\value{
A list. Contains the read and processed data based on the provided specifications.
Regular variables are returned at the top level. Repeated blocks are nested lists with
'loop' and 'data' keys detailing the loop variable and corresponding data.
}
\description{
This function reads and processes regular and repeated blocks of data
based on provided specifications. Regular blocks are read directly, while
repeated blocks are processed recursively based on a set of loops provided
in the specifications.
}
\seealso{
\code{\link{readV6BlockData}}
}
