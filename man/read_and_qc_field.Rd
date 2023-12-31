% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeaSondeRCS.R
\name{read_and_qc_field}
\alias{read_and_qc_field}
\title{Read and Quality Control a Single Field}
\usage{
read_and_qc_field(field_spec, connection, endian = "big")
}
\arguments{
\item{field_spec}{A list containing the specifications for the field to read.
It should contain:
\itemize{
\item \code{type}: the type of data to read, passed to \code{seasonder_readCSField}.
\item \code{qc_fun}: the name of a quality control function. As detailed in \code{seasonder_readSeaSondeCSFileBlock},
this function should be present in the shared environment \code{seasonder_the} and must accept
\code{field_value} as its first argument, followed by any other arguments specified in \code{qc_params}.
\item \code{qc_params}: a list of additional parameters to pass to the quality control function. See
\code{seasonder_readSeaSondeCSFileBlock} for detailed expectations of the QC function behavior.
}}

\item{connection}{A connection to the binary file.}

\item{endian}{A character string indicating the byte order. Options are "big" and "little" (default is "big").}
}
\value{
The value of the field after quality control. Can be the original value, a transformed value,
or NULL if the value fails quality control. The exact behavior of the quality control function, including
the handling of NULL values, is detailed in \code{seasonder_readSeaSondeCSFileBlock}.
}
\description{
This auxiliary function reads a field from a binary file using a provided specification and
applies a quality control function on the retrieved data. The expectations and functioning of the
quality control functions are described in detail in the documentation for \code{seasonder_readSeaSondeCSFileBlock}.
}
\section{Condition Management}{

This function utilizes the \code{rlang} package to manage conditions and provide detailed and structured condition messages:

\strong{Condition Classes}:
\itemize{
\item \code{seasonder_cs_field_skipped}: Condition that indicates a CSField was skipped during reading.
\item \code{seasonder_cs_field_qc_fun_rerun}: Condition that indicates a rerun of the quality control function was triggered.
\item \code{seasonder_cs_field_qc_fun_not_defined_error}: Error raised when the quality control function specified is not found in the shared environment \code{seasonder_the}.
\item \code{seasonder_cs_field_qc_fun_error}: Error raised when an issue occurs while applying the quality control function.
}

\strong{Condition Cases}:
\itemize{
\item If a CSField is skipped during reading, the condition \code{seasonder_cs_field_skipped} is used to skip QC and then is re-signaled.
\item If an alternate QC is rerun using the \code{seasonder_rerun_qc_with_fun} restart, the condition \code{seasonder_cs_field_qc_fun_rerun} is signaled.
\item If the quality control function specified is not found in the shared environment \code{seasonder_the}, the error \code{seasonder_cs_field_qc_fun_not_defined_error} is raised.
\item If there's an issue applying the quality control function, the error \code{seasonder_cs_field_qc_fun_error} is raised.
}

\strong{Restart Options}:
The function provides structured mechanisms to recover from errors/conditions during its execution using \code{withRestarts}. The following restart options are available:

\itemize{
\item \code{seasonder_rerun_qc_with_fun}: Allows for rerunning QC with an alternate function.
\itemize{
\item \strong{Usage}: In a custom condition handler, you can call \code{seasonder_rerun_qc_with_fun(cond, alternateQCfunction)} to trigger this restart and run an alternate QC using \code{alternateQCfunction}. \code{alternateQCfunction} will be used as follows alternateQCfunction(x) being x the value. No extra parameters are passed.
\item \strong{Effect}: If invoked, the function logs an info message detailing the reason of the rerun, and then returns the value returned by \code{alternateQCfunction}.
}
}
}

\seealso{
\code{\link{seasonder_rerun_qc_with_fun}},
\code{\link{seasonder_readCSField}}

It's also important to note that within \code{read_and_qc_field}, the function \code{seasonder_readCSField} is used. This function has its own error management and restart options, which are detailed in its documentation.
}
