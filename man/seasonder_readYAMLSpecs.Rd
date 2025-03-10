% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasonder_specsHandling.R
\name{seasonder_readYAMLSpecs}
\alias{seasonder_readYAMLSpecs}
\title{Read Specifications from a YAML File}
\usage{
seasonder_readYAMLSpecs(file_path, path = rlang::zap())
}
\arguments{
\item{file_path}{A string. The path to the YAML file.}

\item{path}{A character vector. Represents the path within the YAML file to
access the desired information. For example, to access fields of version V2
of the header, the path would be \code{c("header", "versions", "V2")}.}
}
\value{
A list. The information extracted from the YAML file based on the
provided path.
}
\description{
This function reads a YAML file containing specifications, handles potential
reading errors, and extracts specific information based on a provided path.
}
\section{Error Handling}{

The function has built-in error handling which aborts the function's execution and logs
detailed error messages in case of:
\itemize{
\item File not found.
\item Error in reading the YAML content.
\item If the read YAML content is not of list type.
\item If no data is found for the provided path in the YAML content.
}
Errors generated are of class \code{"seasonder_read_yaml_file_error"}.
Detailed error information including the file path and path within the file
is provided. For logging and aborting, this function utilizes the
\code{\link[=seasonder_logAndAbort]{seasonder_logAndAbort}} function.
}

\examples{
\dontrun{
# Assuming a YAML file named "example.yaml" exists with appropriate content
result <- seasonder_readYAMLSpecs("example.yaml", c("header", "versions", "V2"))
print(result)
}

}
\seealso{
\code{\link[yaml]{read_yaml}} for the underlying YAML reading.

\code{\link[purrr]{pluck}} for the data extraction mechanism used.
}
