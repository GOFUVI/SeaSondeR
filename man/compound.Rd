% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{\%<>\%}
\alias{\%<>\%}
\title{Assigment pipe}
\usage{
lhs \%<>\% rhs
}
\arguments{
\item{lhs}{An object which serves both as the initial value and as target.}

\item{rhs}{A function call using the magrittr semantics.}
}
\value{
The result of calling \code{rhs(lhs)}.
}
\description{
See \code{magrittr::\link[magrittr:pipe]{\%<>\%}} for details.
}
\keyword{internal}
