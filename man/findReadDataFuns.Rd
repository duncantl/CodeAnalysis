\name{findReadDataFuns}
\alias{findReadDataFuns}
\alias{findWriteDataFuns}
\alias{findGraphicsDevFuns}
\title{
Identify functions that call read and write functions
}
\description{
  With no arguments, each of these functions either return a fixed set of known functions
  that read data or write data or create a file for a graphics device.
  One can also pass the source of function definitions, e.g., a named list of
  functions, a character vector of R file names, a directory containing
  R files, an environment, etc. and the functions analyze those
  functions to see if they call any of the the foundatinal functions and
  if so, include those wrapper functions in the return value.
}
\usage{
findReadDataFuns(funs, ..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
findWriteDataFuns(funs, ..., primitiveFuns = c(PrimitiveSaveDataFuns, ...))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{funs}{the source of function definitions. This can be the name
    of a directory, a file, a named list of functions, an environment
    containing functions.}
  \item{\dots}{character strings naming additional primitive functions.
    Functions that contain a call to any of these are considered matches.}
  \item{primitiveFuns}{the character vector providing the names of the
    primitive functions.
  }
}
\details{
  \code{findReadDataFuns} is quite general and can be readily repurposed
  by specifying a different vector for \code{primitiveFuns}
  to find functions that call any of these different functions.
}
\value{
a character vector
}
\author{
Duncan Temple Lang
}
%\seealso{}
%\examples{}
\keyword{IO}
\concept{static code analysis}
\concept{meta programming}
