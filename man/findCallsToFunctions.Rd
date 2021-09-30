\name{findCallsToFunctions}
\alias{findCallsToFunctions}
\title{Find all calls or specific parameters to given functions}
\description{
  This analyzes R code and finds all calls to functions in the given
  set of interest and either returns these or the specified argument(s),
  either by position or name.
  If argIndices has length zero, the calls are returned.
  Otherwise, the corresponding parameters from each call are returned.
}
\usage{
findCallsToFunctions(allCalls, funNames, argIndices = integer(), definitions = NULL)
}
\arguments{
  \item{allCalls}{a character vector of file or directory names or
    parsed R code or list of calls}
  \item{funNames}{the names of functions of interest}
  \item{argIndices}{an integer or character vector identifying the
    parameters of interest in each of the calls}
  \item{definitions}{a list of functions that provides the definitions
    for functions that are not available on the search path. These are
    used in \code{\link[base]{match.call}}.}
}
\details{
}
\value{
  If \code{argIndices} has length zero, a list of all the matching
  \code{Call} objects.
  Otherwise, a list of all the parameters in each matching call
  identified by \code{argIndices} either by position or name.
}
\author{Duncan Temple Lang}

%\seealso{}
\examples{
}
\concept{meta-programming}
\concept{static analysis}