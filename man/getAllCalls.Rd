\name{getAllCalls}
\alias{getAllCalls}
\title{Get all function calls in R code}
\description{
  This statically analyzes the R code and finds all calls
  both at the top-level in a script and in functions, etc.
}
\usage{
getAllCalls(x, ...)
}
\arguments{
  \item{x}{a character vector of file or directory names or the parsed code}
  \item{\dots}{arguments for methods}
}
\value{
A list of \code{rstatic} \code{Call} objects.
}
\references{
\code{\link{rstatic}}
}
\author{Duncan Temple Lang}

\seealso{}
\examples{
}
\concept{meta-programming}
\concept{static analysis}