\name{getAllCalls}
\alias{getAllCalls}
\title{Get all function calls in R code}
\description{
  This statically analyzes the R code and finds all calls
  both at the top-level in a script and in functions,
  in a directory of R files, etc.
  
  One situation in which this can be useful is to identify
  which packages need to be loaded/imported for a collection
  of files, e.g., when creating a package from existing files,
  or knowing which calls to \code{link[base]{library}}
  are needed.
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
\code{rstatic} package \url{https://github.com/nick-ulle/rstatic.git}
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{findCallsTo}}
}
\examples{
\dontrun{
  # calls by file in the directory R/
  f = getAllCalls("R")
  # flatten to a list of all calls
  k = unlist(f, recursive = FALSE)
  # What is the class
  table(sapply(k, function(x) class(x$fn)[1]))
  funs = sapply(k[sapply(k, function(x) is(x$fn, "Symbol"))], function(x) x$fn$value)
  table(funs[ !(funs %in% ls("package:base"))])
}
}
\concept{meta-programming}
\concept{static analysis}
