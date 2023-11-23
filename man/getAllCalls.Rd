\name{getAllCalls}
\alias{getAllCalls}
\alias{getAllSymbols}
\alias{all_symbols}
\alias{numCalls}
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

  \code{getAllSymbols} gets all the symbols in an  R language object.
}
\usage{
getAllCalls(x, ...)
numCalls(x
getAllSymbols(expr, predicate = is.symbol)
}
\arguments{
  \item{x}{a character vector of file or directory names or the parsed code}
  \item{\dots}{arguments for methods}
  \item{expr}{the R code/language object}
  \item{predicate}{the function that determines whether the language object is a symbol
    and one we want to capture. This can be more specific such as
    \code{function(x) is.symbol(x) && as.character(x) \%in\% c("foo", "bar")}
   }
}
\value{
A list of \code{rstatic} \code{Call} objects.
}
\references{
\code{rstatic} package \url{https://github.com/nick-ulle/rstatic.git}
}
\author{Duncan Temple Lang, Nick Ulle}
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
