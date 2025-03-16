\name{mkGlobalsLocal}
\alias{mkGlobalsLocal}
\title{Modify a function to make the global variables additional parameters}
\description{
This function analyzes a collection of functions and identifies the global variables each
uses and then adds parameters for each of these parameters to allow the caller
to override the global variable. The default value for each additional parameter we add is the original
global variable so that each function behaves the same, by default, but is no longer
constrained to use only those global variables.
For example, consider a function that uses a global variable a
\code{function(x) x + a}. This is modified to be
\code{function(x, .a = a) x + .a}.
This allows a caller to use \code{f(10)} and use the global value of
\code{a}
or \code{f(10, 4)} to override \code{a}.
}
\usage{
mkGlobalsLocal(..., .funs = list(...), .addDefaults = rep(TRUE, length(.funs)))
}
\arguments{
  \item{\dots}{zero or more function objects to be analyzed and modified}
  \item{.funs}{an alternative way to specify the collection of function objects}
  \item{.addDefaults}{a logical vector/value controlling whether the
    default value for the added parameters is the original corresponding
    global variable}
}
\value{
A list of updated functions.
}
\author{
Duncan Temple Lang (?)
}

\seealso{
  \code{\link{getGlobals}}
}
\examples{
\dontrun{
f = function(x) x + a
f2 = mkGlobalsLocal(f)
formals(f)
formals(f2)
f
f2
}
}
\keyword{programming}
\concept{static code analysis}
