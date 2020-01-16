\name{findCallsParam}
\alias{findCallsParam}
\title{Determine if a function directly invokes any its arguments as a function}
\description{
  This function analyzes the code in a function and determines
  if it directly or indirectly invokes any of its parameters
  and, if so, which ones.
}
\usage{
findCallsParam(fun, asCalls = FALSE)
}
\arguments{
  \item{fun}{the R function}
  \item{asCalls}{a logical value. If \code{TRUE}, the function returns
    all of the \code{rstatic} \code{Call} nodes to these parameters.
    Otherwise, it returns the names of the parameters that may be invoked.
  }
}
\value{
 If \code{asCalls} is \code{FALSE}, a character vector identifying the names of all instances of calls to
 a parameter.
 Otherwise, a list of \code{rstatic} \code{Call} nodes.
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link{getGlobals}} and what it identifies as a function rather
than a regular symbol/variable.
}
\examples{
foo = function(x,f)    
           f(x + 1) + 2
findCallsParam(foo)

findCallsParam(optim)
}
\keyword{programming}
\concept{static code analysis}
