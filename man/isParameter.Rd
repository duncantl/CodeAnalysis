\name{isParameter}
\alias{isParameter}
\alias{isLocalVar}
\title{Determine if a variable name is a function parameter or a local variable}
\description{
  These functions allow one to query whether a variable name corresponds
  to either a formal argument/parameter of a given function
  or to a variable that is created locally in the body of the function,
  or neither.
}
\usage{
isParameter(p, fun)
isLocalVar(p, fun, notParam = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{the name of the variable to query}
  \item{fun}{the R function}
  \item{notParam}{a logical value related to how
    we treat parameters/formal arguments that are also local variables.
    A parameter which has no local assignments to its name will not be considered
    a local variable.
    However, if there is one or more assignments to that parameter name in the body of the
    function, \code{notParam} determines whether \code{isLocalVar} returns
    \code{TRUE} for a local variable or not in this case.
    If this is \code{TRUE}, then the parameter is never considered a local variable.
    If this is \code{FALSE},
    then a variable name that corresponds to a parameter and has local assignments within the body
    is considered a local variable so the result will be \code{TRUE}.
  }
}
\value{
  A logical value
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{findAssignsTo}}
  \code{\link[base]{formals}}
}
\examples{
  f = function(x, y) {
     len = length(x)
     w = !is.na(x)
     (x[w] + y[w])/len
  }
  isLocalVar("x", f)
  isLocalVar("x", f, FALSE)
  isLocalVar("w", f)
  isLocalVar("len", f)

  isParameter("x", f)
  isParameter("len", f)
  isParameter("other", f)
}
\keyword{programming}
\concept{metaprogamming}
