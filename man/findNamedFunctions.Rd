\name{findNamedFunctions}
\alias{findNamedFunctions}
\title{Find Named/Non-Anonymous Nested Functions}
\description{
Given a function, find the named-function definitions.
}
\usage{
findNamedFunctions(fun, skipChained = FALSE)
}
\arguments{
  \item{fun}{a function}
  \item{skipChained}{a scalar logical controlling whether
    chained assignments of the form \code{a = b = function() ...}
    are considered a match (\code{TRUE}) or not.
    }
}
\value{
  A list of assignment calls in which the
  right hand side is a call to \code{function}.
}
\author{Duncan Temple Lang}

%\seealso{}
%\examples{}

\keyword{programming}
\concept{meta-programming}

