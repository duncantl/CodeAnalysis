\name{getIfCond}
\alias{getIfCond}
\alias{getIfTrue}
\alias{getIfFalse}
\alias{isIfFalse}
\title{Accessor functions for elements of \code{if} language objects}
\description{
  Typically, these are not needed as the user/programmer
  knows the structure of the \code{if} language objects.
  They hide the details when introducing meta-programming.

  \code{isIfFalse} determines whether the condition is
  a literal \code{FALSE} value and so the
  code within the \code{if} body will never be evaluated.
}
\usage{
getIfCond(code)
getIfTrue(code)
getIfFalse(code)
}
\arguments{
  \item{code}{the R language object}
}
\value{
  The relevant language objects.
}
\author{
Duncan Temple Lang
}
\seealso{
 \code{\link{findCallsTo}}
}
\examples{
}
\keyword{programming}
\concept{metaprogramming}

