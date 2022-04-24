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
  The relevant language object
}
\author{
Duncan Temple Lang
}
\seealso{}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
