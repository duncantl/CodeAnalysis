\name{findLoopConcat}
\alias{findLoopConcat}
\title{Identify loop code that concatenates results}
\description{
  This function helps to identify \code{for} loops in R code
  that uses the idiom
  \code{ x = c() ; for(i in y) x = c(x, g(i))}
  This is a somewhat common idiom used by new R programmers.
  It is very costly in memory and hence time. 
  Accordingly, we want to identify it as a problem.

  At present, this function identifies the variables which are used in
  this concatenating manner. It does not currently rewrite the expressions.
}
\usage{
findLoopConcat(expr, possibleVars = character())
}
\arguments{
  \item{expr}{the R expressions to search for the use of the concatenation idiom.}
  \item{possibleVars}{a character vector of names of variables that
    might be concatenated.  This should not be provided by the caller.
    It is provided in recursive calls to the function.}
}
\value{
  Currently, this returns a character vector identifying
  the variables that are concatenated using this idiom.
}
\author{
Duncan Temple Lang
}

\examples{
 e = quote({ x = c(); for(i in z)  x = c(x, f(i)) })
 findLoopConcat(e)
}
\keyword{programming}

