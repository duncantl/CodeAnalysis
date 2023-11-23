\name{isSymbol}
\alias{isSymbol}
\title{Helper function for comparing a language object to a symbol/name, optionally with one of
  specified values}
\description{
  When statically analyzing R code, we often
  want to check if a language object is a name/symbol, and if it is,
  does its value correspond to one of a set in which we are interested.
  This function simplifies this predicate test/check
  which typically involves calls to 3 functions,
  rather than a single one using this function.
}
\usage{
isSymbol(x, sym = character())
}
\arguments{
  \item{x}{the R language object}
  \item{sym}{an optional character vector. If this is not empty,
     if \code{x} is a name/symbol, we check if the value is in this set.
   }
}
\value{
 \code{TRUE} or \code{FALSE}
}

\author{
Nick Ulle, Duncan Temple Lang
}
\seealso{
  \code{\link[base]{is.name}}
  \code{\link[codetools]{walkCode}}
}
\examples{
 e1 = quote(foo(x, y, z + 1))
 e2 = quote(foo$bar(x, y, z + 1))

 isSymbol(e1[[1]])
 isSymbol(e2[[1]])

 sapply(e1, isSymbol)
 sapply(e2, isSymbol)
}
\keyword{programming}
\concept{metaprogramming}
