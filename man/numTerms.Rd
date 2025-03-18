\name{numTerms}
\alias{numTerms}
\alias{getTerms}
\title{
Compute the Number of Language Elements in Given Code
}
\description{
  This traverses the R language object and computes the number of individual elements,
  e.g., symbols, calls, \code{\{}, parameters/formal arguments.

    \code{getTerms} can be helpful to see the individual elements and understand
    the details of a complex language object. It is like recursive unlist of a top-level language object.
}
\usage{
numTerms(x, ctr = mkCounter(self = self, skipIfFalse = skipIfFalse), skipIfFalse = TRUE, self = FALSE)
}
\arguments{
  \item{x}{the R language/code object}
  \item{ctr}{the code walker which is a list of functions passed to
    \code{\link[codetools]{walkCode}}.
    It is only useful to specify this explicitly if one
    wants to walk more than one code object, i.e., reuse the
    walker across multiple calls to \code{numTerms} or \code{getTerms}
    to cumulate all the terms together. 
   }
  \item{skipIfFalse}{a scalar logical indicating whether to skip code in \code{if(FALSE)} constructs.}
  \item{self}{a scalar logical indicating whether to include certain objects in the count. Typically
      leave this as \code{FALSE}}
}
\value{
  \code{numTerms} returns a number.

  \code{getTerms} returns a list of all the language elements found in the code.
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link[codetools]{walkCode}}
}
\examples{
   numTerms(extractFunctions)

\dontrun{
   els = as.list(getNamespace("tools"), all.names = TRUE)

   #  takes about 3.7 seconds (on a macbook pro)
   nt = sapply(els, numTerms)
   summary(nt)
}

}
\keyword{programming}
\concept{meta-programming}
