\name{findLiterals}
\alias{findLiterals}
\title{Find all literal language objects in R code object}
\description{
  This finds all literal values such as
  \code{TRUE}, \code{FALSE}, numbers,
  strings in an R code/language object.
}
\usage{
findLiterals(code,
             walker = mkLiteralCollector(ignoreParams, skipIfFalse = skipIfFalse, ...),
             ignoreParams = TRUE, skipIfFalse = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{code}{the R language/code object in which to search for literals.}
  \item{walker}{a code-walker function that traverses the code
    and tests whether a language object is a literal value and
    collects it if so.
  }
  \item{ignoreParams}{a scalar logical value controlling
    whether to process the code for the default values of parameters/formal arguments
    in a function object, with \code{FALSE} indicating to process them and \code{TRUE}
    meaning we skip/ignore them.
  }
  \item{skipIfFalse}{
    a logical value that if \code{TRUE} means we ignore code of the form
     \code{if(FALSE) {...}}, i.e., code that cannot be executed.
   }
  \item{\dots}{additional arguments that are passed to \code{mkLiteralCollector}}
}
\value{
 This returns the collected values from \code{walker}.
}
\references{
  The \code{codetools} package.
}

\author{
  Duncan Temple Lang
}
\seealso{
  \code{\link{findCallsTo}}
}
\examples{
f = function(x = "abc") {
    a = 1L
    b = 3.1415
    c = "xyz"
    if(a < 10)
        paste(x, c, sep = " - ")
    else
        x
}
findLiterals(f)
}
\keyword{programming}
\concept{metaprogramming}

