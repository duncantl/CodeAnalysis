\name{S3Assignments}
\alias{S3Assignments}
\title{Identify S3 class assignments in R code}
\description{
  This function analyzes an R function's code and
  finds the assignment of class attributes
  to any object. These are S3 class definitions.
}
\usage{
S3Assignments(fun, asNode = FALSE)
}
\arguments{
  \item{fun}{the R code to search for S3 class assignments}
  \item{asNode}{a logical value controlling
    whether the function returns the \code{rstatic} nodes
    in the abstract syntax tree in which the assignments to the class
    are made, or alternativel (\code{FALSE}) the names of the classes.
  The latter can include \code{NA} values indicating non-literal values
  for the class names.}
}
\value{
  If \code{asNode} is \code{FALSE}, a character vector
  that can also include  \code{NA} values.

  If \code{asNode} is \code{TRUE}, a list
  containing the \code{rstatic} node objects
  from the abstract syntax tree (AST) corresponding
  to the S3 class assignments.
}
\author{
Duncan Temple Lang
}
\examples{
f2 =
function(x)
{
    ans = c(low = min(x), high = max(x))
    class(ans) = "Extent"
    ans
}


S3Assignments(f2)

f4 =
function(x)
    structure(c(low = min(x), high = max(x)), class = c("Extent", "1D"))

S3Assignments(f4)
}
\keyword{programming}
\concept{static code analysis}
