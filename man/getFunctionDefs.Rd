\name{getFunctionDefs}
\alias{getFunctionDefs}
\title{
Get function definitions in R code files
}
\description{
  Using static analysis (rather than \code{\link[base]{source}}),
  this returns the top-level function definitions in the files in a
  directory or an individual file.
}
\usage{
getFunctionDefs(x, ...)
}
\arguments{
  \item{x}{a character vector of file or directory names or  a language
    object from parsing R code}
  \item{\dots}{additional arguments for methods}
}
\value{
A list of top-level functions defined in the given code.
}
\author{Duncan Temple Lang}

%\seealso{}
\examples{
}
\concept{meta-programming}
\keyword{programming}

