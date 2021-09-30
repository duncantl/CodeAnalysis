\name{rlangType}
\alias{rlangType}
\title{Get the R language type of an rstatic AST object}
\description{
  This converts an rstatic object to the corresponding R language
  representation, i.e., symbols/names, calls, if statements, loops
  and returns the class of the resulting R object.
}
\usage{
rlangType(x)
}
\arguments{
  \item{x}{an rstatic AST object}
}
\value{
  a character vector
}
\author{Duncan Temple Lang}
\seealso{
\code{\link[rstatic]{to_ast}}
\code{\link[rstatic]{as_language}}  
}
\examples{}
\keyword{programming}

