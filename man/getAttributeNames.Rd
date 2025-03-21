\name{getAttributeNames}
\alias{getAttributeNames}
\title{
Find Names of Attributes Queried and Set in R Code
}
\description{

  Currently this only finds names that are literal strings
  in the calls to \code{\link[base]{attr}}, \code{\link[base]{attributes}}
  (and their assignment equivalents) and \code{\link[base]{structure}}.
}
\usage{
getAttributeNames(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the R code object to analyze}
}
\value{
  A character vector giving the names of the attributes found in the code.
  The name of each element of the character vector indicates the function in which
  that attribute names was found.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{findCallsTo}}  
  \code{\link{findUsedOptions}}
}

\examples{
  getAttributeNames(tools:::.install_packages)
}
\keyword{programming}
\concept{meta-programming}
