\name{getAttributeNames}
\alias{getAttributeNames}
\title{
Find Names of Attributes Queried and Set in R Code
}
\description{

  This  finds names that are literal strings
  in the calls to \code{\link[base]{attr}}, \code{\link[base]{attributes}}
  (and their assignment equivalents) and \code{\link[base]{structure}}.

  It currently doesn't do constant propagation, look backs or inference to
  handle indirect references to attributes, i.e., via a variable containing the attribute name.
}
\usage{
getAttributeNames(x)
}
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

  getAttributeNames(tools::prepare_Rd)
}
\keyword{programming}
\concept{meta-programming}
