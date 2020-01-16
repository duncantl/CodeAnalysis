\name{getR6ClassInherits}
\alias{getR6ClassInherits}
\title{Compute the class hierarchy for a collection of R6 class definitions}
\description{
  This function analyzes the R6 class definitions in a package
  and computes the relationships between them
  in terms of which classes inherit/extend other classes.
  This creates a class hierarchy which we can then query
  and visualize.
}
\usage{
getR6ClassInherits(pkg, ns = getNamespace(pkg))
}
\arguments{
  \item{pkg}{the name of an R package (without the \code{"package:"} prefix.}
  \item{ns}{the namespace for the package}
}
\value{
}
\author{
Duncan Temple Lang
}
\seealso{
}
\examples{
}
\keyword{programming}
\keyword{methods}
\concept{class hierarchy}
\concept{code analysis}
