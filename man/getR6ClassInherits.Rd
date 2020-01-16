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
  A 2-column \code{data.frame} providing the to-from edges for the graph
  that identifies which class directly inherits from which other class.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{mkClassGraph}}
}
\examples{
g = getR6ClassInherits("rstatic")
if(require(igraph))
   plot(graph_from_data_frame(g), cex.arrow.size = .3)
}
\keyword{programming}
\keyword{methods}
\concept{class hierarchy}
\concept{code analysis}
