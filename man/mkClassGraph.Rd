\name{mkClassGraph}
\alias{mkClassGraph}
\title{Create graph of S4 class hierarchy}
%XXX add different types of links in the plot for representing as() relationships.
\description{
  This constructs a hierarchical structure representing
  the inheritance relationships between between a collection of S4 classes.
}
\usage{
mkClassGraph(pkg, addIs = TRUE, classes = getClasses(pkg),
              classDefs = structure(lapply(classes, getClass), names = classes))
}
\arguments{
  \item{pkg}{the name of the R package in which to find the S4 class definitions}
  \item{addIs}{a logical value controlling whether we also add the explicit 'is' relationships
    defined via \code{\link[methods]{setIs}}.}
  \item{classes}{the names of the S4 classes of interest}
  \item{classDefs}{the definitions of the S4 classes of interest}
}
%\details{}
\value{
  A \code{data.frame} with two columns \code{to} and \code{from}
  giving the directed edges for the graph.
}
\references{
  The \code{methods} package.
}
\author{
Duncan Temple Lang (?)
}

%\seealso{}
\examples{
if(require(CodeDepends)) {
  g = mkClassGraph("package:CodeDepends")
  if(require(igraph))
     plot(as(g, "igraph"), cex.arrow.size = .3)
}
}
\keyword{programming}
\keyword{methods}
\concept{code analysis}
