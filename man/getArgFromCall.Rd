\name{getArgFromCall}
\alias{getArgFromCall}
\alias{isFunAssign}
\alias{pkgSymbolsUsed}
\title{Accessor Functions}
\description{
  These are higher-level accessor functions for working
  with language objects and meta-programming
  so that we can talk about high-level operations
  and concepts and not the specific representation
  of R language objects.
}
\usage{
getArgFromCall(call, arg, asCharacter = TRUE)
isFunAssign(x, toSymbol = TRUE) 
pkgSymbolsUsed(pkg, fromPkg, ns = getNamespace(pkg), exportedOnly = TRUE) 
}
\arguments{
  \item{call,x}{the R call language object}
  \item{arg}{the name of the argument to extract from the call}
  \item{asCharacter}{a logical value controlling whether to deparse
    the result or leave it is a language object}
  \item{pkg}{the name of the package which is calling functions from another package}
  \item{fromPkg}{the name of the other package whose functions are being called}
  \item{ns}{the namespace's environment}
  \item{exportedOnly}{a logical value.  If \code{TRUE}, only the exported symbols are considered,
    otherwise all the symbols in the namespace are processed.}
  \item{toSymbol}{a logical value that if \code{TRUE} causes \code{isFunAssign}
      not look at the value of the symbol to which the value is being assigned.}
}
\value{
 a character vector/string or a language object.
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
}
\keyword{programming}
\concept{metaprogramming}

