\name{findUsedOptions}
\alias{findUsedOptions}
\title{Find options set or read in R code}
\description{
  This analyzes R code to find any reference to an R option, e.g.,
  \code{options()$prompt}, \code{getOption("prompt", "> ")}, etc.
}
\usage{
findUsedOptions(code, asNodes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{code}{the R code}
  \item{asNodes}{a logical value which if \code{TRUE} returns the
  \code{rstatic} nodes from the abstract sytax tree, i.e., the language
  objects.
  If \code{FALSE}, the function gets the names of the options.}
}
\value{
  If \code{asNodes} is \code{FALSE}, a character vector.
  
  If \code{asNodes} is \code{TRUE}, a list of \code{rstatic}
  language objects.
}
\author{
  Duncan Temple Lang
}
\seealso{
  \code{\link[rstatic]{find_nodes}}
}
\examples{
findUsedOptions(body(dev.new))
findUsedOptions(body(dev.new), TRUE)

# Shows can handle options in the default values of a function parameter.
f = function(x, ext = getOption("MyOpt", FALSE)) {
  if(ext)
    readLines(x)
  x
}
findUsedOptions(f)
}
\keyword{programming}
\concept{static code analysis}
