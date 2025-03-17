\name{toc}
\alias{toc}
\alias{dynToc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Generate Table-of-Contents for a Collection of R code, either
  via Static or Dynamic Analysis.
}
\description{

  \code{toc} statically reads all the function definitions and returns
  a \code{data.frame} with the names of the functions and the file each is in.
}
\usage{
toc(dir = ".", files = getRFiles(dir, pattern = pattern), pattern = "\\\\.[RrsSq]$")    
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{the directory in which to find the \code{R} files.}
  \item{files}{a character vector of the paths to the \code{R} files.}
  \item{pattern}{the pattern for finding the \code{R} files
    that is passed to \code{\link[base]{list.files}}.}
}
\value{
  A \code{data.frame}.


}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{getFunctionDefs}}
}
\examples{
d = system.file("sampleCode", package = "CodeAnalysis")
df = toc(d)
head(df)
}
\keyword{programming}
\concept{meta-programming}
