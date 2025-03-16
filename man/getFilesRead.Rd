\name{getFilesRead}
\alias{getFilesRead}
\title{Find which files are read in code}
\description{
  This finds the names of the files that are read by R functions
  such as \code{read.table}, \code{read.csv}, \code{scan}, etc.
}
\usage{
getFilesRead(fun, ..., readFileFuns = ReadFileFuns, recursive = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{R code}
  \item{\dots}{additional arguments passed to \code{isReadFileCall}}
  \item{readFileFuns}{a character vector of function names identifying functions that are known to
    read data.}
  \item{recursive}{a logical value, currently ignored.}
}
\value{
  \code{rstatic} nodes giving the names of the "files" that were found in
  the code.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{findPkgReadFuns} (unexported in \code{CodeAnalysis}) for
  finding functions that read files.
}
\examples{
f = system.file("samples", "results-multi.R", package = "CodeDepends")
getFilesRead(f)
}
\keyword{IO}
\concept{static code analysis}
