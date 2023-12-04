\name{getSourceInfo}
\alias{getSourceInfo}
\title{Which R files source other files}
\description{
  This function and its methods construct the
  graph matrix identifying which other R files each R file
  \code{\link[base]{source}}s.
  This is useful when analyzing the scripts in a project
  to determine the relationships between them.
}
\usage{
getSourceInfo(x, recursive = TRUE, ...)
}
\arguments{
  \item{x}{the R code to process. This cane be the name of a file, the parsed code,
    an expression.}
  \item{recursive}{a logical value. If \code{TRUE}, recursively process the files being sourced
   to see what files they source.}
  \item{\dots}{additional arguments passed to methods. Currently not used.}
}
\value{
  a 2-column data.frame with each row containing
  the name of the R file that sources the file
  given in the second column.

  If \code{recursive} is \code{TRUE}, \code{getSourceInfo}
  each file will have \code{<n>} rows in the result
  where \code{<n>} is the number of files that the file \code{source()}s.
  This will be 0 if the file A does not \code{source()} any other files.

  If a file sourced by one file doesn't exist, we don't know
  if it \code{source()}s any other files and so add NA.
  This allows the caller to see all files that are intended to be \code{source()}d
  and then check which exist (rather than omitting these files from the result.)
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{findCallsTo}}
}
\examples{
f = system.file("sampleCode/source.R", package = "CodeAnalysis")
a = getSourceInfo(f, FALSE)
a

b = getSourceInfo(f)
b
}
\concept{meta-programming}
\concept{static code analysis}
