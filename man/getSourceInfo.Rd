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
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{findCallsTo}}
}
\examples{
f = system.file("sampleCode/source.R", package = "CodeAnalysis")
getSourceInfo(f)
}
\concept{meta-programming}
\concept{static code analysis}
