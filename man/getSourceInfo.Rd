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
getSourceInfo(x, ...)
}
\arguments{
  \item{x}{the R code to process. This cane be the name of a file, the parsed code,
    an expression.}
  \item{\dots}{additional arguments passed to methods. Currently not used.}
}
\value{
  a 2-column matrix with each row containing
  the name of the R file that sources the file
  given in the second column
}
\author{
Duncan Temple Lang
}
\seealso{}
\examples{}
\concept{meta-programming}
\concept{static code analysis}
