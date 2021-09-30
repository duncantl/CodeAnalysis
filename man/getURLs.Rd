\name{getURLs}
\alias{getURLs}
\title{Find literal URLs in R code}
\description{
  This function statically analyzes R code and finds all literal
  strings that appear to be URLs.
}
\usage{
getURLs(dir)
}
\arguments{
  \item{dir}{a directory or file name, or vector of these}
}
\value{
a character vector
}

\author{
Duncan Temple Lang
}
\seealso{
\code{\link{getFunctionDefs}}
}
%\examples{}
