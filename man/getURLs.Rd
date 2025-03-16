\name{getURLs}
\alias{getURLs}
\title{Find literal URLs in R code}
\description{
  This function statically analyzes R code and finds all literal
  strings that appear to be URLs.
}
\usage{
getURLs(dir, ignoreParams = FALSE, skipIfFalse = FALSE)
}
\arguments{
  \item{dir}{a directory or file name, or vector of these}
  \item{ignoreParams}{a scalar logical controlling whether to also
    include the default values of parameters of functions in the search for
    URLs.}
  \item{skipIfFalse}{scalar logical controlling whether to skip/ignore code within
    \code{if(FALSE)} blocks.}
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
