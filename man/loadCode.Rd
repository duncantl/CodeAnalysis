\name{loadCode}
\alias{loadCode}
\title{Load R code from a directory}
\description{
  source() code from a directory. This actually evaluates the code.
  Often we want to analyze it statically, i.e., without running the
  code.
  This function is useful if the code is mostly function definitions.
  But for that, we can now use \code{getFunctionDefs} in \code{CodeDepends}.
}
\usage{
loadCode(dir, env = new.env(), files = list.files(dir, pattern = "\\\\.[RrsSqQ]$", full.names = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{the directory in which to find the R code files}
  \item{env}{the environment into which to \code{\link[base]{source}} the code.}
  \item{files}{the full paths to the files whose code we read.
  This allows the caller to provide a subset of the files and also to
  control the order in which the files are \code{\link[base]{source}}'d.}
}
\details{
This sources the code into the given environment.
}
\value{
  This returns the environment \code{env}
 which will contain the variables defined in the sour
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link[CodeDepends]{getFunctionDefs}}
}
%\examples{}
\keyword{programming}
