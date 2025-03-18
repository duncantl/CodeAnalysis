\name{getRFiles}
\alias{getRFiles}
\title{
  List all R Files in a Directory
}
\description{
  This is a very simple wrapper for \code{\link[base]{list.files}}
  to get all the R files in a directory and, by default,
  recursively for sub-directories.
  This simply sets the defaults for a call to \code{\link[base]{list.files}}
  for R files.
}
\usage{
getRFiles(dir, pattern = "\\\\.[RrSsQq]$", recursive = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{the path to the directory to search for R files}
  \item{pattern}{the pattern to identify the files of interest}
  \item{recursive}{a scalar logical controlling whether to
   process sub-directories and their sub-directories, etc.}
}
\value{
A character vector
}
\author{
Duncan Temple Lang

}
\seealso{
  \code{\link[base]{list.files}}
}
%\examples{}
\keyword{file}

