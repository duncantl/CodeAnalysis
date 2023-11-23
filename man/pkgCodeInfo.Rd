\name{pkgCodeInfo}
\alias{pkgCodeInfo}
\alias{fileCodeInfo}
\title{Information about code in an R file or directory of R files}
\description{
  These functions analyze the code in an R file or a collection of R files
  in a directory (not necessarily a formal package)
  and returns a description summarizing the top-level objects in the code,
  i.e., functions, methods, variables.

  It ignores code in \code{if(FALSE)} expressions.
}
\usage{
pkgCodeInfo(dir, rfiles = list.files(dir, pattern = "\\\\.[RrSs]$", full.names = TRUE))
fileCodeInfo(file, code = parse(file), addClass = TRUE)
}
\arguments{
  \item{dir}{the path to a directory containing the R code, or a file}
  \item{rfiles}{the full paths to the files to process, useful if one only wants to process a subset
    of the files in the directory or combine files from multiple directories.}
  \item{file}{the full path to the R file containing the code to process}
  \item{code}{the R code to analyze}
  \item{addClass}{a logical value indicating whether to add the \code{FileCodeInfo} class to the
    resulting data.frame.}
}
\value{
  A data.frame with 6 elements:
  \item{name}{the name of the top-level object}
  \item{type}{the R type}
  \item{op}{the top-level operation, often '=' or '<-', but also
    calls to Vectorize, setAs, setGeneric, ...}
  \item{expr}{a list of the R code/language objects}
  \item{file}{the name of the file in which the expression is located}
  \item{index}{the index of that top-level expression in that file.}
}

\author{
Duncan Temple Lang
}

%\seealso{}
\examples{
  dir = system.file("sampleCode", package = "CodeAnalysis")
  info = pkgCodeInfo(dir)
  dim(info)
  table(info$op)
  table(info$file)
}
\keyword{programming}
\concept{metaprogramming}
