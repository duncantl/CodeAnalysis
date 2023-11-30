\name{getInputFiles}
\alias{getInputFiles}
\alias{getInputFiles,ScriptInfo-method}
\alias{getInputFiles,ScriptNodeInfo-method}
\alias{getInputFiles,character-method}
\alias{getOutputFiles}
\alias{getGraphicsOutputFiles}
\title{Find the file names that are read or written in R code}
\description{
  These functions analyze R code to find the names of the files
  in R calls that read or write data or plots.
  The functions find all calls to functions known to read or write
  data or create file-based graphics output.
  One can identify the names of functions to be considered
  and also use \code{\link{findReadDataFuns}},
  \code{\link{findWriteDataFuns}},
  \code{\link{findGraphicsDevFuns}}
  to analyze other functions to see if they call these primitive
  functions  and so include those functions as targets.
}
\usage{
getInputFiles(x, ...)
getOutputFiles(x, ...)
getGraphicsOutputFiles(x, ...)
}
\arguments{
  \item{x}{a character vector of R file or directory names, an expression
    from parsing an R script, the output from \code{\link{getAllCalls}}. }
  \item{\dots}{arguments for methods. These include funNames. }
}
\value{
  a character vector or list.
  Where the functions operate on literal file names,
  a string is returned. Where the call operates on a variable or call
  that is not known to be a constant, the (rstatic) language object is returned.
}
\author{Duncan Temple Lang}
\seealso{
\code{\link{findReadDataFuns}},
  \code{\link{findWriteDataFuns}},
  \code{\link{findGraphicsDevFuns}}
}
\examples{
  f = system.file("sampleCode/inputFiles.R", package = "CodeAnalysis")
  i = getInputFiles(f)
}
\keyword{IO}
\concept{meta-programming}
\concept{static analysis}

