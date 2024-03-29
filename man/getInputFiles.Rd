\name{getInputFiles}
\alias{getInputFiles}
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
getInputFiles(x, lang = TRUE, ..., .funNames = getReadDataFuns(...))
getOutputFiles(x, lang = TRUE, ..., .funNames = getOutputDataFuns(...))
getGraphicsOutputFiles(x, lang = TRUE, ..., .funNames = getGraphicsDevFuns(...))
}
\arguments{
  \item{x}{a character vector of R file or directory names, an expression
    from parsing an R script, the output from \code{\link{getAllCalls}} or generally any R language
    objects. }
  \item{lang}{a logical value. If \code{FALSE},  only literal arguments are returned, basically, literal
    strings representing file names (but possibly also logical, integer or numeric values);
    if \code{TRUE}, expressions/language objects that are the arguments.}
  \item{\dots}{a collection of \code{function = parameter-name(s)} pairs
    specifying additional functions and their parameters that identify where a function
    is writing to a file/connection.  }
  \item{.funNames}{the list of function-parameter(s) pairs identifying the functions
    writing to a file/connection.}
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
  getInputFiles(f)
  # includes a call to sprintf()

  getInputFiles(f, lang = FALSE)
  # NA in place of the sprintf()


\dontrun{
    # Project from git@github.com:mespe/Variety_trial_analysis.git
  ff = list.files("~/Book/ExploreCode/Variety_trial_analysis/code", pattern = "\\.R$", full = TRUE)
  inp = lapply(ff, getInputFiles, FALSE)
  names(inp) = basename(ff)

  out = lapply(ff, getOutputFiles, FALSE)
  names(out) = basename(ff)

  grout = lapply(ff, getGraphicsOutputFiles, FALSE)
  names(grout) = basename(ff)
}
}
\keyword{IO}
\concept{meta-programming}
\concept{static analysis}

