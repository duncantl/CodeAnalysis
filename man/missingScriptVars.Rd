\name{missingScriptVars}
\alias{missingScriptVars}
\title{
Find Definitions for Variables in Scripts  
}
\description{
  For a given script, this function identifies
  variables used but not previously defined in the script
  and then searches other scripts to find code
  that creates those variables.
}
\usage{
missingScriptVars(script, dir = ".",
                  allScripts = list.files(dir, pattern = "\\.R$", full.names = TRUE),
                  varDefs = varDefsByFile(, allScripts),
                  unbox = TRUE)
}
\arguments{
  \item{script}{the paths to the file names containing the R code to be analyzed}
  \item{dir}{the directory in which to find the other scripts}
  \item{allScripts}{the full paths to all the scripts in which to look for
  variable definitions}
\item{varDefs}{the pre-computed collection of variable definitions across all scripts.}
\item{unbox}{a scalar logical that controls whether
     to keep the list structure for the return (\code{FALSE})
    or return its first element corresponding to the single script.}
}
\value{

  A list with the same length as \code{script} with an element corresponding to each element of
  \code{script}.
  Each element is a named list of character vectors.
  The names identify the variables found undefined but used in the \code{script}.
  The character vector gives the names of the files containing one or more definition for that variable.

  If there is only one element in \code{script} and \code{unbox} is \code{TRUE}, that first element is returned directly,
  i.e., not a list of length 1 containing the element.
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link{getScriptGlobals}}
}
%\examples{}
\keyword{progamming}
\concept{meta-programming}
\concept{static code analysis}
