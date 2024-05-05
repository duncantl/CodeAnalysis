\name{getDepends}
\alias{getDepends}
\title{
Get dependent input, output and data files for an R script.
}
\description{
  This function takes an R script, and identifies the files which are
  read (input and/or data) and written (output).
}
\usage{
getDepends(file, info = as(readScript(file), "ScriptInfo"),
           fileFunctionNames = FileFunctionNames(),
           prev = info, loadPackages = TRUE, addExpressionNum = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
An *.R file to analyze.
}
  \item{info}{
An object of class \code{ScriptInfo}, created by \code{readScript()}
containing the script information.
}
  \item{fileFunctionNames}{
The names of functions which can read or write files. Default functions
are provided by \code{FileFunctionNames()}
}
\item{prev}{the previous \code{ScriptNodeInfo} from a \code{ScriptInfo} list
    for the node/block just before \code{info}, or \code{NULL}.}
  \item{loadPackages}{
logical, should the packages required by the script be loaded? 
}
  \item{addExpressionNum}{
	logical, should the expressions be numbered.
}
}
\value{
  Returns a \code{data.frame} consisting of,
  \item{filename}{The name of the file which is being read/loaded or
	written/saved}
  \item{operation}{The operation being conducted on that file name}
  \item{expressionNum}{optional, if \code{addExpressionNum = TRUE}, adds
	the expression number}
}
%\references{}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{getFilesRead}}
  \code{\link[CodeDepends]{getVariableDepends}}
}

%\examples{}
\concept{static code analysis}
\keyword{IO}
\keyword{programming}

