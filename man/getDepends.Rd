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
getDepends(file, info = as(readScript(file), "ScriptInfo"), fileFunctionNames = FileFunctionNames(), prev = info, loadPackages = TRUE, addExpressionNum = TRUE)
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
  \item{prev}{
	
}
  \item{loadPackages}{
logical, should the packages required by the script be loaded? 
}
  \item{addExpressionNum}{
	logical, should the expressions be numbered.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a \code{data.frame} consisting of,
  \item{filename}{The name of the file which is being read/loaded or
	written/saved}
  \item{operation}{The operation being conducted on that file name}
  \item{expressionNum}{optional, if \code{addExpressionNum = TRUE}, adds
	the expression number}
}
\references{
}
\author{
Duncan Temple Lang
}
\note{
}


\seealso{
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file, info = as(readScript(file), "ScriptInfo"), fileFunctionNames = FileFunctionNames(), 
    prev = info, loadPackages = TRUE, addExpressionNum = TRUE) 
{
    tmp = vector("list", length(info))
    for (i in seq(along = info)) tmp[[i]] = getDepend(info[[i]], 
        fileFunctionNames = fileFunctionNames, prev = if (missing(prev)) 
            info[seq_len(i - 1L)]
        else prev, loadPackages = loadPackages)
    i = !sapply(tmp, is.null)
    if (!any(i)) {
        tmp = data.frame(filename = character(), operation = character(), 
            stringsAsFactors = FALSE)
        if (addExpressionNum) 
            tmp$expressionNum = integer()
        return(tmp)
    }
    ans = as.data.frame(do.call(rbind, tmp[i]), stringsAsFactors = FALSE)
    if (addExpressionNum) 
        ans$expressionNum = rep(which(i), sapply(tmp[i], function(x) if (is.data.frame(x)) nrow(x) else 1))
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
