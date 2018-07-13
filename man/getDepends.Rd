\name{getDepends}
\alias{getDepends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getDepends(file, info = as(readScript(file), "ScriptInfo"), fileFunctionNames = FileFunctionNames(), prev = info, loadPackages = TRUE, addExpressionNum = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{fileFunctionNames}{
%%     ~~Describe \code{fileFunctionNames} here~~
}
  \item{prev}{
%%     ~~Describe \code{prev} here~~
}
  \item{loadPackages}{
%%     ~~Describe \code{loadPackages} here~~
}
  \item{addExpressionNum}{
%%     ~~Describe \code{addExpressionNum} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
