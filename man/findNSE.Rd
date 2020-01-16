\name{findNSE}
\alias{findNSE}
\title{Find Non-standard evaluation via static code analysis}
\description{
  This analyzes the R code to find instances of non-standard evaluation,
  e.g. uses of \code{eval}, \code{evalq}, \code{eval.parent}, \code{get}, \code{assign}, \code{rm}.
}
\usage{
findNSE(code, asNodes = TRUE)
}
\arguments{
  \item{code}{the R code object}
  \item{asNodes}{a logical value controlling whether to return the
    resulting NSE code objects as nodes or indices.
  }
}
\value{
  the nodes in the \code{rstatic} abstract syntax tree
  that are identified as NSE.
}
%\references{}
\author{Duncan Temple Lang}
\seealso{
\code{\link[rstatic]{find_nodes}}
}
\examples{
e = parse(text = "if(cleanup) rm(x, y)")
findNSE(e)

findNSE(body(lm))
}
\keyword{programming}
\concept{static code analysis}

