\name{findLoops}
\alias{findLoops}
\alias{numNestedLoops}
\title{Find explicit loops in R code}
\description{
 This finds the explicit loops in R code, \code{for}, \code{while} and \code{repeat} loops.
}
\usage{
findLoops(f, nested = TRUE, code = parse(f),
          w = mkLoopWalker(nested, skipIfFalse = skipIfFalse),
          skipIfFalse = TRUE)
numNestedLoops(code, recursive = TRUE)
}
\arguments{
  \item{f}{either an R language object or
      the path to a file containing R code which will be parsed} % can this be a directory? No - not yet.
  \item{nested}{a logical value that controls whether we look for loops inside loops
    or ignore the code in the body of a loop.}
  \item{skipIfFalse}{a logical value. If \code{TRUE}, skip \code{if(FALSE){}} code, but do
      process any \code{else} clause. If \code{FALSE}, process the body of the \code{if(FALSE)} code.}  
  \item{code}{the R code, providing a way to pass code already parsed and not necessarily in a file.
    For \code{numNestedLoops}
  }
  \item{w}{the code walker that collects the loop nodes.}
  \item{recursive}{a logical value, currently ignored}
}
\details{
  This uses codetools::walkCode to traverse the  abstract syntax tree
  and captures nodes corresponding to these types of loops.
  One could add criteria to find specific types of loops, e.g.,
  with a loop variable named \code{i}, a symbol/name as the third element of the loop (i.e., what is
  being looped over) or a call to seq, seq_len, etc., 
  or a \code{while} condition  that is \code{TRUE}.
  One can also post-process the results.
}
\value{
 A list of the language objects corresponding to the loop objects found in the R code.
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link[codetools]{walkCode}}
  \code{\link[indexWalkCode]{indexWalkCode}}
}
\examples{
  fns = getFunctionDefs(getNamespace("tools"))
  loops = lapply(fns, function(x) findLoops(code = x))
  nl = sapply(loops, length)
  table(nl)
  # These include nested loops.
  which.max(nl)


  # Let's look at all the loops and the number of calls in each
  loops2 = unlist(loops, recursive = FALSE)
  length(loops2)
  summary(sapply(loops2, numCalls))

  # number of nested loops.
  nn = sapply(loops2, numNestedLoops)
}
\keyword{programming}
\concept{metaprogramming}

