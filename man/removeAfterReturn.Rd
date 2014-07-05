\name{removeAfterReturn}
\alias{removeAfterReturn}
\title{Remove any code after a top-level \code{return} call}
\description{
  This function removes dead code that cannot possible be reached by the
  interpreter because it comes after an explicit call to \code{return}.

  One of the purposes of this function is to merely clean and simplify
  code.  We use it to remove extraneous code before compiling R code
  in the \code{RLLVMCompile} package.
}
\usage{
removeAfterReturn(fun, recurse = TRUE)
}
\arguments{
  \item{fun}{the function whose code we analyze and clean, or an
  expression or call, i.e. a \code{language} object.}
  \item{recurse}{a logical value that controls whether we recursively
  process each expression or just process the top-level expressions in \code{fun}}
}
\value{
  The modified version of the function with any extraneous code in the
  body removed.
}
\author{Duncan Temple Lang}

\seealso{
\code{\link{removeConstIf}}
}
\examples{
 f =
 function(x) {
     print(x)
     return(x + 1)

     length(x)
 }
}
\keyword{programming}
