\name{findUnusedArgs}
\alias{findUnusedArgs}
\alias{findUnusedAssignments}
\title{Find unused parameters and local variables}
\description{
  These functions analyze an R function and identify
  parameters or local variables that are not used in the code.

  It is useful to remove any dead code with
  \code{\link{removeConstIf}} and \code{\link{removeAfterReturn}}.
  This is done by default.
}
\usage{
findUnusedArgs(fun, remove = TRUE, clean = TRUE)
findUnusedAssignments(fun, remove = TRUE, clean = TRUE)
}
\arguments{
  \item{fun}{the function to analyze}
  \item{clean}{a logical value controlling whether we remove dead code
     before processing the function with  \code{\link{removeConstIf}}
     and \code{\link{removeAfterReturn}}}
   \item{remove}{a logical value controlling whether the code eliminates
   the dead code and returns the modified code.}
}
\value{
 A character vector giving the names of the parameters or local
 variables that are not used.
}
\author{Duncan Temple Lang}


\seealso{
    \code{\link{removeConstIf}}, \code{\link{removeAfterReturn}}
}
\examples{
f =
function(a, b, u)
{
  z = a + b
  ans = g(z)
}

findUnusedArgs(f)

f =
function()
{
  a = 1
  b = 2
  c = 3
  d = 10
  a + c
}

findUnusedAssignments(f)
}

\keyword{programming}
