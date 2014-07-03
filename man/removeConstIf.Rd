\name{removeConstIf}
\alias{removeConstIf}
\title{Eliminate dead if code}
\description{
  This function identifies and eliminates
  parts of if-else expressions that are known
  at analysis time to be unreachable.
  These are of the form \code{if(FALSE) \{...\} }.
  This is typically used to "comment" out code.
  We want to eliminate it both to simplify and clarify the code
  and also reduce the code we would compile (see \code{RLLVMCompile}.
}
\usage{
removeConstIf(expr)
}
\arguments{
  \item{expr}{the R function or expression to process}
}
\value{
 The potentially modified expression/function with any dead if
 statements eliminated.
}
\author{
Duncan Temple Lang
}
%\seealso{}
\examples{

f = function()
{
  if(length(x) > 10)
      print(x)
  else if(length(x) > 5 && FALSE)
      show(x)
  else
      cat("x is small\n")
}

removeConstIf(f)
}
\keyword{progamming}
