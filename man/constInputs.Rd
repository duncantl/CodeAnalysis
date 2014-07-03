\name{constInputs}
\alias{constInputs}
\title{Determine which parameters are not modified in a function}
\description{
  This function determines which functions are "constant" in
  a function, i.e. read-only.
  These are the formal arguments/parameters that are not changed
  by code in the body of this function.
  The purpose of knowing this is that we can, in theory,
  avoid copying the value of the argument before calling
  this function, but still know that the content will not change.
  This is useful when compiling code.

  This currently does not work recursively for calls to other functions.
  This will be added later.
}
\usage{
constInputs(f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{the function object whose parameters and body we process}
}
\value{
  A character vector giving the names of the parameters that are not
  modified in the body of the function.
  THis might be empty.
}
\author{Duncan Temple Lang}

%\seealso{}
\examples{
f =
function(x)
{
  y = numeric(length(n))
  for(i in seq(along = x))
    y[i] = x[i]*2
}

constInputs(f)  # "x"

g =
function(x)
{
  for(i in seq(along = x))
    x[i] = x[i]*2
}

constInputs(g)
}
\keyword{programming}
