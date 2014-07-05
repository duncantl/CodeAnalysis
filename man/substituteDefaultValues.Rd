\name{substituteDefaultValues}
\alias{substituteDefaultValues}
\title{Insert code for default values into the body of the code}
\description{
  This function creates a modified version of the specified function
  by adding code to compute the default value of each parameter
  just before it is first used.
  This facilitates compiling code and code analysis to determine
  global variables correctly. It honors lazy evaluation by not
  evaluating the code until the variable is referenced.
}
\usage{
substituteDefaultValues(f, removeDefaults = TRUE,
                        sc = new("Script", as.list(body(f))[-1]), info = getInputs(sc))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{the function to process}
  \item{removeDefaults}{a logical value that controls whether we remove
    the original default value expressions in the \code{formals} of the
    returned function.}
  \item{sc}{a \code{Script} object created via the \code{CodeDepends}
    package.
   The caller does not need to specify this, but can if they have
   computed the \code{Script} object already for the code in the body of
 the function.}
  \item{info}{like \code{sc}, this is input computed via the
    \code{CodeDepends} package from the \code{Script} object and which
    contains the input/output information for each expression in the
    body of the code.}
}
\value{
  A function. This is  the modified version of the original function.
}

\author{Duncan Temple Lang}
\note{
  It is possible that the semantics of the new function
  may differ from the original version in the case
  that the order of evaluation of the arguments
  may be different.
  This might occur in the case that two arguments
  are missing and we evaluate their default values
  in a specific order that is different from how
  they are evaluated in the actual call to another function.
  For example, in \code{foo(x, y)}, with \code{x} and \code{y}
  missing in the calling function, we would first evaluate \code{x} and
  then \code{y} via their default via expressions.
  However, if \code{foo(x, y)} actually uses \code{y} first, then
  our order would be incorrect.
}


\seealso{
\code{\link{getGlobals}}
}
\examples{
g =
function(a = x, b = y)
{
   x = 1
   y = 2
   a + b * (x + y)
}

substituteDefaultValues(g)
}
\keyword{programming}
\concept{code rewriting}
