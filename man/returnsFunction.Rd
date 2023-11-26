%XXX Flesh out
\name{returnsFunction}
\alias{returnsFunction}
\title{Checks whether a function returns a function?}
\description{
  This function statically analyzes the code in a function
  to see if any of the possible return values 
  could be a function.
}
\usage{
returnsFunction(fun, recursive = FALSE, envir = globalenv(),
                 functionsReturningFunctions = character(), any = FALSE)
}
\arguments{
  \item{fun}{function object}
  \item{recursive}{a logical value controlling whether we examine
    a return value that is a call to another function
    to see if that function returns a function.
  }
  \item{envir}{environment in which we look for functions identified by name} % fix
  \item{functionsReturningFunctions}{a character vector of the names
    of functions known to return a function object.
  }
  \item{any}{currently ignored}
}
\value{
  A list of the language objects that identify the return values
  that are functions.
}
\author{
Duncan Temple Lang
}
\seealso{}
\examples{
f3 = function(x) {
    if(length(x) > 3) {
        y = x[-(1:3)]
        function(z)
            prod(z + y)
    } else
        sum
}
# returns anonymous function and sum
returnsFunction(f3)


f3.5 = function(x) {
    sum = 2
    if(length(x) > 3) {
        y = x[-(1:3)]
        function(z)
            prod(z + y)
    } else
        sum
}
# returns only the anonymous function since sum
# is defined locally and not a function
returnsFunction(f3.5)


f2 = function(x)
    Vectorize(bar)
returnsFunction(f2, functionsReturningFunctions = "Vectorize")
}
\keyword{programming}
\concept{metaprogramming}
