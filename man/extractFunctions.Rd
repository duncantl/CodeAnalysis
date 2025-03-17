\name{extractFunctions}
\alias{extractFunctions}
\title{
Extract and Remove Nested Functions
}
\description{
  This function extracts nested named functions (not anonymous functions)
  from a function
  and returns both these nested functions and the modified function with
  the functions removed.

  This deals with nested functions but does not indicate where
  the extracted functions occured.

  The purpose of this is to facilitate refactoring code
  and moving nested functions to separate functions
  that can be reused and tested separately from the original function.

  \code{\link{mkGlobalsLocal}} can also be used to
  update both the extracted functions and the original function
  to fix calls to include the additional non-local arguments.
}
\usage{
extractFunctions(fun)
}
\arguments{
  \item{fun}{the function to be processed}
}
\value{
  A list with 2 elements

  \item{newFun}{the modified version of the original function}
  \item{nested}{a named list of the nested functions}
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{mkGlobalsLocal}}
}

% \examples{}
\keyword{programming}
\concept{meta-programming}

