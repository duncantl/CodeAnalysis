\name{findCallsToFunctions}
\alias{findCallsTo}
\alias{findCallsToFunctions}
\alias{findAssignsTo}
\alias{isCallTo}
\title{Find all calls to given functions or all assignments to specific variables}
\description{
  These analyze R code and find all calls to functions in the given
  set of interest and either returns these or the specified argument(s),
  either by position or name.

  For \code{findCallsToFunctions}, if argIndices has length zero, the calls are returned.
  Otherwise, the corresponding parameters from each call are returned.

  \code{findCallsTo} uses the regular R AST, not \code{rstatic}'s
  and returns the R language objects for the call.

  \code{findAssignsTo} identifies assignments in code with
  the left-hand-side being one of a specified set of possible variables
  of interest.
}
\usage{
findCallsTo(code, funNames = character(), walker = mkCallWalker(funNames)) 
findCallsToFunctions(allCalls, funNames, argIndices = integer(), definitions = NULL)
findAssignsTo(code, var, index = FALSE, recursive = TRUE, ...) 
isCallTo(code, funName)
}
\arguments{
  \item{code}{the R code to analyze}
  \item{funNames,var,funName}{the names of functions or variables of interest}  
  \item{walker}{the \code{codetools}-style functions that traverse the AST to find the relevant calls.}
  \item{allCalls}{a character vector of file or directory names or
    parsed R code or list of calls}
  \item{index}{a logical value controlling whether to return the assignments as indices
      in \code{code} or as language objects.}
  \item{argIndices}{an integer or character vector identifying the
    parameters of interest in each of the calls}
  \item{recursive}{a logical value controlling whether to descend
   into the body of for loops, if statements, etc. or process only top-level assignments.}
  \item{definitions}{a list of functions that provides the definitions
    for functions that are not available on the search path. These are
    used in \code{\link[base]{match.call}}.}
  \item{\dots}{additional arguments passed to methods.}
}
\details{
}
\value{
  If \code{argIndices} has length zero, a list of all the matching
  \code{Call} objects.
  Otherwise, a list of all the parameters in each matching call
  identified by \code{argIndices} either by position or name.
}
\author{Duncan Temple Lang}

%\seealso{}
\examples{
}
\concept{meta-programming}
\concept{static analysis}
