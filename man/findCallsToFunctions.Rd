\name{findCallsToFunctions}
\alias{findCallsTo}
\alias{findCallsToFunctions}
\alias{findAssignsTo}
\alias{findAssignsTo2}
\alias{findAssignsTo,function-method}
\alias{findAssignsTo,\{-method}
\alias{findAssignsTo,if-method}
\alias{findAssignsTo,list-method}
\alias{findAssignsTo,<--method}
\alias{findAssignsTo,=-method}
\alias{findAssignsTo,call-method}
\alias{findAssignsTo,name-method}
\alias{isCallTo}
\alias{numCalls}
\alias{mkCallWalkerPred}
\title{Find all calls to given functions or all assignments to specific variables}
\description{
  These analyze R code and find all calls to functions in the given
  set of interest and either returns these or the specified argument(s),
  either by position or name.

  For \code{findCallsToFunctions}, if argIndices has length zero, the calls are returned.
  Otherwise, the corresponding parameters from each call are returned.

  \code{findCallsTo} uses the regular R AST, not \code{rstatic}'s
  and returns the R language objects for the call.

  \code{findAssignsTo} and \code{findAssignsTo2} identifies assignments in code with
  the left-hand-side being one of a specified set of possible variables
  of interest.  \code{findAssignsTo2} may be more accurate/comprehensive.

  \code{numCalls} is a simple derivative of \code{findCallsTo}
  and computes the number of calls in a function. This gives
  a sense of the complexity of the code.
}
\usage{
findCallsTo(code, funNames = character(), walker = mkCallWalker(funNames),
             parse = any(!sapply(funNames, is.name))) 
findCallsToFunctions(allCalls, funNames, argIndices = integer(), definitions = NULL)
findAssignsTo(code, var, index = FALSE, recursive = TRUE, ...)
findAssignsTo2(code, var)
isCallTo(code, funName)
mkCallWalkerPred(pred, ...)
numCalls(code)
}
\arguments{
  \item{code}{the R code to analyze}
  \item{funNames,var,funName}{the names of functions or variables of interest. This can be a
    character vector of names or of R calls/expressions, or a list of previously parsed
    call language objects.
   }  
  \item{walker}{the \code{codetools}-style functions that traverse the AST to find the relevant calls.}
  \item{parse}{whether to parse the \code{funNames} in \code{findCallsTo} and treat them as language
     objects.}
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
  \item{pred}{a function that takes two arguments - a call and a logical value indicating if the
     function being called is a symbol/name or itself a call - and returns a logical scalar value indicating
     whether to collect and return the specific call. The }
   \item{\dots}{additional arguments passed to the \code{pred} function.}
   % discuss this wrt to indexWalkCode and mkIsCallTo().  Could we pass the vector of functions
   % we want to match via the ...
   % mkCallWalkerPred(isCallTo, "foo")
}
\value{

  \code{findCallsTo} returns a list of the call language objects.

  \code{mkCallWalkerPred} returns a list with 3 elements which are functions:
  \item{leaf}{walks the different elements of the AST}
  \item{call}{processes a call language object and determines whether to add it to the results}
  \item{handler}{returns NULL}

 \code{isCallTo} returns a scalar logical value - \code{TRUE} or \code{FALSE}.

 \code{findAssignsTo2} seems to be more correct/accurate than \code{findAssignsTo}
  and considerably less code.
 
  If \code{argIndices} has length zero, a list of all the matching
  \code{Call} objects.
  Otherwise, a list of all the parameters in each matching call
  identified by \code{argIndices} either by position or name.
}
\author{Duncan Temple Lang}

%\seealso{}
\examples{
  egFun = findCallsTo

  findCallsTo(egFun, c("lapply", "walkCode"))

  findCallsTo(getNamespace("parallel"), "eval")

  isLApplyWalkCodeCall = function(x, isName, ...) isSymbol(x[[1]], "lapply") && isSymbol(x[[3]], "walkCode")
  findCallsTo(egFun, walker = mkCallWalkerPred(isLApplyWalkCodeCall))

  isLApplyWalkCodeCall = function(x, isName, ...) isCallTo(x, "lapply") && isSymbol(x[[3]], "walkCode")
  findCallsTo(egFun, walker = mkCallWalkerPred(isLApplyWalkCodeCall))

   # findAssignsTo()

  findAssignsTo(egFun, "ans")

  findAssignsTo(egFun, "isEnv") # not correct.

  findCallsTo(egFun, walker = mkCallWalkerPred(function(x, ...) isCallTo(x, c("=", "<-")) && isSymbol(x[[2]], "isEnv")))


  findAssignsTo2(egFun, "code")

  findAssignsTo2(egFun, c("code", "code3"))
}
\concept{meta-programming}
\concept{static analysis}
