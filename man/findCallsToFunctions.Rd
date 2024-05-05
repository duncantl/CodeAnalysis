\name{findCallsToFunctions}
\alias{findCallsTo}
\alias{findCallsToFunctions}
\alias{isCallTo}
\alias{numCalls}
\alias{mkCallWalkerPred}
\alias{getIndirectCallFunList}
\title{Find all calls to given functions or all assignments to specific variables}
\description{
  These analyze R code and find all calls to functions in the given
  set of interest and either returns these or the specified argument(s),
  either by position or name.

  For \code{findCallsToFunctions}, if argIndices has length zero, the calls are returned.
  Otherwise, the corresponding parameters from each call are returned.

  \code{findCallsTo} uses the regular R AST, not \code{rstatic}'s
  and returns the R language objects for the call.

  \code{numCalls} is a simple derivative of \code{findCallsTo}
  and computes the number of calls in a function. This gives
  a sense of the complexity of the code.
}
\usage{
findCallsTo(code, funNames = character(),
         indirectCallFuns = getIndirectCallFunList(),
         walker = if(!missing(rx))
                      mkCallWalkerPred(findCallsRXPred(rx), skipIfFalse = FALSE)
                  else
                      mkCallWalker(funNames, indirect = indirectCallFuns, skipIfFalse = skipIfFalse),
         parse = any(!sapply(funNames, is.name)), skipIfFalse = TRUE,
         rx = character()) 
findCallsToFunctions(allCalls, funNames, argIndices = integer(), definitions = NULL)
isCallTo(code, funName, indirect = getIndirectCallFunList(), isLHS = NA)
mkCallWalkerPred(pred, ..., skipIfFalse = TRUE)
numCalls(code)
getIndirectCallFunList(..., .els = list(...))
}
\arguments{
  \item{code}{the R code to analyze, or path to an R source file or directory containing R code}
  \item{funNames,funName}{the names of functions or variables of interest. This can be a
    character vector of names or of R calls/expressions, or a list of previously parsed
    call language objects.
  }
  \item{rx}{a regular expression used to identify functions of interest, rather than actual names}
  \item{indirect,indirectCallFuns}{a named character vector or a logical value.
  If \code{TRUE}, we use the value from a call to \code{getIndirectCallFunList()}.
  If \code{FALSE}, we use the empty vector and so do not consider indirect calls.
  For a character vector value, the names identify the
    function of interest and the corresponding value identifies the
    name of the parameter of that function corresponding to the function argument,
    e.g., \code{c(do.call = "what", lapply = "FUN")}.
    The default provides the information from functions in base.    
  }
  \item{walker}{the \code{codetools}-style functions that traverse the AST to find the relevant calls.}
  \item{parse}{whether to parse the \code{funNames} in \code{findCallsTo} and treat them as language
    objects.}
  \item{skipIfFalse}{a logical value. If \code{TRUE}, skip \code{if(FALSE){}} code, but do
      process any \code{else} clause. If \code{FALSE}, process the body of the \code{if(FALSE)} code.}
  \item{allCalls}{a character vector of file or directory names or
    parsed R code or list of calls}
% \item{index}{a logical value controlling whether to return the assignments as indices
%      in \code{code} or as language objects.}
  \item{argIndices}{an integer or character vector identifying the
    parameters of interest in each of the calls}
%  \item{recursive}{a logical value controlling whether to descend
%   into the body of for loops, if statements, etc. or process only top-level assignments.}
  \item{definitions}{a list of functions that provides the definitions
    for functions that are not available on the search path. These are
    used in \code{\link[base]{match.call}}.}
  \item{pred}{a function that takes two arguments - a call and a logical value indicating if the
     function being called is a symbol/name or itself a call - and returns a logical scalar value indicating
     whether to collect and return the specific call. The }
   \item{\dots}{additional arguments passed to the \code{pred} function,
     and individual named-arguments in call to \code{getIndirectCallFunList}}
   \item{.els}{a named-list identifying functions that have a parameter/formal argument
     that is expected to be a function that it will call, e.g., \code{lapply} and \code{FUN}.
   }
   % discuss this wrt to indexWalkCode and mkIsCallTo().  Could we pass the vector of functions
   % we want to match via the ...
   % mkCallWalkerPred(isCallTo, "foo")
   \item{isLHS}{a logical value. Intended to indicate whether the call \code{x}
     is the left hand side of an assignment. If so, since
     this is a call and not a name/symbol, use the replacement function form,
     e.g., \code{fun<-} when calling \code{\link[base]{match.call}}.
     This is not passed down yet from the functions that call \code{isCallTo} as part of the
     higher-level analyses.
   }
   \
}
\value{
  \code{findCallsTo} returns a list of the call language objects.

  \code{mkCallWalkerPred} returns a list with 3 elements which are functions:
  \item{leaf}{walks the different elements of the AST}
  \item{call}{processes a call language object and determines whether to add it to the results}
  \item{handler}{returns NULL}

 \code{isCallTo} returns a scalar logical value - \code{TRUE} or \code{FALSE}.
 
  If \code{argIndices} has length zero, a list of all the matching
  \code{Call} objects.
  Otherwise, a list of all the parameters in each matching call
  identified by \code{argIndices} either by position or name.

  \code{getIndirectCallFunList} returns a named character vector
  with the values being the parameter names and the names being
  the function with the parameter.
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


  f = system.file("sampleCode/source.R", package = "CodeAnalysis")
  a = findCallsTo(f, "source")
  b = findCallsTo(f, "source", skipIfFalse = FALSE)
  setdiff(b, a)


  # Regular expression

  z = findCallsTo(findCallsTo, rx = "is\\\\.")

  z = findCallsTo(findCallsTo, rx = "file.info")

  z = findCallsTo(findCallsTo, rx = "apply")


  # Compare matching any function name and no function names.
  z = findCallsTo(findCallsTo, rx = ".*")
  z2 = findCallsTo(findCallsTo)
  setdiff(z2, z)
  # The regular expression does not include the walker$ans() call
  # since the findCallsTo() with no function names includes all calls.


  

\dontrun{

    # Project from git@github.com:mespe/Variety_trial_analysis.git
  ff = list.files("~/Book/ExploreCode/Variety_trial_analysis/code", pattern = "\\.R$", full = TRUE)
  e = unlist(lapply(ff, parse), recursive = FALSE)
  k = findCallsTo(e)
  z = sapply(k, function(x) deparse(x[[1]]))
  sort(table(z), decreasing = TRUE)
}
}
\concept{meta-programming}
\concept{static analysis}
