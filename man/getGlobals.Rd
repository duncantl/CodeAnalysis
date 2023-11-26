\name{getGlobals}
\alias{getGlobals}
\alias{getGlobalFunctions}
\title{Compute information about uses of globals, etc. for a function}
\description{
 This is an alternative version of \code{\link[codetools]{findGlobals}}
 in  the \code{codetools} package for finding the names of
 global variables and functions used in the code of an R function.
 This version uses more context, has additional features we need, and
 also computes additional information that we use in compilation of R code in the \code{RLLVMCompile} package
 and which also may be useful for other purposes.
 
 I also find this function easier to understand and adapt as it is more
 direct. (However, I wrote it so of course I understand it!)
}
\usage{
getGlobals(f, expressionsFor = character(), .ignoreDefaultArgs = FALSE,
           skip = c(".R", ".typeInfo", ".signature", ".pragma", ".Internal", ".Primitive"),
           .debug = TRUE, .assert = TRUE, localVars = character(),
           mergeSubFunGlobals = TRUE, old = TRUE,
           indirectCallFunctions = names(getIndirectCallFunList()),
           handleTextConnections = TRUE)
}
\arguments{
  \item{f}{the function to be analyzed}
  \item{expressionsFor}{names of R functions we are interested in so
    that we collect all calls to those functions in the body of this function}
  \item{.ignoreDefaultArgs}{a logical value which controls whether we
 ignore or process the code in the default values of the
 parameters/formal arguments of the function.}
  \item{localVars}{this is not intended to be provided by the caller. a character vector}
  \item{skip}{a character vector that allows us to ignore calls to
 particular functions, e.g. \code{.R} or \code{.debug}. These are
 typically functions that will be handled in a special way by, e.g., a compiler.}
  \item{.debug}{whether ignore calls to \code{.debug} or another
 debugging function. This can be a logical value to enable/disable
 skipping \code{.debug} calls, or it can be the name of another
 function, e.g. \code{"myDebug"} which we will  add to the
 \code{skip} vector.}
  \item{.assert}{whether ignore calls to \code{.assert} or another
 assertion function. This can be a logical value to enable/disable
 skipping \code{.assert} calls, or it can be the name of another
 function, e.g. \code{"assert_that"} which we will  add to the
 \code{skip} vector.}
 \item{mergeSubFunGlobals}{a logical scalar value that
   controls whether we merge the global functions used in any function
  defined within this functions \code{f} into the answer or keep them separate.}
\item{old}{ a logical value controlling whether we use the old mechanism or the new mechanism.
  Will be removed in the future when we decide on correct approach.}
 \item{indirectCallFunctions}{a character vector specifying the names of
    functions which treat one of the arguments as a function, e.g.,
    \code{\link[base]{do.call}}, \code{\link[base]{lapply}}.}
  \item{handleTextConnections}{a logical value that controls whether we treat
    calls to \code{\link[base]{textConnection}} such as
    \code{textConnection("bob", open = "w", local = TRUE)}.
    This creates a local variable named \code{bob} and if code uses that
    (rather than \code{\link[base]{textConnectionValue}})
    that is a legitimate local variable.
  }
}
\value{
  A list of class \code{GlobalUses} with numerous elements:
  \item{localVariables}{the names of all the variables that are assigned
 in the body of the function.}  
  \item{variables}{the names of non-local/global variables referenced in
 this code.}
  \item{functions}{the names of non-local/global functions referenced in
 this code.}
  \item{variablesByFun}{a list with an element for each globally
 referenced function. Each element is a frequency table of the
 symbols (i.e., variables/functions) that are in the calls for this
 function. In other words, these are the symbols used in the calls to
 the external function. }
  \item{expressions}{a list with an element for each function named in
 \code{expressionsFor} that was actually invoked in the body of the
 function. Each such element is a collection of the expressions which
 called that function in \code{expressionsFor}.}
  \item{subFunctions}{a list with an element for each of the nested functions
 defined within the body of this code. Each element is the result from
 calling \code{getGlobals} on that function definition.}
  \item{skippedExpressions}{a list containing all of the call
        expressions that were skipped based on the call contents of \code{skip}.}
}
\author{Duncan Temple Lang}


\seealso{
\code{\link[codetools]{findGlobals}}
}
\examples{
g =
function(a = x, b = y)
{
   x = 1
   y = 2
   a + b * (x + y)
}

getGlobals(g)$variables

tmp = substituteDefaultValues(g)
getGlobals(tmp, .ignoreDefaultArgs = TRUE)$variables


#############

account =
function(balance = 0L)
{
    deposit = function(amt)
                  balance <<- balance + amt
    withdrawl = function(amt)
                    balance <<- balance - amt

    list(deposit = deposit, withdraw = withdrawl,  balance = function() balance)
}
g
}
\keyword{programming}
\concept{code analysis}
