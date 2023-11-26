\name{findAssignsTo}
\alias{findAssignsTo}
\alias{isComplexAssignTo}
\alias{isSimpleAssignTo}
\alias{isAssignTo}
\title{Find assignments to variables in R code}
\description{
  \code{findAssignsTo} identifies assignments in code with
  the left-hand-side being one of a specified set of possible variables
  of interest.

  There are two types of assignments this function can detect.
  The simple/non-complex assignments are of the form 
  \code{x = val}, \code{x <- val} and \code{x -> val}
  where val can be a variable, a literal value or a call.

  Complex assigments are include calls such as
  \code{x[[1]][[i]] = val},
  \code{x$y$z = val}
  and \code{foo(x, a) = val}.
  These involve replacement functions.
}
\usage{
findAssignsTo(code, var = character(), complex = TRUE,
              pred = if (complex) isComplexAssignTo else isAssignTo)
isComplexAssignTo(x, var) 
isSimpleAssignTo(x, var = character())
}
\arguments{
  \item{code,x}{the R code/language object}
  \item{var}{the names of the variables being assigned. If this is empty for \code{findAssignsTo},
    all assignments are returned.}
  \item{complex}{a logical value. If \code{TRUE},
    the function includes both simple and complex assignments;  if \code{FALSE}
    only simple assignments  are considered.
    Complex assignments include expressions such as 
    is used to detect 
  }
  \item{pred}{the predicate function to identify the assignments of interest.
    One can specify a different predicate than \code{isComplexAssignTo} or
   \code{isAssignTo}.}
}
\value{
A list of the assignment calls found in the code.
}
\references{
R Language manual.
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link{findUnusedArgs}}, \code{\link{findUnusedAssignments}}
}
\examples{
  bar = function(x) {
     a = 1 + x
     b = 2 + x^2
     ans = a + b
     ans
  }
  findAssignsTo(bar, "a")
  findAssignsTo(bar, "b")
  findAssignsTo(bar, "ans")



  # simple and complex assignments
  foo = function(x, y) {
    z = 3
    
    x[10] = 1
    y$el = 3

    foo(y, "abc") = 100
    bar(y) = "xyz"
    attr(z, "bar") = TRUE

    x[[1]][[2]][[3]] = val
    x$y$z = val

    y$a + sum(x) + z
  }

  findAssignsTo(foo, "x")
  findAssignsTo(foo, "y")
  findAssignsTo(foo, "z")
}
\keyword{programming}
\concept{metaprogramming}

% From findCallsToFunctions.Rd before we moved to new findAssignsTo function

%egFun = findCallsTo
%findAssignsTo(egFun, "ans")
%findAssignsTo(egFun, "isEnv") # not correct.
%findCallsTo(egFun, walker = mkCallWalkerPred(function(x, ...) isCallTo(x, c("=", "<-")) && isSymbol(x[[2]], "isEnv")))
%findAssignsTo2(egFun, "code")
%findAssignsTo2(egFun, c("code", "code3"))
