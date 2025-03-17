\name{mkModifyCodeWalker}
\alias{mkModifyCodeWalker}
\alias{genAddArgsToCalls}
\alias{genRemoveCode}
\alias{genRewriteVars}

\title{
Low-level Modifying Code Walker and Function Generators to Rewrite Code
}
\description{
  \code{mkModifyCodeWalker} generates a code-walker object
  that can be passed to \code{\link[codetools]{walkCode}}.
  Rather than analyzing code, this actively rewrites code and
  assembles  the results to create a modified version of the code
  passed to \code{\link[codetools]{walkCode}}.

  The other functions are simple generator-functions 
  that take information and return a function that
  does any rewriting of each code element it is passed.
  The result of these are passed as the \code{rewrite}
  argument of \code{mkModifyCodeWalker}.
  These are closure generators and helper functions.
}
\usage{
mkModifyCodeWalker(rewrite = function(x, ...) x, skipIfFalse = TRUE, mkLiteralMap = FALSE, ..., verbose = FALSE)
genAddArgsToCalls(funArgs)
genRemoveCode(pred)
genRewriteVars(names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rewrite}{a function that is called with
    the current language element (and the code walker object)
    and can return that object or a modified version of it
    to be inserted into the updated code.
    Alternatively, it can return \code{NULL} and this element  of the code
    will be removed.
  }
  \item{skipIfFalse}{a scalar logical indicating whether to skip or descend into \code{if(FALSE)}
  blocks of code.}
  \item{mkLiteralMap}{a scalar logical. This is likely to be moved. For now it helps avoid any
    errors in less-tested code.
   }
  \item{\dots}{additional arguments}
  \item{verbose}{a scalar logical controlling whether verbose messages about the processing are
    displayed on the console. Useful only for debugging/understanding.}

  \item{funArgs}{a list of character vectors, one for each of the functions to  whose calls
     need to be updated in the original function(s).
      Ideally these should be named character mapping the names of the new parameters to arguments.}
     % FIX.
  \item{pred}{a function that identifies which language objects (typically \code{call}s to remove.}
  \item{names}{a named-character vector. The names identify the names of current/existing variables
    and the corresponding values give the new names to which these should be mapped.}
}
%\details{}
\value{
  An object which is list of functions that can be passed to \code{\link[codetools]{walkCode}}.
}
\references{
  \code{codetools}
}
\author{
Duncan Temple Lang
}
\seealso{
  There are several higher level functions that use this low-level mechanism,
  e.g., \code{\link{extractFunctions}}, \code{\link{mkGlobalsLocal}}.
}
%\examples{}
\keyword{programming}
\concept{meta-progamming}
