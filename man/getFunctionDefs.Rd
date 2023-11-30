\name{getFunctionDefs}
\alias{getFunctionDefs}
\alias{getFunctionDefs,call-method}
\alias{getFunctionDefs,function-method}
\alias{getFunctionDefs,expression-method}
\alias{getFunctionDefs,character-method}
\alias{getFunctionDefs,environment-method}
\alias{getFunctionDefs,list-method}
\alias{getFunctionDefs,complex-method}
\alias{getFunctionDefs,integer-method}
\alias{getFunctionDefs,numeric-method}
\alias{getFunctionDefs,logical-method}
\alias{getFunctionDefs,name-method}
\alias{getFunctionDefs,NULL-method}
\alias{getFunctionDefs,if-method}
\alias{getFunctionDefs,while-method}
\alias{getFunctionDefs,for-method}
\alias{getFunctionDefs,\{-method}
\alias{getFunctionDefs,(-method}
\alias{getFunctionDefs,=-method}
\alias{getFunctionDefs,<--method}
%\alias{findFunctionDefs}
\alias{getPkgFunctions}
\title{
Get function definitions in R code files, functions, environments, namespaces and packages.
}
\description{
  These functions use static analysis (rather than \code{\link[base]{source}}'ing and evaluating the
  code)  to find function definitions in the specified R code.
  The code can be an individual R code file,
  a directory,  an environment,
  a list (of functions), 
  or an individual function,
  an expression, a call (language) object.

  These skip/ignore code within \code{if(FALSE)} clauses.
  
  \code{getFunctionDefs} can optionally recursively process the body of the
  functions to find function definitions contained there.
  \code{findFunctionDefs} finds only top-level functions.
  

  \code{getFunctionDefs} reads the code sequentially as if evaluating it
  and replaces/reassigns functions assigned to the same variable name.
  In contrast, \code{findFunctionDefs} collects all function
  definitions, including those assigned to the same variable name.

  \code{findFunctionDefs} knows about functions that return functions,
  e.g., \code{\link[base]{Vectorize}}, and can be told about others.
  It will then include top-level assignments in the results
  when they are created by calls to such functions.
  \code{getFunctionDefs} does not currently do this.


  At some point in the future, we may merge the functionality of
  \code{findFunctionDefs} into \code{getFunctionDefs}.

  \code{getPkgFunctions} is shorter than \code{getFunctionDefs(getNamespace(pkgName))}
  and/but does currenty load the package.
}
\usage{
getFunctionDefs(x, ...)
getPkgFunctions(pkg)
}
\arguments{
  \item{x}{a character vector of file or directory names or  a language
    object from parsing R code}
  \item{pkg}{the name of the installed package as a character vector of length 1.}
  \item{\dots}{additional arguments for methods.
    Additional arguments include \code{recursive} that controls
    whether the bodies of top-level functions are
    searched for function definitions, both anonymous and assigned functions.
  }
}
\value{
A list of top-level functions defined in the given code.
}
\author{Duncan Temple Lang}

%\seealso{}
\examples{

  # A package namespace/environment
  fns = getFunctionDefs(getNamespace("base"))
  fns = getFunctionDefs(.BaseNamespaceEnv)

  # A file
  f = system.file("sampleCode/sampleFuns.R", package = "CodeAnalysis")
  
  f1 = getFunctionDefs(f)
  # Find nested functions
  f2 = getFunctionDefs(f, recursive = TRUE)
  f2[[3]]
  attributes(f2)

  # An environment
  e = new.env()
  source(f, e)
  f3 = getFunctionDefs(e)

  # a function so find nested functions
  nested = getFunctionDefs(tools:::.check_packages)
  length(nested)

  # An expression - same as from reading the file directly
  # but if we start from an expression ....
  ex = parse(f)
  f4 = getFunctionDefs(ex)
  f5 = getFunctionDefs(ex, recursive = TRUE)

  # A call
  getFunctionDefs(quote(x <- function(x, y) x + y))
  getFunctionDefs(quote(function(x, y) x + y))

  # A call that creates a function, but is not an actual function until evaluated.
  getFunctionDefs(quote(function(x)  function(mu, sd) prod(dnorm(x, mu, sd))))
  getFunctionDefs(quote(function(x)  function(mu, sd) prod(dnorm(x, mu, sd))), recursive = TRUE)

  ifCall = quote(if(x< 2) function(mu, sd) prod(dnorm(x, mu, sd)))
  getFunctionDefs(ifCall)

  whileCall = quote(while( x < 2) {
                     optim(c(0, 1), function(mu, sd) prod(dnorm(x, mu, sd)))
                    })
  getFunctionDefs(whileCall)


  #XXX better example
  forCall = quote(for(i in x) {
                      f = function(a) a + i
                      f(10)
                    })
  getFunctionDefs(forCall)
}
\concept{meta-programming}
\keyword{programming}

%  tmp = list(f = function(x) x + 1, g = function(x,y) x + y)
%  getFunctionDefs(
