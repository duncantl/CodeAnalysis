\name{freeVariables}
\alias{freeVariables}
\title{Find undefined variables in top-level R code/script}
\description{
  This statically analyzes the code and finds variables that may be used
  directly but that are not previously defined.
  This optionally can take into account code that is \code{\link[base]{source}}d,
   variables that are available via the code loading packages, and variables that are created via \code{\link[base]{load}}.
  This is analogous to   \code{\link{getGlobals}} and
  \code{\link[codetools]{findGlobals}} but for R scripts rather than functions.
 }
\usage{
freeVariables(sc, load = TRUE, packages = TRUE, includeSource = TRUE,
              exclude = getSearchPathVariables(), inputs = getInputs(sc))
}
\arguments{
  \item{sc}{the R code, either created via \code{\link[base]{parse}} or
    the name(s) of a file or directory containing R code.}
  \item{load}{a logical value controlling whether to read files that are
  \code{\link[base]{load}}ed in the code to determine the names of the
  variables that would be created.}
  \item{packages}{a logical value that controls whether to determine the
  names of the variables that become available to the code when a packge
is loaded via \code{\link{library}} or \code{\link{require}} calls in
the code.}
  \item{includeSource}{a logical value controlling whether 
   this function processes calls to \code{\link[base]{source}} in the code analysis by
    replacing the call to \code{source} with the contents of the file that would be
     \code{source}'d.}
  \item{exclude}{which variables to exclude from the undefined
    variables, but treat as being externally defined.}
  \item{inputs}{typically not provided but the \code{ScriptInfo}
    computed on the code after \code{source} commands have been
    substituted with the actual code from those R files.}
}
\value{
a character vector
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{getGlobals}}
  \code{\link[codetools]{findGlobals}}
}
\examples{
}
\concept{meta-programming}
\concept{static analysis}
