\name{getArgFromCall}
\alias{getArgFromCall}
\alias{isFunAssign}
\alias{pkgSymbolsUsed}
\title{Accessor Functions}
\description{
  These are higher-level accessor functions for working
  with language objects and meta-programming
  so that we can talk about high-level operations
  and concepts and not the specific representation
  of R language objects.
}
\usage{
getArgFromCall(call, arg, asCharacter = TRUE)
}
\arguments{
  \item{call}{the R call language object}
  \item{arg}{the name of the argument to extract from the call}
  \item{asCharacter}{a logical value controlling whether to deparse
    the result or leave it is a language object}
}
\value{
 a character vector/string or a language object.
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
