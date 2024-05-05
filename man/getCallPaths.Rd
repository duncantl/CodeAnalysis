\name{getCallPaths}
\alias{getCallPaths}
\title{Get all call paths/sequences to a function}
\description{
  Given a call graph of direct calls, this function
  computes all possible complete call paths/sequences
  to this function from top-level entry points to
  the target function.
  It does this by recursively traversing from
  what functions called the target, and what functions
  called those functions, ...
}
\usage{
getCallPaths(fun, callGraph, recursive = TRUE, cur = "", depth = 1, asString = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{the name of the target function}
  \item{callGraph}{a data.frame of the direct calls
    in all of the code identifying which function directly calls which other
    function(s).}
  \item{recursive}{a logical value}
  \item{cur}{a character string, used for communicating state between recursive calls
    and not specified by the caller.}
  \item{depth}{an integer value giving the depth of the recursion of this function, with 1 being
    the initial top-level call.}
  \item{asString}{a scalar logical value controlling whether to return
    each path as a single string of the form "A;B;C",
    or a character vector of the form \code{c("A", "B", "C")}.
    }
}
%\details{}
\value{
  A list.
}
\author{Duncan Temple Lang}

\seealso{
\code{\link{callGraph}}
}
\examples{
#
#  A -> B -> C -> fn
#
#  X -> Y -> fn

gr0 =  c("A", "B",
         "B", "C",
         "C", "fn",
         "X", "Y",
         "Y", "fn"
         )
gr1 = as.data.frame(matrix(gr0, , 2, byrow = TRUE))
getCallPaths("fn", gr1)

#
# Add D -> C to give
#            D
#             \
#    A -> B -> C -> fn

gr2 = c(gr0, "D", "C")
gr3 = as.data.frame(matrix(gr2, , 2, byrow = TRUE))
getCallPaths("fn", gr3)


# Now add

#   H
#    \ 
#     G
#      \
#       F    D
#        \    \
#    A -> B -> C -> fn

gr4 = c(gr2, "F", "B", "G", "F", "H", "G")
gr5 = as.data.frame(matrix(gr4, , 2, byrow = TRUE))
getCallPaths("fn", gr5)        
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{programming}
\concept{metaprogramming}
