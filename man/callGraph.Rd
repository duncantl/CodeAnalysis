\name{callGraph}
\alias{callGraph}
\alias{callGraph,FunctionsByFile-method}
\alias{callGraph,call-method}
\alias{callGraph,character-method}
\alias{callGraph,list-method}
\alias{callGraph,environment-method}
\alias{callGraph,function-method}
\alias{plot.CallGraphEdges}
\title{Create a Function Call Graph}
\description{
  This generic and methods creates a call graph (or data.frame of the caller and callee)
  identifying which function calls which other functions directly in its body
  or default values of the parameters.
}
\usage{
callGraph(obj, ...)
}
\arguments{
  \item{obj}{the object containing the function(s) of interest.
    This can be a function object, a list of functions,
    the name of a package, an environment/namespace}
  \item{\dots}{additional arguments passed to the methods}
}
\value{
  A data.frame with 2 columns. These specify the edges in the graph.
}
%\references{}
\author{
Duncan Temple Lang
}

\seealso{
\code{\link[CodeDepends]{makeCallGraph}}
}
\examples{

g = callGraph(quote(lm(mpg ~ wt + log(disp), mtcars)), recursive = 2)
plot(g)

g2 = as(g, "igraph")

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
