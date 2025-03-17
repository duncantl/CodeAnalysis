\name{usesDots}
\alias{usesDots}
\title{
Checks for Use of \dots.
}
\description{
  Find uses of \dots in a function.
}
\usage{
usesDots(fun)
}
\arguments{
  \item{fun}{a function}
}
\value{
  A list of the calls in the function that use
  \dots.
}
\author{Duncan Temple Lang}
%\seealso{}
\examples{
bar = 
function(x, y = rep(1, length(x)), ...)
{
    a = list(...)
    plot(x, y, ...)    
    if(a[1] > 2)
        text(x, y, ...)
        
}

d = usesDots(bar)

# In the default value of a formal argument/parameter
bar2 = 
function(..., .objs = list(...)) 
   sapply(.objs, class)

d = usesDots(bar2)
}
\keyword{progamming}
\concept{meta-programming}
