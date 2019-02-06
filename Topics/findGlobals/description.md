<!-- 
There is explorations/findGlobals.R and R/getGlobals
The former is the more recent. I broke it and this relates
to astTraverse in rstatic.
-->

# Finding Global/Non-local Variables and Functions

The function findGlobals() in the codetools package is a very useful function.
It identifies variables a function uses that are not defined within the
function, and also which other functions it calls.
We can use this to create the call graph (which function
calls which other functions) and also to find "free" variables,
typically to eliminate them.


The findGlobals() allows the caller to separate global
variables and functions used in the function.
However, the current implementation of findGlobals() has (at least) 
three issues that could be improved.
1. It reports terms such as stats::quantile simply as ::, 
 ignoring the namespace (stats) and the actual function being called (quantile).
 We'd like this to be reported as stats::quantile or some higher level object
 that indicates the nature of the expression.
2. It reports functions used in the *apply() functions as variables, e.g.,
  `lapply(x, mean)` reports mean as a variable and not as a function.
  This makes logical sense but we do know that mean is being used as a function
  here and this is often more useful.
3. It incorrectly treats variables defined after their use as locally defined,
   e.g.
```
f =
function(x)
{
   y = x + a
   a = 1
   y
}
```
does not report `a` as a global variable eventhough there is no binding for it
when it is used in the first expression.

None of these issues are clearly that important, as otherwise somebody would
have fixed findGlobals(). 


<!--
An additional issue with codetools is that its code indicates that the
programming interface is temporary (although that was at least a decade
ago). Furthermore, the interface is a little cryptic.  However, an increasing
number of people are "programming on the language" and reimplement their own
facilities for processing the language objects.
-->

In CodeAnalysis, we provide an alternative implementation of findGlobals()
which address the issues above.




