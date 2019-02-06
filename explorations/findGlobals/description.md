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
three issues that could be improved:
1. it reports terms such as stats::quantile simply as ::, 
 ignoring the namespace (stats) and the actual function being called (quantile).
 We'd like this to be reported as stats::quantile or some higher level object
 that indicates the nature of the expression and allows us to identify that it is
 the quantile function in the stats package that is being called.
2. it reports functions used in the *apply() functions as variables, e.g.,
  `lapply(x, mean)` reports mean as a variable and not as a function.
  This makes logical sense but we do know that mean is being used as a function
  here and reporting it as a function call is typically more useful.
3. it incorrectly treats variables defined after their use as locally defined,
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
4. nested functions are not processed. One can argue that this make sense as the 
 nested functions are not actually evaluated and created until the top-level function
 is actually called. However, static code analysis can still be useful and correct.
 Consider the code 
```
m =
function(n)
{
    ans = 1
    function(x) {
       ans <<- ans + (x * n +  a)
    }
}
```
There is a global variable `a` being used within the nested function.


None of these issues are clearly that important, as otherwise somebody would
have fixed findGlobals().

An additional issue with codetools is that its code indicates that the
programming interface is temporary (although that was at least a decade
ago). Furthermore, the interface is a little cryptic.  However, an increasing
number of people are "programming on the language" and reimplement their own
facilities for processing the language objects.


In CodeAnalysis, we provide an alternative implementation of findGlobals() named getGlobals()
which address the issues above.






# Deficiencies of getGlobals()

There are some issues to fix with getGlobals().
Specifically when a nested function is defined and it refers to a non-local variable
that is later defined in the parent function, but only after the nested function is called.
For example,
```
f = function()
{
   g = function(x) x + a
   
   ans = g(10)
   
   a = 20
   
   ans
}
```
In this case, g is defined but a is not when g() is actually invoked.

getGlobals() identifies a as being a non-local variable for `g`.
However, it has no sense of order of evaluation of the expressions.
So the result is that we see a being defined as a local variable within `f`
and we cannot tell when `a` is created.
We need a richer model for this.

This becomes more complex when the variable is defined conditionally.
```
f = function(n)
{
   g = function(x) x + a
   
   ans = g(10)
   
   if(n > 10)
       a = 20
   
   ans
}
```
This is bad code. But that is exactly what we want to detect and correct.

getGlobals() returns no information about when or whether a is defined.
It reports if() is used, but does not connect this to the creation of a.

We could make getGlobals() treat if(), for(), while(), etc. statements like functions
and return information about the scope of variables.
This would not convey the 'when' a variable is created.

While these are issues, they are also issues with codetools, so we are not worse off
with getGlobals().
