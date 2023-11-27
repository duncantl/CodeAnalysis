# Instructions from when this was in R/functionsReturningFunctions.R and we were testing directly.
# To define these functions
# eval(parse("functionsReturningFunctions.R")[[1]][[3]])
# To run tests
# lapply(as.list(parse("functionsReturningFunctions.R")[[2]][[3]])[-1], eval)
#

# Vectorize() is an interesting example. The last expression is
# (function() {  var <- function(){ ...} ; formals(var) ...  ; var })()
# It is the body of the outer function that returns the function.
# So get the return value. There are 2. Interested in the second.
# It is a call(). The called object is a call to function.
# So creating that function and then calling it.
# Pass that to returnsFunction.  But have to evaluate it to make it an actual function, not a call to function.
# Have to drill through the ().
# See returnsFunction() now.

library(CodeAnalysis)

lik = function(x)
    function(mu, sd)
        prod(dnorm(x, mu, sd))

f2 = function(x)
    Vectorize(bar)

f3 = function(x) {
    if(length(x) > 3) {
        y = x[-(1:3)]
        function(z)
            prod(z + y)
    } else
        sum
}

f3.5 = function(x) {
    sum = 2
    if(length(x) > 3) {
        y = x[-(1:3)]
        function(z)
            prod(z + y)
    } else
        sum
}    

f4 = function(x) {
    f = function(x, y) x + y
    attr(f, "class") = "SpecialFunction"
    f
}

f5 = function(x) {
    f = function() {}
    formals(f)$a = 10
    formals(f)$b = 2
    body(f)[[2]] = quote(a^b)
    attr(f, "class") = "SpecialFunction"
    f
}

f6 = function() x + y


stopifnot(length(returnsFunction(lik) ) == 1)
stopifnot(length(returnsFunction(f2, functionsReturningFunctions = "Vectorize")) == 1)
stopifnot(length(returnsFunction(f3)) == 2)  # inline & sum
stopifnot(length(returnsFunction(f3.5)) == 1)  #just the inline definition
stopifnot(length(returnsFunction(f4)) == 1) # 
stopifnot(length(returnsFunction(f5)) == 1) # 
stopifnot(length(returnsFunction(f6)) == 0)    
