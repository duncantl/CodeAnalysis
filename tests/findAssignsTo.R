library(CodeAnalysis)

foo =
function(x, y)
{
    z = 3
    
    x[10] = 1
    y$el = 3

    foo(y, "abc") = 100
    bar(y) = "xyz"
    attr(z, "bar") = TRUE

    y$a + sum(x) + z
}

stopifnot(length(findAssignsTo(foo, "y")) == 3)
stopifnot(length(findAssignsTo(foo, "x")) == 1)
stopifnot(length(findAssignsTo(foo, "z")) == 2)


###############

f = function(x) if(isEnv <- is.environment(x))  TRUE else FALSE
findAssignsTo(f, "isEnv")
stopifnot(length(findAssignsTo(f, "isEnv")) == 1)

# Check if {} changes anything
f = function(x) {
    if(isEnv <- is.environment(x))  TRUE else FALSE
}

#findAssignsTo(f, "isEnv")
