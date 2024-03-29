library(CodeAnalysis)

if(FALSE) {
    e = new.env(parent = getNamespace("base"))
    source("~/GitWorkingArea/CodeAnalysis/tests/getGlobals.R", e, echo = TRUE)
}

f = function(x = foo(1))
{
#    foo = function(val) 2*val + 1
    x + 2
}
gv = getGlobals(f)
stopifnot(identical(gv$variables, character()))
stopifnot(identical(gv$functions, c("+", "foo")))
# And the order is right as we see + before x which is its first argument.


f2 = function(x = foo(1))
{
    foo = function(val) 2*val + 1
    x + 2
}
gv = getGlobals(f2)
stopifnot(identical(gv$variables, character()))
stopifnot(identical(gv$functions, c("+", "+", "*")))



f3 = function(x = foo(1), y = global)
{
    foo = function(val) 2*val + 1
    global = 10
    x + y
}
gv = getGlobals(f3)
# global appears as a non-local variable but is actually local.
# y is not referenced until after global is defined as a local variable
stopifnot(identical(gv$variables, character()))
stopifnot(identical(unique(gv$functions), c("+", "*")))


f4 = function(x = foo(1), y = global)
{
    foo = function(val) 2*val + 1
#    global = 10
    x + y
}
gv = getGlobals(f4)
stopifnot(identical(gv$variables, "global"))
stopifnot(identical(unique(gv$functions), c("+", "*")))

#########


f =
    #
    # This is a good example as we see foo used in the default values
    # and defined before x is referenced. So foo should not be a global variable
    # We can extend getGlobals() to handle this, up to conditionals.
    
function(x = foo(globalVar), y = length(x))
{
   y = 1
   foo = function(w)
             w + y
   bar = function()
            y + a + g

   foo(x)
   a = 10
   z = bar()
}

g = 1; ans = f(1:3)
stopifnot(identical(ans, 12))

gv = getGlobals(f)
tmp = unique(gv$variables)
# Identifies g but not w since w is a parameter of foo.
# Note that it also identifies that a will be available for 
stopifnot(identical(tmp, c("globalVar", "g")))

# problem. Thinks foo is a global but actually should be defined by time x is used.
# This is because it processes the default values before the body of the function,
# not the first time each parameter is first used.
gv$functions

#XXX!!! Not  integrating global functions from functions defined in this.
#stopifnot(identical(gv$functions, c("length", "+")))




# If we use x before foo is defined, foo should become a global variable.
f2 = function(x = foo(globalVar), y = length(x))
{
   x + 1
   y = 1
   foo = function(w)
             w + y
   bar = function()
            y + a + g

   foo(x)
   a = 10
   z = bar()
}
gv = getGlobals(f2)
tmp = unique(gv$variables)



g =
function(a = x, b = y)
{
   x = 1
   y = 2
   a + b * (x + y)
}

getGlobals(g)$variables
tmp = substituteDefaultValues(g)
getGlobals(tmp, .ignoreDefaultArgs = TRUE)$variables

formals(tmp)[] = replicate(length(formals(tmp)), formals(getGlobals)[[1]], simplify = FALSE)
getGlobals(tmp)$variables

###################################################################
# These  are about identifying functions called indirectly via *apply(), outer, kronecker, do.call, etc.

m = function(x, y) mapply(sort, x, y)
tmp = getGlobals(m)
stopifnot(tmp$functions == c("mapply", "sort"))

m = function(x, y) mapply(x, y, FUN = sort)
tmp = getGlobals(m)
stopifnot(tmp$functions == c("mapply", "sort"))


df = function(...) do.call(rbind, list(...))
tmp = getGlobals(df)
stopifnot(tmp$functions == c("do.call", "rbind", "list"))


f = function(d)  aggregate(len ~ ., data = d, mean)
tmp = getGlobals(f)
stopifnot(tmp$functions == c("aggregate", "mean", "~"))  # why is ~ third?


# Shouldn't find f since a parameter.
ng = function(x, f) sapply(x, f, FALSE)
tmp = getGlobals(ng)
#[fixed] Includes f
stopifnot(identical(tmp$functions, "sapply"))
stopifnot(identical(tmp$variables, character()))
#??? Why is sapply() in the $variables output


ap = function(x) apply(x, 1, which.max)
tmp = getGlobals(ap)$functions
stopifnot(tmp == c("apply", "which.max"))


# Note the
f = function(x, y) outer(y, x, "^")
tmp = getGlobals(f)$functions
stopifnot(identical(tmp, c("outer", '"^"')))

f = function(x, y) outer(y, x, `^`)
tmp = getGlobals(f)$functions
stopifnot(identical(tmp, c("outer", "^")))

#####################################
# Tests for findCallsParam
if(exists("findCallsParam")) { # XXXX

cf = function(x, f) for(i in x) f(i)
tmp = findCallsParam(cf)
stopifnot(identical(tmp,  "f"))

cf = function(x, f) for(i in x) { x = f(i); y = g(x) }
tmp = findCallsParam(cf)
stopifnot(identical(tmp,  "f"))


getGlobals(cf)$functions
# Broken now with lazy evaluation of default parameters.
#[Done] Needs to skip for and {.
}


# Check to see if we call a fn and then assign over its name we
# capture that call
f = function(x) { g = g(x)}
tmp = getGlobals(f)$functions
stopifnot(identical(tmp, "g"))

# Here we assign to g before calling it so it is not a global. So this is empty, no globals.
# The code is also wrong as g is not a function, but getGlobals doesn't do that
# analysis.
f = function(x) { g = 1; g(x)}
tmp = getGlobals(f)$functions
stopifnot(identical(tmp, character()))

#
f = function(x) x + 1
fv = Vectorize(f)
tmp = getGlobals(fv)
stopifnot("names" %in% tmp$functions)
#[fixed] misses names()



f = function(x) if(FALSE) g(x) else x + 1
tmp = getGlobals(f)
stopifnot(identical(tmp$functions, "+"))

f = function(x) if(TRUE) g(x) else x + 1
tmp = getGlobals(f)
stopifnot(identical(tmp$functions, "g"))





##################

# Tests for getGlobalFunctions

f =
function(x, y)
{
    foo = function(a) 1 + bar(a)
    bar = function(x) 2*sin(x)

    foo(x) + bar(y)
}

# Should identify +, * and sin as global functions.
# Fix this. Why is the CodeDepends::getFunctionGlobals code referencing
# a generic function in CodeAnalysis!
if(FALSE) {
gv = getGlobals(f)
tmp = CodeDepends::getFunctionGlobals(gv, TRUE)
#  getGlobals = function(x) unlist(getGlobals(x)[c("functions", "variables")]))
stopifnot(identical(tmp, c("+", "*", "sin")))
}

gv = getGlobals(f, mergeSubFun = TRUE)
stopifnot(identical(unique(gv$functions), c("+", "*", "sin")))


# 
# $<-
f = function(x) {
      other$comment = var
      x$foo = 2
      foo(extra) = var2
      foo(extra)$element = var3      
    }
stopifnot(sort(unique(getGlobals(f)$variables)) == sort(c("var", "other", "extra", "var2", "var3")))


f = function(x, id) {
    x[["el"]] = var
    x[[ elName ]] = var2
    y[[ id ]] = x + 1
}
stopifnot(sort(unique(getGlobals(f)$variables)) == sort(c("var", "elName", "var2", "y")))



####
# See connectionsGlobalVars.R
f = function(x) {
    textConnection("bob", open = "w", local = TRUE)
    bob
}

stopifnot(length(getGlobals(f)$variables) == 0)

#######
f =
function(x)
{
    if(any(x < 0))
        x[x < 0 ] = a

    sum(x)
}

stopifnot(getGlobals(f)$variables == "a")


##

f =
function(x)
{
    if(any(w <- (x < 0)))
        x[w ] = 0.0

    sum(x)
}

stopifnot(identical(getGlobals(f)$variables, character()))

f =
function(x)
{
    if(any(w <- (x < epsilon)))
        x[w ] = 0.0

    sum(x)
}



####################################
#
# Bad code but getGlobals() doesn't flag this.
#
#XXXXX

# conditional evaluation 
f =
function(x)
{
    if(any(x < 0))
        a = 1

    sum(x) + a
}

stopifnot(getGlobals(f)$variables == "a")

# conditional evaluation 
f =
function(x)
{
    v = 0
    if(length(x) == 0 || any(w <- is.na(x)))
          v = sum(w)

    sum(x)
}




