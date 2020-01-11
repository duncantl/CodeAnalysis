library(CodeAnalysis)

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

#XXX problem. Thinks foo is a global but actually should be defined by time x is used.
gv$functions

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
tmp = getGlobals(m)$functions
stopifnot(tmp$functions == c("mapply", "sort"))

m = function(x, y) mapply(x, y, FUN = sort)
tmp = getGlobals(m)$functions
stopifnot(tmp$functions == c("mapply", "sort"))


df = function(...) do.call(rbind, list(...))
tmp = getGlobals(df)$functions
stopifnot(tmp$functions == c("do.call", "rbind", "list"))


f = function(d)  aggregate(len ~ ., data = d, mean)
tmp = getGlobals(f)$functions
stopifnot(tmp$functions == c("do.call", "rbind", "list"))


# Shouldn't find f since a parameter.
ng = function(x, f) sapply(x, f, FALSE)
tmp = getGlobals(ng)$functions
stopifnot(length(tmp) == 1 && tmp == "sapply")


ap = function(x) apply(x, 1, which.max)
tmp = getGlobals(ap)$functions
stopifnot(tmp == c("apply", "which.max"))


# Note the
f = function(x, y) outer(y, x, "^")
tmp = getGlobals(f)$functions
stopifnot(identical(tmp, c("outer", "^")))

f = function(x, y) outer(y, x, `^`)
tmp = getGlobals(f)$functions
stopifnot(identical(tmp, c("outer", "^")))

#
cf = function(x, f) for(i in x) f(i)
tmp = findCallsParam(cf)
stopifnot(identical(tmp$functions,  "f"))

cf = function(x, f) for(i in x) { x = f(i); y = g(x) }
tmp = findCallsParam(cf)
stopifnot(identical(tmp$functions,  "f"))

getGlobals(cf)$functions
#[Done] Needs to skip for and {.



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
fv = Vector(f)
tmp = getGlobals(fv)
#[fixed] misses names()



f = function(x) if(FALSE) g(x) else x + 1
tmp = getGlobals(f)
stopifnot(identical(tmp$functions, "+"))

f = function(x) if(TRUE) g(x) else x + 1
tmp = getGlobals(f)
stopifnot(identical(tmp$functions, "g"))

