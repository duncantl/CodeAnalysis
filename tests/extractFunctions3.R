library(CodeAnalysis)

g =
function(x)
{
    a = b = function(x) doit(x, "foo")
    formals(a)[[1]] = TRUE
    formals(b)[[1]] = FALSE
    list(a, b)
}

# Currently raises an error in removeNamedFunctionDefs
# It leaves a = b = function() as  a =  and no 3rd element.
# Could avoid the error, but it is a symptom of different issue.

# extractFunctions calls findNamedFunctions and gets b = function().
# We need to make extractFunctions/findNamedFunctions understand 2 things:
# + recognize that the function object is modified later in the function so shouldn't extract it.
# + b = function is the RHS of an assignment.
#
# A 'hack'/quick fix  is for our code-walker to identify when there are chained assignments
# and mark 
# 


stopifnot(length(findNamedFunctions(g)) == 0)

e = extractFunctions(g)
stopifnot(identical(e$newFun, g))


####


f = function() { FOO1 <- FOO2 <- function() 1}
h = function() { FOO1 <- FOO2 <- function() 1; g = function(x) x + 1; g(2)}

fn = findNamedFunctions(f)
stopifnot(length(fn) == 0)

fn = findNamedFunctions(h)
stopifnot(length(fn) == 1)

e2 = extractFunctions(h)

stopifnot("g" == names(e2$nested))

# findNamedFunctions(e2$newFun)
z = findAssignsTo(e2$newFun)
stopifnot(length(z) == 2)
a = sapply(z, function(x) as.character(x[[2]]))
stopifnot(all(c("FOO1", "FOO2") %in% a))



