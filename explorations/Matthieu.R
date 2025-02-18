# From https://github.com/duncantl/CodeDepends/issues/51
# 2 possible approaches depending on what the precise specification of what we want as
# the result in more general examples of code.
#
# This one finds the regular function calls and reports on those.


library(CodeAnalysis)
# From Mathieu + some additions.
library(CodeDepends)

expr1 <- quote(if(TRUE) {fun1(inp1)} else {fun2(input2)})
expr2 <- quote(if(file.exists("file.csv")) {read.csv("file2.csv")} else {write.csv("file2.csv")})    

if(FALSE) {
o = getInputs(expr1)
o@inputs
# inp1 and inut2 but together as a single character vector
}


expr0 <- quote(if(TRUE) fun1(inp1) else fun2(input2))
expr3 <- quote(if(TRUE)
                   fun1(inp1)
               else fun2(input2) + fun3(input3, 1L) - 2)

expr4 <- quote(if(inp0)
                   fun1(inp1)
               else fun2(input2) + fun3(input3, 1L) - 2)

foo = 
function(e, omit = c("if", "for", "while", "{"),
          useFunNames  = TRUE)
{
    k = findCallsTo(e)
    # Can add others to omit. But note that ignore if, for and while
    # means we don't look at their conditions, e.g.,
    # see foo(expr4)
    # Similarly, if we have x + 1 and we discard + we'd miss x as an input.
    # We can handle calls to if, for and while specially and only discard {
    # For if and for and while, we might only process the 2nd element - the condition
    # and arrange to process the calls with the bodies using this function.
    # Depends what we really want. And we can control this with different parameters/formal arguments.
    w = ! sapply(k, isCallTo, omit) 
    k = k[ w ]
    ans = lapply(k, getInputs)
    # Putting the call as the names. Could put the name of the function being
    # called
    # Note that we are not guaranteed x[[1]] is a symbol as the function being called may
    # be a call itself, e.g. x$foo()
    names(ans) = if(useFunNames)
                     sapply(k, function(x) paste(deparse(x[[1]]), collapse = ""))
                 else
                     sapply(k, function(x) paste(deparse(x), collapse = ""))
    ans
}

if(FALSE) {
z = foo(expr1)
names(z)
lapply(z, slot, "inputs")


z = foo(expr0)
names(z)
lapply(z, slot, "inputs")

z = foo(expr2)
names(z)
lapply(z, slot, "strings")


z = foo(expr4)
}


