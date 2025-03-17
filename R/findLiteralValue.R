# Instead of propagating the constants before analysis, for now we will
# find a Symbol and walk back through the script to see if we have a literal value
# This is for an rstatic AST object.



countNestedFunctionLevel =
    #
    # Given R language object find the functions and the functions within those and the functions ....
    #
function(code, count = 0)
{
    funs = getNestedFunctions(code)
    sapply(funs, getNestedDepth )
}

getNestedFunctions =
function(code, depth = 1L)
{
    # This is finding all the function() calls at all levels,
    # not just in the body of the current function.
    # We add 1 to maxFunDepth if this is a call - a call to function  - rather than
    # being is.function() and so of type closure.
    # The latter is at the top-level when dealing with a function. But we need an additional level
    # for calls to function() to get past the top-level.
    funs = findCallsTo(code, "function", maxFunDepth = 1 + is.call(code))
    funs = funs[!sapply(funs, identical, code)]
    
    lapply(funs, function(x) {
                    ans = list(this = x)
                    sub = getNestedFunctions(x, depth = depth + 1L)
                    if(length(sub))
                        ans$sub = sub
                    ans
                 })
}

getNestedDepth =
function(x, depth = 1L)
{
    if(length(x$sub))
        return(max(sapply(x$sub, getNestedDepth, depth = depth + 1L)))

    depth
}

