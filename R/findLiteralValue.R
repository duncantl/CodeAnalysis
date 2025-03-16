# Instead of propagating the constants before analysis, for now we will
# find a Symbol and walk back through the script to see if we have a literal value
# This is for an rstatic AST object.

findLiteralValue =
function(sym)
{
        # Assume an argument in an ArgumentList so get to the call.
   call = sym$parent$parent

   idx = where_is(asToplevelExpr(call))
   
   script = asScript(call)
   before = script$contents[rev(seq_len(idx - 1))]
   lit = sapply(before, function(x) is(x, "Assignment") && x$write == sym && is(x$read, "Literal"))
   if(any(lit))
       as_language(before[[ which(lit)[1] ]]$read)
   else
       sym
}

asScript =
function(x)
{
    while(!is.null(x$parent))
        x = x$parent
    x
}

asToplevelExpr =
function(x)
{
    if(is.null(x$parent))
        return(x)
    
    while(!is.null(x$parent$parent))
        x = x$parent
    x
}

asFunction =
function(x)
{
    while(!is.null(x)) {
        if(is(x, "Function"))
            return(x)
        x = x$parent
    }
    
    NULL
}


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

