# evaluates to what type - not an explicit return but can handle that too in as much as we can handle types at all
# in this narrow context.
# The context is that we are given a call during the walkCode().


# evalsToFunction just does the obvious and then calls getReturnType() if that didn't work.
# getReturnType() does a heuristic analysis.
# Need to turn this into some much more robust.
# But for now we are trying to handle the case of
#  f  = if(...) function() ... else function(...) ...
# Which is pretty simple.


# XXX There is an existing function named evalsToFunction in freeVariables.R
evalsToFunction2 =
function(x)
{
    if( isCallTo(x, "function") )
        return(TRUE)

    !is.na(ty <- getReturnType(x)) && ty == "function"
}


getReturnType =
function(x)    
{
    if(isLiteral(x))
        return(literalType(x))

    if(isCallTo(x, "function"))
        return("function")


    if(isSimpleAssignTo(x))
        return(getReturnType(x[[3]]))
    
    if(isCallTo(x, "{")) {
        y = as.list(x)[-1]
        last = y[[length(y)]]
        if(!is.na(ty <- getReturnType(last)))
            return(ty)

        # now need to look at the earlier calls in this {.
        # Missing context of calls before that. See if we can get those passed
        # to this during the walkCode()

        if(isSymbol(last)) {
            # look for assignments and updates within {, i.e. earlier elements

            vals = findAssignsTo(y[ - length(y) ], as.character(last))
            # this should probably be a for() loop and we get the return type
            # of the previous one and update it.
            ty = sapply(vals, getReturnType)
            browser()
        }
        
    }

    if(isCallTo(x, "if")) {
        # if call to "if", return the return types from all branches.

        tys = sapply(x[3:4], getReturnType)
        return(unique(tys))
    }

    # for while and for and repeat
    
    # if it is a call to a function that we can find()/getAnywhere(), we could
    # try to do type analysis on it.

    
    NA 
}
